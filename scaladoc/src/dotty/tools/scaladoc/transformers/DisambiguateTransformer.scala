package dotty.tools.scaladoc
package transformers

object DisambiguateTransformer {
  object A
}

class X {
  object A
}

class DisambiguateTransformer(using DocContext) extends (Module => Module):
  override def apply(original: Module): Module =
    original.updateMembers {
      case m if m.needsOwnPage => disambiguateMember(original, m)
      case m => m
    }

  object A2

  val x = DisambiguateTransformer.A
  val y = X().A

  extension (x: TermParameterList) {
    def flattenSignatures = x.parameters.map(_.signature)
  }

  private def signaturesWithinKind(k: Kind): Seq[Signature] =
    val sigs = k match {
      case Kind.Class(tys, args) => tys.map(_.signature) ++ args.flatMap(_.flattenSignatures)
      case Kind.Trait(tys, args) => tys.map(_.signature) ++ args.flatMap(_.flattenSignatures)
      case Kind.Enum(tys, args) => tys.map(_.signature) ++ args.flatMap(_.flattenSignatures)
      case Kind.Def(args) => args.flatMap { either =>
        either
          .map(_.map(_.signature))
          .swap
          .map(_.flattenSignatures)
          .fold(identity, identity)
      }
      case Kind.Extension(ExtensionTarget(_, tys, args, sig, _, _), defkind) =>
        sig +: tys.map(_.signature) ++: args.flatMap(_.flattenSignatures)
        ++: signaturesWithinKind(defkind)
      case Kind.Constructor(kind) => signaturesWithinKind(kind)
      case Kind.Exported(kind) => signaturesWithinKind(kind)
      case Kind.Type(_, _, typeParams, _) => typeParams.map(_.signature)
      case Kind.Given(kind, sig, _) => sig ++: signaturesWithinKind(kind)
      case Kind.Implicit(kind, _) => signaturesWithinKind(kind)
      case Kind.EnumCase(kind) => signaturesWithinKind(kind)
      case Kind.Var | Kind.Val => Nil
      case Kind.RootPackage | Kind.Package | Kind.Object => Nil
      case Kind.Unknown => Nil
    }
    sigs.distinct

  private def signaturesWithinMember(m: Member): Seq[Signature] = {
    m.signature
      +: signaturesWithinKind(m.kind)
      ++: m.directParents.map(_.signature)
      ++: m.parents.map(_.signature)
      ++: m.selfType.toSeq.map(_.signature)
      ++: m.knownChildren.map(_.signature)
  }

  private def disambiguateMember(original: Module, m: Member): Member = {
    if (!m.fullName.contains("DisambiguateTransformer")) {
      return m
    }

    println("hi " + m)

    val sigs = signaturesWithinMember(m) ++: m.members.flatMap(signaturesWithinMember)

    println(sigs.filter(_.getName == ""))

    println(sigs.map(_.getName).mkString("\n"))

    val grouped = sigs
      .flatMap(_.collect {
        case Type(name, Some(dri)) => (name, dri)
      })
      .toSet
      .groupMap(_._1)(_._2)

    println(grouped.mkString("\n"))
    println(grouped.map { (k,v) => (k,v.toSeq.flatMap(v => original.members.get(v).map(_.fullName)))})

    // returns an association list of DRI to its disambiguated name.
    def disambiguateOneName(possible: Iterable[DRI]): Seq[(DRI, String)] = {
      if (possible.size <= 1) {
        return Nil
      }
      val (dris, names) = possible.toSeq.flatMap { v =>
        original.members.get(v)
          .map(_.fullName)
          .filter(_.nonEmpty)
          .map(v -> _)
      }.unzip

      val splitNames = names.map(_.split("\\.", -1).toSeq)
      val maxLen = splitNames.map(_.size).max

      val newNames = Iterator.range(0, maxLen).flatMap { tryLen =>
        val newNames = splitNames.map(_.takeRight(tryLen))
        Option.when(newNames.distinct.size == newNames.size) {
          newNames.map(_.mkString("."))
        }
      }.nextOption

      newNames match {
        case Some(newNames) => dris.zip(newNames)
        case _ => Nil
      }
    }

    println(grouped.map(x => (x._1, disambiguateOneName(x._2))))

    m
  }

