package zio.wasm.componentmodel

import zio.Chunk
import zio.wasm.*
import zio.wasm.internal.Graph

/** Describes dependencies between different component model sections */
object Dependencies {

  /** Gets a map of sections indexed by their unified index (as their index type is section type specific) and returns
    * the ordered list of these indexes.
    */
  def topologicalSort(sections: Map[SectionReference, Section[ComponentIndexSpace]]): Option[Chunk[SectionReference]] =
    Graph(
      Chunk
        .fromIterable(sections.keys)
        .map(idx => Graph.Edge(idx, idx)) ++
        dependencies(sections)
    ).topologicalSort.map { sorted =>
      // Filtering out dependencies that are not in the original input
      sorted.filter(sections.contains)
    }

  private def dependenciesOfExternDesc(
      idx: SectionReference,
      externDesc: ExternDesc
  ): Chunk[Graph.Edge[SectionReference]] =
    externDesc match {
      case ExternDesc.Module(typeIdx)                        =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ExternDesc.Func(typeIdx)                          =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ExternDesc.Val(ComponentValType.Defined(typeIdx)) =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ExternDesc.Type(TypeBound.Eq(typeIdx))            =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ExternDesc.Instance(typeIdx)                      =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ExternDesc.Component(typeIdx)                     =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case _                                                 =>
        Chunk.empty
    }

  private def dependenciesOfAlias(
      level: Int,
      idx: SectionReference,
      alias: Alias
  ): Chunk[Graph.Edge[SectionReference]] =
    alias match {
      case Alias.InstanceExport(_, instanceIdx, _)     =>
        Chunk(Graph.Edge(SectionReference.Instance(instanceIdx), idx))
      case Alias.CoreInstanceExport(_, instanceIdx, _) =>
        Chunk(Graph.Edge(SectionReference.Instance(instanceIdx), idx))
      case Alias.Outer(aliasKind, AliasTarget(l, i))   =>
        if (level == l) Chunk(Graph.Edge(aliasToReference(aliasKind, i), idx))
        else Chunk.empty
    }

  private def dependenciesOfComponentType(
      level: Int,
      idx: SectionReference,
      ct: ComponentType
  ): Chunk[Graph.Edge[SectionReference]] =
    ct match {
      case ComponentType.Defined(ComponentDefinedType.Record(fields))                            =>
        fields.flatMap {
          case (_, ComponentValType.Defined(typeIdx)) =>
            Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
          case _                                      =>
            Chunk.empty
        }
      case ComponentType.Defined(ComponentDefinedType.Variant(cases))                            =>
        cases.flatMap {
          case VariantCase(_, Some(ComponentValType.Defined(typeIdx)), _) =>
            Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
          case _                                                          =>
            Chunk.empty
        }
      case ComponentType.Defined(ComponentDefinedType.List(ComponentValType.Defined(typeIdx)))   =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ComponentType.Defined(ComponentDefinedType.Tuple(types))                              =>
        types.flatMap {
          case ComponentValType.Defined(typeIdx) =>
            Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
          case _                                 =>
            Chunk.empty
        }
      case ComponentType.Defined(ComponentDefinedType.Union(types))                              =>
        types.flatMap {
          case ComponentValType.Defined(typeIdx) =>
            Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
          case _                                 =>
            Chunk.empty
        }
      case ComponentType.Defined(ComponentDefinedType.Option(ComponentValType.Defined(typeIdx))) =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ComponentType.Defined(ComponentDefinedType.Result(maybeOk, maybeErr))                 =>
        (Chunk.fromIterable(maybeOk) ++ Chunk.fromIterable(maybeErr)).flatMap {
          case ComponentValType.Defined(typeIdx) =>
            Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
          case _                                 =>
            Chunk.empty
        }
      case ComponentType.Defined(ComponentDefinedType.Own(typeIdx))                              =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ComponentType.Defined(ComponentDefinedType.Borrow(typeIdx))                           =>
        Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
      case ComponentType.Func(ComponentFuncType(params, result))                                 =>
        params
          .flatMap {
            case (_, ComponentValType.Defined(typeIdx)) =>
              Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
            case _                                      =>
              Chunk.empty
          }
          .concat(result match {
            case ComponentFuncResult.Unnamed(ComponentValType.Defined(typeIdx)) =>
              Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
            case ComponentFuncResult.Named(types)                               =>
              types.flatMap {
                case (_, ComponentValType.Defined(typeIdx)) =>
                  Chunk(Graph.Edge(SectionReference.ComponentType(typeIdx), idx))
                case _                                      =>
                  Chunk.empty
              }
            case _                                                              =>
              Chunk.empty
          })
      case ComponentType.Component(declarations)                                                 =>
        declarations.flatMap {
          case ComponentTypeDeclaration.Type(ct)                               =>
            dependenciesOfComponentType(level + 1, idx, ct)
          case ComponentTypeDeclaration.Alias(alias)                           =>
            dependenciesOfAlias(level, idx, alias)
          case ComponentTypeDeclaration.Import(ComponentImport(_, externDesc)) =>
            dependenciesOfExternDesc(idx, externDesc)
          case ComponentTypeDeclaration.Export(_, externDesc)                  =>
            dependenciesOfExternDesc(idx, externDesc)
          case _                                                               =>
            Chunk.empty
        }
      case ComponentType.Instance(declarations)                                                  =>
        declarations.flatMap {
          case InstanceTypeDeclaration.Type(ct)              =>
            dependenciesOfComponentType(level + 1, idx, ct)
          case InstanceTypeDeclaration.Alias(alias)          =>
            dependenciesOfAlias(level, idx, alias)
          case InstanceTypeDeclaration.Export(_, externDesc) =>
            dependenciesOfExternDesc(idx, externDesc)
          case _                                             =>
            Chunk.empty
        }
      case ComponentType.Resource(_, Some(destructor))                                           =>
        Chunk(Graph.Edge(SectionReference.ComponentFunc(destructor), idx))
      case _                                                                                     =>
        Chunk.empty
    }

  private def dependenciesOfComponentImport(
      idx: SectionReference,
      ci: ComponentImport
  ): Chunk[Graph.Edge[SectionReference]] =
    dependenciesOfExternDesc(idx, ci.desc)

  private def dependenciesOfComponentExport(
      idx: SectionReference,
      ce: ComponentExport
  ): Chunk[Graph.Edge[SectionReference]] =
    ce.desc.map(dependenciesOfExternDesc(idx, _)).getOrElse(Chunk.empty) :+
      (ce.kind match {
        case ComponentExternalKind.Module    =>
          Graph.Edge(SectionReference.Module(ModuleIdx.fromInt(ce.idx)), idx)
        case ComponentExternalKind.Func      =>
          Graph.Edge(SectionReference.ComponentFunc(ComponentFuncIdx.fromInt(ce.idx)), idx)
        case ComponentExternalKind.Value     =>
          Graph.Edge(SectionReference.Value(ValueIdx.fromInt(ce.idx)), idx)
        case ComponentExternalKind.Type      =>
          Graph.Edge(SectionReference.ComponentType(ComponentTypeIdx.fromInt(ce.idx)), idx)
        case ComponentExternalKind.Instance  =>
          Graph.Edge(SectionReference.Instance(InstanceIdx.fromInt(ce.idx)), idx)
        case ComponentExternalKind.Component =>
          Graph.Edge(SectionReference.Component(ComponentIdx.fromInt(ce.idx)), idx)
      })

  private def dependenciesOfComponentInstance(
      idx: SectionReference,
      ci: ComponentInstance
  ): Chunk[Graph.Edge[SectionReference]] =
    ci match {
      case ComponentInstance.Instantiate(componentIdx, args) =>
        Chunk.single(
          Graph.Edge(SectionReference.Component(componentIdx), idx)
        ) ++
          args.flatMap { case ComponentInstantiationArg(name, desc) =>
            dependenciesOfExternDesc(idx, desc)
          }

      case ComponentInstance.FromExports(exports) =>
        // TODO
        Chunk.empty
    }

  private def aliasToReference(aliasKind: OuterAliasKind, i: Int): SectionReference =
    aliasKind match {
      case OuterAliasKind.CoreModule =>
        SectionReference.Module(ModuleIdx.fromInt(i))
      case OuterAliasKind.CoreType   =>
        SectionReference.CoreType(TypeIdx.fromInt(i))
      case OuterAliasKind.Type       =>
        SectionReference.ComponentType(ComponentTypeIdx.fromInt(i))
      case OuterAliasKind.Component  =>
        SectionReference.Component(ComponentIdx.fromInt(i))
    }

  private def dependencies(
      sections: Map[SectionReference, Section[ComponentIndexSpace]]
  ): Chunk[Graph.Edge[SectionReference]] =
    Chunk
      .fromIterable(sections)
      .collect {
        case (idx, ct: ComponentType)     =>
          dependenciesOfComponentType(1, idx, ct)
        case (idx, ci: ComponentImport)   =>
          dependenciesOfComponentImport(idx, ci)
        case (idx, alias: Alias)          =>
          dependenciesOfAlias(1, idx, alias)
        case (idx, ce: ComponentExport)   =>
          dependenciesOfComponentExport(idx, ce)
        case (idx, ci: ComponentInstance) =>
          dependenciesOfComponentInstance(idx, ci)
      }
      .flatten
}
