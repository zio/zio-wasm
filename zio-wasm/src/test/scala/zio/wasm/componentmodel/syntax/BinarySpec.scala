package zio.wasm.componentmodel.syntax

import zio.{Chunk, ZIO}
import zio.parser.*
import zio.test.{Spec, TestAspect, ZIOSpecDefault, assertCompletes, assertTrue, check}
import zio.wasm.{Name, Url}
import zio.wasm.componentmodel.{AstGen, Component, ComponentExport, ComponentExternalKind, ExternName, Instance}
import zio.wasm.syntax.BinarySpec.suite
import zio.wasm.syntax.Binary as WasmBinary

object BinarySpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] =
    suite("Component Model Binary syntax")(
      test("alias") {
        check(AstGen.alias) { alias =>
          for {
            bytes <- ZIO.fromEither(Binary.alias.print(alias))
            result = Binary.alias.parseChunk(bytes)
          } yield assertTrue(result == Right(alias))
        }
      },
      test("core type") {
        check(AstGen.coreType) { coreType =>
          for {
            bytes <- ZIO.fromEither(Binary.coreType.print(coreType))
            result = Binary.coreType.parseChunk(bytes)
          } yield assertTrue(result == Right(coreType))
        }
      },
      test("core instance") {
        check(AstGen.instance) { coreInstance =>
          for {
            bytes <- ZIO.fromEither(Binary.instance.print(coreInstance))
            result = Binary.instance.parseChunk(bytes)
          } yield assertTrue(result == Right(coreInstance))
        }
      },
      test("component external kind") {
        check(AstGen.componentExternalKind) { componentExternalKind =>
          for {
            bytes <- ZIO.fromEither(Binary.componentExternalKind.print(componentExternalKind))
            result = Binary.componentExternalKind.parseChunk(bytes)
          } yield assertTrue(result == Right(componentExternalKind))
        }
      },
      test("component export") {
        check(AstGen.componentExport) { componentExport =>
          for {
            bytes <- ZIO.fromEither(Binary.componentExport.print(componentExport))
            result = (Binary.componentExport ~ Syntax.end).parseChunk(bytes)
          } yield assertTrue(result == Right(componentExport))
        }
      },
      test("component import") {
        check(AstGen.componentImport) { componentImport =>
          for {
            bytes <- ZIO.fromEither(Binary.componentImport.print(componentImport))
            result = (Binary.componentImport ~ Syntax.end).parseChunk(bytes)
          } yield assertTrue(result == Right(componentImport))
        }
      },
      test("component instance") {
        check(AstGen.componentInstance) { componentInstance =>
          for {
            bytes <- ZIO.fromEither(Binary.componentInstance.print(componentInstance))
            result = Binary.componentInstance.parseChunk(bytes)
          } yield assertTrue(result == Right(componentInstance))
        }
      },
      test("component type") {
        check(AstGen.componentType) { componentType =>
          val bytes  = Binary.componentType.print(componentType)
          val result = bytes.flatMap(Binary.componentType.parseChunk)
          assertTrue(result == Right(componentType))
        }
      },
      test("canonical option") {
        check(AstGen.canonicalOption) { canonicalOption =>
          val bytes  = Binary.canonicalOption.print(canonicalOption)
          val result = bytes.flatMap(Binary.canonicalOption.parseChunk)
          assertTrue(result == Right(canonicalOption))
        }
      },
      test("canon") {
        check(AstGen.canon) { canon =>
          val bytes  = Binary.canon.print(canon)
          val result = bytes.flatMap(Binary.canon.parseChunk)
          assertTrue(result == Right(canon))
        }
      },
      test("start") {
        check(AstGen.componentStart) { start =>
          val bytes  = Binary.componentStart.print(start)
          val result = bytes.flatMap(Binary.componentStart.parseChunk)
          assertTrue(result == Right(start))
        }
      },
      test("full component") {
        check(AstGen.component) { component =>
          val bytes  = Binary.component.print(component)
          val result = bytes.flatMap(Binary.component.parseChunk)
          assertTrue(result == Right(component))
        }
      } @@ TestAspect.samples(10) @@ TestAspect.ignore, // TODO: Enable after the section refactor
      test("instance example #1") {
        /*
        (core instance (;1;)
          (export "fetch" (func 0))
        )
         */
        val bytes  = Chunk[Byte](0x1, // export
          0x1,  // count
          0x5,  // char count
          0x66, // f
          0x65, // e
          0x74, // t
          0x63, // c
          0x68, // h
          0x0,  // func
          0x0) // 0
        val result = Binary.instance.parseChunk(bytes)
        assertTrue(result.isRight)
      },
      test("instance example #2") {
        /*
        (core instance (;13;)
            (export "$imports" (table 0))
            (export "0" (func 22))
            (export "1" (func 23))
            (export "2" (func 24))
            (export "3" (func 25))
            (export "4" (func 26))
            (export "5" (func 27))
            (export "6" (func 28))
            (export "7" (func 29))
            (export "8" (func 30))
            (export "9" (func 31))
            (export "10" (func 32))
            (export "11" (func 33))
          )
          (core instance (;14;) (instantiate 3
              (with "" (instance 13))
            )
          )*/
        val bytes  = Chunk[Byte](
          0x2,                                            // two instances
          0x1,                                            // first instance: fromexports
          0xd,                                            // export count (13)
          0x8,                                            // char count
          0x24, 0x69, 0x6d, 0x70, 0x6f, 0x72, 0x74, 0x73, // $imports
          0x1,                                            // table
          0x0,                                            // table idx
          0x1,                                            // char count
          0x30,                                           // "0"
          0x0,                                            // func
          0x16,                                           // func idx
          0x1, 0x31, 0x0, 0x17,                           // "1"
          0x1, 0x32, 0x0, 0x18,                           // "2"
          0x1, 0x33, 0x0, 0x19,                           // "3"
          0x1, 0x34, 0x0, 0x1a,                           // "4"
          0x1, 0x35, 0x0, 0x1b,                           // "5"
          0x1, 0x36, 0x0, 0x1c,                           // "6"
          0x1, 0x37, 0x0, 0x1d,                           // "7"
          0x1, 0x38, 0x0, 0x1e,                           // "9"
          0x1, 0x39, 0x0, 0x1f,                           // "10"
          0x2, 0x31, 0x30, 0x0, 0x20,                     // "11"
          0x2, 0x31, 0x31, 0x0, 0x21,                     // "12"
          0x0,                                            // second instance: instantiate
          0x3,                                            // module idx
          0x1,                                            // number of args
          0x0,                                            // name length
          0x12,                                           // component
          0xd                                             // component id (13)
        )
        val result = WasmBinary.vec(Binary.instance).parseChunk(bytes)
        assertTrue(result.isRight)
      },
      test("component type example #1") {
        /* (type (;0;)
          (instance
            (type (;0;) u32)
            (export (;1;) "input-stream" (type (eq 0)))
            (type (;2;) (func (param "this" 1)))
            (export (;0;) "drop-input-stream" (func (type 2)))
            (type (;3;) u32)
            (export (;4;) "output-stream" (type (eq 3)))
            (type (;5;) (list u8))
            (type (;6;) (record))
            (export (;7;) "stream-error" (type (eq 6)))
            (type (;8;) (result u64 (error 7)))
            (type (;9;) (func (param "this" 4) (param "buf" 5) (result 8)))
            (export (;1;) "write" (func (type 9)))
            (type (;10;) (func (param "this" 4)))
            (export (;2;) "drop-output-stream" (func (type 10)))
          )
        )
         */

        val bytes =
          Chunk[Byte](0x42,                                                               // ComponentType.Instance
            0xe,                                                                          // count = 14
            0x1,                                                                          // InstanceTypeDeclaration.Type #0
            0x79,                                                                         // U32
            0x4,                                                                          // InstanceTypeDeclaration.Export #1
            0xc,                                                                          // name length (12)
            0x69, 0x6e, 0x70, 0x75, 0x74, 0x2d, 0x73, 0x74, 0x72, 0x65, 0x61, 0x6d,       // "input-stream"
            0x0,                                                                          // url length (0)
            0x3,                                                                          // ComponentExternalKind.Type
            0x0,                                                                          // TypeBounds.Eq
            0x0,                                                                          // type idx
            0x1,                                                                          // InstanceTypeDeclaration.Type #2
            0x40,                                                                         // ComponentType.Func
            0x1,                                                                          // function parameter count
            0x4,                                                                          // parameter name length
            0x74, 0x68, 0x69, 0x73,                                                       // "this"
            0x1,                                                                          // type idx
            0x1,                                                                          // ComponentFuncResult.Named
            0x0,                                                                          // result count
            0x4,                                                                          // InstanceTypeDeclaration.Export #3
            0x11,                                                                         // name length (17)
            0x64, 0x72, 0x6f, 0x70, 0x2d, 0x69, 0x6e, 0x70, 0x75, 0x74, 0x2d, 0x73, 0x74, 0x72, 0x65, 0x61,
            0x6d,                                                                         // "drop-input-stream"
            0x0,                                                                          // url length
            0x1,                                                                          // ComponentExternalKind.Func
            0x2,                                                                          // componentFuncIdx
            0x1,                                                                          // InstanceTypeDeclaration.Type #4
            0x79,                                                                         // U32
            0x4,                                                                          // InstanceTypeDeclaration.Export #5
            0xd,                                                                          // name length (13)
            0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x2d, 0x73, 0x74, 0x72, 0x65, 0x61, 0x6d, // "output-stream"
            0x0,                                                                          // url length
            0x3,                                                                          // ComponentExternalKind.Type
            0x0,                                                                          // TypeBounds.Eq
            0x3,                                                                          // typeIdx
            0x1,                                                                          // InstanceTypeDeclaration.Type #6
            0x70,                                                                         // list
            0x7d,                                                                         // U8
            0x1,                                                                          // ComponentExternalKind.Type #7
            0x72,                                                                         // record
            0x0,                                                                          // field count
            0x4,                                                                          // InstanceTypeDeclaration.Export #8
            0xc,                                                                          // name length (12)
            0x73, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x2d, 0x65, 0x72, 0x72, 0x6f, 0x72,       // "stream-error"
            0x0,                                                                          // url length
            0x3,                                                                          // ComponentExternalKind.Type
            0x0,                                                                          // TypeBounds.Eq
            0x6,                                                                          // typeIdx
            0x1,                                                                          // ComponentExternalKind.Type #9
            0x6a,                                                                         // result
            0x1,                                                                          // Some
            0x77,                                                                         // U64
            0x1, 0x7, 0x1, 0x40, 0x2, 0x4, 0x74, 0x68, 0x69, 0x73, 0x4, 0x3, 0x62, 0x75, 0x66, 0x5, 0x0, 0x8, 0x4, 0x5,
            0x77, 0x72, 0x69, 0x74, 0x65, 0x0, 0x1, 0x9, 0x1, 0x40, 0x1, 0x4, 0x74, 0x68, 0x69, 0x73, 0x4, 0x1, 0x0,
            0x4, 0x12, 0x64, 0x72, 0x6f, 0x70, 0x2d, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74, 0x2d, 0x73, 0x74, 0x72, 0x65,
            0x61, 0x6d, 0x0, 0x1, 0xa)

        val result = Binary.componentType.parseChunk(bytes)
        assertTrue(result.isRight)
      },
      test("component example #1") {
        val component  = Component(
          modules = Chunk.empty,
          instances = Chunk.empty,
          coreTypes = Chunk.empty,
          components = Chunk.empty,
          componentInstances = Chunk.empty,
          types = Chunk.empty,
          aliases = Chunk.empty,
          canons = Chunk.empty,
          starts = Chunk.empty,
          imports = Chunk.empty,
          exports = Chunk(
            ComponentExport(
              ExternName(Name.fromString("0"), Url.fromString("0")),
              ComponentExternalKind.Module,
              0,
              None
            )
          ),
          custom = Chunk.empty
        )
        val bytes      = Binary.component.print(component)
        val component2 = bytes.flatMap { bs =>
          println(bs.map(b => f"$b%02x").mkString(" "))
          Binary.component.parseChunk(bs)
        }

        assertTrue(component == component2)
      } @@ TestAspect.ignore                            // TODO: Enable after the section refactor
    ) @@ TestAspect.samples(1000)
}
