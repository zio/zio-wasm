package zio.wasm.internal

import zio.test.*
import zio.*

import Graph.Edge

object GraphSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Graph")(
      test("topological sort") {
        val g1 = Graph(
          Chunk(
            Edge("a", "a"),
            Edge("b", "b"),
            Edge("c", "c"),
            Edge("d", "d"),
            Edge("e", "e"),
            Edge("a", "b"),
            Edge("a", "c"),
            Edge("d", "b"),
            Edge("c", "d")
          )
        )

        val g2 = Graph(
          Chunk(
            Edge("a", "b"),
            Edge("b", "c"),
            Edge("c", "a")
          )
        )

        assertTrue(
          g1.topologicalSort == Some(Chunk("e", "a", "c", "d", "b")),
          g2.topologicalSort == None
        )
      }
    )
}
