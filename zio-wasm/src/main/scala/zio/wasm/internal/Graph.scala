package zio.wasm.internal

import zio.Chunk
import Graph.Edge

import scala.annotation.tailrec

final case class Graph[A](edges: Chunk[Edge[A]]) {
  lazy val vertices: Set[A] = edges.foldLeft(Set.empty[A]) { case (acc, Edge(from, to)) =>
    acc + from + to
  }

  def topologicalSort: Option[Chunk[A]] = {
    @tailrec
    def kahn(edges: Chunk[Edge[A]], ordered: Chunk[A], unordered: Chunk[A]): Option[Chunk[A]] =
      unordered.headOption match {
        case None       =>
          if (edges.isEmpty) Some(ordered) else None
        case Some(node) =>
          val tail                        = unordered.tail
          val (matchingEdges, otherEdges) = edges.partition(_.from == node)
          val insert                      = matchingEdges.map(_.to).filterNot(n => otherEdges.map(_.to).contains(n))
          kahn(otherEdges, ordered :+ node, tail ++ insert)
      }
    kahn(edges, Chunk.empty, Chunk.fromIterable(vertices.filterNot(n => edges.map(_.to).contains(n))))
  }
}

object Graph {
  final case class Edge[A](from: A, to: A)

}
