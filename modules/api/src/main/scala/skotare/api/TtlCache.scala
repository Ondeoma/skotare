package skotare.api

import cats.effect.*
import cats.syntax.all.*

/**
 * 短い間だけ値を覚えておく（ノードごと）. 忘れる（forget・clear）たびに世代を進め、忘れる前に読み始めた値は書き戻さない
 * （変更の直後に、変更前の値が覚え直されないように）. 期限の切れたものは、書くときに捨てる.
 */
final class TtlCache[K, V](
  ttlMs: Long) {
  private val state = Ref.unsafe[IO, (Long, Map[K, (Long, V)])]((0L, Map.empty))

  def get(
    k: K
  )(
    load: IO[V]
  ): IO[V] =
    (IO.realTime.map(_.toMillis), state.get).flatMapN { case (now, (gen, m)) =>
      m.get(k) match {
        case Some((at, v)) if now - at < ttlMs => IO.pure(v)
        case _ => load.flatTap(v =>
            state.update { case (g, cur) =>
              if g != gen then (g, cur)
              else
                (
                  g,
                  cur.filter(
                    (
                      _,
                      x
                    ) => now - x._1 < ttlMs
                  ) + (k -> (now, v))
                )
            }
          )
      }
    }

  def forget(
    k: K
  ): IO[Unit] = state.update(
    (
      g,
      m
    ) => (g + 1, m - k)
  )
  def clear: IO[Unit] = state.update(
    (
      g,
      _
    ) => (g + 1, Map.empty)
  )
}
