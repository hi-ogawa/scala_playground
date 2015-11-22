import org.scalameter.api._
import org.scalameter.picklers.noPickler._

import net.hiogawa.playground.Stuff._


object Playground0Bench extends Bench.LocalTime {

  val list = List.range(0, 100000)

  val f : Int => Int => Option[Int] =
    memo => elem => {
      (memo + elem) match {
        case sum if sum >= 10 => None
        case sum              => Some(sum)
      }
    }

  performance of "folding list" in {
    measure method "foldLeftOption" in {
      using (Gen.unit("")) in {
        _ => foldLeftOption(list, 0, f)
      }
    }

    measure method "foldLeftOpt which use (optimized) version of foldLeft internally" in {
      using (Gen.unit("")) in {
        _ => foldLeftOpt(list, 0, f)
      }
    }

    measure method "naive implementation" in {
      using (Gen.unit("")) in {
        _ => {
          list.foldLeft(0)((memo, elem) => memo + elem) match {
            case sum if sum >= 10 => None
            case sum              => sum
          }
        }
      }
    }
  }
}
