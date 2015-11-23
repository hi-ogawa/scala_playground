// TODO: c b o doesn't work anymore (when trying seperated configuration for benchmark)
// package com.example.algorithm_practice

import org.specs2.mutable._

import net.hiogawa.playground.Stuff._

class StuffTest extends Specification {

  "calculates sum with upper limitation" >> {
    val f : Int => Int => Option[Int] =
      memo => elem => {
        (memo + elem) match {
          case sum if sum >= 60 => None
          case sum              => Some(sum)
        }
      }

    ".foldLeftOption" >> {
      foldLeftOption(List.range(1, 11), 0, f) must be_==(Some(55))
      foldLeftOption(List.range(1, 12), 0, f) must be_==(None)
    }

    ".foldRightOption" >> {
      foldRightOption(List.range(1, 11), 0, f) must be_==(Some(55))
      foldRightOption(List.range(1, 12), 0, f) must be_==(None)
    }

    ".foldLeftOpt" >> {
      foldLeftOpt(List.range(1, 11), 0, f) must be_==(Some(55))
      foldLeftOpt(List.range(1, 12), 0, f) must be_==(None)
    }

    // performance test to provide advantage of this `Option` approach
    // http://scalameter.github.io/home/gettingstarted/0.7/sbt/
  }

  "simple state monad" >> {
    "State" >> {
      val ex0 : State[Int, Int] =
        for {
          x0 <- get
          _  <- put(x0 * 2)
          x1 <- get
          _  <- put(x1 + 3)
          x2 <- get
        } yield { x2 + 4 }

      ex0.unState(10) must be_==((23, 27))
    }
  }


  "State monad combined with error monad (adhocly, without transformer)" >> {
    "SE" >> {

      def divE : Int => Int => Either[String, Int] =
        i0 => i1 => {
          i1 match {
            case 0 => Left("you cannot divide number by zero")
            case _ => Right(i0 / i1)
          }
        }

      val ex0 : SE[Int, String, Int] =
        for {
          x0 <- getSE
          x1 <- liftSE(divE(10)(x0))
          _  <- putSE(x1)
        } yield { x1 + 4 }

      ex0.unSE(5) must be_==(Right((2, 6)))
      ex0.unSE(0) must be_==(Left("you cannot divide number by zero"))
    }
  }


  "State monad combined with option monad as a transformer" >> {
    "StateT" >> {

      def divO: Int => Int => Option[Int] =
        i0 => i1 => {
          i1 match {
            case 0 => None
            case _ => Option(i0 / i1)
          }
        }

      val ex0 : StateT[Int, Int, Option] =
        // TODO: only first `getT` doesn't require implicit argument (donno why)
        getT.flatMap(MonadOption)( x0 =>
          lift(MonadOption)(divO(10)(x0)).flatMap(MonadOption)( x1 =>
            putT(MonadOption)(x1).map(MonadOption)(_ => x1 + 4 )
          )
        )

      // TODO: I want to realize this computation with `for` sugar
      // NOTE: desugar algorithm and open bugs
      // http://stackoverflow.com/questions/4380831/why-does-filter-have-to-be-defined-for-pattern-matching-in-a-for-loop-in-scala
      // val ex0 : StateT[Int, Int, Option] =
      //   for {
      //     x0 <- getT
      //     // x0 : Int <- getT
      //     // x0 : Int <- getT(MonadOption)

      //     x1 : Int <- lift(MonadOption)(div(10)(x0))
      //     _  <- putT(MonadOption)(x1)
      //   } yield { 100 }

      ex0.unStateT(5) must be_==(Some((2, 6)))
      ex0.unStateT(0) must be_==(None)
    }
  }
}
