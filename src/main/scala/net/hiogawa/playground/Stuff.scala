package net.hiogawa.playground

import scala.language.higherKinds

object Stuff {

  // first, use the simplest monad Option (except Identity)
  val x : Option[Int] = Some(0)
  val y : Option[Int] = None

  def sugar =
    for {
      x_ <- x
      y_ <- y
    } yield {x_ + y_}

  def desugar : Option[Int] =
    x flatMap (_x => y map (_y => _x + _y))


  // NOTE: thinking about what flatmap does in terms of Kleisli arrow

  // in haskell
  // Option[Int] -> Option[Int]
  // Int -> Int
  // Option[Option[Int]] -> Option[Int]

  // f : Int -> Option[Int]
  // x : 1 -> Option[Int]

  // x flatMap f : 1 -> Option[Int] -> Option[Option[Int]] -> Option[Int]
  //                 x            Option[f]               nu_Int

  //                         f  : A         -> Option[B]
  //                 flatmap(f) : Option[A] -> Option[B]

  // x : 1 -> Option[A]

  // f : Int --> Option[Int]
  //      _x |->

  // y : 1 -> Option[Int]

  //          Option[Int] -> Option[Int]
  // (env. _x : Int)  _y => _x + _y : Int -> Int
  // Int -> Int^Int

  // locally thinking (as constants/points x, y)
  // globally thingkiing (as envinronment `_x` for `y map (_y => _x + _y)`)

  // consider how to mix those thinking

  // y : Option[Int], _x : Int |- y map : (Int -> Int) -> Option[Int]     _x : Int |- (_y => _x + _y) : Int -> Int
  // y : Option[Int], _x : Int |- y map (_y => _x + _y) : Int
  //           y : Option[Int] |- _x => y map (_y => _x + _y) : Int -> Option[Int]


  //////////////////////////////////////
  // folding list with error handling //
  //////////////////////////////////////

  def foldLeftOption[A, B] (l : List[A], b : B, f : (A => B => Option[B])) : Option[B] = {
    l match {
      case Nil  => Some(b)
      case h::t =>
        f(h)(b) flatMap (_b => foldLeftOption(t, _b, f))

        // NOTE: write the above with `for`, `yield` sugar
        // for {
        //   _b <- f(h)(b)
        //   _r <- foldLeftOption(t, _b, f)
        // } yield {_r}

        // NOTE: these two won't work.
        //       there's no `return` in scala. `for` without `yield` is a sugar of `foreach`.
        // for {
        //   _b <- f(h)(b)
        //   foldLeftOption(t, _b, f)
        // }

        // for (_b <- f(h)(b)) {
        //   foldLeftOption(t, _b, f)
        // }
    }
  }

  def foldRightOption[A, B] (l : List[A], b : B, f : (A => B => Option[B])) : Option[B] = {
    l match {
      case Nil  => Some(b)
      case h::t =>
        for {
          _b <- foldRightOption(t, b, f)
          _r <- f(h)(_b)
        } yield {_r}
    }
  }

  // NOTE: then I noticed I can use `foldLeft` for the same purpose
  // but it turned out the definition of List#foldLeft doesn't fit what I meant to do
  // and funny thing is the file name, lol
  // https://github.com/scala/scala/blob/v2.11.7/src/library/scala/collection/LinearSeqOptimized.scala#L119-L128
  def foldLeftOpt[A, B] (l : Traversable[A], b : B, f : (A => B => Option[B])) : Option[B] = {
    l.foldLeft(Option(b)){(opt_b, a) =>
      opt_b flatMap (_b => f(a)(_b))
    }
  }


  /////////////////
  // State monad //
  /////////////////

  def mu[S, A](a : A) : State[S, A] =
    State[S, A](s => (s, a))

  def put[S](s : S) : State[S, Unit] =
    State[S, Unit](_ => (s, Unit))

  def get[S] : State[S, S] =
    State[S, S](s => (s, s))

  def run[S, A] : State[S, A] => S => (S, A) =
    comp => {
      val State(f) = comp
      f
    }

  case class State[S, A](f: S => (S, A)) {
    def unState = f

    def map[B](g : A => B) : State[S, B] = {
      State[S, B]{ s0 =>
        val (s1, a) = f(s0)
        (s1, g(a))
      }
    }

    def flatMap[B](g : A => State[S,B]) : State[S, B] = {
      State[S, B]{ s0 =>
        val (s1, a) = f(s0)
        val State(h) = g(a)
        h(s1)
      }
    }
  }


  /////////////////////////////////////////////////////////////
  // State monad combined with Maybe (Error) Monad (adhocly) //
  /////////////////////////////////////////////////////////////


  sealed trait Either[E, A] {
    def map[B] : (A => B) => Either[E, B] =
      f => {
        this match {
          case Left(e) => Left(e)
          case Right(a) => Right(f(a))
        }
      }
    def flatMap[B] : (A => Either[E, B]) => Either[E, B] =
      f => {
        this match {
          case Left(e) => Left(e)
          case Right(a) => {
            f(a) match {
              case l@Left(e) => l
              case Right(b) => Right(b)
            }
          }
        }
      }
  }
  case class Left[E, A](e : E) extends Either[E, A]
  case class Right[E, A](a : A) extends Either[E, A]


  def throwSE[S, E, A] : E => SE[S, E, A] =
    e => SE[S, E, A](_ => Left(e))

  def putSE[S, E] : S => SE[S, E, Unit] =
    s => SE[S, E, Unit](_ => Right((s, Unit)))

  def getSE[E, S] : SE[S, E, S] =
    SE[S, E, S](s => Right((s, s)))

  def liftSE[S, E, A] : Either[E, A] => SE[S, E, A] =
    e => SE[S, E, A]{ s =>
      e match {
        // case l@Left(e) => l     // <- why not allowed?
        case Left(e) => Left(e)
        case Right(a)  => Right((s, a))
      }
    }

  // NOTE: there is another choice of state error monad defined as S => (S, Either[E, A])
  case class SE[S, E, A](f: S => Either[E, (S, A)]) {
    def unSE = f

    def map[B] : (A => B) => SE[S, E, B] = {
      g => SE[S, E, B]{ s0 =>
        f(s0) map (sa => (sa._1, g(sa._2)))
      }
    }

    def flatMap[B] : (A => SE[S, E, B]) => SE[S, E, B] = {
      g => SE[S, E, B]{ s0 =>
        for {
          sa <- f(s0)
          sb <- g(sa._2).unSE(sa._1)
        } yield sb
      }
    }
  }


  /////////////////////////////
  // NOTE: Monad definition? //
  /////////////////////////////

  // TODO: combining error (option) with other monad state (or something)
  //  - combine error
  //    - define transformer, given kind, return kind (higher class? or using implicit?)

  // TODO: I feel I don't want to call this "higher" kind, but why?

  trait Functor[F[_]] {
    def fmap[A, B] : (A => B) => (F[A] => F[B])
  }

  // NOTE: Type class cannot be a class (this is a definition of characteristics of type itself)
  //       this is just ad-hoc polymorphism
  //       how can i connect ad-hoc polymorphism with flatMap, map kinds of primitive??
  //       this is correct word usage to represent what i want to do here
  //       http://stackoverflow.com/questions/16027237/typeclass-and-the-scala-collection-interface

  //       how to assure class T has a flatMap method? (is that exacly what's called `bounds`?)
  //       or don't we even have to define type class for this?
  //       instead, just define `StateT[.., T[_]]` kinds of thing with assureing T has a map, flatMap method?
  //       this could be it but not so clean way http://twitter.github.io/scala_school/advanced-types.html#structural
  trait Monad[T[_]] extends Functor[T] {
    def eta[A]     :       A  => T[A]
    def mu[A]      : T[T[A]]  => T[A]

    // thinking in adjunction transpose has much more computation taste, which I like.
    def trans[A, B] : (A => T[B]) => (T[A] => T[B]) =
      f => mu compose fmap(f)

    // def flatMap[A, B] : (A => T[B]) => T[B]
    // def map[A, B] : (A => B) => T[B]
  }

  // NOTE: this won't work
  // implicit object FunctorOption extends Functor[Option] {
  //   def fmap[A, B] = f => oa => oa map f
  // }

  // implicit object MonadOption extends Monad[Option] {
  //   def eta[A] = a => Some(a)
  //   def mu[A]  = ooa =>
  //     for {
  //       oa <- ooa
  //       a  <- oa
  //     } yield { a }
  // }

  implicit object MonadOption extends Monad[Option] with Functor[Option] {
    def fmap[A, B] = f => oa => oa map f
    def eta[A] = a => Some(a)
    def mu[A]  = ooa =>
      for {
        oa <- ooa
        a  <- oa
      } yield { a }
  }

  // http://ncatlab.org/nlab/show/strong+monad#alternative_definition
  def strength[A, B, T[_]](implicit ev: Monad[T]) : A => T[B] => T[(A, B)] =
    a => m => ev.fmap((b : B) => (a, b))(m)

  // http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-State-Lazy.html
  // Is it possible to use the fact T has a flatMap ??
  // but the thing is scalaz doesn't even use such a way!!
  // that's such an inconvienience of scala and flexibleness of `flatMap` and `for`
  // https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/StateT.scala#L61-L65
  case class StateT[S, A, T[_]](f : S => T[(S, A)]) {
    def unStateT = f

    def flatMap[B](implicit ev: Monad[T]) : (A => StateT[S, B, T]) => StateT[S, B, T] =
      // NOTE: type lists
      // f : S => T[(S, A)]
      // g : A => S => T[(S, B)]  ~   (A, S) => T[(S, B)]

      g => StateT[S, B, T]{ s0 =>
        // NOTE: three kinds of similar function types
        // f: A => B       => C
        // f: (A, B)       => C
        // f: Tuple2[A, B] => C
        val _g : Tuple2[S, A] => T[(S, B)] =
          sa => g(sa._2).unStateT(sa._1)
        ev.trans(_g)(f(s0))

        // NOTE: what I want to realize is this!
        // like the monad definition of haskell's StateT
        // http://hackage.haskell.org/package/transformers-0.4.3.0/docs/src/Control-Monad-Trans-State-Lazy.html#line-192
        // for {
        //   (s1, a) <- f(s0)
        //   res <- unStateT(g(a))(s0)
        // } yield { res }
      }

    def map[B](implicit ev: Monad[T]) : (A => B) => StateT[S, B, T] =
      g => StateT[S, B, T]{ s0 =>
        ev.fmap((sa : Tuple2[S, A]) => (sa._1, g(sa._2)))(f(s0))
      }

    def filter (f : A => Boolean) : StateT[S, A, T] = this
    def withFilter (f : A => Boolean) : StateT[S, A, T] = this
    // def filter : (A => Boolean) => StateT[S, A, T] = f => this
    // def withFilter : (A => Boolean) => StateT[S, A, T] = (_ : A => Boolean) => this
  }

  // http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Class.html#v:lift
  def lift[S, A, T[_]](implicit ev: Monad[T]) : T[A] => StateT[S, A, T] =
    m => StateT[S, A, T](s0 => strength(ev)(s0)(m))

  def getT[S, T[_]](implicit ev: Monad[T]) : StateT[S, S, T] =
    StateT[S, S, T](s => ev.eta((s, s)))

  def putT[S, T[_]](implicit ev: Monad[T]) : S => StateT[S, Unit, T] =
    s => StateT[S, Unit, T](_ => ev.eta((s, Unit)))



  // TODO:
  //  - write the same thing with scalaz


  // NOTE: thinking about commutativity of monads

  // Option[List[A]]

  // B -> Option[List[A]]

  // ? : Option[List[A]] -> (A -> Option[B]) -> Option[List[B]]
  //
  //   List[A] -> List[Option[B]]

  // generally,
  //   Option[List[A]] -> (A -> Option[List[B]]) -> Option[List[A]]

  // if two monads commutes, they can be combined? (there is theory something like this)
  // f  : A               -> Option[List[B]]
  // f^ : Option[List[A]] -> Option[List[B]]
}
