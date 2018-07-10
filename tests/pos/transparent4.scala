object ListIntConcat {
  sealed trait List {
    transparent def ++(that: List): List =
      if (this.isInstanceOf[Nil.type]) that
      else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)
  }
  case object Nil extends List
  case class Cons(head: Int, tail: List) extends List

  val x1: Nil.type = Nil ++ Nil
  val x2: { Cons(1, Nil) } = Cons(1, Nil) ++ Nil
  val x3: { Cons(1, Nil) } = Nil ++ Cons(1, Nil)
  val x4: { Cons(1, Cons(2, Nil)) } = Cons(1, Nil) ++ Cons(2, Nil)
  val x5: { Cons(1, Cons(2, Cons(3, Nil))) } = Cons(1, Nil) ++ Cons(2, Cons(3, Nil))
  val x6: { Cons(1, Cons(2, Cons(3, Nil))) } = Cons(1, Cons(2, Nil)) ++ Cons(3, Nil)
}

object ListGenericConcat {
  sealed trait List[T] {
    transparent def ++(that: List[T]): List[T] =
      if (this.isInstanceOf[Nil[T]]) that
      else Cons(this.asInstanceOf[Cons[T]].head, this.asInstanceOf[Cons[T]].tail ++ that)
  }
  case class Nil[T]() extends List[T]
  case class Cons[T](head: T, tail: List[T]) extends List[T]

  val nil = new Nil[Int]()

  val x1: Nil[Int] = nil ++ nil
  val x2: { Cons(1, nil) } = Cons(1, nil) ++ nil
  val x3: { Cons(1, nil) } = nil ++ Cons(1, nil)
  val x4: { Cons(1, Cons(2, nil)) } = Cons(1, nil) ++ Cons(2, nil)
  val x5: { Cons(1, Cons(2, Cons(3, nil))) } = Cons(1, nil) ++ Cons(2, Cons(3, nil))
  // val x6: { Cons(1, Cons(2, Cons(3, nil))) } = Cons(1, Cons(2, nil)) ++ Cons(3, nil) // needs 230 steps
}

object ListCovariantConcat {
  sealed trait List[+T] {
    transparent def ++[TT >: T](that: List[TT]): List[TT] =
      if (this.isInstanceOf[Nil.type]) that
      else Cons(this.asInstanceOf[Cons[T]].head, this.asInstanceOf[Cons[T]].tail ++ that)
  }
  case object Nil extends List[Nothing]
  case class Cons[+T](head: T, tail: List[T]) extends List[T]

  val x2: { Cons(1, Nil) } = Cons(1, Nil) ++ Nil
  val x3: { Cons(1, Nil) } = Nil ++ Cons(1, Nil)
  val x4: { Cons(1, Cons(2, Nil)) } = Cons(1, Nil) ++ Cons(2, Nil)
  val x5: { Cons(1, Cons(2, Cons(3, Nil))) } = Cons(1, Nil) ++ Cons(2, Cons(3, Nil))
  // val x6: { Cons(1, Cons(2, Cons(3, Nil))) } = Cons(1, Cons(2, Nil)) ++ Cons(3, Nil) // needs 230 steps
}
