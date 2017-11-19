

sealed trait List[+A]
{

  def tail2: List[A] =
    this match {
      case Nil => Nil
      case Cons(x, tail) => tail
    }

  def head2:Option[A] =
    this match {
      case Nil => None
      case Cons (x, _) => Some(x)
    }

// New text added

  def dropWhile2[A] ( f:A=>Boolean):List[A] =
  {
    this match {
      case Nil => Nil
      case Cons(x:A,xs:List[A]) => if (f(x)) xs.dropWhile2( f)  else Cons(x, xs.dropWhile2( f))

    }
  }
}


case object Nil extends  List [Nothing]

case class Cons[A](head:A, tail:List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] = {
    as.length match {
      case 0 => Nil
      case _ => Cons[A](as.head, apply(as.tail: _*))
    }
  }


  def tail[A](xs: List[A]): List[A] =
    xs match {
      case Nil => Nil
      case Cons (x, tail) => tail
    }

  def setHead[A] (x:A, xs:List[A]):List[A] =
  {
    xs match {

      case Nil => List(x)
      case Cons ( h, tail) => Cons( x, tail)

    }

  }


  def drop[A](xs:List[A], n:Int):List[A]=
  {
    n match {
      case 0 => xs
      case _ => drop (List.tail(xs), n -1)
    }
  }


  def init[A] (xs:List[A]):List[A] = {
    xs match
      {
      case Nil => Nil
      case Cons( h, Nil) => Nil
      case Cons(h, t) =>   Cons ( h, init (t))

    }

  }
  def dropWhile[A] (xs:List[A], f:A=>Boolean):List[A] =
  {
    xs match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs, f)  else Cons(x, dropWhile(xs, f))

    }
  }

}

val l1 = List(11,13,5, -1,2,2,4,7)

val t2 = l1.tail2

val h2 = l1.head2

val s33 = List.setHead(33, l1)


val d2 = List.drop(l1, 2)

val noevens = List.dropWhile( l1, (x:Int)=> x % 2 == 0)

val noevens2 = l1.dropWhile2((x:Int)=> { x % 2 == 0})


val all_but_last = List.init(l1)

def foldRight[A,B] ( xs:List[A], z:B, f:(A,B)=>B):B =
{
  xs match
    {
    case Nil => z
    case Cons(h:A,t:List[A] ) =>  f( h, foldRight (  t, z, f))
  }

}

def foldRight2[A,B] ( xs:List[A], z:B)( f:(A,B)=>B):B =
{
  xs match
  {
    case Nil => z
    case Cons(h:A,t:List[A] ) =>  foldRight2( t, f( h, z)) (f)
  }

}


val lint = List[Int] (2,3,4,1)


val  s1 = foldRight[Int,Int](lint, 0,  _ + _)


val  prod = foldRight[Int,Int] ( lint, 1,  (x:Int, y:Int)=> x * y)