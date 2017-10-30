

sealed trait List[+A]
{
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


  def dropWhile[A] (xs:List[A], f:A=>Boolean):List[A] =
  {
    xs match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs, f)  else Cons(x, dropWhile(xs, f))

    }
  }

 // @annotation.tailrec

  def foldRight [A,B]( xs:List[A], z:B) ( f:(A,B)=>B ):B =
  {

    xs match {
      case Nil => z
      case Cons(h,t) => f( h, foldRight( t, z)(f))

    }

  }

  @annotation.tailrec
  def foldLeft[A,B] ( xs:List[A], z:B) (f:(B,A)=> B):B =
  {
    xs match
    {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h ))(f)

    }


  }



}

val l1 = List(11,13,5, -1,2,2,4,7)


val s33 = List.setHead(33, l1)


val d2 = List.drop(l1, 2)

//val noevens = List.dropWhile( l1, (x:Int)=> x % 2 == 0)

//val noevens2 = l1.dropWhile2((x:Int)=> { x % 2 == 0})


val lint = List( 2,3,4,6 )

val sumint = List.foldLeft[Int,Int](lint, 0)(_+_)

val prodint = List.foldRight(lint, 1) ( (x:Int, y:Int) =>  x * y)

val prodint2 = List.foldRight(lint, Nil:List[Int])( Cons(_,_))

def length[A](xs:List[A]):Int =
{
  xs match {
    case Nil => 0
    case Cons( h, t) => 1 + length(t)

  }

}





val l2 = length(lint)




def revviafoldleft[A](xs:List[A] ) = {


  List.foldLeft(xs, List[A]())((acc,h) => Cons(h,acc))



}


def length2[A](xs:List[A]):Int =
{

  //List.foldRight[A, Int](xs, 0)((x:A ,y:Int ) => if ( x == Nil) 0 else  1 )

  List.foldRight[A, Int](xs, 0)((x:A ,y:Int ) => if ( x == Nil) 0 else y + 1 )

}


val l3 = length2(lint)



val rev_list = revviafoldleft(lint)


def append_via_foldright[A](xs:List[A], app:List[A]) =
{

  List.foldRight(xs, app) ( (x,y) => Cons(x,y))

}

val app_list = append_via_foldright( List(2,3,4), List(1,1,1))