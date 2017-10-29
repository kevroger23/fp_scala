/*trait Show[A] {
  def show(a: A): String

}*/


abstract class Show[A]
{
  def show(a: A): String


}

object Show {
  val intCanShow: Show[Int] =
    new Show[Int] {
      def show(int: Int): String = s"int $int"
    }
}


println(Show.intCanShow.show(20))
