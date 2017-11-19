
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfter, FunSuite}

class intAdderSpec extends FunSuite with BeforeAndAfter with MockitoSugar {

  val mockgetUserName = mock[getUserName]

  when(mockgetUserName.getUserFromId(2))thenReturn("userTwo")
  when(mockgetUserName.getUserFromId(3))thenReturn("userThree")

  class testintAdder(x:Int) extends intAdder(x)
  {
    //val mockgetUserName = mock[getUserName]

    override def getUserFromId(id: Int): String = mockgetUserName.getUserFromId(id)

  }


  trait testAddertrait extends getUserName
  {
    override def getUserFromId(id: Int): String = mockgetUserName.getUserFromId(id)
  }

  test ("intAdder  which is already mixed with existing trait")
  {
    val testObj1 =  intAdder(2)
    assert ( testObj1.gettimesUser(2) === "default" * 2 )
  }


  test ("testintAdder  which is already mixed with existing trait")
  {

    val testObj2 = new testintAdder( 2)
    assert ( testObj2.gettimesUser(2) === "userTwo" * 2 )

  }


  test ("testintAdder 3")
  {

    val testObj3  = new  intAdder(2) with testAddertrait

    assert ( testObj3.gettimesUser(2) === "userTwo" * 2 )

  }

}
