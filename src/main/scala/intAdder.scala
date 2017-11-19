case class intAdder (times:Int)  extends getUserName {


  def gettimesUser(id:Int) =  getUserFromId(id) * times

  def printtimesUser(id:Int) = println( gettimesUser(id))


}


trait getUserName {

  def getUserFromId( id:Int):String = "default"


}


trait defaultgetUserName extends {

  def getUserFromId( id:Int):String = "default"


}