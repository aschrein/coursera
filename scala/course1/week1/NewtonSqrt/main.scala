
import scala.annotation.tailrec
object Main
{
    @tailrec
    def factorial( k : Int , a : Int ) : Int =
        {
            if( a == 0 ) k
            else factorial( k * a , a - 1 )
        }
    def factorial( a : Int ) : Int = factorial( 1 , a )
    def main(args: Array[String]): Unit = {
        println( factorial( 2 ) )
  }
}
