package recfun
import scala.annotation.tailrec
object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
        if( c == 0 || r == c ) 1
        else pascal( c - 1 , r - 1 ) + pascal( c , r - 1 )
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        var balance = 0
        var char = ' '
        for( char <- chars )
        {
            if( char == '(' )
            {
                balance += 1;
            } else if( char == ')' )
            {
                balance -= 1;
            }
            if( balance < 0 )
            {
                return false
            }
        }
        return balance == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

        if( money < 0 ) 0
        else
        if( money == 0 ) 0
        else
        if( coins.isEmpty ) 0
        else
        {
            var acc = 0
            var coin = 0
            var k = 0
            while( k * coins.head <= money )
            {
                if( k * coins.head == money )
                {
                    acc += 1
                } else
                {
                    acc += countChange( money - k * coins.head , coins.tail )
                }
                k += 1
            }
            /*for( coin <- Set( coins ) )
            {
                if( coin == money )
                {
                    acc += 1
                } else if( money > coin )
                {
                    acc += countChange( money - coin , coins )
                }
            }*/
            return acc
        }
    }
  }
