package recfun

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


      if (c==0 || r==0 || r==c) return 1
      return pascal(c, r-1) + pascal(c-1, r-1)

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def rec_balance(xchars: List[Char], track_ouvrante: Int): Boolean = {

        if (track_ouvrante < 0) return false;

        if (xchars.isEmpty) {
          if (track_ouvrante == 0) return true else return false;
        }

        if (xchars.head == '(') return rec_balance(xchars.tail, track_ouvrante+1);
        if (xchars.head == ')') return rec_balance(xchars.tail, track_ouvrante-1);

        return rec_balance(xchars.tail, track_ouvrante);
      }

      rec_balance(chars, 0);

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def sum(xmoney: Int, xcoins: List[Int]): Int = {

        def recSum(coinId: Int, coinsFromId: List[Int]): Int = {

          if (coinId >= xcoins.size) return 0
          countChange((xmoney-xcoins(coinId)), coinsFromId) +
            recSum(coinId+1, coinsFromId.tail)
        }

        recSum(0, xcoins)

      }
      if (money<0) return 0
      if (coins.isEmpty) return 0;
      if (money == 0) return 1
      return sum(money, coins)

    }
  }
