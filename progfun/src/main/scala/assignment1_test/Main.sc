object Main {
  def pascal(c: Int, r: Int): Int = {
    def stoppage(c: Int, r: Int): Boolean = {
      (c == 0) || (c == r) || (r == 0)
    }

    if (stoppage(c, r)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

}