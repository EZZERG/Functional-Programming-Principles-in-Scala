object product_func {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def factoriel(a: Int) = product(x => x)(1, a)

  factoriel(4)
  factoriel(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int,
                            b: Int): Int = {
    if(a>b) zero
    else
      combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  mapReduce(x =>x, (x,y) => x+y, 0)(1,10)
  mapReduce(x =>x, (x,y) => x*y, 1)(1,5)
}