/** TwinPrimes
  *
  * Uses repeated squaring to efficiently compute a raised to n mod m
  * */
def modpow(a: Int, n : Int, m: Int) : Int = {
  n match {
    case 0 => 1
    case 1 => a
    case _ =>
      if(n%2 == 1) (a * modpow(a, n-1, m)) % m
      else {
        val temp = modpow(a, n/2, m) % m
        (temp * temp) % m
      }
  }
}

modpow(3, 69, 11)

/** prime
  *
  * Takes an integer and returns a Boolean indicating whether the integer parameter is a prime number
  * Uses Fermat's Primality Test: given a raised to n-1 mod n = b mod n,
  *   if b == 1, n is probably prime; otherwise n is composite
  * */
def prime(a: Int): Boolean = {
  a match {
    case 1 => false
    case 2 => true
    case _ =>
      modpow(2, a-1, a) match {
        case 1 => true
        case _ => false
      }
  }

}

prime(1)
prime(3)
prime(4)
prime(113)

/** TwinPrimes
  *
  * takes 2 integer parameters and returns a Boolean indicating whether the parameters are two prime
  *   numbers with a difference of 2
  * */
def twinprimes(a: Int, b: Int) : Boolean = prime(a) && prime(b) && Math.abs(b - a) == 2

twinprimes(41, 43)
twinprimes(43, 47)
twinprimes(107, 109)

/** TwinPrimesList
  *
  * takes an integer, n, parameter and returns an integer list of all the twin primes starting up to n
  * */
def twinprimelist(n: Int) : List[Int] = {
  n match {
    case _ if n <= 2 => List()
    case _ if !prime(n) => twinprimelist(n-1)
    case _ if prime(n) =>
      n match {
        case _ if n == 5 => n-2 :: List()
        case _ if twinprimes(n, n-2) => n :: n-2 :: twinprimelist(n-2)
        case _ => twinprimelist(n-1)
      }
  }
}

twinprimelist(50)
twinprimelist(110)


/** golbach
  *
  * Takes an integer and prints the solution satisfying the Goldbach Conjecture:
  *   "every positive even number greater than 2 is the sum of two prim numbers"
  * */
def goldbach(n: Int) : String = {
  if (n < 4) return "Numbers less than 4 do not satisfy the Golbach Conjecture"
  if (n % 2 != 0) return "Odd numbers do not satisfy the Golbach Conjecture"

  var a = getprimepair(n, primesuntil(n))
  return "Result: " + n + " = " + a + " + " + (n-a)
}

def primesuntil(n: Int) : List[Int] = {
  n match {
    case _ if (n < 1) => List()
    case _ if prime(n) => n :: primesuntil(n-1)
    case _ if !prime(n) => primesuntil(n-1)
  }
}

primesuntil(28)
primesuntil(48)
primesuntil(4)

def sums(h:Int, t: List[Int]) : List[Int] = {
  t.map(_ + h)
}

def getprimepair(n: Int, t: List[Int]): Int = {
  t match {
    case Nil => -1
    case thead :: ttail => if (sums(thead, t).contains(n)) thead else getprimepair(n, ttail)
  }
}



goldbach(28)
goldbach(36)
goldbach(48)
goldbach(4)
goldbach(12)
goldbach(82)

