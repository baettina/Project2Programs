val english : List[String] = List("zero", "one","two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val pinyin: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")

/** myMember
  *
  * Checks to see if an element is within a list.
  * */
def myMember(element: String, lis: List[String]) : Boolean = {
  lis match {
    case Nil => false
    case listHead :: listTail => if (element == listHead) true else myMember(element, listTail)
  }
}

/** englishToInt
  *
  * Converts an english word to its equivalent integer value from 0 to 10
  * */
def englishToInt(en: String) : Int = {
  en match {
    case "zero"  => 0
    case "one"   => 1
    case "two"   => 2
    case "three" => 3
    case "four"  => 4
    case "five"  => 5
    case "six"   => 6
    case "seven" => 7
    case "eight" => 8
    case "nine"  => 9
    case "ten"   => 10
  }
}

/** pinyinToInt
  *
  * Converts pinyin to its equivalent integer value from 0 to 10
  * */
def pinyinToInt(py: String) : Int = {
  py match {
    case "ling" => 0
    case "yi"   => 1
    case "er"   => 2
    case "san"  => 3
    case "si"   => 4
    case "wu"   => 5
    case "liu"  => 6
    case "qi"   => 7
    case "ba"   => 8
    case "jiu"  => 9
    case "shi"  => 10
  }
}

/** translates
  *
  * Converts a list of strings to a list of their equivalent integer values
  * Words that are not english or pinyin numbers from 1 - 10 will be ignored.
  * */
def translate(lis: List[String]) : List[Int] = {
  lis match {
    case Nil => List()
    case lhead :: ltail =>
      lhead match {
        case _ if(myMember(lhead, english)) => englishToInt(lhead) :: translate(ltail)
        case _ if(myMember(lhead, pinyin)) => pinyinToInt(lhead) :: translate(ltail)
        case _ => translate(ltail)
      }
  }
}

/** go
  *
  * Takes a list of phonetic words and translates them to their equivalent integer values.
  * The numbers are then added and multiplied.
  * */
def go(lis: List[String]) = {
  val intList = translate(lis)
  println("Translation: " + intList.mkString(" "))
  println("Addition: " + intList.mkString(" + ") + " = " + intList.foldRight(0)(_ + _))
  println("Multiplication: " + intList.mkString(" * ") + " = " + intList.foldLeft(1)(_ * _))
}

go(List("yi", "nine", "six", "ba"))

go(List("yi", "josh", "three", "si"))


