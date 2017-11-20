// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Ma. Bettina Bienvenida

// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)


// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (remainingBits, carryBit) match {
    case (Nil, false) => List()
    case (Nil, true)  => carryBit :: List()
    case (rhead :: rtail, _) =>
      addBits(rhead, carryBit, false) :: finishBinaryAdd(rtail, getNextCarryBit(rhead, carryBit, false))
  }
}

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit, qBit, carryBit) match {
    case (false, false, false) => false
    case (false, false, true)  => false
    case (false, true,  false) => false
    case (false, true,  true)  => true
    case (true,  false, false) => false
    case (true,  false, true)  => true
    case (true,  true,  false) => true
    case (true,  true,  true)  => true
  }
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit, qBit, carryBit) match {
    case (false, false, false) => false
    case (false, false, true)  => true
    case (false, true,  false) => true
    case (false, true,  true)  => false
    case (true,  false, false) => true
    case (true,  false, true)  => false
    case (true,  true,  false) => false
    case (true,  true,  true)  => true
  }
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (pBits,qBits) match {
    case (Nil, _) => finishBinaryAdd(qBits, carryBit)
    case (_, Nil) => finishBinaryAdd(pBits, carryBit)
    case (phead::ptail, qhead::qtail) =>
      addBits(phead, qhead, carryBit) :: doBinaryAddition(ptail, qtail, getNextCarryBit(phead, qhead, carryBit))
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) = {
  intList map {
    case 1 => true
    case 0 => false
  }
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList map {
    case true => 1
    case false => 0
  }
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  var result = doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, false)
  convertBooleanListToIntList(result.reverse)
}

// This functions pads a binary integer list with 0s to meet specified length (leng)
def pad(pList: List[Int], leng: Int): List[Int] = {
  (pList, leng) match {
    case (Nil, 0) => List()
    case (Nil, _) => 0 :: pad(pList, leng - 1)
    case (phead :: ptail, _) => phead :: pad (ptail, leng - 1)
  }
}

// This function converts a binary integer list to it's two's complement
def twosComplement(pList: List[Int]): List[Int] ={
  val nList = pList map {
    case 0 => 1
    case 1 => 0
  }
  val temp = finishBinaryAdd(convertIntListToBooleanList(nList).reverse, true)
  convertBooleanListToIntList(temp.reverse)
}

/* This function performs binary subtraction with the following cases:
    1. if minuend and subtrahend have equal length, convert subtrahend to its two's complement then add
    2. if minuend's length is greater than subtrahend's, pad subtrahend before converting to two's complement then add
    3. if subtrahend's length is greater than minuend's, switch the minuend and subtrahend then subtract
*/
def binarySubtraction(pList: List[Int], qList: List[Int]): List[Int] = {
  var result: List[Int] = List()
  pList match {
    case _ if(pList.length == qList.length) => result = binaryAddition(pList, twosComplement(qList))
    case _ if(pList.length > qList.length) => result = binaryAddition(pList, twosComplement(pad(qList.reverse, pList.length).reverse))
    case _ if(pList.length < qList.length) => return binarySubtraction(qList, pList)
  }

  result.drop(pList.length - qList.length)
}

// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")
