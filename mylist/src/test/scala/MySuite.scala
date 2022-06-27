import List.*
class MySuite extends munit.FunSuite {


  test("sum test1") {
    val obtained = 10
    val expected = sum(List(5,5))
    assertEquals(obtained, expected)
  }

  test("sum test2") {
    val obtained = 20
    val expected = sum(List(5,5,5,5))
    assertEquals(obtained, expected)
  }

  test("sum test3") {
    val obtained = 15
    val expected = sum(List(0,5,5,5))
    assertEquals(obtained, expected)
  }

  test("sum test4") {
    val obtained = 0
    val expected = sum(List())
    assertEquals(obtained, expected)
  }
  test("sum_old test1") {
    val obtained = 10
    val expected = sum_old(List(5,5))
    assertEquals(obtained, expected)
  }  

  test("sum_old test2") {
    val obtained = 0
    val expected = sum_old(List(1,-1))
    assertEquals(obtained, expected)
  }   

  test("sum_old test3") {
    val obtained = 0
    val expected = sum_old(List())
    assertEquals(obtained, expected)
  }    

  test("sum_old test4") {
    val obtained = 10
    val expected = sum_old(List(1,2,3,4))
    assertEquals(obtained, expected)
  }    


  test("foldLeft test1") {
    val obtained = 10
    val expected = List(5,5).foldLeft(0)(_+_)
    assertEquals(obtained, expected)
  }

  test("foldLeft test2") {
    val obtained = 30
    val expected = List(1,2,5,3).foldLeft(1)(_*_)
    assertEquals(obtained, expected)
  }
  
  test("foldLeft test3") {
    val obtained = 0
    val expected = List(1,2,5,0).foldLeft(1)(_*_)
    assertEquals(obtained, expected)
  }
  
  test("foldLeft test4") {
    val obtained = 1
    val expected = List(1,0).foldLeft(0)(_+_)
    assertEquals(obtained, expected)
  }

  test("dotProduct test1") {
    val obtained = 32
    val expected = dotProduct(List(1,2,5,3),List(1,2,3,4))
    assertEquals(obtained, expected)
  }

  test("dotProduct test2") {
    val obtained = 14
    val expected = dotProduct(List(1,2,3),List(1,2,3))
    assertEquals(obtained, expected)
  }

  test("dotProduct test3") {
    val obtained = 32
    val expected = dotProduct(List(1,2),List(12,10))
    assertEquals(obtained, expected)
  }

  test("dotProduct test4") {
    val obtained =0
    val expected = dotProduct(List(),List())
    assertEquals(obtained, expected)
  }

  test("dotProduct_old test1") {
    val obtained = 31
    val expected = dotProduct_old(List(0,2,5,3),List(1,2,3,4))
    assertEquals(obtained, expected)
  }

  test("dotProduct_old test2") {
    val obtained = 0
    val expected = dotProduct_old(List(),List())
    assertEquals(obtained, expected)
  }

  test("dotProduct_old test3") {
    val obtained = 4
    val expected = dotProduct_old(List(-2,3),List(1,2))
    assertEquals(obtained, expected)
  }

  test("dotProduct_old test4") {
    val obtained = 0
    val expected = dotProduct_old(List(-2,3),List(0,0))
    assertEquals(obtained, expected)
  }

  test("zipWith test1"){
    val obtained = List("aA","bB","cC")
    val expected = zipWith(List("a", "b", "c"), List("A", "B", "C"),(_ + _))
    assertEquals(obtained, expected)
  }
  test("zipWith test2"){
    val obtained = List(0,0,0)
    val expected = (zipWith(List(0, 0, 0), List(0, 0, 0),(_ + _)))
    assertEquals(obtained, expected)
  }
  test("zipWith test3"){
    val obtained = List(7,114,650)
    val expected = (zipWith(List(1, 2, 10), List(7, 57, 65),(_ * _)))
    assertEquals(obtained, expected)
  }

  test("zipWith test3"){
    val obtained = List(7,114,650)
    val expected = (zipWith(List(1, 2, 10), List(7, 57, 65),(_ * _)))
    assertEquals(obtained, expected)
  }

  test("zipWith test4"){
    val obtained = List(-6,-55,-55)
    val expected = (zipWith(List(1, 2, 10), List(7, 57, 65),(_ - _)))
    assertEquals(obtained, expected)
  }

  test("IsEmpty test1") {
    val obtained = true 
    val expected =  List().isEmpty
    assertEquals(obtained, expected)
 }

  test("IsEmpty test2") {
    val obtained = false 
    val expected =  List('a').isEmpty
    assertEquals(obtained, expected)
  }

  test("foldRight test1") {
    val obtained = 10
    val expected = List(5,5).foldRight(0)(_+_)
    assertEquals(obtained, expected)
  }
  test("foldRight test2") {
    val obtained = 1
    val expected = List(6,5).foldRight(0)(_-_)
    assertEquals(obtained, expected)
  }

  test("toString test1"){
    val expected = "[1, 2, 3, 4]"
    val actual = List.of(1,2,3,4).toString
    assertEquals(expected,actual)
  }

  test("toString test2"){
    val expected = "[11, 22, 33, 44]"
    val actual = List.of(11,22,33,44).toString
    assertEquals(expected,actual)
  }
}