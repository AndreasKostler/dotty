class Foo(x: Int) {
  var from: String = _
  var to: String = _
  val message = "hello, world"

  if (x > 5)
    from = "Jack"
  else
    to = "Jim"

  from.size  // error
  to.size    // error

  if (x > 5) {
    from = "Jack"
    to = "Jim"
  }
  else {
    from = "Jack"
    to = "Jim"
  }

  from.size
  to.size
}


import scala.annotation.partial

class Bar(x: Int, m: String @partial) {
  var from: String = _
  var to: String = _
  val message = "hello, world"

  if (x > 5) {
    from = m
    to = "Jim"
  }
  else {
    from = "Jack"
    to = "Jim"
  }

  from.size  // error
  to.size
}