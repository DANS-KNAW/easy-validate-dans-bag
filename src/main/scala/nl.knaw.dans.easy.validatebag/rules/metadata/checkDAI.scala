package nl.knaw.dans.easy.validatebag.rules.metadata

object checkDAI extends App {
  
  val s = "info:eu-repo/dai/nl/358163587".stripPrefix("info:eu-repo/dai/nl/")
  println(digest(s.slice(0, s.length - 1), 9) == s.last)

  def digest(message: String, modeMax: Int): Char = {
    val reverse = message.reverse
    var f = 2
    var w = 0
    var mod = 0
    mod = 0
    while ( { mod < reverse.length }) {
      val cx = reverse.charAt(mod)
      val x = cx - 48
      w += f * x
      f += 1
      if (f > modeMax) f = 2

      { mod += 1; mod }
    }
    mod = w % 11
    if (mod == 0) '0'
    else {
      val c = 11 - mod
      if (c == 10) 'X'
      else (c + 48).toChar
    }
  }
}
