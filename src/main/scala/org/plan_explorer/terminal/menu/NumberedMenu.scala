package org.plan_explorer.terminal.menu

import org.jline.reader.LineReader

case class NumberedMenu(options: (String, () => Action)*) extends Action {
  val menu: Map[Int, () => Action] = options.zipWithIndex.map(x => (x._2, x._1._2)).toMap

  override def chooseOptionFromReader(reader: LineReader): Action = {
    println(createMenu())
    while (true) {
      val input = reader.readLine("> ")
      try {
        val asInt = input.toInt

        menu.get(asInt) match {
          case Some(act) =>
            return act()

          case None =>
            println("No such option exists")
        }
      } catch {
        case _: NumberFormatException =>
          println("Need a number")
      }
    }

    throw new RuntimeException("this should never happen")
  }

  private def createMenu(): String = {
    options.zipWithIndex.map {
      case ((label, _), idx) =>
        s"$idx - $label"
    }.mkString(System.lineSeparator())
  }
}
