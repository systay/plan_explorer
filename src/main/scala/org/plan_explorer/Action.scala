package org.plan_explorer

import org.jline.reader.LineReader

trait Action {
  def chooseOptionFromReader(reader: LineReader): Action
}

case object Quit extends Action {
  def chooseOptionFromReader(reader: LineReader): Action = {
    System.exit(0)
    this
  }
}

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
        case e: NumberFormatException =>
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

case class Menu(options: (String, () => Action)*) extends Action {
  val encodedOptions: Map[String, Char] = options.foldLeft(Map.empty[String, Char]) {
    case (used, (line, _)) =>
      val letter = line.find(c => !used.values.exists(c.toLower == _)).getOrElse(throw new RuntimeException("failed to build menu"))
      used + (line -> letter.toLower)
  }

  def chooseOptionFromReader(reader: LineReader): Action = {
    println(createMenu())

    while (true) {
      val input = reader.readLine("> ")
      if (input.length != 1)
        println("Input should be a single character")
      else {
        encodedOptions.find(p => p._2 == input.head) match {
          case Some((line, _)) =>
            return options.find(p => p._1 == line).get._2()

          case None =>
            println("No such option exists")
        }
      }
    }

    throw new RuntimeException("this should never happen")
  }

  private def createMenu(): String = {
    val builder = new StringBuilder
    for {
      (line, _) <- options
      letter <- encodedOptions.get(line)
    } {
      builder.append(s"$letter - $line").append(System.lineSeparator())
    }
    builder.toString()
  }
}