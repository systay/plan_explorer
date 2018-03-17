package org.plan_explorer.terminal

import org.jline.reader.LineReader
import org.plan_explorer.terminal.menu.Action

case object SystemExit extends Action {
  def chooseOptionFromReader(reader: LineReader): Action = {
    System.exit(0)
    this
  }
}



