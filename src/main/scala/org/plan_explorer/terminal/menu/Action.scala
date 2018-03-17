package org.plan_explorer.terminal.menu

import org.jline.reader.LineReader

trait Action {
  def chooseOptionFromReader(reader: LineReader): Action
}
