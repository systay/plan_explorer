package org.plan_explorer.tvision

import jexer.TApplication
import jexer.TApplication.BackendType
import jexer.menu.TMenu

class MyApp extends TApplication(BackendType.SWING) {
  val ENTER_QUERY = 2000

  fileMenu()
  new MainWindow(this)

  private def fileMenu() = {
    val fileMenu = addMenu("&Menu")
    fileMenu.addDefaultItem(TMenu.MID_EXIT)
    fileMenu
  }


}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val app = new MyApp
      new Thread(app).start()
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}