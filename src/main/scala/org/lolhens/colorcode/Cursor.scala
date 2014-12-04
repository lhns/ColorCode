package org.lolhens.colorcode

import java.util

/**
 * Created by LolHens on 04.12.2014.
 */
class Cursor(var program: Program, var x: Int, var y: Int, var dir: Int) extends Runnable {
  val stack: util.List[Byte] = new util.ArrayList[Byte]()

  override final def run(): Unit = {

  }
}
