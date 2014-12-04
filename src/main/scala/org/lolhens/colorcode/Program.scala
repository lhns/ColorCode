package org.lolhens.colorcode

import java.awt.image.BufferedImage
import java.io.File
import java.util
import java.util.concurrent.{Executors, ExecutorService}
import javax.imageio.ImageIO

/**
 * Created by LolHens on 04.12.2014.
 */
class Program(file: File) {
  val image: BufferedImage = ImageIO.read(file)
  private val cursors: util.List[Cursor] = new util.ArrayList[Cursor]()
  private val executorService: ExecutorService = Executors.newCachedThreadPool()

  addCursor(0, 0)

  def addCursor(x: Int, y: Int, dir: Int = 1) = {
    val cursor = new Cursor(this, x, y, dir)
    cursors.add(cursor)
    executorService.execute(cursor)
  }
}
