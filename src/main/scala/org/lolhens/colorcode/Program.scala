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

  val width = image.getWidth()
  val height = image.getHeight()
  val data = Array.ofDim[Int](width, height)
  for (y <- 0 until height; x <- 0 until width) data(x)(y) = image.getRGB(x, y) & 0xFFFFFFFF

  private val cursors: util.List[Cursor] = new util.ArrayList[Cursor]()
  private val executorService: ExecutorService = Executors.newCachedThreadPool()

  addCursor(0, 0)

  def addCursor(x: Int, y: Int, dir: Int = 1, parent: Cursor = null) = {
    val cursor = new Cursor(this, x, y, dir, parent)
    cursors.add(cursor)
    executorService.execute(cursor)
  }
}
