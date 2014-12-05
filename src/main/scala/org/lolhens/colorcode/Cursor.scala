package org.lolhens.colorcode

import java.util

/**
 * Created by LolHens on 04.12.2014.
 */
class Cursor(var program: Program, var x: Int, var y: Int, var dir: Int) extends Runnable {
  val register = new Array[Short](16)
  private val stack: util.List[Short] = new util.ArrayList[Short]()

  def popStack: Short = {
    val index = stack.size - 1
    val data = stack.get(index)
    stack.remove(index)
    data
  }

  def pushStack(short: Short): Unit = stack.add(short)

  private var running: Boolean = true
  private var paused: Boolean = false

  override final def run(): Unit = {
    while (running) {
      op(program.data(x)(y))
      while (paused) synchronized(wait(100))
      move(
        dir match {
          case 1 => 1
          case 3 => -1
          case _ => 0
        },
        dir match {
          case 0 => -1
          case 2 => 1
          case _ => 0
        })
    }
  }

  private def move(xOff: Int, yOff: Int): Unit = {
    if (xOff != 0) {
      if (x + xOff >= program.width) {
        x = 0
        if (y + 1 >= program.height) y = 0
        else y += 1
      } else if (x + xOff < 0) {
        x = program.width - 1
        if (y - 1 < 0) y = program.height - 1
        else y -= 1
      } else
        x += xOff
    } else if (yOff != 0) {
      if (y + yOff >= program.height) {
        y = 0
        if (x + 1 >= program.width) x = 0
        else x += 1
      } else if (y + yOff < 0) {
        y = program.height - 1
        if (x - 1 < 0) x = program.width - 1
        else x -= 1
      } else
        y += yOff
    }
  }

  private def op(data: Int) = {
    val op = data >>> 20
    val args = new Array[Int](5)
    for (i <- 0 until args.length) args(i) = data >>> (i * 4) & 0xF
    val short: Short = (data & 0xFFFF).asInstanceOf[Short]

    op match {
      case 1 => register(args(0)) = short // SLOAD
      case 2 => register(args(0)) = register(args(1)) // MV
      case 3 => pushStack(register(args(0))) // PUSH
      case 4 => register(args(0)) = popStack // POP
      case 5 => register(args(0)) = System.in.read().asInstanceOf[Byte] // IN
      case 6 => System.out.write(register(args(0))) // OUT

      case _ => // NOP
    }
    synchronized(wait(1))
  }

  def pause() = paused = true

  def resume() = {
    paused = false
    synchronized(notify())
  }

  def end() = running = false
}