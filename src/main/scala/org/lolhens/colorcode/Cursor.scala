package org.lolhens.colorcode

import java.util

/**
 * Created by LolHens on 04.12.2014.
 */
class Cursor(private val program: Program,
             var x: Int,
             var y: Int,
             var dir: Int,
             private val parent: Cursor) extends Runnable {

  val register = new Array[Short](256)
  var compare: Int = 0

  private val _stack: util.List[Short] = new util.ArrayList[Short]()

  def stack: util.List[Short] = if (parent == null) _stack else parent.stack

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
    val op = data >>> 24 & 0xFF
    val args = Array[Int](
      data >>> 16 & 0xFF,
      data >>> 8 & 0xFF,
      data & 0xFF
    )
    val short: Short = (data & 0xFFFF).asInstanceOf[Short]

    op match {
      case 1 =>
        register(args(0)) = short // SLOAD
      case 2 => register(args(0)) = register(args(1)) // MV
      case 3 => pushStack(register(args(0))) // PUSH
      case 4 => register(args(0)) = popStack // POP
      case 5 => register(args(0)) = System.in.read().asInstanceOf[Byte] // IN
      case 6 =>
        System.out.write(register(args(0)))
        System.out.flush() // OUT
      case 7 =>
        x = register(args(0))
        y = register(args(1))
        dir = register(args(2)) // JMP
      case 8 =>
        pause
        program.addCursor(register(args(0)), register(args(1)), register(args(2)), this) // CALL
      case 9 =>
        parent.resume
        end // RET
      case 10 => program.addCursor(register(args(0)), register(args(1)), register(args(2))) // RUN
      case 11 => register(args(0)) = (register(args(0)) + register(args(1))).asInstanceOf[Short] // ADD
      case 12 => register(args(0)) = (register(args(0)) - register(args(1))).asInstanceOf[Short] // SUB
      case 13 => register(args(0)) = (register(args(0)) * register(args(1))).asInstanceOf[Short] // MUL
      case 14 => register(args(0)) = (register(args(0)) / register(args(1))).asInstanceOf[Short] // DIV
      case 15 => register(args(0)) = (register(args(0)) & register(args(1))).asInstanceOf[Short] // AND
      case 16 => register(args(0)) = (register(args(0)) | register(args(1))).asInstanceOf[Short] // OR
      case 17 => register(args(0)) = (register(args(0)) ^ register(args(1))).asInstanceOf[Short] // XOR
      case 18 => register(args(0)) = (register(args(0)) >> register(args(1))).asInstanceOf[Short] // SHIFT-RIGHT
      case 19 => register(args(0)) = (register(args(0)) << register(args(1))).asInstanceOf[Short] // SHIFT-LEFT
      case 20 => register(args(0)) = (register(args(0)) >>> register(args(1))).asInstanceOf[Short] // USHIFT-RIGHT
      case 21 => compare = register(args(1)) - register(args(0)) // CP
      case 22 => compare = short - register(args(0)) // CP
      case 23 => if (compare == 0) dir = args(0) // BREQ
      case 24 => if (compare != 0) dir = args(0) // BRNE
      case 25 => if (compare < 0) dir = args(0) // BRLT
      case 26 => if (compare > 0) dir = args(0) // BRGT
      case 27 => if (compare <= 0) dir = args(0) // BRLE
      case 28 => if (compare >= 0) dir = args(0) // BRGE
      case 29 => dir = args(0) // BR
      case 30 => // IN
      case 255 => end // END
      case _ => // NOP
    }
    synchronized(wait(1))
  }

  def pause = paused = true

  def resume = {
    paused = false
    synchronized(notify())
  }

  def end = running = false
}