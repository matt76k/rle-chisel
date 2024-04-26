package rle

import chisel3._
import chisel3.util._
import chisel3.ChiselEnum
import _root_.circt.stage.ChiselStage

class EncoderIO(width: UInt) extends Bundle {
  val in = Flipped(Decoupled(width))
  val out = Decoupled(width)
}

object EncoderState extends ChiselEnum {
  val Idle, Counting = Value
}

class Encoder(dataW: UInt, relZero: UInt) extends Module {
  require(relZero.getWidth <= dataW.getWidth, "rleZero must fit within the width of gen")

  val io = IO(new EncoderIO(dataW))

  val state = RegInit(EncoderState.Idle)
  val count = RegInit(0.U(dataW.getWidth.W))

  io.out.bits := DontCare
  io.in.ready := true.B
  io.out.valid := false.B

  when (io.in.valid) {
    switch (state) {
      is(EncoderState.Idle) {
        when(io.in.bits =/= relZero) {
          io.out.bits := io.in.bits
          io.out.valid := true.B
          io.in.ready := true.B
        }.elsewhen(io.in.bits === relZero) {
          count := 1.U
          state := EncoderState.Counting
          io.out.bits := io.in.bits
          io.out.valid := true.B
          io.in.ready := true.B
        }
      }
      is(EncoderState.Counting) {
        when(io.in.bits =/= relZero) {
          io.out.bits := count
          io.out.valid := true.B
          io.in.ready := false.B
          state := EncoderState.Idle
          count := 0.U
        }.elsewhen(io.in.bits === relZero) {
          when(count ===  (math.pow(2, dataW.getWidth) - 1).toInt.U) {
            io.out.bits := count
            io.out.valid := true.B
            io.in.ready := false.B
            state := EncoderState.Idle
            count := 0.U
          }.otherwise {
            count := count + 1.U
            io.out.valid := false.B
            io.in.ready := true.B
          }
        }
      }
    }
  }.otherwise {
    io.out.valid := false.B
  }
}

object Encoder extends App {
  ChiselStage.emitSystemVerilogFile(
    new Encoder(UInt(8.W), 0.U),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}