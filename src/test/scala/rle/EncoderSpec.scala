package rle

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chisel3.simulator.EphemeralSimulator._

class EncoderSpec extends AnyFlatSpec with Matchers {

  def run_test (seq_in: Seq[UInt], seq_out: Seq[UInt], description: String):Unit = {
    it should description in {
      simulate(new Encoder(UInt(8.W), 0.U)) { c =>
        c.reset.poke(true.B)
        c.clock.step()
        c.reset.poke(false.B)
        c.clock.step()

        var sent, received: Int = 0

        while (sent != seq_in.length && received != seq_out.length) {
          if (sent < seq_in.length) {
            c.io.in.valid.poke(true.B)
            c.io.in.bits.poke(seq_in(sent))
            if (c.io.in.ready.peek().litToBoolean) {
              sent += 1
            }
          }

          if (received < seq_out.length) {
            c.io.out.ready.poke(true.B)
            if (c.io.out.valid.peek().litToBoolean) {
              c.io.out.bits.expect(seq_out(received))
              received += 1
            }
          }

          c.clock.step()
        }
      }
    }
  }

  var description = "a stream with no zeros"
  var seq_in = Seq(1.U, 2.U, 3.U, 4.U, 5.U, 6.U, 7.U, 8.U, 9.U)
  var seq_out = Seq(1.U, 2.U, 3.U, 4.U, 5.U, 6.U, 7.U, 8.U, 9.U)
  run_test(seq_in, seq_out, description)

  description = "a stream with both zeros and non-zeros"
  seq_in = Seq(1.U, 2.U, 0.U, 0.U, 0.U, 0.U, 7.U, 8.U, 9.U)
  seq_out = Seq(1.U, 2.U, 0.U, 4.U, 7.U, 8.U, 9.U)
  run_test(seq_in, seq_out, description)

  description = "a stream with a single zero"
  seq_in = Seq(1.U, 2.U, 0.U, 7.U, 8.U, 9.U)
  seq_out = Seq(1.U, 2.U, 0.U, 1.U, 7.U, 8.U, 9.U)
  run_test(seq_in, seq_out, description)

  description = "a stream with a single zero at the end"
  seq_in = Seq(1.U, 2.U, 7.U, 8.U, 9.U, 0.U)
  seq_out = Seq(1.U, 2.U, 7.U, 8.U, 9.U, 0.U, 1.U)
  run_test(seq_in, seq_out, description)

}