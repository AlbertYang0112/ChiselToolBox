package AXI4

import Chisel._
import chisel3.util.Irrevocable

class RubbishSender extends Module {
  val axi4param = AXI4Parameter(
    idBits = 2,
    addrBits = 4,
    dataBits = 32,
    userBits = 0,
    wcorrupt = false,
    isLite = true,
    isMaster = true
  )
  val io = IO(new Bundle{
    val axitest = Irrevocable(new AXI4BundleResponse(axi4param))
  })
  val randomID = RegNext(LFSR16())
  val randomResp = RegNext(LFSR16())
  val valid = RegInit(false.B)
  // io.axitest.valid := true.B
  io.axitest.bits.id := 0.U(axi4param.idBits.W)
  io.axitest.bits.resp := 0.U(axi4param.respBits.W)
  io.axitest.valid := valid
  when(io.axitest.ready) {
    valid := true.B
    io.axitest.bits.id := randomID
    io.axitest.bits.resp := randomResp
    //io.axitest.bits.sig := counter(0)
  }
  .otherwise{
    valid := true.B
  }
}
