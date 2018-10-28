package AXI4

import chisel3._
import chisel3.util.Irrevocable

class RubbishRecivier extends Module {
  val axi4param = AXI4Parameter(
    idBits = 2,
    addrBits = 4,
    dataBits = 32,
    userBits = 0,
    wcorrupt = false,
    isLite = true,
    isMaster = false
  )
  val io = IO(new Bundle{
    //val axi4lite = Flipped(Irrevocable(new AXI4BundleResponse(axi4param)))
    val axi4test = Flipped(Irrevocable(new AXI4BundleResponse(axi4param)))
    val holdOn = Output(Bool())
    val recvResp = Output(UInt(axi4param.respBits.W))
    val recvID = Output(UInt(axi4param.idBits.W))
  })
  val valid = RegNext(io.axi4test.valid)
  val resp = RegInit(0.U(axi4param.respBits.W))
  val id = RegInit(0.U(axi4param.idBits.W))
  // io.axi4test.ready := true.B
  io.recvID := id
  io.recvResp := resp
  when(valid) {
    io.holdOn := true.B
    resp := io.axi4test.bits.resp
    id := io.axi4test.bits.id
    io.axi4test.ready := true.B
  }
  .otherwise {
    io.holdOn := false.B
    io.axi4test.ready := false.B
  }
}