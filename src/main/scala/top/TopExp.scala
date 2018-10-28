package top

import chisel3._
import AXI4.{RubbishRecivier, RubbishSender}

class TopExp extends Module {
  val dst = Module(new RubbishRecivier)
  val src = Module(new RubbishSender)
  val io = IO(new Bundle{
    val onFire = Output(Bool())
    val recvResp = Output(UInt(src.axi4param.respBits.W))
    val recvID = Output(UInt(src.axi4param.idBits.W))
  })
  io.onFire := dst.io.holdOn
  io.recvResp := dst.io.recvResp
  io.recvID := dst.io.recvID
  dst.io.axi4test <> src.io.axitest
}

object TopExp extends App{
  chisel3.Driver.execute(args, () => new TopExp)
}
