package AXI4
import chisel3.util.isPow2
import scala.math.max

case class AXI4Parameter(
                   idBits:   Int, // Require idSize for master smaller than 4, slave 8
                   addrBits: Int,
                   dataBits: Int,
                   userBits: Int,
                   wcorrupt: Boolean,
                   isLite: Boolean=false,
                   isMaster: Boolean=true
                   ){
  require(dataBits >= 8, s"AXI4 data bits must be >= than 8 (got $dataBits)")
  require(addrBits >= 1, s"AXI4 address bits must be >= 1 (got $addrBits)")
  require(idBits >= 1, s"AXI4 id bits must be >= 8 (got $idBits)")
  require(isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")
  val lenBits   :Int    = 8
  val sizeBits  :Int    = 3
  val burstBits :Int    = 2
  val cacheBits :Int    = 4
  val protBits  :Int    = 3
  val qosBits   :Int    = 4
  val regionBits:Int    = 4
  val respBits  :Int    = 2

  def union(x: AXI4Parameter) =
    AXI4Parameter(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      max(idBits,   x.idBits),
      max(userBits, x.userBits),
      wcorrupt || x.wcorrupt)
}
