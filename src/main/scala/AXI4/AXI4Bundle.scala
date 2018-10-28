package AXI4
import Chisel._
import chisel3.util.Irrevocable
/*

case class AXI4BundleParameters(
                                 addrBits: Int,
                                 dataBits: Int,
                                 idBits: Int,
                                 userBits: Int,
                                 wcorrupt: Boolean=false
                               )
{
  require (dataBits >= 8, s"AXI4 data bits must be >= 8 (got $dataBits)")
  require (addrBits >= 1, s"AXI4 addr bits must be >= 1 (got $addrBits)")
  require (idBits >= 1, s"AXI4 id bits must be >= 1 (got $idBits)")
  require (isPow2(dataBits), s"AXI4 data bits must be pow2 (got $dataBits)")

  // Bring the globals into scope
  val lenBits   = AXI4Parameters.lenBits
  val sizeBits  = AXI4Parameters.sizeBits
  val burstBits = AXI4Parameters.burstBits
  val lockBits  = AXI4Parameters.lockBits
  val cacheBits = AXI4Parameters.cacheBits
  val protBits  = AXI4Parameters.protBits
  val qosBits   = AXI4Parameters.qosBits
  val respBits  = AXI4Parameters.respBits

  def union(x: AXI4BundleParameters) =
    AXI4BundleParameters(
      max(addrBits, x.addrBits),
      max(dataBits, x.dataBits),
      max(idBits,   x.idBits),
      max(userBits, x.userBits),
      wcorrupt || x.wcorrupt)
}
*/

abstract class AXI4BaseBundle(val param: AXI4Parameter) extends Bundle
{
  override def cloneType = {
    try {
      this.getClass.getConstructors.head.newInstance(param).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throw new Exception("Unable to use GenericParameterizedBundle.cloneType on " +
          this.getClass + ", probably because " + this.getClass +
          "() takes more than one argument.  Consider overriding " +
          "cloneType() on " + this.getClass, e)
    }
  }
}

class AXI4BundleTest(param: AXI4Parameter) extends AXI4BaseBundle(param)
{
  val sig = UInt(width = 1)
}

class AXI4BundleAddr(param: AXI4Parameter) extends AXI4BaseBundle(param) {
  val id        = UInt(width = param.idBits)
  val addr      = UInt(width = param.addrBits)
  val len       = UInt(width = param.lenBits)
  val size      = UInt(width = param.sizeBits)
  val burst     = UInt(width = param.burstBits)
  val lock      = Bool()
  val cache     = UInt(width = param.cacheBits)
  val prot      = UInt(width = param.protBits)
  val qos       = UInt(width = param.qosBits)
  val region    = UInt(width = param.regionBits)
  val user      = if(param.userBits > 0) Some(UInt(width = param.userBits)) else None
  //val valid     = Bool()
  //val ready     = Bool()

}

class AXI4BundleData(param: AXI4Parameter) extends AXI4BaseBundle(param){
  val id        = UInt(width = param.idBits)
  val data      = UInt(width = param.dataBits)
  val strobe    = UInt(width = param.dataBits / 8)
  val last      = Bool()
  val user      = if(param.userBits > 0) Some(UInt(width = param.userBits)) else None
  //val valid     = Bool()
  //val ready     = Bool()
}

class AXI4BundleResponse(param: AXI4Parameter) extends AXI4BaseBundle(param){
  val id        = UInt(width = param.idBits)
  val resp      = UInt(width = param.respBits)
  val user      = if(param.userBits > 0) Some(UInt(width = param.userBits)) else None
  //val valid     = Bool()
  //val ready     = Bool()
}

class AXI4Bundle(param: AXI4Parameter) extends AXI4BaseBundle(param){
  val aw = Irrevocable(new AXI4BundleAddr(param))
  val ar = Irrevocable(new AXI4BundleAddr(param))
  val w  = Irrevocable(new AXI4BundleData(param))
  val r  = Irrevocable(new AXI4BundleData(param))
  val b  = Irrevocable(new AXI4BundleResponse(param))

  def tieoff() {
    ar.ready.dir match {
      case INPUT =>
        // The direction of Ready is input means the port is for master.
        ar.ready := Bool(false)
        aw.ready := Bool(false)
        w.ready  := Bool(false)
        r.valid  := Bool(false)
        b.valid  := Bool(false)
      case OUTPUT =>
        // The direction of Ready is output means the port is for slave.
        ar.valid := Bool(false)
        aw.valid := Bool(false)
        w.valid  := Bool(false)
        r.ready  := Bool(false)
        b.ready  := Bool(false)
      case _ =>
    }
  }
}

object AXI4Bundle
{
  def apply(params: AXI4Parameter) = new AXI4Bundle(params)
}
