package AXI4
import Chisel._
import chisel3.util.Irrevocable

trait AXI4RespCode {
  val _axiRespWidth = 2
  val OKAY: UInt = 0.U(_axiRespWidth.W)
  val EXOKAY: UInt = 1.U(_axiRespWidth.W)
  val SLVERR: UInt = 2.U(_axiRespWidth.W)
  val DECERR: UInt = 3.U(_axiRespWidth.W)
}

trait AXI4ARCacheCode {
  val _axiCacheWidth = 4
  val DEV_NONBUFFERABLE                 = 0x0.U(_axiCacheWidth.W)
  val DEV_BUFFERABLE                    = 0x1.U(_axiCacheWidth.W)
  val NORMAL_NONCACHEABLE_NONBUFFERABLE = 0x2.U(_axiCacheWidth.W)
  val NORMAL_NONCACHEABLE_BUFFERABLE    = 0x3.U(_axiCacheWidth.W)
  val WRITE_THROUGH_NO_ALLOCATE         = 0xA.U(_axiCacheWidth.W)
  val WRITE_THROUGH_READ_ALLOCATE       = 0xE.U(_axiCacheWidth.W)
  val WRITE_THROUGH_WRITE_ALLOCATE      = 0xA.U(_axiCacheWidth.W)
  val WRITE_THROUGH_READ_WRITE_ALLOCATE = 0xE.U(_axiCacheWidth.W)
  val WRITE_BACK_NO_ALLOCATE            = 0xB.U(_axiCacheWidth.W)
  val WRITE_BACK_READ_ALLOCATE          = 0xF.U(_axiCacheWidth.W)
  val WRITE_BACK_WRITE_ALLOCATE         = 0xB.U(_axiCacheWidth.W)
  val WRITE_BACK_READ_WRITE_ALLOCATE    = 0xF.U(_axiCacheWidth.W)
}

trait AXI4AWCacheCode {
  val _axiCacheWidth = 4
  val DEV_NONBUFFERABLE                 = 0x0.U(_axiCacheWidth.W)
  val DEV_BUFFERABLE                    = 0x1.U(_axiCacheWidth.W)
  val NORMAL_NONCACHEABLE_NONBUFFERABLE = 0x2.U(_axiCacheWidth.W)
  val NORMAL_NONCACHEABLE_BUFFERABLE    = 0x3.U(_axiCacheWidth.W)
  val WRITE_THROUGH_NO_ALLOCATE         = 0x6.U(_axiCacheWidth.W)
  val WRITE_THROUGH_READ_ALLOCATE       = 0x6.U(_axiCacheWidth.W)
  val WRITE_THROUGH_WRITE_ALLOCATE      = 0xE.U(_axiCacheWidth.W)
  val WRITE_THROUGH_READ_WRITE_ALLOCATE = 0xE.U(_axiCacheWidth.W)
  val WRITE_BACK_NO_ALLOCATE            = 0x7.U(_axiCacheWidth.W)
  val WRITE_BACK_READ_ALLOCATE          = 0x7.U(_axiCacheWidth.W)
  val WRITE_BACK_WRITE_ALLOCATE         = 0xF.U(_axiCacheWidth.W)
  val WRITE_BACK_READ_WRITE_ALLOCATE    = 0xF.U(_axiCacheWidth.W)
}

trait AXI4LockCode {
  val _axiLockWidth = 1
  val NORMAL_ACCESS = 0.U(_axiLockWidth.W)
  val EXCLUSIVE_ACCESS = 1.U(_axiLockWidth.W)
}

trait AXI4ProtectCode {
  val _axiProtWidth = 1
  val UNPRIVILEGED_ACCESS = 0.U(_axiProtWidth.W)
  val PRIVILEGED_ACCESS = 1.U(_axiProtWidth.W)
  val SECURE_ACCESS = 0.U(_axiProtWidth.W)
  val NONSECURE_ACCESS = 1.U(_axiProtWidth.W)
  val DATA_ACCESS = 0.U(_axiProtWidth.W)
  val INST_ACCESS = 1.U(_axiProtWidth.W)
}

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

class AXI4BundleDataWrite(param: AXI4Parameter) extends AXI4BaseBundle(param){
  // val id        = UInt(width = param.idBits)
  val data      = UInt(width = param.dataBits)
  val strobe    = UInt(width = param.dataBits / 8)
  val last      = Bool()
  val user      = if(param.userBits > 0) Some(UInt(width = param.userBits)) else None
  //val valid     = Bool()
  //val ready     = Bool()
}

class AXI4BundleDataRead(param: AXI4Parameter) extends AXI4BaseBundle(param){
  val id        = UInt(width = param.idBits)
  val data      = UInt(width = param.dataBits)
  val resp      = UInt(width = param.respBits)
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
  val aw = DecoupledIO(new AXI4BundleAddr(param))
  val ar = DecoupledIO(new AXI4BundleAddr(param))
  val w  = DecoupledIO(new AXI4BundleDataWrite(param))
  val r  = Flipped(DecoupledIO(new AXI4BundleDataRead(param)))
  val b  = Flipped(DecoupledIO(new AXI4BundleResponse(param)))

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
