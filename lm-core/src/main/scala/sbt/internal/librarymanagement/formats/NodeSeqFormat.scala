package sbt.internal.librarymanagement.formats

import sjsonnew._
import scala.xml._

trait NodeSeqFormat { self: BasicJsonProtocol =>
  given NodeSeqFormat: JsonFormat[NodeSeq] = projectFormat[NodeSeq, String](
    xml => <binary>{xml}</binary>.toString,
    str => XML.loadString(str).child
  )
}
