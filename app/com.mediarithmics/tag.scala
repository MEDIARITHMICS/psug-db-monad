package com.mediarithmics

trait Tagged[U]

object tag {

  type TLong[U] = Long with Tagged[U]

  def TLong[U](s: Long): TLong[U] = s.asInstanceOf[TLong[U]]

  implicit class LongConverter(val l: Long) extends AnyVal {
    def tag[U]: TLong[U] = TLong[U](l)
  }

}
