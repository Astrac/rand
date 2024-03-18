package astrac.rand
package randimpl

private[rand] transparent trait Errors:
  this: Rand.type =>

  sealed trait Error extends Exception
  class DropLimitReached(dropped: Long)
      extends Exception(s"Too many dropped generations after $dropped attempts")
      with Error
