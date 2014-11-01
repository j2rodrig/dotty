import annotation.tmt._

trait polydef1 {
    def myAsInstanceOf[T <: Class[T]](cls: java.lang.Class[_]): Class[T] = cls.asInstanceOf[Class[T]]
}