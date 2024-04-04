package urru
package base


abstract class BaseException(_msg: String, _throw: Throwable = null)
    extends RuntimeException(_msg, _throw)
