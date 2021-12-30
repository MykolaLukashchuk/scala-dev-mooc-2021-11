package futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val rfs = futures.map(_ recover {
      case t: Throwable => t
    })

    Future.foldLeft(rfs)(List[A]() -> List[Throwable]())((result, v) => {
      val (success, failed) = result
      v match {
        case t: Throwable => success -> (failed :+ t)
        case a: A => (success :+ a) -> failed
      }
    })
  }
}
