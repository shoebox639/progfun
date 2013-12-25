package suggestions
package observablex

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import rx.lang.scala.Observable
import rx.lang.scala.subjects.AsyncSubject
import rx.lang.scala.subjects.ReplaySubject

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    val subject = ReplaySubject[T]
    f onComplete {
      case Success(c) => {subject.onNext(c); subject.onCompleted()}
      case Failure(e) => {subject.onError(e) }
    }
    
    subject
  }

}