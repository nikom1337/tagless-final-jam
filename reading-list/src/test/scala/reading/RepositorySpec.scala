package reading

import cats.data.{EitherT, State}
import cats.implicits._
import org.scalatest.{MustMatchers, WordSpec}
import reading.StateBookRepository._
import reading.StateUserRepository._
import reading.domain._

import scala.util.Try

class InMemoryUserRepository extends UserRepository[Try] {
  private var users = Map.empty[UserId, User]

  override def getUser(id: UserId): Try[Option[User]] = Try {
    users.get(id)
  }

  override def addUser(user: User): Try[Unit] = Try {
    users = users + (user.id.get -> user)
  }

  override def updateUser(user: User): Try[Unit] = Try {
    users = users + (user.id.get -> user)
  }
}

object StateUserRepository {
  type UserState     = Map[UserId, User]
  type UserStateM[A] = EitherT[State[(BookState, UserState), ?], Throwable, A]
}

class StateUserRepository extends UserRepository[UserStateM] {

  override def getUser(id: UserId): UserStateM[Option[User]] = EitherT.right {
    State.get[(BookState, UserState)].map(_._2.get(id))
  }

  override def addUser(user: User): UserStateM[Unit] = EitherT.right {
    State.modify[(BookState, UserState)] { state =>
      (state._1, state._2 + (user.id.get -> user))
    }
  }

  override def updateUser(user: User): UserStateM[Unit] = EitherT.right {
    State.modify[(BookState, UserState)] { state =>
      (state._1, state._2 + (user.id.get -> user))
    }
  }
}

class InMemoryBookRepository extends BookRepository[Try] {
  private var books = Map.empty[BookId, Book]

  override def listBooks(): Try[List[Book]] = Try {
    books.values.toList
  }

  override def getBook(id: BookId): Try[Option[Book]] = Try {
    books.get(id)
  }

  override def addBook(book: Book): Try[Unit] = Try {
    books = books + (book.id.get -> book)
  }
}

object StateBookRepository {
  type BookState     = Map[BookId, Book]
  type BookStateM[A] = EitherT[State[(BookState, UserState), ?], Throwable, A]
}

class StateBookRepository extends BookRepository[BookStateM] {
  override def listBooks(): BookStateM[List[Book]] = EitherT.right {
    State.get[(BookState, UserState)].map { state =>
      state._1.values.toList
    }
  }

  override def getBook(id: BookId): BookStateM[Option[Book]] = EitherT.right {
    State.get[(BookState, UserState)].map { state =>
      state._1.get(id)
    }
  }

  override def addBook(book: Book): BookStateM[Unit] = EitherT.right {
    State.modify[(BookState, UserState)] { state =>
      (state._1 + (book.id.get -> book), state._2)
    }
  }
}

class RepositorySpec extends WordSpec with MustMatchers {
  val userId = UserId("1")
  val user = User(
    id = Some(userId),
    firstName = "Eli",
    lastName = "Jordan",
    books = List.empty
  )

  val bookId = BookId("1")
  val book = Book(
    id = Some(bookId),
    title = "A Game of Thrones",
    author = "George R. R. Martin"
  )

  "UserRepository" should {
    "InMemoryUserRepository: added user can be retrieved" in {
      val repository = new InMemoryUserRepository
      val userOpt = for {
        _    <- repository.addUser(user)
        user <- repository.getUser(user.id.get)
      } yield user

      userOpt.get.isDefined mustBe true
    }

    "StateUserRepository: added user can be retrieved" in {
      val repository = new StateUserRepository
      val stateComputation = for {
        _    <- repository.addUser(user)
        user <- repository.getUser(user.id.get)
      } yield user

      val (_, Right(userOpt)) = stateComputation.value.run(Map.empty[BookId, Book] -> Map.empty[UserId, User]).value

      userOpt mustBe defined
    }

    "InMemoryUserRepository: user can be updated" in {
      val repository = new InMemoryUserRepository
      val userOpt = for {
        _    <- repository.addUser(user)
        _    <- repository.updateUser(user.copy(firstName = "Fred"))
        user <- repository.getUser(userId)
      } yield user

      userOpt.get mustBe defined
      userOpt.get.get.firstName mustBe "Fred"
    }

    "StateUserRepository: user can be updated" in {
      val repository = new StateUserRepository
      val eitherTComputation = for {
        _    <- repository.addUser(user)
        _    <- repository.updateUser(user.copy(firstName = "Fred"))
        user <- repository.getUser(userId)
      } yield user

      val (_, Right(userOpt)) = eitherTComputation.value.run(Map.empty[BookId, Book] -> Map.empty[UserId, User]).value

      userOpt mustBe defined
      userOpt.get.firstName mustBe "Fred"
    }
  }

  "BookRepository" should {
    "InMemoryBookRepository: added book can be retrieved" in {
      val repository = new InMemoryBookRepository
      val bookF = for {
        _    <- repository.addBook(book)
        book <- repository.getBook(bookId)
      } yield book

      bookF.get mustBe defined
    }

    "StateBookRepository: added book can be retrieved" in {
      val repository = new StateBookRepository
      val eitherTComputation = for {
        _    <- repository.addBook(book)
        book <- repository.getBook(bookId)
      } yield book

      val (_, Right(bookOpt)) = eitherTComputation.value.run(Map.empty[BookId, Book] -> Map.empty[UserId, User]).value

      bookOpt mustBe defined
    }

    "InMemoryBookRepository: added book can be listed" in {
      val repository = new InMemoryBookRepository
      val bookList = for {
        _     <- repository.addBook(book)
        books <- repository.listBooks()
      } yield books

      bookList.size mustBe 1
    }

    "StateBookRepository: added book can be listed" in {
      val repository = new StateBookRepository
      val eitherTComputation = for {
        _     <- repository.addBook(book)
        books <- repository.listBooks()
      } yield books

      val (_, Right(bookList)) = eitherTComputation.value.run(Map.empty[BookId, Book] -> Map.empty[UserId, User]).value

      bookList.size mustBe 1
    }
  }
}
