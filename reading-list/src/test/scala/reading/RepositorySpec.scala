package reading

import cats._
import cats.data.State
import cats.implicits._
import org.scalatest.{MustMatchers, WordSpec}
import reading.domain._

// Exercise -
// 1. Implement InMemoryBookRepository or StateBookRepository
// 2. Implement a test that adds a book, then lists the books, and asserts the added book is listed.

class InMemoryUserRepository extends UserRepository[Id] {
  private var users = Map.empty[UserId, User]

  override def getUser(id: UserId): Id[Option[User]] =
    users.get(id)

  override def addUser(user: User): Id[Unit] =
    users = users + (user.id.get -> user)

  override def updateUser(user: User): Id[Unit] =
    users = users + (user.id.get -> user)
}

object StateUserRepository {
  type UserState     = Map[UserId, User]
  type UserStateM[A] = State[UserState, A]
}

import reading.StateUserRepository._

class StateUserRepository extends UserRepository[UserStateM] {

  override def getUser(id: UserId): UserStateM[Option[User]] =
    State.get[UserState].map(_.get(id))

  override def addUser(user: User): UserStateM[Unit] =
    State.modify[UserState] { state =>
      state + (user.id.get -> user)
    }

  override def updateUser(user: User): UserStateM[Unit] =
    State.modify[UserState] { state =>
      state + (user.id.get -> user)
    }
}

class InMemoryBookRepository extends BookRepository[Id] {
  private var books = Map.empty[BookId, Book]

  override def listBooks(): Id[List[Book]] =
    books.values.toList

  override def getBook(id: BookId): Id[Option[Book]] =
    books.get(id)

  override def addBook(book: Book): Id[Unit] =
    books = books + (book.id.get -> book)
}

object StateBookRepository {
  type BookState     = Map[BookId, Book]
  type BookStateM[A] = State[BookState, A]
}

import reading.StateBookRepository._

class StateBookRepository extends BookRepository[BookStateM] {
  override def listBooks(): BookStateM[List[Book]] =
    State.get[BookState].map { state =>
      state.values.toList
    }

  override def getBook(id: BookId): BookStateM[Option[Book]] =
    State.get[BookState].map { state =>
      state.get(id)
    }

  override def addBook(book: Book): BookStateM[Unit] =
    State.modify[BookState] { state =>
      state + (book.id.get -> book)
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

      userOpt.isDefined mustBe true
    }

    "StateUserRepository: added user can be retrieved" in {
      val repository = new StateUserRepository
      val stateComputation = for {
        _    <- repository.addUser(user)
        user <- repository.getUser(user.id.get)
      } yield user

      val (_, userOpt) = stateComputation.run(Map.empty).value

      userOpt mustBe defined
    }

    "InMemoryUserRepository: user can be updated" in {
      val repository = new InMemoryUserRepository
      val userOpt: Id[Option[User]] = for {
        _    <- repository.addUser(user)
        _    <- repository.updateUser(user.copy(firstName = "Fred"))
        user <- repository.getUser(userId)
      } yield user

      userOpt mustBe defined
      userOpt.get.firstName mustBe "Fred"
    }

    "StateUserRepository: user can be updated" in {
      val repository = new StateUserRepository
      val stateComputation = for {
        _    <- repository.addUser(user)
        _    <- repository.updateUser(user.copy(firstName = "Fred"))
        user <- repository.getUser(userId)
      } yield user

      val (_, userOpt) = stateComputation.run(Map.empty).value

      userOpt mustBe defined
      userOpt.get.firstName mustBe "Fred"
    }
  }

  "BookRepository" should {
    "InMemoryBookRepository: added book can be retrieved" in {
      val repository = new InMemoryBookRepository
      val bookOpt = for {
        _    <- repository.addBook(book)
        book <- repository.getBook(bookId)
      } yield book

      bookOpt mustBe defined
    }

    "StateBookRepository: added book can be retrieved" in {
      val repository = new StateBookRepository
      val stateComputation = for {
        _    <- repository.addBook(book)
        book <- repository.getBook(bookId)
      } yield book

      val (_, bookOpt) = stateComputation.run(Map.empty).value

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
      val stateComputation = for {
        _     <- repository.addBook(book)
        books <- repository.listBooks()
      } yield books

      val (_, bookList) = stateComputation.run(Map.empty).value

      bookList.size mustBe 1
    }
  }
}