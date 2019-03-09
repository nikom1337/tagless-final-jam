package reading

import cats.data.{EitherT, State}
import cats.implicits._
import org.scalatest.{MustMatchers, WordSpec}
import reading.StateBookRepository.BookState
import reading.StateUserRepository.UserState
import reading.domain._
import reading.interpreters._

import scala.util.{Failure, Try}

class ReadingListServiceCompilerSpec extends WordSpec with MustMatchers {

  val userId = UserId("1")
  private def memoryFixture = new {
    val userRepository = new InMemoryUserRepository
    val bookRepository = new InMemoryBookRepository

    val book1 = Book(id = Some(BookId("1")), title = "A Game of Thrones", author = "George R. R. Martin")
    val book2 = Book(id = Some(BookId("2")), title = "A Clash of Kings", author = "George R. R. Martin")
    val books = List(book1, book2)

    val user = User(
      id = Some(userId),
      firstName = "Eli",
      lastName = "Jordan",
      books = List(
        BookId("1"),
        BookId("2")
      )
    )

    userRepository.addUser(user)
    books.traverse(bookRepository.addBook)

    val readingListService = new ReadingListServiceCompiler[Try](userRepository, bookRepository)
  }

  private def stateFixture = new {
    val userRepository = new StateUserRepository
    val bookRepository = new StateBookRepository

    val book1 = Book(id = Some(BookId("1")), title = "A Game of Thrones", author = "George R. R. Martin")
    val book2 = Book(id = Some(BookId("2")), title = "A Clash of Kings", author = "George R. R. Martin")
    val books = List(book1, book2)

    val user = User(
      id = Some(userId),
      firstName = "Eli",
      lastName = "Jordan",
      books = List(
        BookId("1"),
        BookId("2")
      )
    )

    val initialUsers = Map(userId -> user)
    val initialBooks = Map(
      BookId("1") -> book1,
      BookId("2") -> book2
    )

    val initialState = (initialBooks, initialUsers)

    type AppM[A] = EitherT[State[(BookState, UserState), ?], Throwable, A]

    val readingListService = new ReadingListServiceCompiler[AppM](userRepository, bookRepository)
  }

  "ReadingListService" should {
    "InMemory: Return the reading list for a given user" in {
      val f = memoryFixture
      import f._

      val readingList = readingListService.getReadingList(UserId("1"))
      readingList.get.user mustBe user
      readingList.get.books mustBe books
    }

    "State: Return the reading list for a given user" in {
      val f = stateFixture
      import f._

      val readingList = readingListService.getReadingList(UserId("1"))
      val (_, Right(list)) = readingList.value.run(initialState).value

      list.user mustBe user
      list.books mustBe books
    }

    "InMemory: Add a book to the reading list" in {
      val f = memoryFixture
      import f._

      val book = Book(id = Some(BookId("3")), title = "Fake Book", author = "Fake Author")
      val readingList = for {
        _ <- bookRepository.addBook(book)
        _ <- readingListService.addToReadingList(userId, book.id.get)
        list <- readingListService.getReadingList(userId)
      } yield list

      readingList.get.books must contain(book)
    }

    "State: Add a book to the reading list" in {
      val f = stateFixture
      import f._

      val book = Book(id = Some(BookId("3")), title = "Fake Book", author = "Fake Author")
      val readingList = for {
        _ <- bookRepository.addBook(book)
        _ <- readingListService.addToReadingList(userId, book.id.get)
        list <- readingListService.getReadingList(userId)
      } yield list

      val (_, Right(list)) = readingList.value.run(initialState).value

      list.books must contain(book)
    }

    "InMemory: Remove a book from the reading list" in {
      val f = memoryFixture
      import f._

      val readingList = for {
        _ <- readingListService.removeFromReadingList(userId, book1.id.get)
        list <- readingListService.getReadingList(userId)
      } yield list

      readingList.get.books must not contain book1
    }

    "State: Remove a book from the reading list" in {
      val f = stateFixture
      import f._

      val readingList = for {
        _ <- readingListService.removeFromReadingList(userId, book1.id.get)
        list <- readingListService.getReadingList(userId)
      } yield list

      val (_, Right(list)) = readingList.value.run(initialState).value

      list.books must not contain book1
    }

    "InMemory: Result in NoSuchUserException if the specified user doesn't exist" in {
      val f = memoryFixture
      import f._

      val Failure(e) = readingListService.getReadingList(UserId("2"))

      e mustBe a[NoSuchUserException]
    }

    "State: Result in NoSuchUserException if the specified user doesn't exist" in {
      val f = stateFixture
      import f._

      val readingList = readingListService.getReadingList(UserId("2"))
      val (_, Left(e)) = readingList.value.run(initialState).value

      e mustBe a[NoSuchUserException]
    }
  }
}
