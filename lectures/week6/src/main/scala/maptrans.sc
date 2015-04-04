//translate
case class Book(title: String, authors: List[String])

val books: List[Book] = List( Book("a", List("hi", "There")), Book("b", List("What","Huh")) )

for (b <- books; a <- b.authors if a startsWith "What") yield b.title

//into higher-order functions

books.flatMap( i => i.authors.withFilter(j => j.startsWith("What")).map( k => i.title) )

