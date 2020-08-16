object Parser {

  type Parser[A] = String => List[(A, String)]

  def pure[A](a: A): Parser[A] = input => List((a, input))

  def zero[A]: Parser[A] = input => List()

  def item: Parser[Char] = input => {
    input.headOption.foldLeft(List.empty[(Char, String)])((l, o) => (o, input.tail) :: l)
  }

  def flatMap[A, B](parser: Parser[A])(f: A => Parser[B]): Parser[B] =
    input => {
      parser(input).flatMap { case (a, s) =>
        val fa: Parser[B] = f(a)
        fa(s)
      }
    }

  def satisfies(p: Char => Boolean): Parser[Char] = input => {
    item(input).flatMap { case (c, s) => if (p(c)) List((c, s)) else List() }
  }

  // utilizes parser flatMap, parser pure and parser zero
  def satisfiesWithParserFlatMap(p: Char => Boolean): Parser[Char] = {
    val result: Parser[Char] = input => item(input)
    flatMap(result) { char => if (p(char)) pure(char) else zero }
  }

  // utilizes parser satisfies
  def char(c: Char): Parser[Char] = satisfies(i => i == c)

  // utilizes parser satisfies
  def digit: Parser[Char] = satisfies(i => i.isDigit)

  def lower: Parser[Char] = satisfies(i => i.isLower)

  def upper: Parser[Char] = satisfies(i => i.isUpper)

  def plus[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] =
    input => {
      parser1(input) ++ parser2(input)
    }

  // utilizes parser plus, lower, upper
  def letter: Parser[Char] = plus(lower, upper)

  // utilizes parser plus, letter, digit
  def alphanumeric: Parser[Char] = plus(letter, digit)

  // utilizes parser flatMap, letter, getWord, pure
  def word: Parser[String] = {
    val result = flatMap(letter) { c =>
      flatMap(word) { s =>
        pure(c + s)
      }
    }
    plus(result, pure(""))
  }

  // utilizes parser flatMap, word, pure, zero
  def string(target: String): Parser[String] =
    flatMap(word) { w =>
      if (w == target) pure(w) else zero
    }

  //Zero or more
  def many[A](a: Parser[A]): Parser[List[A]] = {
    val maybeSuccess: Parser[List[A]] = flatMap(a) { a1 =>
      flatMap(many(a)) { (a2: List[A]) =>
        pure(a1 +: a2)
      }
    }
    plus(maybeSuccess, pure(List()))
  }

  //1 or more
  def many1[A](a: Parser[A]): Parser[List[A]] = {
    flatMap(a) { a1 =>
      flatMap(many(a)) { (a2: List[A]) =>
        pure(a1 +: a2)
      }
    }
  }

  def nat: Parser[Int] = ???

  //Playing around with how input is treated
  def inputExploration1: Parser[Char] = {
    letter
    letter
    letter
  }

  def inputExploration2: Parser[Char] = {
    flatMap(letter) { l =>
      flatMap(letter) { l2 =>
        flatMap(letter) { l3 =>
          pure(l3)
        }
      }
    }
  }

  def inputExploration3: Parser[String] = {
    flatMap(letter) { l =>
      flatMap(letter) { l2 =>
        flatMap(letter) { l3 =>
          pure(l.toString + l2.toString + l3.toString)
        }
      }
    }
  }
}