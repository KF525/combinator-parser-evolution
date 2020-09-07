object Parser {

  type Parser[A] = String => List[(A, String)]

  def pure[A](a: A): Parser[A] = input => List((a, input))

  def zero[A]: Parser[A] = _ => List()

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
    item(input).flatMap { case (c, s) =>
      if (p(c)) List((c, s)) else List()
    }
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

  def plus[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] = input => { parser1(input) ++ parser2(input) }

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

  def zeroOrMore[A](a: Parser[A]): Parser[List[A]] = {
    val maybeSuccess: Parser[List[A]] = flatMap(a) { a1 =>
      flatMap(zeroOrMore(a)) { (a2: List[A]) =>
        pure(a1 +: a2)
      }
    }
    plus(maybeSuccess, pure(List()))
  }

  def oneOrMore[A](parser: Parser[A]): Parser[List[A]] = {
    flatMap(parser) { a1 =>
      flatMap(zeroOrMore(parser)) { (a2: List[A]) =>
        pure(a1 +: a2)
      }
    }
  }

  def zeroOrOne[A](parser: Parser[A]): Parser[Option[A]] = {
    input => parser(input) match {
      case List((a: A, input2)) => List((Some(a), input2))
      case _ => List((None, input))
    }
  }

  def nat: Parser[Int] = {
    flatMap(oneOrMore(digit)) { d =>
      pure(d.foldLeft(0)((n, c) => Integer.parseInt(c.toString) + (n*10)))
    }
  }

  def ident: Parser[String] = {
      flatMap(lower) { l =>
        flatMap(zeroOrMore(alphanumeric)) { al =>
          pure(l + al.mkString)
      }
    }
  }

  def number: Parser[Int] = plus(nat, pure(0))

  private def negate(int: Int): Int = -int

  def int: Parser[Int] = {
    flatMap(zeroOrOne(char('-'))) {
      case Some(_) => flatMap(nat) { n =>
        pure((negate(n)))
      }
      case None => nat
    }
  }

  def sepby1[A,B](parser: Parser[A])(sep: Parser[B]): Parser[List[A]] = {
    flatMap(parser){ (a1: A) =>
      flatMap(zeroOrOne(sep)) {
        case Some(_) => flatMap(sepby1(parser)(sep)) { (a2: List[A]) => pure(a1 +: a2) }
        case None => pure(List(a1))
        }
      }
  }

  def ints: Parser[List[Int]] = {
    flatMap(char('(')) { _ =>
      flatMap(sepby1(Parser.int)(Parser.char(','))) { (is: List[Int]) =>
        flatMap(char(')')) { _ =>
          pure(is)
        }
      }
    }
  }

  def bracket[A,B,C](open: Parser[A], parser: Parser[B], close: Parser[C]): Parser[B] = {
    flatMap(open) { _ =>
      flatMap(parser) { p =>
        flatMap(close) { _ =>
          pure(p)
        }
      }
    }
  }

  def intsWithBracket: Parser[List[Int]]  = {
    flatMap(bracket(
      Parser.char('('),
      Parser.sepby1(Parser.int)(Parser.char(',')),
      Parser.char(')'))) { i =>
      pure(i)
    }
  }

  def sepby[A,B](parser: Parser[A])(sep: Parser[B]): Parser[List[A]] = {
    val result: Parser[List[A]] = flatMap(parser){ (a1: A) =>
      flatMap(zeroOrOne(sep)) {
        case Some(_) => flatMap(sepby1(parser)(sep)) {
          (a2: List[A]) =>
            pure(a1 +: a2) }
        case None => pure(List(a1))
      }
    }
    plus(result, pure(List()))
  }

  def constant[A, B](parser: Parser[A], value: B): Parser[B] = {
    flatMap(parser) { _ =>
      pure(value)
    }
  }

  def chainl1Rescurse[A](parser1: Parser[A], parser2: Parser[(A, A) => A], result: A): Parser[A] = {
    flatMap(zeroOrOne(parser2)) { maybeOp: Option[(A,A) => A] =>
      flatMap(zeroOrOne(parser1)) { maybeA: Option[A] =>
        (maybeOp, maybeA) match {
          case (Some(op), Some(a)) => chainl1Rescurse(parser1, parser2, op.apply(a, result))
          case _ => pure(result)
        }
      }
    }
  }

  def chainl1[A](parser1: Parser[A], parser2: Parser[(A, A) => A]): Parser[A] = {
    flatMap(parser1) { a1 =>
        chainl1Rescurse(parser1, parser2, a1)
    }
  }

  def first[A](parser: Parser[A]): Parser[A] = input => {
     parser(input) match {
       case h::_ => List(h)
       case _ => List()
    }
  }

  def +++[A](parser1: Parser[A], parser2: Parser[A]): Parser[A] = Parser.first(plus(parser1, parser2))

  def color: Parser[String] = Parser.+++(Parser.string("yellow"), Parser.string("orange"))

  private def isSpace: Char => Boolean =  c => c.equals(' ') || c.equals('\n') || c.equals('\t')

  def spaces: Parser[()] =
    flatMap(oneOrMore(satisfies(isSpace))) { s =>
      pure(s)
    }

  def commentStart(target: String): Parser[String] =
    flatMap(dashes) { c =>
      if (c == target) pure(c) else zero
    }

  def dashes: Parser[String] = {
    val result = flatMap(char('-')) { c =>
      flatMap(dashes) { s =>
        pure(c + s)
      }
    }
    plus(result, pure(""))
  }

  def comment: Parser[()] =
    flatMap(commentStart("--")) { _ =>
      flatMap(zeroOrMore(satisfies(x => x != '\n'))) { c =>
        pure(c)
    }
  }

  def junk: Parser[()] = flatMap(zeroOrMore(+++(spaces, comment))) { j =>
    pure(j)
  }

  def parse[A](parser: Parser[A]): Parser[A] =
    flatMap(junk) { _ =>
      flatMap(parser) { a =>
        pure(a)
      }
    }

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
