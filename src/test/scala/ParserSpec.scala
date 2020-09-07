import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import Parser.Parser

class ParserSpec extends AnyFlatSpecLike with Matchers {
  "pure" should "succeed returning value without consuming any input" in {
    val value = "value"
    val input = "input"
    Parser.pure(value)(input) shouldBe List((value, input))
  }

  it should "should handle empty input" in {
    val value = "value"
    Parser.pure(value)("") shouldBe List((value, ""))
  }

  "zero" should "fail regardless of input" in {
    Parser.zero("fail") shouldBe List()
    Parser.zero("") shouldBe List()
  }

  "item" should "consume the first character if input is non-empty" in {
    val input = "consume"
    Parser.item(input) shouldBe List((input.head, input.tail))
  }

  it should "handle input with one character" in {
    val input = "c"
    Parser.item(input) shouldBe List(('c', ""))
  }

  it should "fail if input is empty" in {
    Parser.item("") shouldBe List()
  }

  /*
  The flatMap combinator avoids the problem of nested tuples of results
  because the results of the first parser are made directly available for processing by the second,
  rather than being paired up with the other results to be processed later on.
  */
  "flatMap" should "chain parser results" in {
    val parser1: Parser[String] = Parser.pure("one")

    def f: String => Parser[String] = s => Parser.pure[String](s.toUpperCase)

    Parser.flatMap(parser1)(f)("input") shouldBe List(("ONE", "input"))
  }

  it should "chain zero parser results" in {
    val parser: Parser[String] = Parser.zero

    def f(s: String): Parser[String] = Parser.pure[String](s.toUpperCase)

    Parser.flatMap(parser)(f)("input") shouldBe List()
  }

  "satisfies" should "returns value and remaining input if predicate true" in {
    def predicate: Char => Boolean = c => c == ('y')
    Parser.satisfies(predicate)("yes") shouldBe List(('y', "es"))
  }

  it should "returns empty list is predicate false" in {
    def predicate: Char => Boolean = c => c == ('y')
    Parser.satisfies(predicate)("nope") shouldBe List()
  }

  "satisfiesWithParserFlatMap" should "returns value and remaining input if predicate true" in {
    def predicate: Char => Boolean = c => c == ('c')
    Parser.satisfiesWithParserFlatMap(predicate)("congratulations") shouldBe List(('c', "ongratulations"))
  }

  it should "returns empty list is predicate false" in {
    def predicate: Char => Boolean = c => c == ('c')
    Parser.satisfiesWithParserFlatMap(predicate)("nope") shouldBe List()
  }

  "char" should "parse single character if the character matches" in {
    Parser.char('e')("enjoyment") shouldBe List(('e', "njoyment"))
  }

  it should "return an empty list if character does not match" in {
    Parser.char('e')("nope") shouldBe List()
  }

  "digit" should "parse character as digit if valid" in {
    Parser.digit("1test") shouldBe List(('1', "test"))
  }

  it should "not parse multiple character as digit if valid" in {
    Parser.digit("12test") shouldBe List(('1', "2test"))
  }

  it should "return an empty list if character is not valid digit" in {
    Parser.digit("test") shouldBe List()
  }

  "lower" should "parse character if it is lowercase" in {
    Parser.lower("lowercase") shouldBe List(('l', "owercase"))
  }

  it should "not parse character if it is lowercase" in {
    Parser.lower("Lowercase") shouldBe List()
  }

  /*
  Both argument parsers p and q are applied to the same input string,
  and their result lists are concatenated to form a single result list.
  Note that it is not required that p and q accept disjoint sets of strings:
  if both parsers succeed on the input string then more than one result value will be returned,
  reflecting the different ways that the input string can be parsed.
  */
  "plus" should "apply two parsers to the same input and succeed if both succeed" in {
    val parser1 = Parser.char('y')
    val parser2 = Parser.lower

    Parser.plus(parser1, parser2)("yes") shouldBe List(('y', "es"), ('y', "es"))
  }

  it should "return an empty list if both parsers fails" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("yes") shouldBe List()
  }

  it should "return a value if the second parser succeeds" in {
    val parser1 = Parser.char('e')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("1yes") shouldBe List(('1', "yes"))
  }

  it should "return a value if the second parser fails" in {
    val parser1 = Parser.char('y')
    val parser2 = Parser.digit

    Parser.plus(parser1, parser2)("yes") shouldBe List(('y', "es"))
  }

  "letter" should "recognize an upper or lowercase letter" in {
    Parser.letter("letter") shouldBe List(('l', "etter"))
    Parser.letter("Letter") shouldBe List(('L', "etter"))
  }

  it should "fail to parse a letter when given non letter" in {
    Parser.letter("1letter") shouldBe List()
    Parser.letter("!letter") shouldBe List()
  }

  "alphanumeric" should "parse any alphanumeric character" in {
    Parser.alphanumeric("a1") shouldBe List(('a', "1"))
    Parser.alphanumeric("1a") shouldBe List(('1', "a"))
    Parser.alphanumeric("1.") shouldBe List(('1', "."))
  }

  it should "fail to parse a non-alphanumeric character" in {
    Parser.alphanumeric(".1") shouldBe List()
  }

  /*
  That is, word either parses a non-empty word (a single letter followed by a word, using a recursive call to word), in which case the two results are combined to form a string, or parses nothing and returns the empty string.
For example, applying word to the input "Yes!" gives the result [("Yes","!"), ("Ye","s!"), ("Y","es!"), ("","Yes!")]. The first result, ("Yes","!"), is the expected result: the string of letters "Yes" has been consumed, and the unconsumed input is "!". In the subsequent results a decreasing number of letters are consumed. This behaviour arises because the choice operator plus is non-deterministic: both alternatives can be explored, even if the first alternative is successful. Thus, at each application of letter, there is always the option to just finish parsing, even if there are still letters left to be consumed from the start of the input.
   */
  "word" should "parse a collection of letters" in {
    Parser.word("Yes!") shouldBe List(("Yes", "!"), ("Ye", "s!"), ("Y", "es!"), ("", "Yes!"))
  }

  it should "handle when the start of the input is not a word" in {
    Parser.word("!Yes") shouldBe List(("", "!Yes"))
  }

  it should "return an empty list if the target does not match" in {
    Parser.string("string")("unstringify") shouldBe List()
  }

  it should "return an empty list if there is only partial match" in {
    Parser.string("string")("strinly") shouldBe List()
  }

  "zeroOrMore" should "return success if it matches 0 or more times" in {
    val letterParser = Parser.letter

    Parser.zeroOrMore(letterParser)("c123") shouldBe List((List('c'), "123"), (List(), "c123"))
    Parser.zeroOrMore(letterParser)("abc123") shouldBe
      List((List('a', 'b', 'c'), "123"), (List('a', 'b'), "c123"), (List('a'), "bc123"), (List(), "abc123"))
    Parser.zeroOrMore(letterParser)("1a") shouldBe List((List(), "1a"))
    Parser.zeroOrMore(letterParser)("a") shouldBe List((List('a'), ""), (List(), "a"))
  }

  "oneOrMore" should "return success if it matches one or more times" in {
    val letterParser = Parser.letter

    Parser.oneOrMore(letterParser)("c123") shouldBe List((List('c'), "123"))
    Parser.oneOrMore(letterParser)("abc123") shouldBe
      List((List('a', 'b', 'c'), "123"), (List('a', 'b'), "c123"), (List('a'), "bc123"))
    Parser.oneOrMore(letterParser)("a") shouldBe List((List('a'), ""))
    Parser.oneOrMore(letterParser)("1a") shouldBe List()
  }

  "zeroOrOne" should "parse if parser succeeds" in {
    Parser.zeroOrOne(Parser.char('-'))("-12") shouldBe List((Some('-'), "12"))
  }

  it should "parse if parser fails" in {
    Parser.zeroOrOne(Parser.digit)("a1") shouldBe List((None, "a1"))
  }

  "nat" should "parse when there is at least one digit" in {
    Parser.nat("1a") shouldBe List((1, "a"))
    Parser.nat("12a") shouldBe List((12, "a"), (1, "2a"))
  }

  it should "fail to parse when there is no digit character" in {
    Parser.nat("a12") shouldBe List()
  }

  "ident" should "parse lower-case letter followed by 0 or more alphanumeric" in {
    Parser.ident("a1B2.") shouldBe List(("a1B2","."),("a1B","2."),("a1","B2."),("a", "1B2."))
  }

  it should "not successfully parse when it does not start with a lower-case letter" in {
    Parser.ident("A1") shouldBe List()
    Parser.ident("1a") shouldBe List()
  }

  it should "not successfully parse when it starts with non-alphanumeric" in {
    Parser.ident(".a") shouldBe List()
  }

  "number" should "return a default of 0 if no digit is parsed" in {
    Parser.number("hello") shouldBe List((0, "hello"))
  }

  "int" should "parse a positive integer" in {
    Parser.int("12a") shouldBe List((12, "a"), (1, "2a"))
  }

  it should "parse a negative integer" in {
    Parser.int("-12a") shouldBe List((-12, "a"), (-1,"2a"))
  }

  "sepby1" should "parse a list separated by character" in {
    Parser.sepby1(Parser.digit)(Parser.char(','))("1,2") shouldBe List((List('1','2'), ""))
  }

  it should "handle a list of one" in {
    Parser.sepby1(Parser.digit)(Parser.char(','))("1a") shouldBe List((List('1'),"a"))
  }

  it should "fail to parse if input does not initially match parser a" in {
    Parser.sepby1(Parser.digit)(Parser.char(','))("a1,2") shouldBe List()
  }

  "ints" should "parse a list of ints" in {
    Parser.ints("(1,2)") shouldBe List((List(1,2), ""))
  }

  "bracket" should "ignore unnecessary characters" in {
    Parser.bracket(Parser.char('('), Parser.char('a'), Parser.char(')'))("(a)") shouldBe List(('a', ""))
  }

  "intsWithBracket" should "utilize bracket combinator parser" in {
    Parser.ints("(1,2)") shouldBe List((List(1,2), ""))
  }

  //TODO: Figure out why this is failing
  "sepby" should "parse a sequence" in {
    Parser.sepby(Parser.int)(Parser.char(','))("1,2") shouldBe List((List(1,2), ""))
  }

  it should "handle empty sequences" in {
    Parser.sepby(Parser.int)(Parser.char(','))("a") shouldBe List((List(), "a"))
  }

  "chainl1" should "parses non-empty sequences of items separated by operators that associate to left" in {
      def add: (String, String) => String = (a:String, b: String) => a ++ b

      val stringParser: Parser[String] = Parser.string("a")
      val plus: Char => Boolean = (c: Char) => c.equals('+')
      val constantParser = Parser.constant(Parser.satisfies(plus), add)

      val chainParser = Parser.chainl1[String](stringParser, constantParser)
      chainParser("a+a+a") shouldBe List(("aaa", ""))
  }

  it should "handle int parser" in {
    def sub: (Int, Int) => Int = (a:Int, b: Int) => b - a

    val intParser: Parser[Int] = Parser.int
    val plus: Char => Boolean = (c: Char) => c.equals('-')
    val constantParser = Parser.constant(Parser.satisfies(plus), sub)

    Parser.chainl1(intParser, constantParser)("1-2-3") shouldBe List((-4, ""))  // (1-2)-3 => -4
  }

  it should "handle just a single value" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    Parser.chainl1[String](stringParser, constantParser)("a") shouldBe List(("a",""))
  }

  it should "stop appropriately" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    Parser.chainl1[String](stringParser, constantParser)("a+a+ab") shouldBe List(("aaa", "b"))
  }

  it should "chain in the correct order" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParserA = Parser.string("a")
    val stringParserB = Parser.string("b")
    val plusParser = Parser.plus(stringParserA, stringParserB)
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    Parser.chainl1[String](plusParser, constantParser)("a+b+a+b+a") shouldBe List(("ababa", ""))
  }

  "first" should "only return one successfully parsed result" in {
    Parser.first(Parser.int)("123a") shouldBe List((123, "a"))
  }

  it should "return an empty list if there are no successful results" in {
    Parser.first(Parser.int)("hello") shouldBe List()
  }

  "+++" should "be a deterministic version of ++" in {
    Parser.+++(Parser.string("one"), Parser.string("two"))("onetwo") shouldBe List(("one", "two"))
    Parser.+++(Parser.string("one"), Parser.string("two"))("twoone") shouldBe List(("two", "one"))
    Parser.+++(Parser.string("one"), Parser.string("two"))("twotwo") shouldBe List(("two", "two"))
  }

  it should "return an empty list if there are no successful results" in {
    Parser.+++(Parser.string("one"), Parser.string("two"))("threetwo") shouldBe List()
  }

  "color" should "accept the strings yellow or orange" in {
    Parser.color("yelloworange") shouldBe List(("yellow", "orange"))
    Parser.color("orangeyellow") shouldBe List(("orange", "yellow"))
  }

  it should "return an empty list if there are no successful results" in {
    Parser.color("green") shouldBe List()
  }

  "spaces" should "consumes white space from the beginning of a string" in {
    Parser.spaces("   space") shouldBe List(((),"space"), ((), " space"), ((),"  space"))
    Parser.spaces("\ttab") shouldBe List(((), "tab"))
    Parser.spaces("\nnewLine") shouldBe List(((), "newLine"))
  }

  it should "return an empty list if there are no spaces" in {
    Parser.spaces("input") shouldBe List()
  }

  "comment" should "consume single line comment" in {
    Parser.comment("--c") shouldBe List(((),""), ((),"c"))
    Parser.comment("--c\ninput") shouldBe List(((), "\ninput"), ((), "c\ninput"))
  }

  it should "return an empty list f there are no comments" in {
    Parser.comment("input") shouldBe List()
  }

  "junk" should "consume spaces and comments" in {
    Parser.junk("\t--comment") shouldBe List(((),""), ((),"--comment"), ((),	 "\t--comment"))
    Parser.junk("input\t--comment") shouldBe List(((),"input\t--comment"))
  }

  "parse" should "remove junk before parsing" in {
    Parser.parse(Parser.digit)("\t1a") shouldBe List(('1', "a"))
    Parser.parse(Parser.digit)("--c\n1a") shouldBe List(('1', "a"))
  }

  /*
  token
  chainr
  neg
  force
  */

  "inputExploration1" should "input unaffected without flatMap" in {
    Parser.inputExploration1("HELLO") shouldBe List(('H', "ELLO"))
  }

  "inputExploration2" should "input affected with flatMap" in {
    Parser.inputExploration2("HELLO") shouldBe List(('L', "LO"))
  }

  "inputExploration3" should "input affected with flatMap" in {
    Parser.inputExploration3("HELLO") shouldBe List(("HEL", "LO"))
  }
}
