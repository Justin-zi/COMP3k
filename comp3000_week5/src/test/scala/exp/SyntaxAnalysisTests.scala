/**
 * Expression language tests.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import ExpTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    test ("parsing digits as a factor gives the correct tree") {
        factor ("1203") should parseTo[Expression] (IntExp (1203))
    }

    test ("parsing a non-integer as an integer gives an error") {
        integer ("hello") should failParseAt (1, 1, "string matching regex '[0-9]+' expected but 'h' found")
    }

    test ("parsing a simple expression produces the correct tree") {
        expression ("2 + 4") should parseTo[Expression] (PlusExp (IntExp (2), IntExp (4)))
    }

    test ("parsing an expression with associative operators produces the correct tree") {
        expression ("2 + 4 * 6") should parseTo[Expression] (PlusExp (IntExp (2), StarExp (IntExp (4), IntExp (6))))
    }

    // NOTE
    // Some possible extra tests to make it clearer that things are working.
    // There is no way to exhaustively test, since the number of possible
    // input programs is infinite.  All we can do is try to cover all of
    // the constructs at least once, in some common combinations.

    test ("a complex expression is parsed correctly") {
        expression ("1 + total / 3 * sum - 5") should
            parseTo[Expression] (
                MinusExp (PlusExp (IntExp (1),
                                   StarExp (SlashExp (IdnExp("total"), IntExp (3)),
                                            IdnExp ("sum"))),
                          IntExp (5))
          )
    }

    test ("parsing an identifier factor gives the correct tree") {
        factor ("blah") should parseTo[Expression] (IdnExp ("blah"))
    }

    test ("parsing a non-identifier as an identifier gives an error") {
        ident ("1x5") should failParseAt (1, 1, "string matching regex '[a-zA-Z]+' expected but '1' found")
    }

    test ("parsing a set statement gives the correct tree") {
        statement ("set bob = 11") should parseTo[Statement] (SetStmt (IdnExp ("bob"), IntExp (11)))
    }

    test ("parsing an erroneous set statement fails") {
        phrase (statement) ("set bob 11") should failParseAt (1, 9, "'=' expected but '1' found")
    }

    test ("parsing an expression statement gives the correct tree") {
        statement ("11 - number") should parseTo[Statement] (ExpStmt (MinusExp (IntExp (11), IdnExp ("number"))))
    }

    test ("parsing a program gives the correct tree") {
        parser ("set a = 3 set b = 4 + a a * a + b") should
            parseTo (
                ExpProgram (Vector (SetStmt (IdnExp ("a"), IntExp (3)),
                                    SetStmt (IdnExp ("b"), PlusExp (IntExp (4), IdnExp ("a"))),
                                    ExpStmt (PlusExp (StarExp (IdnExp ("a"), IdnExp ("a")), IdnExp ("b")))))
            )
    }

}
