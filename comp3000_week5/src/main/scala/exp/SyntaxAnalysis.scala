/**
 * Expression language parser.
 *
 * Copyright 2009-2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for the expression language.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import ExpTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[ExpProgram] =
        phrase (program)

    lazy val program : PackratParser[ExpProgram] =
        (statement+) ^^ ExpProgram

    lazy val statement : PackratParser[Statement] =
        ("set" ~> ident) ~ ("=" ~> expression) ^^ { case i ~ e => SetStmt (IdnExp (i), e) } |
        ("if" ~> ("(" ~> expression <~ ")") ~ ("{" ~> statement <~ "}")) ^^ { case e ~ s => IfStmt (e, s) } |
        expression ^^ ExpStmt

    lazy val expression : PackratParser[Expression] =
        expression ~ ("+" ~> term) ^^ { case e ~ t => PlusExp (e, t) } |
        // NOTE: there was a bug here: "minus" operator instead of "-"
        expression ~ ("-" ~> term) ^^ { case e ~ t => MinusExp (e, t) } |
        term

    lazy val term : PackratParser[Expression] =
        // NOTE: there was a bug here: PlusExp tree node type instead of StarExp
        term ~ ("*" ~> factor) ^^ { case t ~ f => StarExp (t, f) } |
        term ~ ("/" ~> factor) ^^ { case t ~ f => SlashExp (t, f) } |
        factor

    lazy val factor : PackratParser[Expression] =
        integer ^^ (s => IntExp (s.toInt)) |
        ident ^^ IdnExp

    lazy val integer : PackratParser[String] =
        // NOTE: there was a bug here, [1-9] instead of [0-9]
        regex ("[0-9]+".r)

    lazy val ident : PackratParser[String] =
        regex ("[a-zA-Z]+".r)

}
