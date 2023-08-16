/*
 * This file is part of COMP3000 tokens exercise.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html

package org.mq.tokens

import scala.util.matching._

object Tokens {

  ////////////////////////////  Rev  //////////////////////////////////////

  sealed abstract class RegExp
  case object REmpty extends RegExp               // empty string
  case class RSymbol(s:Char) extends RegExp       // one symbol from language
  case class RStar(r:RegExp) extends RegExp       // r*
  case class RPlus(r:RegExp) extends RegExp       // r+
  case class ROr(r:RegExp, s:RegExp) extends RegExp   // r|s
  case class RSeq(r:RegExp, s:RegExp) extends RegExp  // rs

  def rev(r:RegExp):RegExp = REmpty     // TO DO    (use match)

  ////////////////////////////  Regular expressions //////////////////////

  sealed abstract class Tok
  case class TokName(name:String) extends Tok
  case class TokNum(num:Int) extends Tok
  case object TokAdd extends Tok                      //   +
  case object TokSub extends Tok                      //   -
  case object TokMul extends Tok                      //   *
  case object TokDiv extends Tok                      //   /
  case class TokOther(s:String) extends Tok

  def matchPat(pat:Regex, line:String):List[String] = pat.findAllIn(line).toList

  val pat1 = "1".r    // regular expression for finding all 1's

  val patd = "[0-9]".r    // regular expression for finding all digits

  val patnum = "[0-9]+".r  // regular expression for finding all numbers (non-neg ints)

  val num_nonleading = "[1-9][0-9]*|0".r

  val patname = "[a-zA-Z][a-zA-Z0-9]*".r // regular expression for finding all names of
                          // the form:
                          //  - starts with a letter (upper or lowercase)
                          //  - may be followed by letters or digits
  val patnamenum = "[a-zA-Z][a-zA-Z0-9]*|[0-9]+".r // regular expression for finding
                             // all names and numbers
  val patopnum = "[\\+-/*]|[0-9]+".r   // regular expression for finding
                             // all operators and numbers

  def strToTok(s:String):Tok =
  {// TO DO: convert a string to a token
    val ch = s.head    // what does the first char of the string tell us?
    TokOther(s)
  }

  def listOfStringsToTokens(a:List[String]):List[Tok] = List()  // TO DO

  def aggregate(a:List[Tok]):Option[Int] =
  {// TO DO
    None
  }
}
