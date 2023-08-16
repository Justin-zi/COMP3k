/*
 * This file is part of COMP3000 tokens exercise.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Snake puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.tokens

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TokensTests extends FlatSpec with Matchers {

  import Tokens._

  /////////////////////////////  Rev  ////////////////////////////

  "rev" should "handle an empty string" in {
    assert(rev(REmpty) == REmpty)
  }

  it should "handle a symbol" in {
    assert(rev(RSymbol('y')) == RSymbol('y'))
  }

  it should "handle a star" in {
    assert(rev(RStar(RSymbol('h'))) == RStar(RSymbol('h')))
  }

  it should "handle abc" in {
    assert(rev(RSeq(RSymbol('c'), RSeq(RSymbol('a'), RSymbol('t'))))
           == RSeq(RSeq(RSymbol('t'), RSymbol('a')), RSymbol('c')))
  }

  it should "handle (ab+)*" in {
    assert(rev(RStar(RSeq(RSymbol('a'), RPlus(RSymbol('b')))))
           == RStar(RSeq(RPlus(RSymbol('b')), RSymbol('a'))))
  }

  //////////////////////  Regular Expressions  /////////////////////

  "pat1" should "find all 1s" in {
    assert(matchPat(pat1, "1 gh 1 tuv 11 zz") == List("1", "1", "1", "1"))
  }

  "patd" should "find all digits" in {
    assert(matchPat(patd, "24 25 7 abc9") == List("2", "4", "2", "5", "7", "9"))
  }

  "patnum" should "find all numbers" in {
    assert(matchPat(patnum, "24 25 7 abc9") == List("24", "25", "7", "9"))
  }

  "patname" should "find names" in {
    assert(matchPat(patname, "14 abc 7jj joe43 BAD2") ==
                                    List("abc", "jj", "joe43", "BAD2"))
  }

  "patnamenum" should "find names and numbers" in {
    assert(matchPat(patnamenum, "14 abc 7jj joe43 BAD2") ==
                                List("14", "abc", "7", "jj", "joe43", "BAD2"))
  }

  "patopnum" should "find operators and numbers" in {
    assert(matchPat(patopnum, "14 + abc % 7jj -- joe43* /BAD2") ==
                         List("14", "+", "7", "-", "-", "43", "*", "/", "2"))
  }

  "strToTok" should "convert a name string to TokName" in {
    assert(strToTok("joe") == TokName("joe"))
  }

  it should "convert a numeric string to TokNum" in {
    assert(strToTok("347") == TokNum(347))
  }

  it should "convert an operator string to a token" in {
    assert(strToTok("+") == TokAdd)
  }

  "listOfStringsToTokens" should "convert a list of strings to tokens" in {
    assert(listOfStringsToTokens(List("fred", "25", "+")) ==
                          List(TokName("fred"), TokNum(25), TokAdd))
  }

  "aggregate" should "sum a list of numbers starting with +" in {
    assert(aggregate(List(TokAdd, TokNum(3), TokNum(4), TokNum(5))) == Some(12))
  }

  it should "not work on an invalid list - all numbers" in {
    assert(aggregate(List(TokName("2"), TokNum(3), TokNum(4))) == None)
  }

  it should "not work on an invalid list - tail not all numbers" in {
    assert(aggregate(List(TokName("*"), TokNum(3), TokName("+"))) == None)
  }

}
