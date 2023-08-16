/*
 * This file is part of COMP3000 tokens exercise.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.tokens

/**
  * The top level object of the application.
  */
object Main {

  import Tokens._

  /**
    * Main entry point of the application.
    *
    * @param args the array of options and parameters passed on 
    * the command line.
    */
  def main(args: Array[String]) {
    args.size match
    {
    case 1 => val toks = listOfStringsToTokens(matchPat(patopnum, args(0)))
              println(toks)
              if( toks.size >= 3
               && List(TokAdd, TokSub, TokMul, TokDiv).contains(toks.head))
              {
                aggregate(toks) match
                {
                  case Some(n) => println("result: " + n)
                  case None    => println("error")
                }
              }
    case 2 => println(matchPat(args(0).r, args(1)))
    case _ => println("Usage: run \"regex pattern\" \"text to be matched\"")
              println("   or: run \"aggregration\" (e.g. \"+ 3 5 12 4\")")
    }
  }
}
