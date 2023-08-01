/*
 * This file is part of COMP3000 high-level assembly language.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.hasm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class InstrTests extends FlatSpec with Matchers {

  import Instr._

  "load" should "handle r3 = 27" in {
    assert(parseInstr("r3 = 27") == LoadReg(3, Num(27)))
  }

  it should "handle r3 = r4" in {
    assert(parseInstr("r3 = r4") == LoadReg(3, Reg(4)))
  }

  it should "handle r3 = *r4" in {
    assert(parseInstr("r3 = *r4") == LoadReg(3, IndirectReg(4)))
  }

  it should "handle r3 = &a" in {
    assert(parseInstr("r3 = &a") == LoadReg(3, AddressOnly("a")))
  }

  it should "handle r3 = a" in {
    assert(parseInstr("r3 = a") == LoadReg(3, Address("a")))
  }

  it should "handle r3 = *a" in {
    assert(parseInstr("r3 = *a") == LoadReg(3, IndirectAddress("a")))
  }

  it should "handle r3 = a[r2]" in {
    assert(parseInstr("r3 = a[r2]") == LoadReg(3, AddressIndex("a", 2)))
  }

  it should "handle r3 = *a[r2]" in {
    assert(parseInstr("r3 = *a[r2]") == LoadReg(3, IndirectAddressIndex("a", 2)))
  }

  "store" should "handle a = r5" in {
    assert(parseInstr("a = r5") == StoreReg(5, Address("a")))
  }

  it should "handle *a = r5" in {
    assert(parseInstr("*a = r5") == StoreReg(5, IndirectAddress("a")))
  }

  "add" should "handle r3 = r3 + 27" in {
    assert(parseInstr("r3 = r3 + 27") == Add(3, Num(27)))
  }

  "sub" should "handle r3 = r3 - a" in {
    assert(parseInstr("r3 = r3 - a") == Sub(3, Address("a")))
  }

  "mul" should "handle r3 = r3 * *a" in {
    assert(parseInstr("r3 = r3 * *a") == Mul(3, IndirectAddress("a")))
  }

  "div" should "handle r3 = r3 / &a" in {
    assert(parseInstr("r3 = r3 / &a") == Div(3, AddressOnly("a")))
  }

  "or" should "handle r3 = r3 or a" in {
    assert(parseInstr("r3 = r3 or a") == Or(3, Address("a")))
  }

  "and" should "handle r3 = r3 and a" in {
    assert(parseInstr("r3 = r3 and a") == And(3, Address("a")))
  }

  "xor" should "handle r3 = r3 xor a" in {
    assert(parseInstr("r3 = r3 xor a") == Xor(3, Address("a")))
  }

  "shift left" should "handle r3 = r3 << a" in {
    assert(parseInstr("r3 = r3 << a") == ShiftLeft(3, Address("a")))
  }

  "shift right arithmetic" should "handle r3 = r3 >> a" in {
    assert(parseInstr("r3 = r3 >> a") == ShiftRightArith(3, Address("a")))
  }

  "shift right logical" should "handle r3 = r3 >>> a" in {
    assert(parseInstr("r3 = r3 >>> a") == ShiftRightLogical(3, Address("a")))
  }

  "not" should "handle r3 = not a" in {
    assert(parseInstr("r3 = not a") == Not(3, Address("a")))
  }

  "call" should "handle call *a" in {
    assert(parseInstr("call *a") == Call(IndirectAddress("a")))
  }

  "return" should "parse" in {
    assert(parseInstr("return") == Return)
  }

  "goto" should "handle goto a" in {
    assert(parseInstr("goto a") == Goto(Address("a")))
  }

  "if" should "handle if(r4 < 27) goto a" in {
    assert(parseInstr("if(r4 < 27) goto a")
           == GotoIf(4, CmpLT, Num(27), Address("a")))
  }

  it should "handle if(r4 <= b) goto a" in {
    assert(parseInstr("if(r4 <= b) goto a")
           == GotoIf(4, CmpLE, Address("b"), Address("a")))
  }

  it should "handle if(r4 == *b) goto a" in {
    assert(parseInstr("if(r4 == *b) goto a")
           == GotoIf(4, CmpEQ, IndirectAddress("b"), Address("a")))
  }

  it should "handle if(r4 != &b) goto a" in {
    assert(parseInstr("if(r4 != &b) goto a")
           == GotoIf(4, CmpNE, AddressOnly("b"), Address("a")))
  }

  it should "handle if(r4 >= b[r2]) goto a" in {
    assert(parseInstr("if(r4 >= b[r2]) goto a")
           == GotoIf(4, CmpGE, AddressIndex("b", 2), Address("a")))
  }

  it should "handle if(r4 > *b[r2]) goto a" in {
    assert(parseInstr("if(r4 > *b[r2]) goto a")
           == GotoIf(4, CmpGT, IndirectAddressIndex("b", 2), Address("a")))
  }

  "push" should "handle push(r4, *b)" in {
    assert(parseInstr("push(r4, *b)")
           == Push(4, IndirectAddress("b")))
  }

  "pop" should "handle a = pop(r5)" in {
    assert(parseInstr("a = pop(r5)") == Pop(5, Address("a")))
  }

  "out" should "handle out r5" in {
    assert(parseInstr("out r5") == Out(Reg(5)))
  }

  "alloc" should "handle r3 = alloc(a)" in {
    assert(parseInstr("r3 = alloc(a)") == Alloc(3, Address("a")))
  }

  "free" should "handle free(r5)" in {
    assert(parseInstr("free(r5)") == Free(Reg(5)))
  }

  "a number" should "be treated as data" in {
    assert(parseInstr("23") == DataNum(23))
  }

  "an address" should "be treated as data" in {
    assert(parseInstr("a") == DataAddress("a"))
  }

  "a block of bytes" should "be treated as data" in {
    assert(parseInstr("bytes(20)") == DataBytes(20))
  }

  "an instruction" should "allow a label L1: r3 = *r4" in {
    assert(parseInstr("L1: r3 = *r4")
           == LabelInstr("L1", LoadReg(3, IndirectReg(4))))
  }
}
