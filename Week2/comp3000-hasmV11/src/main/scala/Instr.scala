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

import scala.util.matching._

object Instr {

  sealed abstract class Location
  case class Reg(r:Int) extends Location
  case class Num(n:Int) extends Location
  case class Address(name:String) extends Location
  case class AddressOnly(name:String) extends Location
  case class AddressIndex(name:String, idx:Int) extends Location
  case class IndirectReg(r:Int) extends Location
  case class IndirectAddress(name:String) extends Location
  case class IndirectAddressIndex(name:String, idx:Int) extends Location
  // the following three are not for user usage
  case class AddressNum(n:Int) extends Location
  case class AddressNumIndex(addr:Int, idx:Int) extends Location
  case class IndirectAddressNum(addr:Int) extends Location
  case class IndirectAddressNumIndex(addr:Int, idx:Int) extends Location

  sealed abstract class Cmp
  case object CmpLT extends Cmp
  case object CmpLE extends Cmp
  case object CmpEQ extends Cmp
  case object CmpNE extends Cmp
  case object CmpGE extends Cmp
  case object CmpGT extends Cmp

  sealed abstract class Instr
  case class LoadReg(r:Int, src:Location) extends Instr
  case class StoreReg(r:Int, dest:Location) extends Instr
  case class Add(r:Int, loc:Location) extends Instr
  case class Sub(r:Int, loc:Location) extends Instr
  case class Mul(r:Int, loc:Location) extends Instr
  case class Div(r:Int, loc:Location) extends Instr
  case class Or(r:Int, loc:Location) extends Instr
  case class And(r:Int, loc:Location) extends Instr
  case class Not(r:Int, loc:Location) extends Instr
  case class Xor(r:Int, loc:Location) extends Instr
  case class ShiftLeft(r:Int, loc:Location) extends Instr
  case class ShiftRightArith(r:Int, loc:Location) extends Instr
  case class ShiftRightLogical(r:Int, loc:Location) extends Instr
  case class Call(loc:Location) extends Instr
  case object Return extends Instr
  case class Goto(loc:Location) extends Instr
  case class GotoIf(reg:Int, cmp:Cmp, cmpLoc:Location, goto:Location) extends Instr
  case class Push(stackReg:Int, src:Location) extends Instr
  case class Pop(stackReg:Int, dest:Location) extends Instr
  case class Out(loc:Location) extends Instr
  case class Alloc(r:Int, numBytes:Location) extends Instr
  case class Free(loc:Location) extends Instr
  case class DataNum(num:Int) extends Instr
  case class DataAddress(name:String) extends Instr
  case class DataBytes(numBytes:Int) extends Instr
  case class LabelInstr(name:String, instr:Instr) extends Instr
  case class Txt(s:String) extends Instr
  case object NotUsed extends Instr

  var trace = true

  val baseAddr = 400
  var heapSize = 100
  var stackSize = 100
  var heapBase = 600    // provisional (calculated in loadMemory)
  var topAddr = heapBase + heapSize + stackSize   // provisional

  var symbols = Map[String,Int]()

  var symbolsNotFound = List[String]()

  var currAddr = baseAddr

  // initialise r7 to point to top of memory for the stack
  var registers = Array(0, 0, 0, 0, 0, 0, 0, topAddr)  // initialise r7

  var listOfInstructionsWithSymbols:List[(Int,String,Instr,Instr)] = List()

  var memory:Array[Instr] = Array()

  var badRegisterInstr = false

  def setHeapSize(n:Int):Unit =
  {// want multiple of 4
    heapSize = if(n < 100) 100 else (n + 3) / 4 * 4
  }

  def setStackSize(n:Int):Unit =
  {// want multiple of 4
    stackSize = if(n < 100) 100 else (n + 3) / 4 * 4
  }

  def parseInstr(s:String):Instr =
  {
    def isReg(s:String) = s == "r0" || s == "r1" || s == "r2" || s == "r3" ||
                          s == "r4" || s == "r5" || s == "r6" || s == "r7"

    def isIndexReg(s:String) = s.size == 4 && s(0) == '[' && s(3) == ']' &&
                               isReg(s.take(3).drop(1))

    def parseLocation(w:List[String]):Location =
    {
      val len = w.size
      if(len == 0)
      {
        println("empty location")
        Address("ERROR")
      }
      else if(len == 1 && w(0).head.isDigit)
        Num(w(0).toInt)
      else if(len == 1 && isReg(w(0)))
        Reg(w(0).drop(1).toInt)
      else if(len == 1 && w(0).head.isLetter)
        Address(w(0))
      else if(len == 2 && w(0) == "*" && isReg(w(1)))
        IndirectReg(w(1).drop(1).toInt)
      else if(len == 2 && w(0) == "*" && w(1).head.isLetter)
        IndirectAddress(w(1))
      else if(len == 2 && w(0) == "&" && w(1).head.isLetter)
        AddressOnly(w(1))
      else if(len == 2 && w(0) == "&" && w(1).head.isDigit)
        AddressNum(w(1).toInt)
      else if(len == 2 && w(0).head.isLetter && !isReg(w(0))
           && isIndexReg(w(1)))
        AddressIndex(w(0), w(1).drop(2).take(1).toInt)
      else if(len == 3 && w(0) == "*" && w(1).head.isLetter && !isReg(w(1))
           && isIndexReg(w(2)))
        IndirectAddressIndex(w(1), w(2).drop(2).take(1).toInt)
      else
      {
        println("location not recognised: " + w)
        Address("ERROR")
      }
    }

    val kot = (qsr map ((i:Int)=>i.toChar)).mkString.r.findAllIn(s).toList
    //println(kot)
    val len = kot.size
    if(len == 0)
    {
      println("Missing instruction: " + s)
      DataAddress("ERROR")
    }
    else if(kot.size > 2 && kot(1) == ":"
         && kot(0).head.isLetter && !isReg(kot(0)))
      LabelInstr(kot(0), parseInstr(s.drop(s.indexOf(':') + 1)))
    else if(len >= 5 && isReg(kot(0)) && kot(1) == "="
         && kot(0) == kot(2)
         && List("+", "-", "*", "/", "and", "or", "xor", "<<", ">>", ">>>")
                .contains(kot(3)))
    {// <reg> = <same reg> <op> <location>
      val r = kot(0).drop(1).toInt
      val loc = parseLocation(kot drop 4)
      kot(3) match
      {
      case "+"   => Add(r, loc)
      case "-"   => Sub(r, loc)
      case "*"   => Mul(r, loc)
      case "/"   => Div(r, loc)
      case "and" => And(r, loc)
      case "or"  => Or(r, loc)
      case "xor" => Xor(r, loc)
      case "<<"  => ShiftLeft(r, loc)
      case ">>"  => ShiftRightArith(r, loc)
      case ">>>" => ShiftRightLogical(r, loc)
      case _     => DataAddress("ERROR")
      }
    }
    else if(len >= 4 && isReg(kot(0)) && kot(1) == "="
         && kot(2) == "not")
      Not(kot(0).drop(1).toInt, parseLocation(kot drop 3))
    else if(len >= 6 && isReg(kot(0)) && kot(1) == "="
         && kot(2) == "alloc" && kot(3) == "(" && kot.last == ")")
      Alloc(kot(0).drop(1).toInt, parseLocation(kot.drop(4).dropRight(1)))
    else if(len >= 6 && kot(len - 5) == "=" && kot(len - 4) == "pop"
         && kot(len - 3) == "(" && isReg(kot(len - 2)) && kot(len - 1) == ")")
      Pop(kot(len - 2).drop(1).toInt, parseLocation(kot dropRight 5))
    else if(len >= 3 && isReg(kot(0)) && kot(1) == "=")
      LoadReg(kot(0).drop(1).toInt, parseLocation(kot drop 2))
    else if(len >= 3 && kot(len - 2) == "=" && isReg(kot.last))
      StoreReg(kot.last.drop(1).toInt, parseLocation(kot dropRight 2))
    else if(kot(0) == "out")
      Out(parseLocation(kot drop 1))
    else if(kot(0) == "push" && len >= 6 && kot(1) == "("
         && isReg(kot(2)) && kot(3) == "," && kot.last == ")")
      Push(kot(2).drop(1).toInt, parseLocation(kot.drop(4).dropRight(1)))
    else if(kot(0) == "call")
      Call(parseLocation(kot drop 1))
    else if(kot(0) == "return")
      Return
    else if(kot(0) == "goto")
      Goto(parseLocation(kot drop 1))
    else if(kot(0) == "if" && len >= 8 && kot(1) == "("
         && isReg(kot(2))
         && List("<", "<=", "==", "!=", ">=", ">").contains(kot(3))
         && kot.contains(")") && kot.contains("goto")
         && kot(kot.indexOf("goto") - 1) == ")")
    {
      val rbrac = kot.indexOf("goto") - 1
      val cmp = List(CmpLT, CmpLE, CmpEQ, CmpNE, CmpGE, CmpGT)(
                         List("<", "<=", "==", "!=", ">=", ">").indexOf(kot(3)))
      val cmpLoc = parseLocation(kot.take(rbrac).drop(4))
      val jmpLoc = parseLocation(kot.drop(rbrac + 2))
      GotoIf(kot(2).drop(1).toInt, cmp, cmpLoc, jmpLoc)
    }
    else if(kot(0) == "free" && len >= 4 && kot(1) == "("
         && kot.last == ")")
      Free(parseLocation((kot drop 2).dropRight(1)))
    else if(len == 1 && kot(0).head.isDigit)
      DataNum(kot(0).toInt)
    else if(len == 1 && kot(0).head.isLetter && !isReg(kot(0)))
      DataAddress(kot(0))
    else if(len == 2 && kot(0) == "&" && kot(1).head.isLetter
         && !isReg(kot(1)))
      DataAddress(kot(1))
    else if(kot(0) == "bytes" && len == 4 && kot(1) == "("
         && kot(2).head.isDigit && kot(3) == ")")
      DataBytes(kot(2).toInt)
    else
    {
      println("not recognised: " + s)
      DataAddress("ERROR")
    }
  }

  // check register number is valid, i.e. 0-7
  def checkReg(r:Int, in:Instr):Unit =
  {
    if(r < 0 || r > 7)
    {
      badRegisterInstr = true
      println("bad register number: " + in)
    }
  }

  // check location's register number is valid
  def checkRegInLoc(loc:Location, in:Instr):Unit = loc match
  {
  case Reg(r) => checkReg(r, in)
  case IndirectReg(r) => checkReg(r, in)
  case AddressIndex(_, r) => checkReg(r, in)
  case IndirectAddressIndex(_, r) => checkReg(r, in)
  case _      =>
  }

  // check registernumber and register number in location are valid
  def check2Reg(r:Int, loc:Location, in:Instr):Unit =
  {
    checkReg(r, in)
    checkRegInLoc(loc, in)
  }

  // calculate the size (in bytes) of an instruction based on its location field
  def calcInstrSize(loc:Location):Int = loc match
  {
  case Reg(_) => 2
  case IndirectReg(_) => 2
  case Num(n) => if(n.abs < 32768) 4 else 6
  case _      => 4     // assuming addresses are at most 4 bytes
  }

  // determines whether the comparison requires an extra instruction; and
  // if so, the size in bytes of that instruction
  def calcCmpSize(cmpLoc:Location):Int = cmpLoc match
  {// determine whether CMP instruction is required
  case Num(0) => 0      // don't need extra instruction
  case Num(n) => if(n.abs < 32768) 4 else 6
  case Reg(_) | IndirectReg(_) => 2
  case _      => 4     // assuming addresses are at most 4 bytes
  }

  // calculates the size of the instruction
  // 1  if the instruction is Alloc or Free then immediately have size
  // 2  if opLoc is present then use it to determine size
  // 3  otherwise deal with Return and GotoIf
  def calcInstrSizeInstr(opLoc:Option[Location], in:Instr):Int = in match
  {
  case Alloc(_, _) | Free(_) => 12
  case _ => opLoc match
    {
    case Some(loc) => calcInstrSize(loc)
    case None      => in match
                      {
                      case Return => 2
                      case GotoIf(_, cmp, cmpLoc, jmp) => calcInstrSize(jmp) +
                                                          calcCmpSize(cmpLoc)
                      case _      => 4   // shouldn't happen
                      }
    }
  }

  def assignAddresses(s:List[Instr]):List[(Int,String,Instr,Instr)] =
  {
    // lookup address label and get its address
    def lookupSymbol(name:String):Int = (symbols get name) match
    {
    case Some(addr) => addr
    case None       => if(!symbolsNotFound.contains(name))
                         symbolsNotFound = symbolsNotFound :+ name
                       0
    }

    // convert one instruction into form:
    // (address(number), label (or ""), instruction (without label
    //                                               if it had one)
    def processOneInstr(instr:Instr):(Int,String,Instr) =
    {
      val addr = currAddr
      instr match
      {
        case LoadReg(r, loc) => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case StoreReg(r, loc) => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Add(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Sub(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Mul(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Div(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Or(r, loc)      => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case And(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Not(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Xor(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case ShiftLeft(r, loc) => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case ShiftRightArith(r, loc) => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case ShiftRightLogical(r, loc) => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Call(loc)       => currAddr += calcInstrSize(loc)
                                checkRegInLoc(loc, instr)
                                (addr, "", instr)
        case Return          => currAddr += calcInstrSizeInstr(None, instr)
                                (addr, "", instr)
        case Goto(loc)       => currAddr += calcInstrSize(loc)
                                checkRegInLoc(loc, instr)
                                (addr, "", instr)
        case GotoIf(r, cmp, cmpLoc, jmp) => currAddr += calcInstrSize(jmp) +
                                                        calcCmpSize(cmpLoc)
                                check2Reg(r, cmpLoc, instr)
                                checkRegInLoc(jmp, instr)
                                (addr, "", instr)
        case Push(r, loc)    => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Pop(r, loc)     => currAddr += calcInstrSize(loc)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Out(loc)        => currAddr += calcInstrSize(loc)
                                checkRegInLoc(loc, instr)
                                (addr, "", instr)
        case Alloc(r, loc)     => currAddr += calcInstrSizeInstr(Some(loc),
                                                                 instr)
                                check2Reg(r, loc, instr)
                                (addr, "", instr)
        case Free(loc)         => currAddr += calcInstrSizeInstr(Some(loc),
                                                                 instr)
                                checkRegInLoc(loc, instr)
                                (addr, "", instr)
        case DataNum(_) | DataAddress(_) => currAddr += 4
                                (addr, "", instr)
        case DataBytes(n)   => currAddr += (if(n % 4 == 0) n else
                                                       (n + 3) / 4 * 4)
                                (addr, "", DataNum(0))
        case LabelInstr(name, instr) => processOneInstr(instr) match
                                   {
                                   case (adr, _, _) => symbols += (name -> addr)
                                                       (adr, name, instr)
                                   }
        case Txt(s)          => processOneInstr(parseInstr(
                                      {// strip trailing // comments
                                        val cmt = s.indexOf("//")
                                        if(cmt >= 0)
                                          s take cmt
                                        else
                                          s
                                      }))
        case _               => (addr, "", instr)
      }

    }

    // convert from form:
    //     (address(number), label (or ""), instruction (without label))
    // to:
    //     (address(num), label, instruction, instruction with numbers
    //                                       replacing addresses)
    def addAddressToInstr(a:(Int,String,Instr)):(Int,String,Instr,Instr) =
      a match
      {
      case (addr, name, instr) => (addr, name, instr, addAddress(instr))
      }

    // reconstruct the instruction with any address label replaced by an
    // address number
    def addAddress(instr:Instr):Instr = instr match
    {
    case LoadReg(r, loc) => LoadReg(r, addAddressToLocation(loc))
    case StoreReg(r, loc) => StoreReg(r, addAddressToLocation(loc))
    case Add(r, loc) => Add(r, addAddressToLocation(loc))
    case Sub(r, loc) => Sub(r, addAddressToLocation(loc))
    case Mul(r, loc) => Mul(r, addAddressToLocation(loc))
    case Div(r, loc) => Div(r, addAddressToLocation(loc))
    case Or(r, loc) => Or(r, addAddressToLocation(loc))
    case And(r, loc) => And(r, addAddressToLocation(loc))
    case Not(r, loc) => Not(r, addAddressToLocation(loc))
    case Xor(r, loc) => Xor(r, addAddressToLocation(loc))
    case ShiftLeft(r, loc) => ShiftLeft(r, addAddressToLocation(loc))
    case ShiftRightArith(r, loc) => ShiftRightArith(r, addAddressToLocation(loc))
    case ShiftRightLogical(r, loc) => ShiftRightLogical(r, addAddressToLocation(loc))
    case Call(loc) => Call(addAddressToLocation(loc))
    case Goto(loc) => Goto(addAddressToLocation(loc))
    case GotoIf(r, cmp, cmpLoc, jmp) => GotoIf(r, cmp,
                                       addAddressToLocation(cmpLoc),
                                       addAddressToLocation(jmp))
    case Push(r, loc) => Push(r, addAddressToLocation(loc))
    case Pop(r, loc) => Pop(r, addAddressToLocation(loc))
    case Out(loc)    => Out(addAddressToLocation(loc))
    case Alloc(r, loc) => Alloc(r, addAddressToLocation(loc))
    case DataAddress(name) => DataNum(lookupSymbol(name))
    case _            => instr
    }

    // reconstruct the location with any address label replaced by an
    // address number
    def addAddressToLocation(loc:Location):Location = loc match
    {
    case Address(name) => AddressNum(lookupSymbol(name))
    case AddressOnly(name) => Num(lookupSymbol(name))
    case IndirectAddress(name) => IndirectAddressNum(lookupSymbol(name))
    case AddressIndex(name, idx) => AddressNumIndex(lookupSymbol(name), idx)
    case IndirectAddressIndex(name, idx) => IndirectAddressNumIndex(lookupSymbol(name), idx)
    case _             => loc
    }

    val instrList = s map processOneInstr
 
    // add symbol for base of heap
    heapBase = (currAddr + 3) / 4 * 4
    symbols += ("heap" -> heapBase)

    //println(symbols)
    listOfInstructionsWithSymbols = instrList map addAddressToInstr
    listOfInstructionsWithSymbols
  }

  def displayQuadInstr(s:List[(Int,String,Instr,Instr)]):Unit =
  {
    val maxLen = (s map (x => (x._2.size,
                               x._3.toString.size,
                               x._4.toString.size))) reduce ((x, y)
                                                             => (x._1 max y._1,
                                                                 x._2 max y._2,
                                                                 x._3 max y._3))
    s foreach (x => x match
                    {
                    case (addr, label, orig, revised) => println(
                                        fill(addr.toString, 5) +
                                        fill(label, maxLen._1 + 1) +
                                        fill(orig.toString, maxLen._2 + 1) +
                                        fill(revised.toString, maxLen._3 + 1) +
                                        instrToStr(revised))
                    })
  }

  // determines whether address is in range
  def isAddressInRange(addr:Int):Boolean = baseAddr <= addr && addr <= topAddr

  // populates memory
  // global in: listOfInstructionsWithSymbols 
  // global out: memory 
  // will return immediately if badRegisterInstr is true
  def loadMemory():Boolean =
  {
    if(badRegisterInstr) return false

    // currAddr was incremented as instructions were put into memory
    // now it is the address of the first available memory after the
    // instructions; put heap space and stack space here
    topAddr = heapBase + heapSize + stackSize   // provisional
    println("loading memory at: " + baseAddr)
    println("heap base: " + heapBase + "; heap size: " + heapSize)
    println("stack base: " + (heapBase + heapSize)
             + "; stack size: " + stackSize)

    memory = new Array[Instr](topAddr - baseAddr + 4)
    for(a <- 0 until memory.size) memory(a) = NotUsed

    for(in <- listOfInstructionsWithSymbols) in match
    {
    case (addr, _, _, instr) => if(!isAddressInRange(addr))
                                {
                                  println("address out of bounds")
                                  println(in)
                                  return false
                                }
                                memory(addr - baseAddr) = instr
    }
    true
  }

  // global in: memory 
  def displayMemory():Unit =
  {
    if(!trace) return
    println()
    println("Memory:");
    for(a <- 0 until memory.size)
    {
      val in = memory(a)
      if(in != NotUsed)
        println("" + (a + baseAddr) + "  " + fill(in.toString, 32) +
                instrToStr(in))
    }
    println()
  }

  // determine whether the asm program has a main label and return its address
  // returns -1 on error
  def checkMain():Int = (symbols get "main") match
  {
  case Some(addr) => println("Start address (main): " + addr)
                     addr
  case None       => println("Address main is not defined")
                     -1
  }

  // global in: registers 
  def displayRegisters():Unit =
  {
    if(!trace) return
    println()
    println("Registers:");
    for(a <- 0 until registers.size)
      print("    r" + a + "  " + registers(a))
  }

  var pc = 0    // program counter (address of next instruction to execute)

  // gets the contents of memory at a specified address
  def getMemoryValue(addr:Int):Option[Int] =
  {
    if(isAddressInRange(addr))
    {
      memory(addr - baseAddr) match
      {
      case DataNum(n) => Some(n)
      case NotUsed => println("PC = " + pc +
                              "  address referencing unused memory " + addr)
                      execError = true
                      None
      case _       => println("PC = " + pc +
                              "  address referencing instructions " + addr)
                      execError = true
                      None
      }
    }
    else
    {
      println("PC = " + pc + "  address out of bounds " + addr)
      execError = true
      None
    }
  }

  // gets the contents of memory at the given location
  def getLocationValue(loc:Location):Option[Int] = loc match
  {
  case Reg(r) => Some(registers(r))
  case Num(n) => Some(n)
  case IndirectReg(r) => getMemoryValue(registers(r))
  case AddressNum(n:Int) => getMemoryValue(n)
  case AddressNumIndex(addr, idx) => getMemoryValue(addr + registers(idx))
  case IndirectAddressNum(addr:Int) => getMemoryValue(addr) match
                                       {
                                       case Some(a) => getMemoryValue(a)
                                       case None    => None
                                       }
  case IndirectAddressNumIndex(addr, idx) => getMemoryValue(addr +
                                                         registers(idx)) match
                                       {
                                       case Some(a) => getMemoryValue(a)
                                       case None    => None
                                       }
  case _      => None
  }

  // gets the address of memory for the given location
  def getLocationAddress(loc:Location):Option[Int] = loc match
  {
  case Reg(_) | Num(_) => None
  case IndirectReg(r) => Some(registers(r))
  case AddressNum(n:Int) => Some(n)
  case AddressNumIndex(addr, idx) => Some(addr + registers(idx))
  case IndirectAddressNum(addr:Int) => getMemoryValue(addr)
  case IndirectAddressNumIndex(addr, idx) => getMemoryValue(addr +
                                                            registers(idx))
  case _      => None
  }

  // gets the address of memory for the given location for goto or call
  def getJumpLocationAddress(loc:Location):Option[Int] = loc match
  {
  case Num(_) => None
  case Reg(r) => Some(registers(r))  // register holds address
  case IndirectReg(r) => getMemoryValue(registers(r)) // reg holds indirect addr
  case AddressNum(n:Int) => Some(n)
  case AddressNumIndex(addr, idx) => Some(addr + registers(idx))
  case IndirectAddressNum(addr:Int) => getMemoryValue(addr)
  case IndirectAddressNumIndex(addr, idx) => getMemoryValue(addr +
                                                            registers(idx))
  case _      => None
  }

  // write a number to a memory address
  def writeToMemory(addr:Int, num:Int):Unit =
  {
    if(isAddressInRange(addr))
      memory(addr - baseAddr) = DataNum(num)
    else
    {
      println("PC = " + pc + "  address out of bounds " + addr)
      execError = true
    }
  }

  // write a number to a memory address specified by the given location
  def writeToLocation(loc:Location, num:Int):Unit = loc match
  {
  case Reg(r) => registers(r) = num
  case _      => getLocationAddress(loc) match
                 {
                 case Some(addr) => writeToMemory(addr, num)
                 case None       =>
                 }
  }

  // push a number onto a stack referenced by the specified register
  def push(r:Int, num:Int):Unit =
  {
    registers(r) -= 4
    writeToMemory(registers(r), num)
  }

  // pop a number from a stack referenced by the specified register
  // and write the number to the specified location
  def pop(r:Int, loc:Location):Unit =
  {
    val num = getMemoryValue(registers(r)) match
              {
              case Some(n) => n
              case None    => 0
              }
    writeToLocation(loc, num)
    registers(r) += 4
  }

  var execError = false

  // execute the asm program starting at label main
  def exec(maxSteps:Int):Unit =
  {
    pc = checkMain
    if(pc < 0) return

    // point r7 to top of stack memory
    registers(7) = topAddr

    // heapBase is bottom of heap memory
    // heapBase      holds address of first available heap memory
    // heapBase + 4  holds address of last allocation
    // heapBase + 8  holds the number of allocations
    // heapBase + 12 is start of available heap memory
    writeToMemory(heapBase, heapBase + 12)  // available starts at heapBase + 12
    writeToMemory(heapBase + 4, 0)  // no previous allocation
    writeToMemory(heapBase + 8, 0)  // no allocations done yet

    for(steps <- 1 to maxSteps)
    {
      if(!isAddressInRange(pc))
      {
        println("program counter is out of bounds: " + pc)
        return
      }
      val instr = memory(pc - baseAddr)
      if(trace) println(instr)
      var incrementPC = true
      var useLoc = true
      var instrLoc:Location = Num(0)
      execError = false
      instr match
      {
      case LoadReg(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) = n
                              case None    => return
                              }
      case StoreReg(r, loc) => instrLoc = loc
                              writeToLocation(loc, registers(r))
      case Add(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) += n
                              case None    => return
                              }
      case Sub(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) -= n
                              case None    => return
                              }
      case Mul(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) *= n
                              case None    => return
                              }
      case Div(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) /= n
                              case None    => return
                              }
      case Or(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) |= n
                              case None    => return
                              }
      case And(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) &= n
                              case None    => return
                              }
      case Not(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) = ~ n
                              case None    => return
                              }
      case Xor(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) ^= n
                              case None    => return
                              }
      case ShiftLeft(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) <<= n
                              case None    => return
                              }
      case ShiftRightArith(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) >>= n
                              case None    => return
                              }
      case ShiftRightLogical(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => registers(r) >>>= n
                              case None    => return
                              }
      case Call(loc) => instrLoc = loc
                              getJumpLocationAddress(loc) match
                              {
                              case Some(addr) => if(registers(7) <=
                                                            topAddr - stackSize)
                                                 {
                                                   println("PC = " + pc +
                                                           " stack overflow")
                                                   return
                                                 }
                                                 push(7,
                                                      pc + calcInstrSize(loc))
                                                 pc = getJumpLocationAddress(loc) match
                                                      {
                                                      case Some(a) => a
                                                      case None    => return
                                                      }
                                                 incrementPC = false
                              case None    => println("invalid address")
                                              return
                              }
      case Return => if(registers(7) == topAddr)
                     {
                       println("Program has exited")
                       return
                     }
                     else
                     {
                       getMemoryValue(registers(7)) match
                       {
                       case Some(addr) => registers(7) += 4
                                          pc = addr
                                          incrementPC = false
                       case None => println("PC = " + pc +
                                            " r7 stack corrupted")
                       }
                     }
      case Goto(loc) => instrLoc = loc
                              getJumpLocationAddress(loc) match
                              {
                              case Some(addr) => pc = addr
                                                 incrementPC = false
                              case None    => println("invalid address")
                                              return
                              }
      case GotoIf(r, cmp, cmpLoc, jmp) => useLoc = false
                     val regVal = registers(r)
                     getLocationValue(cmpLoc) match
                     {
                     case Some(num) => if(cmp match
                                          {
                                          case CmpLT => regVal <  num
                                          case CmpLE => regVal <= num
                                          case CmpEQ => regVal == num
                                          case CmpNE => regVal != num
                                          case CmpGE => regVal >= num
                                          case CmpGT => regVal >  num
                                          })
                                       {// condition is true, so jump
                                         getJumpLocationAddress(jmp) match
                                         {
                                         case Some(addr) => pc = addr
                                                            incrementPC = false
                                         case None    => println(
                                                             "invalid address")
                                                         return
                                         }
                                       }
                     case None    => return
                     }
      case Push(r, loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => push(r, n)
                              case None    => return
                              }
      case Pop(r, loc) => instrLoc = loc
                          pop(r, loc)
      case Out(loc) => instrLoc = loc
                              getLocationValue(loc) match
                              {
                              case Some(n) => if(trace) println("Out: " + n)
                                              else      println(n)
                              case None    => return
                              }
      case Alloc(r, loc) => useLoc = false
                              getLocationValue(loc) match
                              {
                              case Some(n) => if(n <= 0)
                                    {
                                      println(
                                        "Alloc bytes must be > 0: " + n)
                                      return
                                    }
                                    val numBytes = if(n % 4 == 0)
                                                     n
                                                   else
                                                     (n + 3) / 4 * 4
                                    val heapNext = getMemoryValue(heapBase).get
                                    if(heapNext + numBytes >
                                                        heapBase + heapSize)
                                    {
                                      println("Alloc insufficient memory: " + n)
                                      return
                                    }
                                    // simplistic allocation
                                    registers(r) = heapNext
                                    // remember the allocation address
                                    writeToMemory(heapBase + 4, heapNext)
                                    // update address of available heap memory
                                    writeToMemory(heapBase, heapNext + numBytes)
                                    // update the number of allocations
                                    writeToMemory(heapBase + 8,
                                           1 + getMemoryValue(heapBase + 8).get)
                              case None    => return
                              }
      case Free(loc) => useLoc = false  // recover memory if it was the last
                                        // allocoated block; otherwise ignore
                              val nAllocs = getMemoryValue(heapBase + 8).get
                              if(nAllocs <= 0)
                              {
                                println("More allocs than frees")
                                return
                              }
                              val heapLastAlloc = getMemoryValue(heapBase+4).get
                              getLocationValue(loc) match
                              {
                              case Some(n) => if( n < heapBase + 8
                                               || n >= topAddr)
                                              {
                                                println(
                                                  "Free bad address: " + n)
                                                return
                                              }
                                              if(n == heapLastAlloc)
                                              {// freeing last allocated block
                                               // so make that the start of
                                               // available memory
                                                writeToMemory(heapBase, n)
                                                // no previous allocation
                                                writeToMemory(heapBase + 4, 0)
                                                // decrement number of allocs
                                                writeToMemory(heapBase + 8,
                                                              nAllocs - 1)
                                              }
                                              else if(nAllocs == 1)
                                              {// all allocs have been unwound
                                               // reset alloc fields
                                                writeToMemory(heapBase,
                                                              heapBase + 12)
                                                writeToMemory(heapBase + 4, 0)
                                                writeToMemory(heapBase + 8, 0)
                                              }
                              case None    => return
                              }
      case _       => println("Address has invalid instruction: " + pc)
                      return
      }

      if(execError) return

      if(incrementPC)
      {
        pc += calcInstrSizeInstr(if(useLoc) Some(instrLoc) else None,
                                 instr)
      }
    }
  }

  def locToStr(loc:Location):String = loc match
  {
  case Reg(r) => "r" + r
  case Num(n) => n.toString
  case Address(name) => name
  case AddressOnly(name) => "&" + name
  case AddressIndex(name, idx) => name + "[r" + idx + "]"
  case IndirectReg(r) => "*r" + r
  case IndirectAddress(name) => "*" + name
  case IndirectAddressIndex(name, idx) => "*" + name + "[r" + idx + "]"
  case AddressNum(n) => "&" + n
  case AddressNumIndex(addr, idx) => "&" + addr + "[r" + idx + "]"
  case IndirectAddressNum(addr) => "*&" + addr
  case IndirectAddressNumIndex(addr, idx) => "*&" + addr + "[r" + idx + "]"
  }

  val qsr = List(92, 91, 114, 91, 48, 45, 55, 93, 92, 93, 124, 114, 91, 48, 45, 55, 93, 124, 91, 65, 45, 90, 97, 45, 122, 93, 43, 91, 65, 45, 90, 97, 45, 122, 48, 45, 57, 93, 42, 124, 91, 48, 45, 57, 93, 43, 124, 61, 61, 124, 33, 61, 124, 60, 61, 124, 62, 61, 124, 60, 60, 124, 62, 62, 62, 124, 62, 62, 124, 91, 45, 43, 42, 47, 60, 62, 61, 40, 41, 58, 44, 38, 93)

  def instrToStr(instr:Instr):String = instr match
  {
  case LoadReg(r, src) => "r" + r + " = " + locToStr(src)
  case StoreReg(r, dest) => locToStr(dest) + " = r" + r
  case Add(r, loc) => "r" + r + " = r" + r + " + " + locToStr(loc)
  case Sub(r, loc) => "r" + r + " = r" + r + " - " + locToStr(loc)
  case Mul(r, loc) => "r" + r + " = r" + r + " * " + locToStr(loc)
  case Div(r, loc) => "r" + r + " = r" + r + " / " + locToStr(loc)
  case Or(r, loc) => "r" + r + " = r" + r + " or " + locToStr(loc)
  case And(r, loc) => "r" + r + " = r" + r + " and " + locToStr(loc)
  case Not(r, loc) => "r" + r + " = not " + locToStr(loc)
  case Xor(r, loc) => "r" + r + " = r" + r + " xor " + locToStr(loc)
  case ShiftLeft(r, loc) => "r" + r + " << " + locToStr(loc)
  case ShiftRightArith(r, loc) => "r" + r + " >> " + locToStr(loc)
  case ShiftRightLogical(r, loc) => "r" + r + " >>> " + locToStr(loc)
  case Call(loc) => "call " + locToStr(loc)
  case Return => "return"
  case Goto(loc) => "goto " + locToStr(loc)
  case GotoIf(reg, cmp, cmpLoc, goto) => "if(r" + reg + " " +
                                           (cmp match
                                            {
                                            case CmpLT => "<"
                                            case CmpLE => "<="
                                            case CmpEQ => "=="
                                            case CmpNE => "!="
                                            case CmpGE => ">="
                                            case CmpGT => ">"
                                            }) + " " + locToStr(cmpLoc) +
                                           ") goto " + locToStr(goto)
  case Push(stackReg, src) => "push(r" + stackReg + ", " + locToStr(src) + ")"
  case Pop(stackReg, dest) => "pop(r" + stackReg + ", " + locToStr(dest) + ")"
  case Out(loc) => "out " + locToStr(loc)
  case Alloc(r, numBytes) => "r" + r + " = Alloc(" + locToStr(numBytes) + ")"
  case Free(loc) => "Free(" + locToStr(loc) + ")"
  case DataNum(num) => num.toString
  case DataAddress(name) => name
  case DataBytes(numBytes) => "bytes(" + numBytes + ")"
  case LabelInstr(name, instr) => name + ": " + instrToStr(instr)
  case Txt(s) => "error: " + s
  case NotUsed => "not used"
  }

  val spaces = "                                                               "

  def fill(s:String, len:Int):String = if(s.size >= len) s + " "
                                       else s + (spaces take (len - s.size))
}
