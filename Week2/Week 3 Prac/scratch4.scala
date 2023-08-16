def foo(s:List[Int]):Int = s match {
  case Nil => 0
  case h :: t => h + foo(t)
}
/* 
Trace Input -> foo(List(1,2,3,4))
foo(List(1,2,3,4))
1 + foo(List(2,3,4))
1 + 2 + foo(List(3,4))
1 + 2 + 3 + foo(List(4))
1 + 2 + 3 + 4 + foo(List())
1 + 2 + 3 + 4 + 0
Returns = 10
*/


def foo(a:Int, b:List[Int]):Int = b match {
  case Nil => a //If the list is empy return a
  case h :: t => foo(a + h, t map (x => x * 2))
}

/* 
Trace Input -> foo(0, List(1,2,3))
foo(0, List(1,2,3))
foo(1, List(4,6))
foo(5, List(12))
foo(17, List())
return 17

 */


def inc(a:Int):Int = a+1 

def map2[A,B](f:(A)=>B, l:List[A]):List[B]
= l match {
  case Nil => Nil
  case h::t => f(h) :: map2(f, t)
}

def foo3(a:Int, b:List[Int]):Int = b match {
case Nil => a
case h :: t => 2 * foo3(a * h, t map (_ + 4))
}

def isSorted(l:List[Int]):Boolean
= l match {
  case a::b::t => a <= b && isSorted(b::t)
  case _ => true
}


def lookup[A,B](key:A, pairs:List[(A, B)]):Option[B]
= pairs match {
  case Nil => None
  case (ha, hb) :: t if (ha==key) => Some(hb)
  case (ha, hb) :: t => lookup(key, t)
}

def subst(sentence:List[String], pairs:List[(String,String)]):List[String]
= sentence match {
  case Nil => Nil
  case h :: t => lookup(h, pairs) match {
    case None    => h :: subst(t, pairs)
    case Some(v) => v :: subst(t, pairs)
  }
}

def substOne(word:String, pairs:List[(String,String)]):String
= lookup(word, pairs) match {
  case None => word
  case Some(otherword) => otherword
}

def subst(sentence:List[String], pairs:List[(String,String)]):List[String]
= sentence map (x => substOne(x, pairs))
