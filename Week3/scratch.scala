def lookup[A,B](key:A, list:List[(A,B)]):Option[B]
= list match {
  case Nil => None
  case (ha, hb)::_ if ha==key => Some(hb)
  case _::tail => lookup(key, tail)
}

def isSorted[A](cmp:(A, A)=>Boolean, list:List[A]):Boolean = list match {
  case a::b::t => cmp(a, b) && isSorted(cmp, b::t)
  case _ => true
}

def map2(fn:(Int)=>Int, l:List[Int]):List[Int] = l match {
  case Nil => Nil
  case h::t => fn(h) :: map2(fn, t)
}

def substOne(a:String, rl:List[(String,String)]):String
= lookup(a, rl) match {
  case None => a
  case (Some(b)) => b
}

def subst(a:List[String], rl:List[(String, String)]):List[String]
= a map (substOne(_, rl))