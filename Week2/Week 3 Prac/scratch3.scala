
def isSorted(l:List[Int]):Boolean = l match {
  case a::b::t => a<=b && isSorted(b::t)
  case _ => true
}

def lookup[A,B](key:A, pairs:List[(A,B)]):Option[B]
= pairs match {
  case Nil => None
  case (ha, hb)::_ if (ha==key) => Some(hb)
  case _ :: t => lookup(key,t)
}

def substOne(word:String, pairs:List[(String,String)]):String
= lookup(word,pairs) match {
  case None => word
  case Some(otherword) => otherword
}

def subst(sentence:List[String], pairs:List[(String,String)]):List[String]
= sentence map (substOne(_, pairs))

def subst2(sentence:List[String], pairs:List[(String,String)]):List[String]
= sentence match {
  case Nil => Nil
  case h::t => substOne(h, pairs) :: subst2(t, pairs)
}

def mapInt(f:Int=>Int, l:List[Int]):List[Int]
= l match {
  case Nil  => Nil
  case h::t => f(h) :: mapInt(f, t)
}

def map[A,B](f:(A)=>B, l:List[A]):List[B]
= l match {
  case Nil  => Nil
  case h::t => f(h) :: map(f, t)
}

