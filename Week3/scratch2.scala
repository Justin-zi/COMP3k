
def lookup[A,B](k:A, l:List[(A, B)]):Option[B] = l match {
  case Nil => None
  case h :: t if (h._1 == k) => Some(h._2)
  case h :: t              => lookup(k, t)
}


def subst(sentence:List[String], pairs:List[(String, String)]):List[String]
= sentence match {
  case Nil => Nil
  case w :: s => lookup(w, pairs) match {
    case None    => w :: subst(s, pairs)
    case Some(v) => v :: subst(s, pairs)
  }
}

def substOne(w:String, pairs:List[(String, String)]):String
= lookup(w,pairs) match {
  case None => w
  case Some(v) => v
}

def subst2(sentence:List[String], pairs:List[(String, String)]):List[String]
= sentence map (w => substOne(w, pairs))



def isSorted(l:List[Int]):Boolean = l match {
  case a :: b :: t => a <= b && isSorted(b::t)
  case h :: Nil => true
  case Nil => true
}

List((4, "meow"), (5, "stuff"))