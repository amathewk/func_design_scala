val s = ((1,2) #:: (2,3) #:: (3,4) #:: Stream.empty)
val i = List(2,3).toSet
//i.contains(1)
val s2 = (1 #:: 2 #:: 3 #:: Stream.empty)
//s2.filter( i.contains(_._1) )
val nn = for {
  ss <- s if i.contains(ss._1)
} yield ss

val nn2 = s2.filter(i.contains(_))
val nn3 = s.filter{case (a,b) => i.contains(a)}


nn3.toSet

//val c= i.contains(2)
//c
