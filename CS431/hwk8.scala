//Sam Kersebet

object Main{
// your sorting methods are placed here
def merge (l: List[Double] , r: List[Double]):List[Double] = 
(l, r) match {
    case(l, Nil) => l
    case(Nil, r) => r
    case(lH::lT, rH::rT) =>
        if (lH < rH) lH::merge(lT,r)
        else rH :: merge(l, rT);
}
def merge_sort(lst: List[Double]): List[Double] = lst match {
    case Nil => lst
    case h::Nil => lst
    case _ =>
        val(l, r) = lst.splitAt(lst.length/2)
        merge(merge_sort(l),merge_sort(r))
}

def sort(lst: List[Double], r1: List[Double], r2: List[Double], m: Double) : List[Double] = lst match {
    case h :: t => if( h > m) sort(t, h :: r1, r2, m) else  sort(t, m :: r1, r2, h)
    case Nil =>  select(r1, r2 :+ m)
}
def select(lst: List[Double], result: List[Double]) : List[Double] = lst match {
    case h :: t => sort(t, Nil, result, h) 
    case Nil => result
}

def selection_sort(lst: List[Double]): List[Double] = {
    select(lst, Nil)
}

def insert(x: Double, xs: List[Double]): List [Double] = {
    if(xs.isEmpty || x < xs.head) x::xs
    else xs.head::insert (x,xs.tail)
}
def insertion_sort(lst: List[Double]): List[Double]={
    if(lst.isEmpty || lst.tail.isEmpty) lst
    else insert(lst.head,insertion_sort(lst.tail))
}


def main(args: Array[String]) {
val lst = List(5.1, 4.2, 11.0, 2.3, 3.5, 1.5, 0.6, 9.7)
println(merge_sort(lst))
println(selection_sort(lst))
println(insertion_sort(lst))

}
}