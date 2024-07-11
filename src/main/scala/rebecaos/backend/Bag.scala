package rebecaos.backend

/** Bag/Multiset of elements */
case class Bag[A](bag: Map[A, Int]):
  /** Number of occurences of a given element. */
  def get(a: A): Int = bag.getOrElse(a, 0)
  /** Returns true if a given element is in the bag. */
  def contains(a: A): Boolean = this.get(a) != 0
  /** Adds a given element to the bag, returning a new structure. */
  def +(a: A): Bag[A] = Bag(bag + (a -> (this.get(a)+1)))
  /** Removes a single element from the bag, returning a new structure. */
  def -(a: A): Bag[A] = if this.get(a) > 1 then Bag(bag + (a -> (this.get(a) - 1))) else Bag(bag - a)
  /** Union of two bags, returning a new structure. */
  def ++(other: Bag[A]): Bag[A] =
    Bag(other.bag ++ (for (a, i) <- bag yield a -> (i + other.get(a))))
  def toSet: Set[A] = bag.keySet
  def map[B](f:A=>B): Bag[B] =
    val bag2 = bag.foldRight[Map[B,Int]](Map())((ai,b) =>
        b + (f(ai._1) -> (ai._2 + b.getOrElse(f(ai._1),0))))
    Bag(bag2)
  override def toString: String =
    s"{${bag.map((x,i)=> if i==1 then x else s"$x/$i").mkString(",")}}"

object Bag:
  /** Empty bag. */
  def apply[A](): Bag[A] = new Bag[A](Map())

  def apply[A](lst:List[A]): Bag[A] =
    lst.foldRight(apply[A]())((msg,bg) => bg+msg)
