package objsets

import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface {

  def filter(p: Tweet => Boolean): TweetSet

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

    def filter(p: Tweet => Boolean): TweetSet = new Empty

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

    def mostRetweeted: Nothing = throw new NoSuchElementException

    def union(that: TweetSet): TweetSet = that

    def descendingByRetweet: TweetList = Nil

    def contains(tweet: Tweet): Boolean = false

    def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

    def remove(tweet: Tweet): TweetSet = this

    def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      if (p(elem)) right filterAcc(p, left.filterAcc(p, acc.incl(elem)))
      else left.filterAcc(p, right.filterAcc(p, acc))
    }

    def filter(p: Tweet => Boolean): TweetSet = {
        val acc = new Empty
        this.filterAcc(p, acc)
    }

    def union(that: TweetSet): TweetSet = {  // @Improve
        right union (left.union(that.incl(elem)))
    }

    def mostRetweeted: Tweet = {
        val empty = new Empty 
        def picker(x1: Tweet, x2: Tweet, x3: Tweet): Tweet = List(x1, x2, x3).maxBy(_.retweets)
        val cheft = left.union(empty) != empty
         val chight = right.union(empty) != empty
        //val maxx = elem //val mole = elem; val mori = elem
        // if (left == empty && right == empty) elem
        if (cheft && chight) {
            val ml = left.mostRetweeted
            val mr = right.mostRetweeted
            picker(ml, elem, mr)
        }
        else if (cheft) {
            val ml = left.mostRetweeted
            if (elem.retweets > ml.retweets) elem else ml
        }
        else if (chight) {
            val mr = right.mostRetweeted
            if (elem.retweets > mr.retweets) elem else mr
        }
        else elem
    }

    def descendingByRetweet: TweetList = {
        val empty = new Empty
        def recur(ordList: List[Tweet], tweets: TweetSet): List[Tweet] = {
            if (tweets.union(empty) equals empty) ordList
            else {
                val tw = tweets.mostRetweeted
                recur(tw::ordList, tweets.remove(tw))
            }
        }
        val twist = recur(List[Tweet](), this)
        //println(twist)
        def builder(twist: List[Tweet], acc: TweetList): TweetList = {
            if (twist.isEmpty) acc else builder(twist.tail, new Cons(twist.head, acc))
        }
        builder(twist, Nil)
    }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = allTweets filter {case tweet: Tweet => google exists(tweet.text contains(_))}
  lazy val appleTweets: TweetSet = allTweets filter {case tweet: Tweet => apple exists(tweet.text contains(_))}

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
