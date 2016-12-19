package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy( identity ).mapValues( _.length ).toList.sortWith(_._1<_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s.flatMap( wordOccurrences ).groupBy( _._1 ).toList.map( x => (x._1 , x._2.map(_._2).reduce( _ + _ ) ) ).sortWith(_._1<_._1)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.map( x=>(wordOccurrences(x),x) ).groupBy(_._1).map( x => ( x._1 , x._2.map( _._2 ) ) )

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences ( wordOccurrences ( word ) )

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def xcombinations[T](list:List[T],n: Int): List[List[T]] =
      if (n > list.length) Nil
      else list match {
        case _ :: _ if n == 1 =>
          list.map(List(_))
        case hd :: tl =>
          xcombinations(tl,n - 1).map(hd :: _) ::: xcombinations(tl,n)
        case _ => Nil
      }
    val unfolded = occurrences.flatMap( x => List.fill(x._2)( x._1 , 1 ) )
    var out_list: List[Occurrences] = List() :: Nil
    for( i <- 1 to unfolded.length ) {
      var cur_list = xcombinations(unfolded,i)
      out_list = List.concat(cur_list,out_list )
    }
    out_list.map(_.groupBy(_._1).toList.map(x=>(x._1 , x._2.length)).sortWith(_._1<_._1))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = List.concat( x , y.map( x => ( x._1 , -x._2)) ).groupBy(_._1).toList.map( x=> (x._1,x._2.map(_._2).reduce( _ + _ )) ).filter( _._2 > 0 ).sortWith(_._1<_._1)

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if( sentence == List[Word]() ){
      return List( Nil)
    }
    def xcombinations[T](list:List[T],n: Int): List[List[T]] =
      if (n > list.length) Nil
      else list match {
        case _ :: _ if n == 1 =>
          list.map(List(_))
        case hd :: tl =>
          xcombinations(tl,n - 1).map(hd :: _) ::: xcombinations(tl,n)
        case _ => Nil
      }
    def trymatch(acc:List[Char],rest:List[Char]): List[List[String]] ={
      var out :List[List[String]] = List(List())
      for( i <- 1 to rest.length ) {
        val cur_list = xcombinations(rest,i)
        out = out ::: cur_list.map(x=>dictionaryByOccurrences.get(compress(acc ::: x))).filter(_.isDefined).map(_.get)
      }
      out
    }
    /*def decomp[T]( a : List[ T ] ) : List[List[ List[ T ] ] ]= {
      if( a.isEmpty ) List()
      else {
        case h :: tl =>
          decomp( tl ).map( h :: _ )
        /*for( i <- 1 until a.length ) {
          out = out ::: decomp( a - i ).map( _ ::: List(i) )
        }*/
       //out.map( y=>y.groupBy(identity).toList.map(x=>(x._1,x._2.length)).toSet )//.map( _.toList.flatMap( x=> List.fill(x._2)( x._1 ) ) )
      }
    }*/
    //println( decomp(4))
    def compress( list : List[Char]) = list.groupBy(identity).toList.map(x=>(x._1,x._2.length))
    def unfold( list : List[(Char,Int)]) = list.flatMap( x => List.fill(x._2)( x._1 ) )
    def diff( a : List[Char] , b : List[Char] ) = unfold(subtract( compress(a) , compress(b) ))
    val occurences = sentenceOccurrences(sentence)
    val unfolded = unfold(occurences)
    def outer[A]( a : List[List[A]] ) : List[List[A]] = a match {
      case (h : List[A]) :: Nil =>
        h.map( x=> List(x) )
      case h :: tl =>
          outer( tl ).flatMap( x => h.map( y => List( y ) ::: x ) )
      case _ => Nil
    }
    println(outer( List( List(0),List(2),List(0,1) ) ))
    //trymatch( List() , unfolded)
    //val comb = combinations( occurences ).filter( dictionaryByOccurrences.get(_).isDefined )
    def selectComb( a : Occurrences ) : List[List[ Occurrences ]] ={
      /*case h :: Nil => {
        if( dictionaryByOccurrences.get ( List(h) ).isDefined ) List(List(List(h))) else null
      }
      case h :: tl => {*/
      //println(combinations ( a ))
        val comb = combinations ( a ).filter ( x => dictionaryByOccurrences.get ( x ).isDefined )

        val ret = comb.map( x => ( selectComb ( subtract ( a, x ) ) , subtract ( a, x ) , x ) )//.filter( x=> x._1 != null && (!x._2.isEmpty || !x._1.isEmpty) )
        ///println(ret)
        var outList = List[List[ Occurrences ]]()
        for( x <-  ret) {
          if( x._1.isEmpty ) {
            if( x._2.isEmpty ) {
              outList = outList ::: List(List(x._3))
            }
          } else {
            outList = outList ::: x._1.map( List(x._3) ::: _ )
          }
        }
      outList//ret.flatMap ( x =>  ).asInstanceOf[List[List[ Occurrences ]]]
      /*}
      case _ => List()*/
    }
    val comb = selectComb( occurences)

    val ret = comb.flatMap(x=>outer(x.map( y=>dictionaryByOccurrences ( y ))))
    ret
  }
}
