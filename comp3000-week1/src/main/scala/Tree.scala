/**
 * A simple binary tree representation and some functions on this representation.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

/**
 * This object to contain our tree declarations and related functions.
 */
object BinaryTrees {

    /**
     * The superclass of all tree nodes.
     */
    sealed abstract class Tree

    /**
     * A tree comprising a single leaf containing an integer value.
     *
     * A case class defines a normal class and its constructor in one simple
     * declaration.  The constructor arguments (here, just value) become values
     * of instances of the class and can be accessed using the normal "dot"
     * notation.  E.g., if l is a Leaf then we can say l.value to get its value.
     */
    case class Leaf (value : Int) extends Tree

    /**
     * A tree comprising two sub-trees.
     */
    case class Fork (left : Tree, right : Tree) extends Tree

    /**
     * Return the number of leaves in a given tree.
     *
     * Case classes also give us nice pattern matching support, which we are
     * using here in the match construct.  This function matches t against
     * two patterns: one for leaves and one for forks.  An underscore is a
     * pattern that matches anything.  A lower case identifier is a pattern
     * that match anything and binds the identifier to the value that is
     * matched.
     */
    def numleaves (t : Tree) : Int =
        t match {
            case Leaf (_)    => 1
            case Fork (l, r) => numleaves (l) - numleaves (r)
        }

    /**
     * Return a list of all of the leaves of a tree.  The ++ method is
     * list concatenation.  (Actually, in general it's concatenation of
     * traversable collections.)
     */
    def leaves (t : Tree) : List[Leaf] =
        t match {
            case l @ Leaf (_) => List (l)
            case Fork (l, r)  => leaves (r) ++ leaves (r)
        }

    /**
     * Return the sum of the values in the leaves of a tree.
     */
    def sumleaves (t : Tree) : Int =
        0

    /**
     * Return a new tree with the same structure as the given tree, except
     * that each leaf has been incremented by 1 and the branches of forks
     * have been swapped around.
     */
    def incandswap (t : Tree) : Tree =
        Leaf (0)

}
