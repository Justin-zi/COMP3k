/**
 * A simple Scala program for COMP332.
 *
 * Copyright 2011-7, Anthony Sloane, Macquarie University, All rights reserved.
 */

/**
 * A Scala object is a way to package functionality together.  In fact,
 * an object is just a singleton class.  It declares the members of a class
 * *and* stands for the one and only instance of that class.  Objects are
 * also commonly used as modules so that functionality can be imported
 * into other classes.
 *
 * In this case the object just contains the main program in the form of
 * method that takes an array of the command-line arguments.
 */
object Main {

    // This import declaration brings everything in from the BinaryTrees
    // object.  I.e., the underscore is a wildcard.
    import BinaryTrees._

    /**
     * Scala has a powerful collection library.  Here we use the List class
     * to collect all of the leaves.  A type Foo[T] is a generic type Foo
     * parameterised by some other type T.  Here, Array[String] means take the
     * generic Array class and parameterise it by the String type.  This
     * notation is similar to the Foo<T> notation of Java.
     */

    // The main method
    def main (args : Array[String]) {

        // Arrays are just objects so we can ask for their size
        if (args.size == 1) {

            // Arrays in Scala are indexed using parentheses, not square
            // brackets, so args(0) is the first argument.

            // A val is given a value when it is declared, but that value
            // cannot be changed later.  Note that the type of the val is
            // not given here.  It is inferred to be Int by the compiler.
            // A full declaration would be
            //   val count : Int = args(0).toInt
            val count = args(0).toInt

            // The rest will only work if the count is > 0
            if (count > 0) {

                // Create a complete binary tree of depth count

                // A var is a variable, whose value can be changed.
                // variables have to be initialised.

                // The tree we are building
                var t = makefork ()

                // The number of levels we have done so far
                var levels = 1

                // Add forks and new sub-trees until the depth is satisifed
                while (levels != count) {
                    t = Fork (t, makefork ())
                    levels = levels + 1
                }

                // Print out the tree
                println (t)

            } else {

                println ("Please enter an argument that is greater than zero")

            }

        } else {

            println ("Please enter a number as the only argument")

        }

    }

    /**
     * Return a binary tree consisting of a single Fork with two leaves.
     */
    def makefork () : Tree =
        Fork (Leaf (88), Leaf (88))

}
