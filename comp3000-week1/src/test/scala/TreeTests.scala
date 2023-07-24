/**
 * Some tests of the binary tree functions.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Some tests of the binary tree functions.  The RunWith etc stuff is there
 * to make it possible for tools such as Eclipse to find these tests.
 */
@RunWith(classOf[JUnitRunner])
class TreeTests extends FunSuite {

    import BinaryTrees._

    /**
     * These tests use the ScalaTest library.  Each test has a title
     * description and a body.  The body uses the assertResult function to
     * run an expression and check that the result is as expected.
     * The general form is assertResult (x) (e) where the expression e is
     * expected to produce the value x.
     */

    test ("a single leaf tree has just one leaf") {
        assertResult (1) (numleaves (Leaf (42)))
    }

    test ("a one-level tree has two leaves") {
        assertResult (2) (numleaves (Fork (Leaf (42), Leaf (99))))
    }

    test ("a more complex tree has the correct number of leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        assertResult (4) (numleaves (t))
    }

    test ("a single leaf tree has just that leaf") {
        assertResult (List (Leaf (42))) (leaves (Leaf (42)))
    }

    test ("a one-level tree has its two leaves") {
        assertResult (List (Leaf (42), Leaf (88))) (
            leaves (Fork (Leaf (42), Leaf (88)))
        )
    }

    test ("a more complex tree has the correct leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        assertResult (List (Leaf (1), Leaf (2), Leaf (3), Leaf (4))) (
            leaves (t)
        )
    }

    test ("the sum of a single leaf tree is the value of the leaf") {
        assertResult (42) (sumleaves (Leaf (42)))
    }

    test ("the sum of a one-level tree is the sum of the two leaves") {
        fail ("replace with a useful test")
    }

    test ("the sum of a mutli-level tree is the sum of its leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        fail (s"replace with a useful test using $t")
    }

    test ("the incandswap of a leaf tree is just the leaf tree incremented") {
        assertResult (Leaf (43)) (incandswap (Leaf (42)))
    }

    test ("the incandswap of a one-level tree is inc and swap of those leaves") {
        fail ("replace with a useful test")
    }

    test ("the incandswap of a mutli-level tree is as expectResulted") {
        fail ("replace with a useful test")
    }

}
