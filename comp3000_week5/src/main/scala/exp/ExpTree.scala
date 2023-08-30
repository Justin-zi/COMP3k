/**
 * Expression language tree definition.
 *
 * Copyright 2009-2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Module containing structures for representing expression language programs.
 */
object ExpTree {

    /**
     * Interface for all expression tree nodes.
     */
    sealed abstract class ExpNode

    /**
     * An expression program consisting of the given statements.
     */
    case class ExpProgram (stmts : Vector[Statement]) extends ExpNode

    /**
     * Superclass of all statement classes.
     */
    sealed abstract class Statement extends ExpNode

    /**
     * A statement that evaluates an expression and throws away its value.
     */
    case class ExpStmt (exp : Expression) extends Statement

    /**
     * A statement that evaluates an expression and assigns its value to a
     * variable.
     */
    case class SetStmt (v : IdnExp, exp : Expression) extends Statement

    /**
     * Superclass of all expression classes.
     */
    sealed abstract class Expression extends ExpNode

    /**
     * An expression whose value is the current value of a variable.
     */
    case class IdnExp (idn : Identifier) extends Expression

    /**
     * An expression whose value is an integer constant.
     */
    case class IntExp (num : Int) extends Expression


    // An expression whose value is IF - with two children: one for the condition expression and one for the list of statements.
    case class IfStmt (e : Expression, s: Statement) extends Statement

    /**
     * An expression whose value is the difference between the values of
     * two expressions.
     */
    case class MinusExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the sum of the values of two expressions.
     */
    case class PlusExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the product of the values of two expressions.
     */
    case class StarExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the division of the values of two expressions.
     */
    case class SlashExp (left : Expression, right : Expression) extends Expression

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}
