/**
 * Expression language implementation main program.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and print the source tree.
 */
object Main {

    import java.io.FileNotFoundException
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.kiama.parsing.Success
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions}

    def main (args : Array[String]) {

        args.size match {

            // If there is exactly one command-line argument
            case 1 =>
                try {
                    // Create a source for the argument file name
                    val source = new FileSource (args (0))
                    
                    // Create a syntax analysis module
                    val positions = new Positions
                    val parsers = new SyntaxAnalysis (positions)

                    // Parse the file
                    parsers.parse (parsers.parser, source) match {
                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>

                            // Pretty print the source tree
                            println (layout (any (sourcetree)))

                        // Parsing failed, so report it
                        case f =>
                            println (f)
                      }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                }

            // Complain otherwise
            case _ =>
                println ("usage: run file.exp")

        }

    }

}
