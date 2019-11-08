package au.edu.imb

/**
 * Parser for phylogenetic trees in Newick and other formats.
 */

import java.io.File
import scala.io.Source
import scala.util.parsing.combinator._


/**
 * Super class of tree parsers that removes comments from input and provided
 * read functions for input.
 * Comments are defined by rectangular brackets [] and must not be nested.
 * @tparam T Type of tree node returned by the parser
 * @param nf Node factory method used by the parser to create a node.
 *           takes name, branch lengths, and list of child name2node to create a new node.
 */
abstract class TreeParser[T](nf: (String,Double,List[T]) => T) extends JavaTokenParsers {
  /** main parser. Needs to be implemented in subclass */
  protected def tree:Parser[T]

  /** removes comments from input text. Can be overridden. */
  protected def removeComments(text:String) = text.replaceAll("""\[.+?\]""","")

  /** reads tree definition from file and returns root node */
  def read(file:File):T = read(Source.fromFile(file).getLines().mkString("\n"))

  /** reads tree definition from string and returns root node */
  def read(text:String):T = parseAll(tree, removeComments(text)) match {
    case Success(result, next) => result
    case failure => throw new Exception(failure.toString)
  }
}


/**
 * A simple parser for tree data.
 * Parses trees such as: "R:0.0 (X:0.1 (A:0.1 B:0.1) Y:0.1 (C:0.1 D:0.1))"
 * Grammar:
 * tree    ::= identifier length [subtree]
 * subtree ::= "(" {tree} ")"
 * length  ::= ":" floatingPointNumber
 * @param nf Node factory method used by the parser to create a node.
 */
class SimpleParser[T](nf: (String,Double,List[T]) => T) extends TreeParser[T](nf) {
  def tree = (ident ~ length ~ opt(subtree)) ^^ {case n~l~d => nf(n,l,d.getOrElse(Nil))}
  def subtree:Parser[List[T]] = "(" ~> rep(tree) <~ ")"
  def length = ":" ~> floatingPointNumber ^^ { _.toDouble }
}


/**
 * Parser for Newick format.
 * Grammar adapted from: http://evolution.genetics.washington.edu/phylip/newick_doc.html
 *
 * tree        ::= subtree ";"
 * descendants ::= "(" subtree {subtree ","} ")"
 * subtree     ::= descendants name length | leaf
 * leaf        ::= name length
 * name        ::= [quoted | unquoted]
 * unquoted    ::= identifier
 * quoted      ::= "'" { not "'" | "''"} "'"
 * length      ::= [":" floatingPointNumber]
 * @param nf Node factory method used by the parser to create a node.
 */
class NewickParser[T](nf: (String,Double,List[T]) => T) extends TreeParser[T](nf) {
  def tree = subtree <~ ";"
  def descendants:Parser[List[T]] = "(" ~> repsep(subtree, ",") <~ ")"
  def subtree = descendants~name~length ^^ {case t~n~l => nf(n,l,t)} | leaf
  def leaf = name~length ^^ {case n~l => nf(n,l,Nil)}
  def name = opt(quoted | unquoted) ^^ { _.getOrElse("") }
  def unquoted = ident
  def quoted = """'([^']|'')*'""".r  ^^ { _.drop(1).dropRight(1).replace("''","'") }
  def length = opt(":" ~> floatingPointNumber) ^^ { _.getOrElse("0").toDouble }
}


/**
 * Advanced parser for Newick format that allows nested comments and brackets
 * in quoted identifiers.
 * @param nf Node factory method used by the parser to create a node.
 */
class NewickParser2[T](nf: (String,Double,List[T]) => T) extends NewickParser[T](nf) {

  /** Parser to remove nested comments */
  protected class CommentParser extends JavaTokenParsers {
    override val skipWhitespace = false
    def remove(text:String):String = parseAll(rest,text) match {
      case Success(result, next) => result
      case failure => throw new Exception(failure.toString)
    }
    def rest = rep(quoted | comment | any) ^^ { _.mkString }
    def any = """.""".r
    def quoted = """'([^']|'')*'""".r
    def comment: Parser[String] = ("["~rep(not("]")~(comment | any))~"]") ^^ {_ => ""}
  }
  private val commentParser = new CommentParser

  override def removeComments(text:String) = commentParser.remove(text)
  override def unquoted = """[^\[\]():;,]+""".r ^^ { _.trim }
}




/** Just some usage examples */
object TreeParserExample extends App {
  val simpleParser = new SimpleParser(DefaultNode.apply)
  val simpleTree = "R:0.0 (X:0.1 (A:0.2 B:0.3) Y:0.4 (C:0.5 D:0.6))"
  simpleParser.read(simpleTree).display()

  val newickParser = new NewickParser(DefaultNode.apply)
  val newickTree = "('A',( :0.1,C)D:0.5)  E;"
  newickParser.read(newickTree).display()

  val newickParser2 = new NewickParser2(DefaultNode.apply)
  val newickTree2 = "('A(1)[2]':0.1, (['B[2]':0.1 [,C] ]) D:0.5)  E;"
  newickParser2.read(newickTree2).display()
}