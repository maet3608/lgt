package au.edu.imb

/**
 * Unit tests for Newick parser
 */

import org.scalatest.FunSuite


// Simple node with a toString method that returns the node and its subtree.
protected case class TestNode(name:String, length:Double, descendants:List[TestNode]) {
  override def toString = name +
    (if(length !=0 ) ":"+length else "") +
    (if(descendants.isEmpty) "" else descendants.mkString("(",",",")"))
}


class SimpleParserTest extends FunSuite  {
  val parser = new SimpleParser(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("names") {
    expect("Name:0.1")(parse("Name:0.1"))
    expect("Name_1:0.1")(parse("Name_1:0.1"))
  }

  test("simple trees") {
    expect("A:0.1")(parse("A:0.1"))
    expect("A:0.1(B:0.2)")(parse("A:0.1 (B:0.2)"))
    expect("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 (B:0.2 C:0.3)"))
    expect("R:0.1(X:0.2(A:0.3,B:0.4),Y:0.5(C:0.6,D:0.7))")(parse("R:0.1 (X:0.2 (A:0.3 B:0.4) Y:0.5 (C:0.6 D:0.7))"))
  }

  test("comments") {
    expect("A:0.1(B:0.2,C:0.3)")(parse("[a comment] A:0.1 (B:0.2 C:0.3)"))
    expect("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 (B:0.2 C:0.3) [a comment]"))
    expect("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 [a comment] (B:0.2 C:0.3)"))
    expect("A:0.1(B:0.2)")(parse("A:0.1 [a comment] (B:0.2 [C:0.3])"))
    expect("A:0.1")(parse("A:0.1 [(B:0.2 C:0.3)]"))
  }
}


class NewickParserTest extends FunSuite  {
  val parser = new NewickParser(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("simple trees without branch lengths") {
    expect("A")(parse("A;"))
    expect("A(B)")(parse("(B)A;"))
    expect("C(A,B)")(parse("(A,B)C;"))
    expect("F(A,E(B,C))")(parse("(A,(B,C)E)F;"))
    expect("F(E((B,C),A))")(parse("(((B,C),A)E)F;"))
  }

  test("simple trees without names") {
    expect("")(parse(";"))
    expect("()")(parse("();"))
    expect("(,)")(parse("(,);"))
    expect("(,(,))")(parse("(,(,));"))
    expect("(((,),))")(parse("(((,),));"))
  }

  test("simple trees with branch lengths") {
    expect("A:0.1")(parse("A:0.1;"))
    expect("A:0.1(B:0.2)")(parse("(B:0.2)A:0.1;"))
    expect("C:0.3(A:0.1,B:0.2)")(parse("(A:0.1,B:0.2)C:0.3;"))
    expect("F:0.5(A:0.1,E:0.4(B:0.2,C:0.3))")(parse("(A:0.1,(B:0.2,C:0.3)E:0.4)F:0.5;"))
    expect("F:0.5(E:0.4((B:0.2,C:0.3),A:0.1))")(parse("(((B:0.2,C:0.3),A:0.1)E:0.4)F:0.5;"))
  }

  test("simple trees") {
    expect("(,,(,))")(parse("(,,(,));"))
    expect("(A,B,(C,D))")(parse("(A,B,(C,D));"))
    expect("F(A,B,E(C,D))")(parse("(A,B,(C,D)E)F;"))
    expect("(:0.1,:0.2,:0.5(:0.3,:0.4))")(parse("(:0.1,:0.2,(:0.3,:0.4):0.5);"))
    expect("(:0.1,:0.2,:0.5(:0.3,:0.4))")(parse("(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;"))
    expect("(A:0.1,B:0.2,:0.5(C:0.3,D:0.4))")(parse("(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"))
    expect("F(A:0.1,B:0.2,E:0.5(C:0.3,D:0.4))")(parse("(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;"))
    expect("A(F:0.1(B:0.2,E:0.5(C:0.3,D:0.4)))")(parse("((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;"))
  }

  test("names unquoted") {
    expect("Name")(parse("Name;"))
    expect("Name_1")(parse("Name_1;"))
    expect("Name:0.1")(parse("Name:0.1;"))
  }

  test("names quoted") {
    expect("Name[(,")(parse("'Name[(,';"))
    expect("My Name's")(parse("'My Name''s';"))
    expect("Name:0.1:0.2")(parse("'Name:0.1':0.2;"))
    expect("Name with blanks(Name(),Name,name)")(parse("('Name()','Name,name')'Name with blanks';"))
  }

  test("branch lengths") {
    expect("A:1.0")(parse("A:1;"))
    expect("A:0.1")(parse("A:0.1;"))
    expect("A:-0.1")(parse("A:-0.1;"))
    expect("A:0.001")(parse("A:1.e-3;"))
    expect("A:-1000.0")(parse("A:-1.e+3;"))
    expect("A:-0.102")(parse("A:-1.02e-01;"))
  }

  test("comments") {
    expect("C:0.3(A:0.1,B:0.2)")(parse("(A:0.1, [comment] B:0.2)C:0.3;"))
    expect("(A:0.1,B:0.2)")(parse("(A:0.1, [comment] B:0.2) [C:0.3];"))
    expect("(A:0.1,B:0.2)")(parse("(A:0.1, [comment,;(] B:0.2) [C:0.3];"))
  }

  test("complex trees") {
    expect("the root:5.0(a,:-1230.0( Names's and (b's)) ,c))")(parse("(a , (' Names''s and (b''s)) ',c) :-1.23e3) 'the root':5;"))
  }
}


class NewickParser2Test extends FunSuite  {
  val parser = new NewickParser2(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("names quoted") {
    expect("Name[(,")(parse("'Name[(,';"))
    expect("Name[1](2):0.2")(parse("'Name[1](2)'[:0.1]:0.2;"))
  }

  test("nested comments") {
    expect("C:0.3(A(1)[2]:0.1,B:0.2)")(parse("('A(1)[2]':0.1, [comment [with in] ] B:0.2)C:0.3;"))
  }
}


class TreeParserTest extends FunSuite {
  (new SimpleParserTest).execute()
  (new NewickParserTest).execute()
  (new NewickParser2Test).execute()
}

