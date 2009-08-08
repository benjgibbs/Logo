package logo

import javax.swing.JPanel
import javax.swing.JFrame
import java.awt.Graphics
import scala.util.parsing.combinator.JavaTokenParsers

class Drawing(points: List[(Int,Int)]) extends JPanel {
  val HEIGHT = 350
  val WIDTH = 350

  var frame = new JFrame();
  frame.getContentPane().add(this,java.awt.BorderLayout.CENTER)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(WIDTH,HEIGHT)
  frame.setVisible(true)
  
  override def paintComponent(g: Graphics)  = {
    var lastPoint : (Int,Int) = null
    for (p <- points.map(p => (p._1+WIDTH/2,p._2+HEIGHT/2))){
      if(lastPoint != null)
        g.drawLine(lastPoint._1, lastPoint._2,p._1,p._2)
      lastPoint = p;
    }
  }
}

object Logo {
  abstract class LogoOp
  case class Home extends LogoOp
  case class Forward(x: Int) extends LogoOp
  case class Turn(x: Int) extends LogoOp
  case class For(i: Int, e: List[LogoOp]) extends LogoOp
  
  implicit def dblToInt(d: Double): Int = 
    if (d  > 0 ) (d +0.5).toInt 
    else (d-0.5).toInt

  def forward(pos: (Int,Int), x: Int, d: Int) : (Int,Int) =
    (pos._1 + x * Math.sin(Math.toRadians(d)), 
      pos._2 + x * Math.cos(Math.toRadians(d)))
 
  def parse(s: String) : List[LogoOp] = LogoParser.parse(s).get
  
  def evaluate(s : String) : List[(Int,Int)]= evaluate(parse(s),(0,0),0); 

  def evaluate(e : List[LogoOp], pos: (Int,Int), heading: Int) 
    : List[(Int,Int)]= e match {
    case Nil => Nil
    case Home() :: _ => (0,0) :: evaluate(e.tail, (0,0),0)
    case Forward(x) :: _ => forward(pos,x,heading) ::
            evaluate(e.tail, forward(pos,x,heading), heading)
    case Turn(x) :: _ => evaluate(e.tail, pos, heading + x)
    case For(0, y) :: _ => evaluate(e.tail, pos, heading)
    case For(x, y) :: tail => evaluate(y ::: For(x-1, y)::e.tail,pos, heading)
    case _ => Nil
  }

  object LogoParser extends JavaTokenParsers {
    def expr : Parser[List[LogoOp]] = rep(home|forward|turn|forexp)
    def home :  Parser[Home]  = "HOME" ^^ (x => Home()) 
    def forward : Parser[Forward] = "FORWARD"~wholeNumber ^^
      {case x~fwd  => Forward(fwd.toInt)} 
    def turn : Parser[Turn] = "TURN"~wholeNumber ^^
      {case x~trn => Turn(trn.toInt)}
    def forexp : Parser[For] = "FOR("~wholeNumber~","~expr~")" ^^
      { case x~i~y~e~z => For(i.toInt,e)}
    
    def parse(text : String) = parseAll(expr, text) 
  }
}

object Main {
  def main(args: Array[String]) = {
    val d = new Drawing(Logo.evaluate(
      if (args.length > 0) args.deepMkString(" ")
      else "HOME FOR(18, TURN 20 FOR(36, FORWARD 10 TURN 10))"));
  }
}

