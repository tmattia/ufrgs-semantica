package interpreter

abstract class Expr
case class N (n:Int) extends Expr
case class B (b:Boolean) extends Expr
case class Sum (e1: Expr, e2: Expr) extends Expr
case class Prod (e1: Expr, e2: Expr) extends Expr
case class Dif (e1: Expr, e2: Expr) extends Expr
case class Eq (e1: Expr, e2: Expr) extends Expr
case class If (e1: Expr, e2: Expr, e3: Expr) extends Expr
case class Asg (e1: Expr, e2: Expr) extends Expr
case class Deref (e: Expr) extends Expr
case class Ref (e:Expr) extends Expr
case class Skip() extends Expr
case class Seq (e1: Expr, e2: Expr) extends Expr
case class W (e1: Expr, e2: Expr) extends Expr
case class Fn (s:String, t: Tipo, e: Expr) extends Expr
case class App (e1: Expr, e2: Expr) extends Expr
case class X (s:String) extends Expr
case class Let (s:String, t: Tipo, e1: Expr, e2: Expr) extends Expr
case class LetRec (f: Tipo, e1: Expr, e2: Expr) extends Expr