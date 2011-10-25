package interpreter

object L2 {
	def testSum() {
		val ex: Expr = Sum(Sum(N(5), N(10)), Sum(N(10), N(100)))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de (5 + 10) + (10 + 100): " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testProd() {
		val ex: Expr = Prod(Prod(N(5), N(10)), N(2))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de (5 * 10) * 2: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testDif() {
		val ex: Expr = Dif(Dif(N(100), N(10)), Dif(N(5), N(1)))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de (100 - 10) - (5 - 1): " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testEq() {
		val ex: Expr = Eq(N(5), N(5))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de 5 = 5: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testIf() {
		val ex: Expr = If(Eq(N(5), N(6)), Sum(N(5), N(5)), Dif(N(6), N(5)))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de if 5 == 6 then 5 + 5 else 6 - 5: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testSeq() {
		val ex: Expr = Seq(Skip(), Seq(Skip(), Sum(N(5), N(3))))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avaliacao de skip; (skip; 5 + 2): " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testAsg() {
		val ex: Expr = Asg(X("l1"), Sum(N(5), N(100)))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avaliacao de l1 := 5 + 100: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testDeref() {
		val ex: Expr = Deref(X("l1"))
		val sigma: List[(String, Int)] = List(("l1", 5), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avaliacao de ! l1: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
 
	def testWhile() {
		val ex: Expr = W(Eq(N(7),Deref(X("l1"))), Asg(X("l1"),Sum(Deref(X("l1")), N(1))))
		val sigma: List[(String, Int)] = List(("l1", 1), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		var interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia�‹o de while (7>=!x) do x=!x+1: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testApp() {
		val ex: Expr = App(Fn("x", Inteiro(), Sum(X("x"), N(1))), N(5))
		val sigma: List[(String, Int)] = List(("l1", 1), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		val interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia��o de (fn x:int = x + 1) 5: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
	def testLet() {
		val ex: Expr = Let("x", Inteiro(), N(5), Eq(X("x"), N(3)))
		val sigma: List[(String, Int)] = List(("l1", 1), ("l2", 7))
		val gamma: List[(String, Tipo)] = List(("x", Inteiro()), ("y", Inteiro()))
		
		val interpretador = new Interpreter()
		
		val tipo = interpretador.typecheck(ex, gamma)
		
		val res = interpretador.eval(ex, sigma)
		
		println()
		println("Expressao L3: " + ex)
		println()
		println("Tipo: " + tipo)
		res match {
			case Some((exp_final, sigma_final)) =>
				println("Resultado da avalia��o de let x:int = 5 in x >= 3: " + exp_final)
				println(sigma_final)
			case None => None
		}
	}
 
	def main(args: Array[String]) {
		//testSum()
		//testProd()
		//testDif()
		//testEq()
		//testIf()
		//testSeq()
		//testAsg()
		//testDeref()
		//testWhile()
		testApp()
		//testLet()
	}
}
