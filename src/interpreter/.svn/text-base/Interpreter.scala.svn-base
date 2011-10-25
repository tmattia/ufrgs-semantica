package interpreter

class Interpreter {
	def typecheck(e: Expr, gamma: List[(String, Tipo)]): Option[Tipo] = e match {
		case N(_) => Some(Inteiro())
		case B(_) => Some(Booleano())
		case Sum(e1, e2) =>
			(typecheck(e1, gamma), typecheck(e2, gamma)) match {
				case (Some(Inteiro()), Some(Inteiro())) => Some(Inteiro())
				case _ => None
			}
		case Prod(e1, e2) =>
			(typecheck(e1, gamma), typecheck(e2, gamma)) match {
				case (Some(Inteiro()), Some(Inteiro())) => Some(Inteiro())
				case _ => None
			}
		case Dif(e1, e2) =>
			(typecheck(e1, gamma), typecheck(e2, gamma)) match {
				case (Some(Inteiro()), Some(Inteiro())) => Some(Inteiro())
				case _ => None
			}
		case Eq(e1, e2) =>
			(typecheck(e1, gamma), typecheck(e2, gamma)) match {
				case (Some(Inteiro()), Some(Inteiro())) => Some(Booleano())
				case _ => None
			}
		case If(e1, e2, e3) =>
			(typecheck(e1, gamma), typecheck(e2, gamma), typecheck(e3, gamma)) match {
				case (Some(Booleano()), Some(Inteiro()), Some(Inteiro())) => Some(Inteiro())
				case (Some(Booleano()), Some(Booleano()), Some(Booleano())) => Some(Booleano())
				case (Some(Booleano()), Some(Unidade()), Some(Unidade())) => Some(Unidade())
				case (Some(Booleano()), Some(Funcao(t11, t12)), Some(Funcao(t21, t22))) => 
					if(t11 == t21 && t12 == t22) {
						Some(Funcao(t11, t12))
					} else {
						None
					}
				case (Some(Booleano()), Some(Refer(t1)), Some(Refer(t2))) => 
					if(t1 == t2) {
						Some(Refer(t1))
					} else {
						None
					}
				case _ => None
			}
		case Asg(e1, e2) =>
			(typecheck(e1, gamma), typecheck(e2, gamma)) match {
				case (Some(Refer(Inteiro())), Some(Inteiro())) => Some(Unidade())
				case _ => None
			}
		case Deref(e: Expr) =>   //!l
		  (typecheck(e, gamma)) match{
		    case Some((Refer(Inteiro()))) => Some(Inteiro())
		    case _ => None
		}
		case Skip() => Some(Unidade())
		case Seq(e1, e2) => 
		    (typecheck(e1, gamma)) match {
		        case (Some(Unidade())) => typecheck(e2, gamma)
		        case _ => None
		    }
		case W(e1, e2) =>
		    (typecheck(e1, gamma), typecheck(e2, gamma)) match {
		        case (Some(Booleano()), Some(Unidade())) => Some(Unidade())
		        case _ => None
		    }
		case Fn(s: String, t: Tipo, e) =>
			typecheck(e, gamma) match {
				case Some(Booleano()) => Some(Funcao(t, Booleano()))
				case Some(Inteiro()) => Some(Funcao(t, Inteiro()))
				case Some(Unidade()) => Some(Funcao(t, Unidade()))
				case Some(Funcao(t1, t2)) => Some(Funcao(t, Funcao(t1, t2)))
				case Some(Refer(t1)) => Some(Funcao(t, Refer(t1)))
				case _ => None
			}
		case App(e1, e2) =>
			typecheck(e1, gamma) match {
				case Some(Funcao(t1, t2)) =>
					if(Some(t1) == typecheck(e2, gamma)) {
						Some(t2)
					} else {
						None
					}
				case _ => None
			}
		case X(s: String) =>
			if(gamma.filter{i => i._1 == s} != List()) {
				Some(gamma.filter{i => i._1 == s}.head._2)
			} else {
				Some(Refer(Inteiro()))
			}
		case Let(s: String, t: Tipo, e1, e2) =>
			if(typecheck(e1, gamma) == Some(t)) {
				typecheck(e2, gamma)
			} else {
				None
			}
		//case LetRec(f: Tipo, e1, e2) =>
		case _ => None
	}
	
	def isvalue(e: Expr): Boolean = e match {
		case N(_) => true
		case X(_) => true
		case B(_) => true
		case Fn(_, _, _) => true
		case Skip() => true
		case _ => false
	}
	
	type Endereco = String
	
	type Memoria = List[(Endereco, Int)]
	
	def step(e: Expr, sigma: Memoria): Option[(Expr, Memoria)] = e match {
		case N(_) => None
		case B(_) => None
		case X(_) => None
		case Sum(e1, e2) =>
			(e1, e2) match {
				case (N(n1), N(n2)) => Some((N(n1 + n2), sigma))
				case (e1, e2) =>
					if (isvalue(e1)) {
						step(e2, sigma) match {
							case Some((e2lin, sigmalin)) => Some((Sum(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						step(e1, sigma) match {
							case Some((e1lin, sigmalin)) => Some((Sum(e1lin, e2), sigmalin))
							case None => None
						}
					}
			}
		case Prod(e1, e2) =>
			(e1, e2) match {
				case (N(n1), N(n2)) => Some((N(n1 * n2), sigma))
				case (e1, e2) =>
					if (isvalue(e1)) {
						step(e2, sigma) match {
							case Some((e2lin, sigmalin)) => Some((Prod(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						step(e1, sigma) match {
							case Some((e1lin, sigmalin)) => Some((Prod(e1lin, e2), sigmalin))
							case None => None
						}
					}
			}
		case Dif(e1, e2) =>
			(e1, e2) match {
				case (N(n1), N(n2)) => Some((N(n1 - n2), sigma))
				case (e1, e2) =>
					if (isvalue(e1)) {
						step(e2, sigma) match {
							case Some((e2lin, sigmalin)) => Some((Dif(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						step(e1, sigma) match {
							case Some((e1lin, sigmalin)) => Some((Dif(e1lin, e2), sigmalin))
							case None => None
						}
					}
			}
		case Eq(e1, e2) =>
			(e1, e2) match {
				case (N(n1), N(n2)) => Some((B(n1 >= n2), sigma))
				case (e1, e2) =>
					if (isvalue(e1)) {
						step(e2, sigma) match {
							case Some((e2lin, sigmalin)) => Some((Eq(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						step(e1, sigma) match {
							case Some((e1lin, sigmalin)) => Some((Eq(e1lin, e2), sigmalin))
							case None => None
						}
					}
			}
		case If(e1, e2, e3) =>
			(e1, e2, e3) match {
				case (B(b), e2, e3) =>
					if(b) {
						if (isvalue(e2)) {
							Some((e2, sigma))
						} else {
							step(e2, sigma)
						}
					} else {
						if (isvalue(e3)) {
							Some((e3, sigma))
						} else {
							step(e3, sigma)
						}
					}
				case (e1, e2, e3) => step(e1, sigma) match {
					case Some((e1lin, sigmalin)) => Some((If(e1lin, e2, e3), sigmalin))
					case None => None
				}
			}
		case Asg(e1, e2) =>
			(e1, e2) match {
				case (X(s), N(n)) =>
					Some(Skip(), _memAsg(sigma, s, n))
				case (e1, e2) => 
					if(isvalue(e1)) {
						step(e2, sigma) match {
							case Some((e2lin, sigmalin)) => Some((Asg(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						step(e1, sigma) match {
							case Some((e1lin, sigmalin)) => Some((Asg(e1lin, e2), sigmalin))
							case None => None
						}
					}
			}
		case Deref(X(s)) =>
			if(_memExists(sigma, s)) {
				Some(N(_memDeref(sigma, s)), sigma)
			} else {
				None
			}
		case Skip() => None
		case Seq(e1, e2) => (e1, e2) match {
				case (Skip(), e2) => step(e2, sigma)
				case (e1, e2) => step(e1, sigma) match {
					case Some((e1lin, sigmalin)) => Some((Seq(e1lin, e2), sigmalin))
					case None => None
				}
			}
		case W(e1, e2) =>
			step(If(e1, Seq(e2,W(e1,e2)), Skip()),sigma)	
		case Fn(s: String, t: Tipo, e) => None
		case App(e1, e2) =>
			(e1, e2) match {
				case (Fn(s, t, e), v) =>
					if(!isvalue(v)) {
						step(v, sigma) match {
							case Some((e2lin, sigmalin)) => Some((App(e1, e2lin), sigmalin))
							case None => None
						}
					} else {
						_replace(s, v, e, sigma)
					}
				case (e1, e2) =>
					step(e1, sigma) match {
						case Some((e1lin, sigmalin)) => Some((App(e1lin, e2), sigmalin))
						case None => None
					}
			}
		case Let(s: String, t: Tipo, e1, e2) => None
		//case LetRec(f: Tipo, e1, e2) =>
	}
	def eval(e: Expr, sigma: Memoria): Option[(Expr, Memoria)] = step(e, sigma) match {
		case None => Some((e, sigma))
		case Some((elin, sigmalin)) => eval(elin, sigmalin)
	}
	
	def _replace(s: String, v: Expr, e: Expr, sigma: Memoria): Option[(Expr, Memoria)] =
		e match {
			case Sum(x1, x2) =>
				if(x1 == X(s)) {
					Some((Sum(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Sum(x1, v), sigma))
				} else {
					None
				}
			case Prod(x1, x2) =>
				if(x1 == X(s)) {
					Some((Prod(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Prod(x1, v), sigma))
				} else {
					None
				}
			case Dif(x1, x2) =>
				if(x1 == X(s)) {
					Some((Dif(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Dif(x1, v), sigma))
				} else {
					None
				}
			case Eq(x1, x2) =>
				if(x1 == X(s)) {
					Some((Eq(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Eq(x1, v), sigma))
				} else {
					None
				}
			case If(x1, x2, x3) =>
				if(x1 == X(s)) {
					Some((If(v, x2, x3), sigma))
				} else if(x2 == X(s)) {
					Some((If(x1, v, x3), sigma))
				} else if(x3 == X(s)) {
					Some((If(x1, x2, v), sigma))
				} else {
					None
				}
			case Asg(x1, x2) =>
				if(x1 == X(s)) {
					Some((Asg(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Asg(x1, v), sigma))
				} else {
					None
				}
			case Deref(x1) =>
				if(x1 == X(s)) {
					Some((Deref(v), sigma))
				} else {
					None
				}
			case Seq(x1, x2) =>
				if(x1 == X(s)) {
					Some((Seq(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((Seq(x1, v), sigma))
				} else {
					None
				}
			case W(x1, x2) =>
				if(x1 == X(s)) {
					Some((W(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((W(x1, v), sigma))
				} else {
					None
				}
			case Fn(x1, x2, x3) =>
				if(x3 == X(s)) {
					Some((Fn(x1, x2, v), sigma))
				} else {
					None
				}
			case App(x1, x2) =>
				if(x1 == X(s)) {
					Some((App(v, x2), sigma))
				} else if(x2 == X(s)) {
					Some((App(x1, v), sigma))
				} else {
					None
				}
			case _ => None
		}
	
	def _memExists(sigma: Memoria, s: String): Boolean =
		sigma.filter{i => i._1 == s} != List()
	
	def _memAsg(sigma: Memoria, s: String, n: Int): Memoria =
		if (!_memExists(sigma, s)) {
			sigma ::: List((s, n))
		} else {
			sigma.filter{i => i._1 != s} ::: sigma.filter{i => i._1 == s}.map{i => (i._1, n)}
		}
	
	def _memDeref(sigma: Memoria, s: String): Int =
		sigma.filter{i => i._1 == s}.head._2
}