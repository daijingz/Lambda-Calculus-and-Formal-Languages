sealed trait STTerm // Inherited general classes STTerm
// Detailed STTerm objects
case class STVar(index: Int) extends STTerm
case class STAbs(type1: STType, term: STTerm) extends STTerm
case class STApp(t1: STTerm, t2: STTerm) extends STTerm
case object STZero extends STTerm
case class STSuc(term: STTerm) extends STTerm
case class STIsZero(term: STTerm) extends STTerm
case object STTrue extends STTerm
case object STFalse extends STTerm
case class STTest(t1: STTerm, t2: STTerm, t3: STTerm) extends STTerm

// Inherited STType classes
sealed trait STType
case object STNat extends STType {
  override def toString() = "nat"
}
case object STBool extends STType {
  override def toString() = "bool"
}
case class STFun(dom: STType, codom: STType) extends STType {
  override def toString() = "(" + dom.toString + ") -> (" + codom.toString + ")"
}

// Inherited ULTerm classes
sealed trait ULTerm
case class ULVar(index: Int) extends ULTerm {
  override def toString() = index.toString()
}
case class ULAbs(t: ULTerm) extends ULTerm {
  override def toString() = "lambda . " + t.toString()
}
case class ULApp(t1: ULTerm, t2: ULTerm) extends ULTerm {
  override def toString() = "(" + t1.toString() + ") (" + t2.toString() + ")"
}

object a2{
  def typecheck(Input: STTerm): Boolean ={ // Return boolean
    Input match {
      // Independent Free Variables cannot be type-checked
      case STVar(_) => false
      // If right part term contains all variables then it fulfils typing rules
      case STAbs(_, term) if filled_Var(term) => true
      case STAbs(type1, term) => type1 == typeOf(term, List()) && typecheck(term)
      case STApp(term1: STAbs, term2) => term1.type1 == typeOf(term2, List()) && typecheck(term1) && typecheck(term2)
      // If STApp left part is not abstraction (out-est layer) then it do not fulfils typing rules
      case STApp(_,_) => false
      // STZero (0) layer fulfils typing rules so return true
      case STZero => true
      case STSuc(term) => typeOf(term, List()) == STNat && typecheck(term)
      case STIsZero(term) => typeOf(term, List()) == STNat && typecheck(term)
      case STTrue => true
      case STFalse => true
      case STTest(t1, t2, t3) => typeOf(t1, List()) == STBool && typeOf(t2, List()) == typeOf(t3, List()) &&
        typeOf(t2, List()) == typeOf(Input, List()) && typecheck(t1) && typecheck(t2) && typecheck(t3)
    }
  }

  def typeOf[A](Input: STTerm, List1: List[A]): STType ={
    // Environment should be empty
    if (List1 != List()){
      throw new Exception("Constructing a non-empty environment")
    }
    Input match {
      case STAbs(type1,term) if filled_Var(term) => STFun(type1, type1)
      case STAbs(type1,term) => STFun(type1, typeOf(term, List()))
      case STApp(term1, term2) => STFun(typeOf(term1, List()), typeOf(term2, List()))
      case STZero => STNat
      case STSuc(_) => STNat
      case STIsZero(_) => STBool
      case STTrue => STBool
      case STFalse => STBool
      case STTest(_,t2,_) => typeOf(t2, List())
    }
  }

  def filled_Var(Input: STTerm): Boolean ={
    Input match {
      case STVar(_) => true
      case STAbs(_, term) => filled_Var(term)
      case STApp(term1, term2) => filled_Var(term1) && filled_Var(term2)
      case STZero => false
      case STSuc(term) => filled_Var(term)
      case STIsZero(term) => filled_Var(term)
      case STTrue => false
      case STFalse => false
      case STTest(_, t2, t3) => filled_Var(t2) && filled_Var(t3)
    }
  }

  def eraseTypes(Terms: STTerm): ULTerm ={
    var index = 0
    def eraseTypes_helper(Terms: STTerm, Index: Int): ULTerm ={
      Terms match{
        case STVar(index: Int) => ULVar(index)
        case STAbs(_, term) => ULAbs(eraseTypes_helper(term, index))
        case STApp(term1, term2) => ULApp(eraseTypes_helper(term1, index), eraseTypes_helper(term2, index))
        case STZero => ULAbs(ULAbs(ULVar(0)))
        case STSuc(term) =>
          index = index + 3
          ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(index - 2),ULApp(ULApp(ULVar(index - 1),ULVar(index - 2)),
            ULVar(index - 3)))))), eraseTypes_helper(term, index))
        case STIsZero(term) =>
          index = index + 2
          ULApp(ULAbs(ULApp(ULApp(ULVar(index - 2), ULAbs(ULAbs(ULAbs(ULVar(index - 2))))),
            ULAbs(ULAbs(ULVar(index - 1))))), eraseTypes_helper(term, index))
        case STFalse => ULAbs(ULAbs(ULVar(0)))
        case STTrue => ULAbs(ULAbs(ULVar(1)))
        case STTest(t1,t2,t3) =>
          index = index + 3
          ULApp(ULApp(ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULApp(ULVar(index - 1),ULVar(index - 2)),ULVar(index - 3))))),
            eraseTypes_helper(t1, index)), eraseTypes_helper(t2, index)), eraseTypes_helper(t3, index))
      }
    }
    eraseTypes_helper(Terms, index)
  }

  def evaluation(Terms: STTerm): String ={
    Terms match{
      case STApp(term1, term2) => evaluation(term1) + evaluation(term2)
      case STZero => 0.toString
      case STTrue => true.toString
      case STFalse => false.toString
      case STSuc(term: STTerm) => 1 + evaluation(term)
      case STIsZero(term: STTerm) => (evaluation(term) == evaluation(STZero)).toString
      case STTest(t1, t2, t3) =>
        val comparison = evaluation(t2) == evaluation(t3)
        (comparison && comparison.toString == evaluation(t1)).toString
    }
  }
}