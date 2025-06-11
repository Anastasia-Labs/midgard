use uplc/constant.{Constant}

pub type DebruijnIndex =
  Int

pub type Term {
  Var(DebruijnIndex)

  Delay(Term)
  // param is excluded since it's always 0
  Lambda { body: Term }

  Apply { function: Term, argument: Term }

  TConstant(Constant)

  Force(Term)

  Error

  Builtin(Int)

  TConstr { tag: Int, fields: List<Term> }

  Case { constr: Term, branches: List<Term> }
}
