use uplc/constant.{Constant}
use uplc/term.{Term}

pub type Env =
  List<Value>

pub type Context {
  FrameAwaitArg(Value, Context)
  FrameAwaitFunTerm(Env, Term, Context)
  FrameAwaitFunValue(Value, Context)
  FrameForce(Context)
  FrameConstr(Env, Int, List<Term>, List<Value>, Context)
  FrameCases(Env, List<Term>, Context)
  NoFrame
}

pub type Value {
  VCon(Constant)
  VDelay(Term, Env)
  VLambda { body: Term, env: Env }
  // don't need to hold builtin state
  // this is literally serializable
  VBuiltin { fun: Int, force_count: Int, args_count: Int, args: List<Value> }
  VConstr { tag: Int, fields: List<Value> }
}
