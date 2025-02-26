use aiken/builtin
use uplc/builtins.{default_arg_counts, default_force_counts}
use uplc/constant.{Constant}
use uplc/tweaked_term.{
  Apply, Blake2b_256, Builtin, Delay, Error, Force, Hash, Lambda, TConstant,
  Term, TermHash, Var, hashing_algo,
}

pub type EnvHash =
  Hash<Blake2b_256, Env>

pub type Env {
  Node { value: ValueHash, next: EnvHash }
  Empty
}

pub type ContextHash =
  Hash<Blake2b_256, Context>

pub type Context {
  FrameAwaitArg(Value, ContextHash)
  FrameAwaitFunTerm(Env, TermHash, ContextHash)
  FrameAwaitFunValue(Value, ContextHash)
  FrameForce(ContextHash)
  FrameConstr(Env, Int, List<TermHash>, List<Value>, ContextHash)
  FrameCases(Env, List<TermHash>, ContextHash)
  NoFrame
}

pub type ValueHash =
  Hash<Blake2b_256, Value>

pub type Value {
  VCon(Constant)
  VDelay(TermHash, Env)
  VLambda { body: TermHash, env: Env }
  // don't need to hold builtin state
  // this is literally serializable
  VBuiltin {
    fun: Int,
    force_count: Int,
    args_count: Int,
    args: List<ValueHash>,
  }
  VConstr { tag: Int, fields: List<ValueHash> }
}

pub type MachineState {
  Compute(Context, Env, TermHash)
  Return(Context, ValueHash)
  Done(TermHash)
  IteratingEnv(Context, EnvHash, Int)
  ErrorState(ByteArray)
}

fn index_env(ctx: Context, env: Env, index: Int) -> MachineState {
  when env is {
    Empty -> ErrorState("Free variable")
    Node { value, next } ->
      if index == 1 {
        Return(ctx, value)
      } else {
        IteratingEnv(ctx, next, index - 1)
      }
  }
}

pub fn iterate_next_env(
  ctx: Context,
  env_hash: EnvHash,
  env: Env,
  index: Int,
) -> MachineState {
  expect env_hash == ( env |> builtin.serialise_data |> hashing_algo )

  index_env(ctx, env, index)
}

// node operator posts merkle root
// - L2 utxo set
// - L2 transactions that are included
// - L2 computation merkle tree root

// pub type MachineState {
//   Compute(Context, Env, TermHash)
//   Return(Context, ValueHash)
//   Done(TermHash)
//   IteratingEnv(Context, Env, Int)
//   ErrorState(ByteArray)
// }

// hashing_algo(state: MachineState + expected_state:MachineState) = leaf of the computation merkle tree

test thing() {
  let term = Error

  let state =
    Compute(NoFrame, Empty, term |> builtin.serialise_data |> hashing_algo)

  let expected_state =
    Return(
      NoFrame,
      VDelay(#"", Empty) |> builtin.serialise_data |> hashing_algo,
    )

  let next_state =
    when state is {
      Compute(ctx, env, term_hash) -> compute(ctx, env, term_hash, term)

      _ -> fail @"rest"
    }

  next_state != expected_state
}

test thing2() fail {
  let term = Error

  let state =
    Compute(NoFrame, Empty, term |> builtin.serialise_data |> hashing_algo)

  let expected_state = ErrorState("Evaluation Failure")

  let next_state =
    when state is {
      Compute(ctx, env, term_hash) -> compute(ctx, env, term_hash, term)

      _ -> fail @"rest"
    }

  next_state != expected_state
}

pub fn compute(
  ctx: Context,
  env: Env,
  term_hash: TermHash,
  term: Term,
) -> MachineState {
  expect term_hash == ( term |> builtin.serialise_data |> hashing_algo )

  when term is {
    Var(index) -> index_env(ctx, env, index)

    Delay(inner_hash) ->
      Return(
        ctx,
        VDelay(inner_hash, env) |> builtin.serialise_data |> hashing_algo,
      )
    Force(inner_hash) -> {
      let ctx = ctx |> builtin.serialise_data |> hashing_algo |> FrameForce

      Compute(ctx, env, inner_hash)
    }
    Lambda { body: body_hash } ->
      Return(
        ctx,
        VLambda(body_hash, env) |> builtin.serialise_data |> hashing_algo,
      )
    Apply { function, argument } ->
      ctx
        |> builtin.serialise_data
        |> hashing_algo
        |> fn(a) { FrameAwaitFunTerm(env, argument, a) }
        |> Compute(env, function)
    TConstant(constant) ->
      Return(ctx, VCon(constant) |> builtin.serialise_data |> hashing_algo)

    Error -> ErrorState("Evaluation Failure")

    Builtin(fun) -> {
      let force_count = builtin.index_bytearray(default_force_counts, fun)

      let args_count = builtin.index_bytearray(default_arg_counts, fun)

      Return(
        ctx,
        VBuiltin { fun, force_count, args_count, args: [] }
          |> builtin.serialise_data
          |> hashing_algo,
      )
    }

    _ -> fail
  }
}
