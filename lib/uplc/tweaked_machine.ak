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
  FrameConstr(Env, Int, List<TermHash>, List<ValueHash>, ContextHash)
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

pub fn return(
  ctx: Context,
  value_hash: ValueHash,
  value: Value,
  inner_context: Option<Context>,
) -> MachineState {
  expect value_hash == ( value |> builtin.serialise_data |> hashing_algo )

  when ctx is {
    FrameAwaitArg(function, ctx_hash) ->
      apply_eval(ctx_hash, function, value_hash, inner_context)
    FrameAwaitFunTerm(arg_env, arg_hash, ctx_hash) -> {
      let frame = FrameAwaitArg(value, ctx_hash)
      Compute(frame, arg_env, arg_hash)
    }
    FrameAwaitFunValue(arg, ctx_hash) -> {
      let arg_hash = arg |> builtin.serialise_data |> hashing_algo
      apply_eval(ctx_hash, value, arg_hash, inner_context)
    }
    FrameForce(ctx_hash) ->
      force_eval(ctx_hash, value_hash, value, inner_context)
    NoFrame ->
      Done(discharge_value(value) |> builtin.serialise_data |> hashing_algo)
    FrameConstr(env, tag, fields, eval_fields, ctx_hash) -> {
      // FrameConstr(Env, Int, List<TermHash>, List<Value>, ContextHash)
      let done = [value_hash, ..eval_fields]

      when fields is {
        [] -> {
          expect Some(inner_context) = inner_context
          let constr_value = VConstr { tag, fields: done }
          let constr_hash =
            constr_value |> builtin.serialise_data |> hashing_algo
          Return(inner_context, constr_hash)
        }
        [next_hash, ..rest] ->
          Compute(FrameConstr(env, tag, rest, done, ctx_hash), env, next_hash)
      }
    }
    FrameCases(env, cs, ctx_hash) ->
      when value is {
        VConstr { tag, fields } -> {
          let branch_hash = cs |> list_at(tag)
          // Use the new iterative approach instead of recursive transfer_fields
          expect Some(ctx) = inner_context
          expect ctx_hash == ( ctx |> builtin.serialise_data |> hashing_algo )
          // Can we use a Return instead of IteratingFields?
          if builtin.null_list(fields) {
            // No fields to transfer
            Compute(ctx, env, branch_hash)
          } else {
            fail @"Not yet implemented"
          }
        }
        _ -> ErrorState("Not a constr")
      }
  }
}

fn force_eval(
  ctx_hash: ContextHash,
  value_hash: ValueHash,
  value: Value,
  ctx: Option<Context>,
) -> MachineState {
  expect Some(ctx) = ctx
  expect ctx_hash == ( ctx |> builtin.serialise_data |> hashing_algo )
  expect value_hash == ( ctx |> builtin.serialise_data |> hashing_algo )
  when value is {
    VDelay(body_hash, env) -> Compute(ctx, env, body_hash)
    VBuiltin { fun, force_count, args_count, args } ->
      if force_count > 0 {
        let new_value =
          VBuiltin { fun, force_count: force_count - 1, args_count, args }
        let new_hash = new_value |> builtin.serialise_data |> hashing_algo
        Return(ctx, new_hash)
      } else {
        ErrorState("builtin term argument expected")
      }
    _ -> ErrorState("nonpolymorphic instantiation")
  }
}

fn apply_eval(
  ctx_hash: ContextHash,
  function: Value,
  argument: ValueHash,
  ctx: Option<Context>,
) -> MachineState {
  expect Some(ctx) = ctx
  expect ctx_hash == ( ctx |> builtin.serialise_data |> hashing_algo )
  when function is {
    VLambda { body: body_hash, env } -> {
      let new_env =
        Node {
          value: argument,
          next: env |> builtin.serialise_data |> hashing_algo,
        }
      Compute(ctx, new_env, body_hash)
    }
    VBuiltin { fun, force_count, args_count, args } ->
      if force_count == 0 {
        let result = eval_builtin(fun, args_count, args, argument)
        Return(ctx, result)
      } else {
        ErrorState("Unexpected Builtin Term Argument")
      }
    _ -> ErrorState("Not a function")
  }
}

fn eval_builtin(
  fun: Int,
  args_count: Int,
  args: List<ValueHash>,
  next_arg: ValueHash,
) -> ValueHash {
  if args_count == 0 {
    fail @"Impossible"
  } else if args_count == 1 {
    // TODO: Implement call_builtin for the tweaked machine
    // This is a placeholder - actual implementation would need to handle builtins
    fail @"call_builtin not implemented"
  } else {
    VBuiltin {
      fun,
      force_count: 0,
      args_count: args_count - 1,
      args: [next_arg, ..args],
    }
      |> builtin.serialise_data
      |> hashing_algo
  }
}

fn list_at(list: List<a>, index: Int) -> a {
  if index == 0 {
    list |> builtin.head_list
  } else {
    list |> builtin.tail_list |> list_at(index - 1)
  }
}

fn discharge_value(value: Value) -> Term {
  // This is a placeholder - actual implementation would need to convert Value to Term
  // Similar to the implementation in machine.ak but adapted for the tweaked machine
  fail @"discharge_value not implemented"
}
