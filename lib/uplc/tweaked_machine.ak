use aiken/builtin
use uplc/builtins.{default_arg_counts, default_force_counts}
use uplc/constant.{Constant, Integer, Unit}
use uplc/tweaked_term.{
  Apply, Blake2b_256, Builtin, Case, Delay, Error, Force, Hash, Lambda,
  TConstant, TConstr, Term, TermHash, Var, hashing_algo,
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
  FrameAwaitFunValue(ValueHash, ContextHash)
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
  IteratingEnv(Context, EnvHash, Int)
  CallingBuiltin(Context, Int, List<ValueHash>)
  Done(Term)
  ErrorState(ByteArray)
}

fn index_env(ctx: Context, env: Env, index: Int) -> MachineState {
  when env is {
    Empty -> ErrorState("Free Var")
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
  expect env_hash == ( env |> serialize_hash )

  index_env(ctx, env, index)
}

pub fn compute(
  ctx: Context,
  env: Env,
  term_hash: TermHash,
  term: Term,
) -> MachineState {
  expect term_hash == ( term |> serialize_hash )

  when term is {
    Var(index) -> index_env(ctx, env, index)

    Delay(inner_hash) -> Return(ctx, VDelay(inner_hash, env) |> serialize_hash)

    Force(inner_hash) ->
      ctx
        |> serialize_hash
        |> FrameForce
        |> Compute(env, inner_hash)

    Lambda { body: body_hash } ->
      Return(ctx, VLambda(body_hash, env) |> serialize_hash)

    Apply { function, argument } ->
      ctx
        |> serialize_hash
        |> fn(a) { FrameAwaitFunTerm(env, argument, a) }
        |> Compute(env, function)

    TConstant(constant) -> Return(ctx, VCon(constant) |> serialize_hash)

    Error -> ErrorState("Eval Fail")

    Builtin(fun) -> {
      let force_count = builtin.index_bytearray(default_force_counts, fun)

      let args_count = builtin.index_bytearray(default_arg_counts, fun)

      Return(
        ctx,
        VBuiltin { fun, force_count, args_count, args: [] }
          |> serialize_hash,
      )
    }
    TConstr { tag, fields } ->
      when fields is {
        [] -> Return(ctx, VConstr(tag, []) |> serialize_hash)

        [t, ..rest] ->
          FrameConstr(
            env,
            tag,
            rest,
            [],
            ctx
              |> serialize_hash,
          )
            |> Compute(env, t)
      }
    Case { constr, branches } ->
      FrameCases(env, branches, ctx |> serialize_hash)
        |> Compute(env, constr)
  }
}

pub fn return(
  ctx: Context,
  value_hash: ValueHash,
  value: Value,
  inner_context: Context,
) -> MachineState {
  expect value_hash == ( value |> serialize_hash )

  let frame_handler =
    fn(callback) {
      when ctx is {
        FrameAwaitArg(function, ctx_hash) ->
          callback(apply_eval(inner_context, function, value_hash), ctx_hash)

        FrameAwaitFunTerm(arg_env, arg_hash, ctx_hash) ->
          // No callback needed since we don't use inner_context
          FrameAwaitArg(value, ctx_hash) |> Compute(arg_env, arg_hash)

        FrameAwaitFunValue(arg_hash, ctx_hash) ->
          callback(apply_eval(inner_context, value, arg_hash), ctx_hash)

        FrameForce(ctx_hash) ->
          callback(force_eval(inner_context, value), ctx_hash)

        NoFrame ->
          // No callback needed since we don't use inner_context
          // We can do something special here and just compare value to unit
          // since we should never reach Done for anything other than unit
          discharge_value(value)

        FrameConstr(env, tag, fields, eval_fields, ctx_hash) -> {
          // FrameConstr(Env, Int, List<TermHash>, List<Value>, ContextHash)
          let done = [value_hash, ..eval_fields]

          when fields is {
            [] -> {
              let constr_hash =
                VConstr { tag, fields: done }
                  |> serialize_hash

              callback(Return(inner_context, constr_hash), ctx_hash)
            }
            [next_hash, ..rest] ->
              // No callback needed since we don't use inner_context
              Compute(
                FrameConstr(env, tag, rest, done, ctx_hash),
                env,
                next_hash,
              )
          }
        }
        FrameCases(env, cs, ctx_hash) ->
          when value is {
            VConstr { tag, fields } ->
              when fields is {
                [] -> {
                  let branch_hash = cs |> list_at(tag)

                  callback(Compute(inner_context, env, branch_hash), ctx_hash)
                }

                [x, ..rest] -> {
                  let arg_frame_hash =
                    FrameAwaitFunValue(x, ctx_hash)
                      |> serialize_hash

                  let remaining_constr =
                    VConstr { tag, fields: rest }
                      |> serialize_hash

                  FrameCases(env, cs, arg_frame_hash)
                    |> Return(remaining_constr)
                }
              }

            _ -> ErrorState("Not a constr")
          }
      }
    }

  let state, ctx_hash <- frame_handler()

  expect ctx_hash == ( inner_context |> serialize_hash )

  state
}

fn force_eval(ctx: Context, function: Value) -> MachineState {
  when function is {
    VDelay(body_hash, env) -> Compute(ctx, env, body_hash)
    VBuiltin { fun, force_count, args_count, args } ->
      if force_count > 0 {
        Return(
          ctx,
          VBuiltin { fun, force_count: force_count - 1, args_count, args }
            |> serialize_hash,
        )
      } else {
        ErrorState("builtin term argument expected")
      }
    _ -> ErrorState("nonpolymorphic instantiation")
  }
}

fn apply_eval(
  ctx: Context,
  function: Value,
  arg_hash: ValueHash,
) -> MachineState {
  when function is {
    VLambda { body: body_hash, env } ->
      Compute(
        ctx,
        Node { value: arg_hash, next: env |> serialize_hash },
        body_hash,
      )
    VBuiltin { fun, force_count: 0, args_count, args } ->
      eval_builtin(ctx, fun, args_count, args, arg_hash)

    VBuiltin { .. } -> ErrorState("Unexpected Builtin Term Argument")
    _ -> ErrorState("Not a function")
  }
}

fn eval_builtin(
  ctx: Context,
  fun: Int,
  args_count: Int,
  args: List<ValueHash>,
  next_arg: ValueHash,
) -> MachineState {
  when args_count is {
    0 -> fail @"Impossible"
    1 -> CallingBuiltin(ctx, fun, [next_arg, ..args])
    _ ->
      Return(
        ctx,
        VBuiltin {
          fun,
          force_count: 0,
          args_count: args_count - 1,
          args: [next_arg, ..args],
        }
          |> serialize_hash,
      )
  }
}

fn list_at(list: List<a>, index: Int) -> a {
  if index == 0 {
    list |> builtin.head_list
  } else {
    list |> builtin.tail_list |> list_at(index - 1)
  }
}

fn serialize_hash(data: Data) -> ByteArray {
  data |> serialize_hash
}

pub fn call_builtin(
  ctx: Context,
  fun: Int,
  arg_hashes: List<ValueHash>,
  args: List<Value>,
) -> MachineState {
  // Remember args are applied in reverse order
  // So the last argument is at the front of the list
  // TODO: Use callback to reduce redundancy
  //
  let function_call =
    fn(callback) {
      if fun < 43 {
        when fun is {
          // AddInteger
          0 ->
            when args is {
              [VCon(Integer(b)) as second, VCon(Integer(a)) as first] -> {
                let return_val_bytes =
                  VCon(Integer(a + b)) |> builtin.serialise_data

                callback(
                  Return(ctx, return_val_bytes |> hashing_algo),
                  [second |> serialize_hash, first |> serialize_hash],
                  return_val_bytes,
                )
              }

              _ -> ErrorState("Bad Args: 0")
            }
          // SubtractInteger
          1 ->
            when args is {
              [VCon(Integer(b)) as second, VCon(Integer(a)) as first] -> {
                let return_val_bytes =
                  VCon(Integer(a - b)) |> builtin.serialise_data

                callback(
                  Return(ctx, return_val_bytes |> hashing_algo),
                  [second |> serialize_hash, first |> serialize_hash],
                  return_val_bytes,
                )
              }

              _ -> ErrorState("Bad Args: 1")
            }
          // Case for negative builtins
          _ -> ErrorState("No Builtin Found")
        }
      } else {
        fail @"TODO"
      }
    }

  let state, actual_arg_hashes, result_bytes <- function_call()

  expect actual_arg_hashes == arg_hashes

  // like a little less than 4x tx size
  expect builtin.length_of_bytearray(result_bytes) <= 64000

  state
}

fn discharge_value(value: Value) -> MachineState {
  if value == VCon(Unit) {
    Done(TConstant(Unit))
  } else {
    ErrorState("non-unit termination")
  }
}
