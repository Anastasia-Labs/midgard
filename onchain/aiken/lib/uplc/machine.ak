use aiken/builtin
use uplc/builtins.{
  add_integer, default_arg_counts, default_force_counts, subtract_integer,
}
use uplc/constant.{Bool, ByteString, Integer, String}
use uplc/term.{
  Apply, Builtin, Case, Delay, Error, Force, Lambda, TConstant, TConstr, Term,
  Var,
}
use uplc/value.{
  Context, Env, FrameAwaitArg, FrameAwaitFunTerm, FrameAwaitFunValue, FrameCases,
  FrameConstr, FrameForce, NoFrame, VBuiltin, VCon, VConstr, VDelay, VLambda,
  Value,
}

fn list_at(list: List<a>, index: Int) -> a {
  if index == 0 {
    list |> builtin.head_list
  } else {
    list |> builtin.tail_list |> list_at(index - 1)
  }
}

pub fn run(term: Term) -> Term {
  compute(NoFrame, [], term)
}

fn compute(ctx: Context, env: Env, term: Term) -> Term {
  when term is {
    Var(name) -> {
      let value = lookup_var(name, env)
      return(ctx, value)
    }
    TConstant(x) -> return(ctx, VCon(x))
    Lambda { body } -> return(ctx, VLambda { body, env })
    Delay(body) -> return(ctx, VDelay(body, env))
    Force(body) -> compute(FrameForce(ctx), env, body)
    Apply { function, argument } ->
      compute(FrameAwaitFunTerm(env, argument, ctx), env, function)
    Error -> fail @"evaluation failure"
    Builtin(fun) -> {
      let force_count = builtin.index_bytearray(default_force_counts, fun)

      let args_count = builtin.index_bytearray(default_arg_counts, fun)

      return(ctx, VBuiltin { fun, force_count, args_count, args: [] })
    }
    TConstr { tag, fields } ->
      when fields is {
        [] -> return(ctx, VConstr(tag, []))
        [t, ..rest] -> compute(FrameConstr(env, tag, rest, [], ctx), env, t)
      }
    Case { constr, branches } ->
      compute(FrameCases(env, branches, ctx), env, constr)
  }
}

fn return(ctx: Context, value: Value) -> Term {
  when ctx is {
    FrameAwaitArg(function, ctx) -> apply_eval(ctx, function, value)
    FrameAwaitFunTerm(arg_var_env, arg, ctx) -> {
      let frame = FrameAwaitArg(value, ctx)
      compute(frame, arg_var_env, arg)
    }
    FrameAwaitFunValue(arg, ctx) -> apply_eval(ctx, value, arg)
    FrameForce(ctx) -> force_eval(ctx, value)
    NoFrame -> {
      let term = discharge_value(value)
      term
    }
    FrameConstr(env, tag, fields, eval_fields, ctx) -> {
      let done = [value, ..eval_fields]

      when fields is {
        [] -> return(ctx, VConstr(tag, done))
        [next, ..rest] ->
          compute(FrameConstr(env, tag, rest, done, ctx), env, next)
      }
    }
    FrameCases(env, cs, ctx) ->
      when value is {
        VConstr { tag, fields } ->
          cs |> list_at(tag) |> compute(transfer_fields(fields, ctx), env, _)
        _ -> fail @"Not a constr"
      }
  }
}

fn transfer_fields(fields: List<Value>, ctx: Context) -> Context {
  when fields is {
    [] -> ctx
    [arg, ..rest] -> transfer_fields(rest, FrameAwaitFunValue(arg, ctx))
  }
}

fn force_eval(ctx: Context, value: Value) -> Term {
  when value is {
    VDelay(body, env) -> compute(ctx, env, body)
    VBuiltin { fun, force_count, args_count, args } ->
      if force_count > 0 {
        //TODO: Should call evalBuiltinApp here for exact semantics
        // but in reality we dont have a builtin that only takes in force
        return(
          ctx,
          VBuiltin { fun, force_count: force_count - 1, args_count, args },
        )
      } else {
        fail @"builtin term argument expected"
      }
    _ -> fail @"nonpolymorphic instantiation"
  }
}

fn apply_eval(ctx: Context, function: Value, argument: Value) -> Term {
  when function is {
    VLambda { body, env } -> compute(ctx, [argument, ..env], body)
    VBuiltin { fun, force_count, args_count, args } ->
      if force_count == 0 {
        return(ctx, eval_builtin(fun, args_count, args, argument))
      } else {
        fail @"Unexpected Builtin Term Argument"
      }
    _ -> fail @""
  }
}

fn eval_builtin(
  fun: Int,
  args_count: Int,
  args: List<Value>,
  next_arg: Value,
) -> Value {
  if args_count == 0 {
    fail @"Impossible"
  } else if args_count == 1 {
    call_builtin(fun, [next_arg, ..args])
  } else {
    VBuiltin {
      fun,
      force_count: 0,
      args_count: args_count - 1,
      args: [next_arg, ..args],
    }
  }
}

fn call_builtin(fun: Int, args: List<Value>) -> Value {
  // Remember args are applied in reverse order
  // So the last argument is at the front of the list
  if fun < 43 {
    when fun is {
      // AddInteger
      0 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] -> VCon(Integer(a + b))
          _ -> fail @"Bad args for AddInteger"
        }
      // SubtractInteger
      1 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] -> VCon(Integer(a - b))
          _ -> fail @"Bad args for SubtractInteger"
        }
      // MultiplyInteger
      2 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] -> VCon(Integer(a * b))
          _ -> fail @"Bad args for MultiplyInteger"
        }
      // DivideInteger
      3 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] -> VCon(Integer(a / b))
          _ -> fail @"Bad args for DivideInteger"
        }
      // QuotientInteger
      4 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] -> VCon(Integer(a % b))
          _ -> fail @"Bad args for QuotientInteger"
        }
      // RemainderInteger
      5 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(Integer(builtin.remainder_integer(a, b)))
          _ -> fail @"Bad args for RemainderInteger"
        }
      // ModInteger
      6 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(Integer(builtin.mod_integer(a, b)))
          _ -> fail @"Bad args for ModInteger"
        }
      // EqualsInteger
      7 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(Bool(builtin.equals_integer(a, b)))
          _ -> fail @"Bad args for EqualsInteger"
        }
      // LessThanInteger
      8 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(Bool(builtin.less_than_integer(a, b)))
          _ -> fail @"Bad args for LessThanInteger"
        }
      // LessThanEqualsInteger
      9 ->
        when args is {
          [VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(Bool(builtin.less_than_equals_integer(a, b)))
          _ -> fail @"Bad args for LessThanEqualsInteger"
        }
      // AppendByteString
      10 ->
        when args is {
          [VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(ByteString(builtin.append_bytearray(a, b)))
          _ -> fail @"Bad args for AppendByteString"
        }
      // ConsByteString
      11 ->
        when args is {
          [VCon(ByteString(b)), VCon(Integer(a))] ->
            VCon(ByteString(builtin.cons_bytearray(a, b)))
          _ -> fail @"Bad args for ConsByteString"
        }
      // SliceByteString
      12 ->
        when args is {
          [VCon(ByteString(c)), VCon(Integer(b)), VCon(Integer(a))] ->
            VCon(ByteString(builtin.slice_bytearray(a, b, c)))
          _ -> fail @"Bad args for SliceByteString"
        }
      // LengthOfByteString
      13 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(Integer(builtin.length_of_bytearray(a)))
          _ -> fail @"Bad args for LengthOfByteString"
        }
      // IndexByteString
      14 ->
        when args is {
          [VCon(Integer(b)), VCon(ByteString(a))] ->
            VCon(Integer(builtin.index_bytearray(a, b)))
          _ -> fail @"Bad args for IndexByteString"
        }
      // EqualsByteString
      15 ->
        when args is {
          [VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.equals_bytearray(a, b)))
          _ -> fail @"Bad args for IndexByteString"
        }
      // LessThanByteString
      16 ->
        when args is {
          [VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.less_than_bytearray(a, b)))
          _ -> fail @"Bad args for LessThanByteString"
        }
      // LessThanEqualsByteString
      17 ->
        when args is {
          [VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.less_than_equals_bytearray(a, b)))
          _ -> fail @"Bad args for LessThanEqualsByteString"
        }
      // Sha2_256
      18 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(ByteString(builtin.sha2_256(a)))
          _ -> fail @"Bad args for Sha2_256"
        }
      // Sha3_256
      19 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(ByteString(builtin.sha3_256(a)))
          _ -> fail @"Bad args for Sha3_256"
        }
      // Blake2b_256
      20 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(ByteString(builtin.blake2b_256(a)))
          _ -> fail @"Bad args for Blake2b_256"
        }
      // Blake2b_224
      21 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(ByteString(builtin.blake2b_224(a)))
          _ -> fail @"Bad args for Blake2b_224"
        }
      // Keccak_256
      22 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(ByteString(builtin.keccak_256(a)))
          _ -> fail @"Bad args for Keccak_256"
        }
      // VerifyEd25519Signature
      23 ->
        when args is {
          [VCon(ByteString(c)), VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.verify_ed25519_signature(a, b, c)))
          _ -> fail @"Bad args for VerifyEd25519Signature"
        }
      // VerifyEcdsaSecp256k1Signature
      24 ->
        when args is {
          [VCon(ByteString(c)), VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.verify_ecdsa_secp256k1_signature(a, b, c)))
          _ -> fail @"Bad args for VerifyEcdsaSecp256k1Signature"
        }
      // VerifySchnorrSecp256k1Signature
      25 ->
        when args is {
          [VCon(ByteString(c)), VCon(ByteString(b)), VCon(ByteString(a))] ->
            VCon(Bool(builtin.verify_schnorr_secp256k1_signature(a, b, c)))
          _ -> fail @"Bad args for VerifySchnorrSecp256k1Signature"
        }
      // AppendString
      26 ->
        when args is {
          [VCon(String(b)), VCon(String(a))] ->
            VCon(String(builtin.append_string(a, b)))
          _ -> fail @"Bad args for AppendString"
        }
      // EqualsString
      27 ->
        when args is {
          [VCon(String(b)), VCon(String(a))] ->
            VCon(Bool(builtin.equals_string(a, b)))
          _ -> fail @"Bad args for EqualsString"
        }
      // EncodeUtf8
      28 ->
        when args is {
          [VCon(String(a))] -> VCon(ByteString(builtin.encode_utf8(a)))
          _ -> fail @"Bad args for EncodeUtf8"
        }
      // DecodeUtf8
      29 ->
        when args is {
          [VCon(ByteString(a))] -> VCon(String(builtin.decode_utf8(a)))
          _ -> fail @"Bad args for DecodeUtf8"
        }
      // Case for negative builtins
      _ -> fail @"Invalid builtin"
    }
  } else {
    fail @"TODO"
  }
}

fn discharge_value(value: Value) -> Term {
  when value is {
    VCon(con) -> TConstant(con)
    VDelay(term, env) -> discharge_term_with_env(0, env, Delay(term))
    VLambda { body, env } -> discharge_term_with_env(0, env, Lambda(body))
    VBuiltin { fun, force_count, args, .. } -> {
      let forces_applied =
        builtin.index_bytearray(default_force_counts, fun) - force_count

      let builtin_forces = discharge_builtin_forces(fun, forces_applied)

      discharge_builtin_args(builtin_forces, args)
    }
    VConstr { tag, fields } -> TConstr(tag, discharge_constr_fields(fields, []))
  }
}

fn discharge_constr_fields(fields: List<Value>, fold: List<Term>) -> List<Term> {
  when fields is {
    [] -> fold
    [field, ..rest] ->
      discharge_constr_fields(rest, [discharge_value(field), ..fold])
  }
}

fn discharge_builtin_forces(fun: Int, forces_applied: Int) -> Term {
  if forces_applied == 0 {
    Builtin(fun)
  } else {
    Force(discharge_builtin_forces(fun, forces_applied - 1))
  }
}

fn discharge_builtin_args(fun: Term, args: List<Value>) -> Term {
  when args is {
    [] -> fun
    [x, ..xs] -> {
      let arg_term = discharge_value(x)

      discharge_builtin_args(fun, xs) |> Apply(arg_term)
    }
  }
}

// The env is for Vars.
// You look up the var based on how many lambdas you descended
// to see if the lambda the var references is among the lambdas
// not yet evaluated or if the var refers to a lambda that put a value in the env
fn discharge_term_with_env(lambda_count: Int, env: Env, term: Term) {
  when term is {
    Lambda { body } ->
      discharge_term_with_env(lambda_count + 1, env, body) |> Lambda
    Var(index) as var ->
      if lambda_count >= index {
        var
      } else {
        discharge_value(lookup_var(index - lambda_count, env))
      }
    Apply { function, argument } ->
      discharge_term_with_env(lambda_count, env, function)
        |> Apply(discharge_term_with_env(lambda_count, env, argument))

    Delay(term) -> discharge_term_with_env(lambda_count, env, term) |> Delay
    Force(term) -> discharge_term_with_env(lambda_count, env, term) |> Force
    t -> t
  }
}

fn lookup_var(index: Int, env: Env) -> Value {
  if index <= 0 {
    fail @"get_or_err: index is negative or zero"
  } else {
    do_get_or_err(env, index, 1)
  }
}

fn do_get_or_err(env: Env, index: Int, current: Int) -> Value {
  if current > index {
    fail @"open term evaluation"
  } else {
    when env is {
      [] -> fail @"open term evaluation"
      [x, ..xs] ->
        if index == current {
          x
        } else {
          do_get_or_err(xs, index, current + 1)
        }
    }
  }
}

test run_1() {
  let term =
    Apply(
      Lambda(Apply(Apply(Builtin(add_integer), Var(1)), Var(1))),
      TConstant(Integer(1)),
    )

  run(term) == TConstant(Integer(2))
}

test run_2() {
  let term =
    Apply(
      Lambda(Lambda(Apply(Apply(Builtin(add_integer), Var(2)), Var(2)))),
      TConstant(Integer(1)),
    )

  run(term) == Lambda(
    Apply(Apply(Builtin(0), TConstant(Integer(1))), TConstant(Integer(1))),
  )
}

test run_3() {
  let term =
    Apply(
      Lambda(
        Apply(
          Lambda(Apply(Apply(Builtin(subtract_integer), Var(1)), Var(2))),
          TConstant(Integer(3)),
        ),
      ),
      TConstant(Integer(1)),
    )

  run(term) == TConstant(Integer(2))
}

test run_4() {
  let term =
    Case(
      TConstr(0, [TConstant(Integer(3)), TConstant(Integer(1))]),
      [Lambda(Lambda(Apply(Apply(Builtin(subtract_integer), Var(2)), Var(1))))],
    )

  run(term) == TConstant(Integer(2))
}
