use aiken/builtin
use uplc/constant.{Constant}
use uplc/term.{DebruijnIndex}

pub type Blake2b_256 =
  ByteArray

pub type Hash<alg, data> =
  ByteArray

pub type TermHash =
  Hash<Blake2b_256, Term>

pub type Term {
  Var(DebruijnIndex)

  Delay(TermHash)
  // param is excluded since it's always 0
  Lambda { body: TermHash }

  Apply { function: TermHash, argument: TermHash }

  TConstant(Constant)

  Force(TermHash)

  Error

  Builtin(Int)

  TConstr { tag: Int, fields: List<TermHash> }

  Case { constr: TermHash, branches: List<TermHash> }
}

pub fn hashing_algo(b: ByteArray) -> ByteArray {
  builtin.blake2b_256(b)
}
