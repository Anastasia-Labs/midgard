//// Full-width ordering and authentication primitives for the deposit and
//// withdrawal histories. The empty asset name authenticates the root; node
//// names are the complete 32-byte ordering key, without a role prefix.
//// Callers supply the deployment-authenticated policy and address, never
//// identities chosen by an evidence redeemer. Mutation validation is separate.

use aiken/collection/dict
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Output}

pub type Position {
  Root
  Key(ByteArray)
}

/// Bound the mutable timestamp's CBOR width so admission can fund every
/// subsequent pointer continuation. POSIX milliseconds fit through year 292M.
pub const max_protection_time: Int = 9_223_372_036_854_775_807

pub type Link<payload> {
  position: Position,
  next: Option<ByteArray>,
  protected_until: Int,
  payload: payload,
}

pub fn valid_key(key: ByteArray) -> Bool {
  bytearray.length(key) == 32
}

pub fn precedes(position: Position, key: ByteArray) -> Bool {
  and {
    valid_key(key),
    when position is {
      Root -> True
      Key(predecessor) -> and {
          valid_key(predecessor),
          bytearray.compare(predecessor, key) == Less,
        }
    },
  }
}

pub fn before_successor(key: ByteArray, next: Option<ByteArray>) -> Bool {
  and {
    valid_key(key),
    when next is {
      None -> True
      Some(successor) -> and {
          valid_key(successor),
          bytearray.compare(key, successor) == Less,
        }
    },
  }
}

pub fn valid_link(link: Link<payload>) -> Bool {
  and {
    link.protected_until >= 0,
    link.protected_until <= max_protection_time,
    when link.position is {
      Root -> True
      Key(key) -> valid_key(key)
    },
    when link.next is {
      None -> True
      Some(next) -> precedes(link.position, next)
    },
  }
}

/// Strict adjacency gap. Equal-key filler absence must be authenticated by
/// the caller's payload decoder; equality is never an insertion gap.
pub fn contains_gap(link: Link<payload>, target: ByteArray) -> Bool {
  and {
    valid_link(link),
    precedes(link.position, target),
    before_successor(target, link.next),
  }
}

pub fn token_name(position: Position) -> ByteArray {
  when position is {
    Root -> #""
    Key(key) -> {
      expect valid_key(key)
      key
    }
  }
}

/// Checks the whole policy bundle, so a root cannot also carry an order token
/// and one output cannot authenticate two logical nodes. Other-policy assets
/// remain possible because a deposit node holds the user's actual Value.
pub fn authenticate(
  output: Output,
  policy: PolicyId,
  address: Address,
) -> Link<Data> {
  expect output.address == address
  expect output.reference_script == None
  expect InlineDatum(data) = output.datum
  expect link: Link<Data> = data
  expect valid_link(link)
  let tokens = output.value |> assets.tokens(policy) |> dict.to_pairs
  expect tokens == [Pair(token_name(link.position), 1)]
  link
}

/// A pointer-only continuation preserves the complete payload and Value.
/// The authorized splice chooses `next`; this helper alone cannot authorize
/// spending, admission, promotion or retirement.
pub fn preserves_predecessor(
  input: Output,
  output: Output,
  policy: PolicyId,
  address: Address,
  next: Option<ByteArray>,
  protected_until: Int,
) -> Bool {
  let before = authenticate(input, policy, address)
  let after = authenticate(output, policy, address)
  and {
    input.value == output.value,
    after == Link { ..before, next, protected_until },
  }
}
