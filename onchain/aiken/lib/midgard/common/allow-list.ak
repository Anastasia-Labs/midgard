//// Stage-0 allow-lists. A deployment either admits every key (`None`) or
//// exactly the keys it lists; the lists are env constants, fixed for the
//// deployment's life.

use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}

/// Whether `allow_list` admits `key`: every key when it is `None`, otherwise
/// exactly the keys it lists.
pub fn admits(
  allow_list: Option<List<VerificationKeyHash>>,
  key: VerificationKeyHash,
) -> Bool {
  when allow_list is {
    None -> True
    Some(keys) -> list.has(keys, key)
  }
}

/// A key `allow_list` admits: `fallback` when every key is admitted, otherwise
/// the first listed key. Fixtures use it to sign as an admitted party under
/// whichever profile the tests compile against.
pub fn admitted_or(
  allow_list: Option<List<VerificationKeyHash>>,
  fallback: VerificationKeyHash,
) -> VerificationKeyHash {
  when allow_list is {
    Some([key, ..]) -> key
    _ -> fallback
  }
}
