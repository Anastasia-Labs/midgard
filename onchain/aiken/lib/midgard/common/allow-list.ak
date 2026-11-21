//// The stage-0 operator allow-list. A deployment either admits every key
//// (`None`) or exactly the keys it lists; the list is an env constant, fixed
//// for the deployment's life.

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
