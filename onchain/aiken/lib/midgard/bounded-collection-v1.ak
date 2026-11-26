//// Bounded-collection constants (docs/spec/midgard-tx.md §4).
////
//// Midgard's nine compact-transaction field commitments are flat blake2b-256
//// hashes over the enveloped field-preimage bytes, produced and opened solely
//// through `midgard/native_tx_field_access_v1`, the one lib-level
//// field-access door.

pub const version: Int = 1

/// One byte per item is the absolute lower encoding floor under the preserved
/// 16,384-byte Cardano transaction envelope.
pub const max_tx_size_derived_item_count: Int = 16384
