//// Executable handler coverage for `validators/field-preimage-certificate.ak`
//// — the permissionless chunk-certificate validator of
//// `docs/spec/midgard-tx.md` §8.4–§8.7.
////
//// The content rules (the tx-id binding, the split shape, the digest vector,
//// the reconstruction hash) are proved at their own seam in
//// `lib/midgard/native-tx-carriage-v1.test.ak` and are not re-proved here.
//// What this module covers is everything that is *only* true of the
//// transaction: which output the certificate lands in, which token exists
//// afterwards, and who may make it stop existing.
////
//// Fixture convention follows `validators/settlement-handlers.test.ak`: one
//// honest builder per handler, and one `fail` test per guard whose body is the
//// honest fixture with a single field mutated, so exactly one conjunct flips.
////
//// The healing test is the one that is not a guard. §8.7 says a failed or
//// malicious publication is healable by anyone, and that claim is only real if
//// an unrelated party — different key, different UTxOs, different min-Ada
//// authority, no relationship to the original publisher — can certify the same
//// content and get a token with the same name. That is asserted here as a
//// positive, against carriage republished from the preimage bytes alone.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{Value}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use field_preimage_certificate as certificate_validator
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  native_tx_version_v1,
}
use midgard/native_tx_carriage_v1.{
  Certify, FieldPreimageCertificateMintRedeemerV1, Retire,
  field_preimage_certificate_v1, split_field_preimage,
}
use midgard/native_tx_field_access_v1.{
  FieldPreimageCertificateV1, chunk_bytes_k, field_commitment,
  field_preimage_certificate_asset_name,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

/// The certificate script hash. Mint and spend are handlers of one validator,
/// so this is both the policy id and the address's payment credential.
const certificate_script_hash: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

/// The original publisher, and the min-Ada reclaim authority they set.
const publisher_hash: ByteArray =
  #"66666666666666666666666666666666666666666666666666666666"

/// An unrelated party. Never signs anything the publisher signed, holds no
/// certificate, and has no relationship to the original carriage — which is the
/// point of the healing test.
const healer_hash: ByteArray =
  #"77777777777777777777777777777777777777777777777777777777"

const carriage_utxo_id: ByteArray =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const healed_carriage_utxo_id: ByteArray =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const certificate_utxo_id: ByteArray =
  #"3333333333333333333333333333333333333333333333333333333333333333"

/// The disputed field. Field 0 keeps the fixture's committed body honest with
/// one non-placeholder slot, which is all the transaction-shape tests need.
const disputed_field: Int = 0

fn grow(acc: ByteArray, length: Int) -> ByteArray {
  if bytearray.length(acc) >= length {
    acc
  } else {
    grow(bytearray.concat(acc, acc), length)
  }
}

/// One byte over `K`: the smallest preimage that is legally tier 3, so the
/// fixture is the cheapest transaction that can carry a certificate at all.
fn preimage() -> ByteArray {
  bytearray.take(grow(#"a5c30f", chunk_bytes_k + 1), chunk_bytes_k + 1)
}

fn witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: zero_hash32,
    script_tx_wits_hash: zero_hash32,
    redeemer_tx_wits_hash: zero_hash32,
  }
}

fn body() -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: field_commitment(preimage()),
    reference_inputs_hash: zero_hash32,
    outputs_hash: zero_hash32,
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: zero_hash32,
    required_signers_hash: zero_hash32,
    mint_hash: zero_hash32,
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
}

fn witness_set_compact_cbor() -> ByteArray {
  encode_native_tx_witness_set_compact(witness_set())
}

fn compact_cbor() -> ByteArray {
  encode_native_tx_compact_v1(
    NativeTxCompact {
      body: body(),
      witness_set_hash: blake2b_256(witness_set_compact_cbor()),
      validity_code: 0,
    },
  )
}

fn disputed_tx_id() -> ByteArray {
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(body()),
  )
}

/// §8.6's constant asset name (#606) — one name for every certificate of the
/// policy. Kept as a helper so every test names the token the same way the
/// handlers do.
fn certificate_asset_name() -> ByteArray {
  field_preimage_certificate_asset_name
}

fn certificate_for(owner: ByteArray) -> FieldPreimageCertificateV1 {
  field_preimage_certificate_v1(
    owner,
    disputed_tx_id(),
    disputed_field,
    preimage(),
  )
}

/// §8.5: raw carriage is a nothing-but-bytes inline datum at the publisher's
/// own key address. Any address, any key — the bytes are what get checked.
fn chunk_input(publisher: ByteArray, utxo_id: ByteArray, index: Int) -> Input {
  expect Some(chunk) = list.at(split_field_preimage(preimage()), index)
  Input {
    output_reference: OutputReference {
      transaction_id: utxo_id,
      output_index: index,
    },
    output: Output {
      address: address.from_verification_key(publisher),
      value: assets.from_lovelace(2000000),
      datum: InlineDatum(builtin.b_data(chunk)),
      reference_script: None,
    },
  }
}

fn chunk_inputs(publisher: ByteArray, utxo_id: ByteArray) -> List<Input> {
  [chunk_input(publisher, utxo_id, 0), chunk_input(publisher, utxo_id, 1)]
}

fn certificate_output(
  certificate: FieldPreimageCertificateV1,
  value: Value,
) -> Output {
  let certificate_data: Data = certificate
  Output {
    address: address.from_script(certificate_script_hash),
    value,
    datum: InlineDatum(certificate_data),
    reference_script: None,
  }
}

fn certificate_value(name: ByteArray, quantity: Int) -> Value {
  assets.from_lovelace(2000000)
    |> assets.add(certificate_script_hash, name, quantity)
}

fn honest_certificate_output() -> Output {
  certificate_output(
    certificate_for(publisher_hash),
    certificate_value(certificate_asset_name(), 1),
  )
}

fn certify_redeemer(
  chunk_ref_input_indices: List<Int>,
  output_index: Int,
) -> FieldPreimageCertificateMintRedeemerV1 {
  Certify {
    compact_cbor: compact_cbor(),
    witness_set_compact_cbor: witness_set_compact_cbor(),
    chunk_ref_input_indices,
    output_index,
  }
}

fn certification_tx(
  outputs: List<Output>,
  mint: Value,
  inputs: List<Input>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    outputs,
    mint,
    reference_inputs: inputs,
  }
}

fn honest_certification_tx() -> Transaction {
  certification_tx(
    [honest_certificate_output()],
    assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
    chunk_inputs(publisher_hash, carriage_utxo_id),
  )
}

fn run_mint(
  redeemer: FieldPreimageCertificateMintRedeemerV1,
  tx: Transaction,
) -> Bool {
  certificate_validator.field_preimage_certificate.mint(
    redeemer,
    certificate_script_hash,
    tx,
  )
}

fn run_certify(tx: Transaction) -> Bool {
  run_mint(certify_redeemer([0, 1], 0), tx)
}

// ---------------------------------------------------------------------------
// Certification (§8.6)
// ---------------------------------------------------------------------------

/// The honest publication: two raw chunks at the publisher's key address, one
/// certificate output at the script's own address carrying exactly one token of
/// the content-addressed name.
test field_preimage_certificate_handler_mint_certifies_an_honest_publication() {
  run_certify(honest_certification_tx())
}

/// §8.5 puts the certificate at script custody. A certificate at a key address
/// would be spendable without the burn the spend handler enforces, so the token
/// could outlive any check the policy made.
test field_preimage_certificate_handler_mint_rejects_a_certificate_output_at_a_foreign_address() fail {
  run_certify(
    certification_tx(
      [
        Output {
          ..honest_certificate_output(),
          address: address.from_verification_key(publisher_hash),
        },
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// A reference script on the certificate output is not something any consumer
/// reads, and refusing it keeps the output's shape exactly what §8.6 describes.
test field_preimage_certificate_handler_mint_rejects_a_certificate_output_carrying_a_reference_script() fail {
  run_certify(
    certification_tx(
      [
        Output {
          ..honest_certificate_output(),
          reference_script: Some(certificate_script_hash),
        },
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// The manifest is read out of a *reference input* by every consuming step, so
/// a datum hash would leave the certificate unreadable — the token would exist
/// and authenticate nothing.
test field_preimage_certificate_handler_mint_rejects_a_certificate_output_without_an_inline_datum() fail {
  run_certify(
    certification_tx(
      [Output { ..honest_certificate_output(), datum: transaction.NoDatum }],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// §8.6's name is one constant for every certificate (#606), and the mint
/// still pins it: the single-pair check requires exactly the constant name at
/// quantity 1, so a token of this policy is always the constant name over a
/// datum the mint proved. The name is branding, not identity — but an
/// unpinned name would let a certification bring an arbitrary-named token of
/// the policy into existence, and anything that trusts "a token of the
/// certificate policy" would read it.
test field_preimage_certificate_handler_mint_rejects_an_asset_name_that_is_not_the_constant() fail {
  let foreign_name = #"deadbeef"
  run_certify(
    certification_tx(
      [
        certificate_output(
          certificate_for(publisher_hash),
          certificate_value(foreign_name, 1),
        ),
      ],
      assets.from_asset(certificate_script_hash, foreign_name, 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// The mint weld at the transaction seam (#606): the named output's datum is
/// what the content check verifies, so a datum whose `field_hash` is not the
/// commitment the chunks reconstruct is refused even when every byte of
/// carriage is honest.
test field_preimage_certificate_handler_mint_rejects_a_datum_with_a_foreign_field_hash() fail {
  run_certify(
    certification_tx(
      [
        certificate_output(
          FieldPreimageCertificateV1 {
            ..certificate_for(publisher_hash),
            field_hash: zero_hash32,
          },
          certificate_value(certificate_asset_name(), 1),
        ),
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// Quantity 1 (§8.6). A second unit of the same name is a certificate that can
/// be split away from the output whose datum was proved.
test field_preimage_certificate_handler_mint_rejects_a_quantity_other_than_one() fail {
  run_certify(
    certification_tx(
      [
        certificate_output(
          certificate_for(publisher_hash),
          certificate_value(certificate_asset_name(), 2),
        ),
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 2),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// One certification proves one certificate. A second asset name of this policy
/// riding along on the same mint would be unexamined by construction, and could
/// land at the script address with any datum its minter liked.
test field_preimage_certificate_handler_mint_rejects_a_second_token_of_the_policy() fail {
  run_certify(
    certification_tx(
      [honest_certificate_output()],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1)
        |> assets.add(certificate_script_hash, #"deadbeef", 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// The §8.3 ladder is at most three chunks. A longer index vector cannot
/// describe a legal preimage, so it is refused before any reference input is
/// resolved.
test field_preimage_certificate_handler_mint_rejects_more_chunk_indices_than_the_ladder_allows() fail {
  run_mint(certify_redeemer([0, 1, 0, 1], 0), honest_certification_tx())
}

/// Carriage that is not the committed preimage. The chunk UTxOs are perfectly
/// well-formed; their bytes are simply not the ones the transaction id
/// committed, which is the only thing §8.5 ever trusts about raw carriage.
test field_preimage_certificate_handler_mint_rejects_chunks_that_are_not_the_committed_preimage() fail {
  let junk = Input {
    ..chunk_input(publisher_hash, carriage_utxo_id, 0),
    output: Output {
      address: address.from_verification_key(publisher_hash),
      value: assets.from_lovelace(2000000),
      datum: InlineDatum(
        builtin.b_data(
          bytearray.take(grow(#"ff", chunk_bytes_k), chunk_bytes_k),
        ),
      ),
      reference_script: None,
    },
  }
  run_certify(
    certification_tx(
      [honest_certificate_output()],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      [junk, chunk_input(publisher_hash, carriage_utxo_id, 1)],
    ),
  )
}

/// `output_index` is positional, and `list.at` is total — it answers `None`
/// rather than aborting — so a negative index would be a silent miss if the
/// guard were not there.
test field_preimage_certificate_handler_mint_rejects_a_negative_output_index() fail {
  run_mint(certify_redeemer([0, 1], -1), honest_certification_tx())
}

/// And past the end of the output list, which is the other side of the same
/// guard: nothing was proved about any output, so nothing may be minted.
test field_preimage_certificate_handler_mint_rejects_an_output_index_past_the_end() fail {
  run_mint(certify_redeemer([0, 1], 1), honest_certification_tx())
}

/// §8.5 pins the whole address, not just its payment credential. Nothing reads
/// the stake credential, which is exactly why leaving it open would be free
/// money: the minter could point the deposit's staking rights at their own
/// reward account on a certification anyone else paid for, and certificates
/// would be spread over a family of addresses instead of one an indexer can
/// enumerate.
test field_preimage_certificate_handler_mint_rejects_a_certificate_output_with_a_stake_credential() fail {
  run_certify(
    certification_tx(
      [
        Output {
          ..honest_certificate_output(),
          address: address.from_script(certificate_script_hash)
            |> address.with_delegation_key(healer_hash),
        },
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// The certificate output carries the proved token and nothing else of this
/// policy. A `quantity_of` check on the one name says nothing about the names
/// it was not asked about, so a second name arriving from an input could ride
/// into the very output whose datum was proved — and be read there by anything
/// that checks "a token of the certificate policy is present" without also
/// checking which one.
test field_preimage_certificate_handler_mint_rejects_a_second_asset_name_in_the_certificate_output() fail {
  run_certify(
    certification_tx(
      [
        certificate_output(
          certificate_for(publisher_hash),
          certificate_value(certificate_asset_name(), 1)
            |> assets.add(certificate_script_hash, #"deadbeef", 1),
        ),
      ],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      chunk_inputs(publisher_hash, carriage_utxo_id),
    ),
  )
}

/// The chunk indices are positional too, and `raw_chunk_bytes` keeps its own
/// guards rather than trusting the caller's. Negative first.
test field_preimage_certificate_handler_mint_rejects_a_negative_chunk_index() fail {
  run_mint(certify_redeemer([-1, 1], 0), honest_certification_tx())
}

/// And past the end of the reference-input list.
test field_preimage_certificate_handler_mint_rejects_a_chunk_index_past_the_end() fail {
  run_mint(certify_redeemer([0, 7], 0), honest_certification_tx())
}

/// §8.5 carriage is a nothing-but-bytes *inline* datum. A referenced UTxO
/// carrying a datum hash — or none at all — has no bytes to certify, so the
/// read fails closed instead of certifying an empty chunk.
test field_preimage_certificate_handler_mint_rejects_a_chunk_without_an_inline_datum() fail {
  run_certify(
    certification_tx(
      [honest_certificate_output()],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      {
        let second = chunk_input(publisher_hash, carriage_utxo_id, 1)
        [
          chunk_input(publisher_hash, carriage_utxo_id, 0),
          Input {
            ..second,
            output: Output { ..second.output, datum: transaction.NoDatum },
          },
        ]
      },
    ),
  )
}

// ---------------------------------------------------------------------------
// §8.7 healing
// ---------------------------------------------------------------------------

/// The permissionless claim, as a transaction. An unrelated party republishes
/// the same content from the preimage bytes — their own key address, their own
/// UTxOs, their own min-Ada reclaim authority — and certifies. The policy asks
/// nothing about who they are, and the certificate they get is byte-identical
/// modulo `owner` (#606: same bytes ⇒ same datum, under the one constant
/// name), so every consuming step that matched the yanked certificate's datum
/// matches the new one.
test field_preimage_certificate_handler_mint_heals_a_yanked_publication_from_an_unrelated_party() {
  let healed = certificate_for(healer_hash)
  let original = certificate_for(publisher_hash)
  and {
    // Same content, same welded commitment; different publisher and different
    // reclaim owner.
    healed.chunk_digests == original.chunk_digests,
    healed.field_hash == original.field_hash,
    healed.owner != original.owner,
    run_mint(
      certify_redeemer([0, 1], 1),
      certification_tx(
        [
          // A decoy output ahead of the certificate, so the positional
          // `output_index` is doing real work rather than picking the only
          // output there is.
          Output {
            address: address.from_verification_key(healer_hash),
            value: assets.from_lovelace(2000000),
            datum: transaction.NoDatum,
            reference_script: None,
          },
          certificate_output(
            healed,
            certificate_value(certificate_asset_name(), 1),
          ),
        ],
        assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
        chunk_inputs(healer_hash, healed_carriage_utxo_id),
      ),
    ),
  }
}

// ---------------------------------------------------------------------------
// Retirement (§8.7)
// ---------------------------------------------------------------------------

fn burn_tx(
  mint: Value,
  outputs: List<Output>,
  signatories: List<ByteArray>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: OutputReference {
          transaction_id: certificate_utxo_id,
          output_index: 0,
        },
        output: honest_certificate_output(),
      },
    ],
    outputs,
    mint,
    extra_signatories: signatories,
  }
}

fn own_ref() -> OutputReference {
  OutputReference { transaction_id: certificate_utxo_id, output_index: 0 }
}

fn run_spend(
  certificate: Option<FieldPreimageCertificateV1>,
  tx: Transaction,
) -> Bool {
  let redeemer: Data = 0
  certificate_validator.field_preimage_certificate.spend(
    certificate,
    redeemer,
    own_ref(),
    tx,
  )
}

fn honest_burn_tx() -> Transaction {
  burn_tx(
    assets.from_asset(certificate_script_hash, certificate_asset_name(), -1),
    [],
    [publisher_hash],
  )
}

/// Cleanup is owner-discretionary (§8.7): the owner signs, the token is burnt,
/// the min-Ada comes back.
test field_preimage_certificate_handler_spend_reclaims_min_ada_when_the_owner_burns_the_certificate() {
  run_spend(Some(certificate_for(publisher_hash)), honest_burn_tx())
}

/// The owner set at mint is the reclaim authority, so an unsigned burn is a
/// stranger taking the deposit.
test field_preimage_certificate_handler_spend_rejects_a_burn_without_the_owner_signature() fail {
  run_spend(
    Some(certificate_for(publisher_hash)),
    burn_tx(
      assets.from_asset(certificate_script_hash, certificate_asset_name(), -1),
      [],
      [healer_hash],
    ),
  )
}

/// The guard that makes forgery impossible: a certificate token that survives
/// its own spend could be re-attached to an output whose datum the minting
/// policy never saw. Stated over outputs rather than over the burn count, so two
/// same-named certificates spent together cannot both point at one `-1`.
test field_preimage_certificate_handler_spend_rejects_a_surviving_certificate_token() fail {
  run_spend(
    Some(certificate_for(publisher_hash)),
    burn_tx(
      assets.zero,
      [
        certificate_output(
          certificate_for(publisher_hash),
          certificate_value(certificate_asset_name(), 1),
        ),
      ],
      [publisher_hash],
    ),
  )
}

/// The same guard, at the corner #606's constant name makes reachable: every
/// certificate token is now the same asset, so "the token survives" and "the
/// token migrates onto a fresh datum" are one move. The owner spends their own
/// certificate and tries to send the constant-name token out on an output
/// wearing a *different* datum — a certificate the minting policy never saw.
/// "No output carries the name" refuses it per-token and order-independently
/// (invariant a of the 2026-08-16 ruling).
test field_preimage_certificate_handler_spend_rejects_the_token_migrating_onto_a_fresh_datum() fail {
  run_spend(
    Some(certificate_for(publisher_hash)),
    burn_tx(
      assets.zero,
      [
        certificate_output(
          certificate_for(healer_hash),
          certificate_value(certificate_asset_name(), 1),
        ),
      ],
      [publisher_hash],
    ),
  )
}

/// A UTxO at this address with no inline datum is not a certificate: nothing
/// names the asset that has to stop existing or the owner who may authorise it.
/// There is no safe default for either, so the handler refuses rather than
/// guessing — a datumless UTxO here is a misdirected payment, not a
/// certificate, and unlocking it would unlock it for anyone.
test field_preimage_certificate_handler_spend_rejects_a_missing_datum() fail {
  run_spend(None, honest_burn_tx())
}

/// The spent UTxO is resolved from the ledger's own `own_ref`, so a transaction
/// whose inputs do not contain it has nothing for the handler to check the
/// address and value of. This is the guard that keeps the policy-id derivation
/// below it honest: without it there would be no input to read the script hash
/// out of.
test field_preimage_certificate_handler_spend_rejects_an_own_ref_that_is_not_an_input() fail {
  run_spend(
    Some(certificate_for(publisher_hash)),
    Transaction {
      ..honest_burn_tx(),
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: healed_carriage_utxo_id,
            output_index: 0,
          },
          output: honest_certificate_output(),
        },
      ],
    },
  )
}

/// The burn branch of the policy: it exists so a burn can happen at all, and it
/// must never be a way around the content proof.
test field_preimage_certificate_handler_retire_permits_a_burn() {
  run_mint(Retire, honest_burn_tx())
}

/// `Retire` with a positive quantity would be an uncertified certificate.
test field_preimage_certificate_handler_retire_rejects_a_mint() fail {
  run_mint(
    Retire,
    certification_tx(
      [honest_certificate_output()],
      assets.from_asset(certificate_script_hash, certificate_asset_name(), 1),
      [],
    ),
  )
}

// ---------------------------------------------------------------------------
// Phase-4 exit measurement — certificate mint and spend (spec §8.10, rows 7–12)
// ---------------------------------------------------------------------------
//
// What a certification and a retirement actually cost, at both sizes the ladder
// really uses: the smallest legally tier-3 preimage (`K + 1`, two chunks) that
// every other test in this module runs on, and the three-chunk corner at the
// §5.4 aggregate cap. Each is paired with a control that builds the same
// transaction and does not run the handler, so the published figure is the
// handler's own work rather than the fixture's.

/// The three-chunk corner, as certification sees it. Certification checks chunk
/// *shape*, digests and the reconstruction hash and never looks inside an item,
/// so the corner only has to be the right length here — 32,763 bytes, the
/// largest §5.3 stride-40 preimage under the §5.4 cap, splitting `[15,148,
/// 15,148, 2,467]`. The door-side corner is `door_corner_preimage` in
/// `lib/midgard/native-tx-carriage-v1.test.ak`: the same length, but built out
/// of real items, because that is the side that reads them. The two are named
/// apart on purpose — they are different bytes serving different claims, and a
/// shared name would make either one un-greppable.
fn mint_corner_preimage() -> ByteArray {
  bytearray.take(grow(#"b7e214", 32763), 32763)
}

fn mint_corner_body() -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    ..body(),
    spend_inputs_hash: field_commitment(mint_corner_preimage()),
  }
}

fn corner_compact_cbor() -> ByteArray {
  encode_native_tx_compact_v1(
    NativeTxCompact {
      body: mint_corner_body(),
      witness_set_hash: blake2b_256(witness_set_compact_cbor()),
      validity_code: 0,
    },
  )
}

fn corner_tx_id() -> ByteArray {
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(mint_corner_body()),
  )
}

fn corner_certificate() -> FieldPreimageCertificateV1 {
  field_preimage_certificate_v1(
    publisher_hash,
    corner_tx_id(),
    disputed_field,
    mint_corner_preimage(),
  )
}

/// Raw §8.5 carriage inputs for a list of chunks, in the order given.
///
/// Written once and taken as a parameter rather than reading the corner
/// fixture, because the neutralisation below needs *the same* inputs with one
/// byte changed. Two copies of this builder would leave the tampered fixture
/// free to differ from the measured one in some second way, and then the
/// refusal it asserts would not be attributable to the tampering.
fn carriage_inputs(chunks: List<ByteArray>) -> List<Input> {
  list.indexed_map(
    chunks,
    fn(index, chunk) {
      Input {
        output_reference: OutputReference {
          transaction_id: carriage_utxo_id,
          output_index: index,
        },
        output: Output {
          address: address.from_verification_key(publisher_hash),
          value: assets.from_lovelace(2000000),
          datum: InlineDatum(builtin.b_data(chunk)),
          reference_script: None,
        },
      }
    },
  )
}

fn corner_chunk_inputs() -> List<Input> {
  carriage_inputs(split_field_preimage(mint_corner_preimage()))
}

fn corner_certify_redeemer() -> FieldPreimageCertificateMintRedeemerV1 {
  Certify {
    compact_cbor: corner_compact_cbor(),
    witness_set_compact_cbor: witness_set_compact_cbor(),
    chunk_ref_input_indices: [0, 1, 2],
    output_index: 0,
  }
}

fn corner_certification_tx() -> Transaction {
  let name = field_preimage_certificate_asset_name
  certification_tx(
    [certificate_output(corner_certificate(), certificate_value(name, 1))],
    assets.from_asset(certificate_script_hash, name, 1),
    corner_chunk_inputs(),
  )
}

/// The corner fixture is the corner it is quoted as. Three chunks, the §8.4
/// shape, five bytes of headroom under the §5.4 cap — asserted so spec §8.10's
/// rows 9 and 10 stay quoted where they were taken.
test certificate_corner_fixture_sits_at_the_three_chunk_corner() {
  let preimage = mint_corner_preimage()
  let total_length = bytearray.length(preimage)
  and {
    total_length == 32763,
    total_length + 5 == 32768,
    list.map(split_field_preimage(preimage), bytearray.length) == [
      chunk_bytes_k, chunk_bytes_k, 2467,
    ],
    list.length(corner_certificate().chunk_digests) == 3,
    list.length(corner_chunk_inputs()) == 3,
  }
}

/// Row 7 — the control for row 8. Builds the smallest tier-3 certification
/// transaction (two chunks at `K + 1` bytes, the certificate, the mint) and
/// stops without running the policy.
test certificate_mint_fixture_only_smallest_tier3() {
  let tx = honest_certification_tx()
  and {
    list.length(tx.reference_inputs) == 2,
    list.length(tx.outputs) == 1,
  }
}

/// Row 8 — one certification at the smallest legally tier-3 size.
test certificate_mint_cost_at_the_smallest_tier3() {
  run_certify(honest_certification_tx())
}

/// Row 9 — the control for row 10, at the corner.
test certificate_mint_fixture_only_at_the_corner() {
  let tx = corner_certification_tx()
  and {
    list.length(tx.reference_inputs) == 3,
    list.length(tx.outputs) == 1,
  }
}

/// Row 10 — one certification at the three-chunk corner. This is the whole of
/// what certification costs at the largest preimage the format admits: three
/// chunk digests, one `blake2b_256` over the 32,763-byte reconstruction, the §3
/// tx-id re-derivation, and the transaction-shape checks.
test certificate_mint_cost_at_the_corner() {
  run_mint(corner_certify_redeemer(), corner_certification_tx())
}

/// Row 11 — the control for row 12.
test certificate_spend_fixture_only() {
  let tx = honest_burn_tx()
  and {
    list.length(tx.inputs) == 1,
    tx.extra_signatories == [publisher_hash],
  }
}

/// Row 12 — one retirement. Size-independent by construction: the spend handler
/// names the constant token (#606) and checks it does not survive — it never
/// touches carriage, so there is one figure rather than one per size.
test certificate_spend_cost() {
  run_spend(Some(certificate_for(publisher_hash)), honest_burn_tx())
}

/// The corner rows are not vacuous. Row 10 measures a `mint` that returned
/// `True`, and a measurement of a handler that would have returned `True` for
/// any input at all is not a measurement of anything. This is the same
/// transaction with one byte of the *last* chunk changed — the ragged tail, the
/// chunk a boundary error is likeliest to leave unchecked — and it is refused.
test certificate_mint_rejects_a_tampered_corner_chunk() fail {
  expect [chunk_0,
        chunk_1, chunk_2] = split_field_preimage(mint_corner_preimage())
  let tampered_tail = bytearray.concat(
    bytearray.take(chunk_2, bytearray.length(chunk_2) - 1),
    #"ff",
  )
  expect bytearray.length(tampered_tail) == bytearray.length(chunk_2)
  expect tampered_tail != chunk_2
  let tampered_inputs = carriage_inputs([chunk_0, chunk_1, tampered_tail])
  let name = field_preimage_certificate_asset_name
  run_mint(
    corner_certify_redeemer(),
    certification_tx(
      [certificate_output(corner_certificate(), certificate_value(name, 1))],
      assets.from_asset(certificate_script_hash, name, 1),
      tampered_inputs,
    ),
  )
}
