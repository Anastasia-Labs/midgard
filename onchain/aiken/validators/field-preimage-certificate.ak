//// The permissionless field-preimage chunk-certificate validator —
//// `docs/spec/midgard-tx.md` §8.4 through §8.7.
////
//// Tier 3 of the carriage ladder is the only tier that needs on-chain
//// certification, and it needs it for one reason: a flat field commitment
//// authenticates the whole preimage and nothing smaller. Once a preimage is
//// split across publications there is no other way to verify an individual
//// chunk before reconstruction, so the certificate supplies that binding —
//// a mint-verified `total_length` and per-chunk digest vector that a consuming
//// step can check one chunk against, instead of re-reading all of them.
////
//// **One script, two handlers (§8.6).** `mint` and `spend` are handlers of the
//// same validator, so the policy id and the spend credential are one script
//// hash: the mint sends the token to its own address and the spend burns its
//// own policy. No external reference-script bootstrap and no cyclic dependency
//// between a policy and the address it pays.
////
//// **Nothing here is privileged (§8.7).** The policy checks content and never
//// identity — no operator role, no signature at mint, no allowlist of
//// publishers. That is what makes a failed or malicious publication healable:
//// because the §8.4 split is a pure function of the preimage bytes, an
//// unrelated party who obtains the same preimage republishes byte-identical
//// chunks and certifies from them, and the certificate they get is
//// interchangeable with the one that was yanked. Worst case for a victim is
//// fees and delay; there is no state anyone can capture, because there is no
//// state — only content.
////
//// **What this validator does not decide.** It never looks at a game, a
//// thread, a block, or a step. A certificate is a statement about one L2
//// transaction's one field, good indefinitely and for every dispute over that
//// transaction (§8.6, _Consumption_); who may use it is the consuming step's
//// question, answered by `authenticated_field_view` against the thread's
//// already-authenticated transaction — never against redeemer-supplied
//// identity.
////
//// The content rules live in `midgard/native_tx_carriage_v1` so a focused test
//// can drive them without building a transaction; this module is the
//// transaction-shape wrapper — which output, which token, which burn.

use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/common/utils
use midgard/native_tx_carriage_v1.{
  Certify, FieldPreimageCertificateMintRedeemerV1, Retire, raw_chunk_bytes,
  verify_field_preimage_certificate_v1,
}
use midgard/native_tx_field_access_v1.{
  FieldPreimageCertificateV1, field_preimage_certificate_asset_name,
  max_tier3_chunk_count,
}

validator field_preimage_certificate {
  mint(
    redeemer: FieldPreimageCertificateMintRedeemerV1,
    own_policy_id: PolicyId,
    tx: Transaction,
  ) {
    when redeemer is {
      Certify {
        compact_cbor,
        witness_set_compact_cbor,
        chunk_ref_input_indices,
        output_index,
      } -> {
        // 1. The certificate output is named positionally, so the index has to
        //    resolve to a real output. `list.at` is total — it answers `None`
        //    past the end and for a negative index — so both ends of the range
        //    are refusals rather than silent misses.
        expect output_index >= 0
        expect Some(certificate_output) = list.at(tx.outputs, output_index)
        let Output { address, value, datum, reference_script } = certificate_output
        // 2. §8.5: the certificate is authenticated data, so it lands at this
        //    script's own address — the same hash as the policy that just
        //    proved it.
        expect address.payment_credential == Script(own_policy_id)
        // 3. And at exactly one shape of that address. Nothing reads the stake
        //    credential, so pinning it costs nothing and buys two things: the
        //    minter cannot point the deposit's staking rights at a stranger's
        //    reward account on the way past, and an indexer looking for
        //    certificates has one address to enumerate rather than a family of
        //    them sharing a payment credential.
        expect address.stake_credential == None
        // 4. No reference script: no consumer reads one, and refusing it keeps
        //    the output's shape exactly what §8.6 describes.
        expect reference_script == None
        // 5. An inline datum, because a consuming step reads the manifest out
        //    of a reference input and a datum hash would make that impossible.
        expect InlineDatum(certificate_data) = datum
        expect certificate: FieldPreimageCertificateV1 = certificate_data

        // 6. §8.6's constant name (#606 retired the per-certificate
        //    derivation — the name is branding, not identity; everything it
        //    encoded is in the datum, which now also carries the mint-welded
        //    `field_hash`). The bounds the derivation used to force ride the
        //    content check instead: step 10 re-derives the tx-id (32 bytes by
        //    construction) and reads the commitment positionally, which
        //    refuses a `field_index` outside 0..8.
        let asset_name = field_preimage_certificate_asset_name
        // 7. Exactly one token of this policy in the whole transaction, and it
        //    is the constant name — the mint pins the name even though nothing
        //    derives from it. Without the single-pair check a second,
        //    unexamined asset name of this policy could ride along on a valid
        //    certification and land wherever its minter liked.
        //
        //    The consequence is intended and worth stating: one pair means one
        //    certificate per transaction, and no batching — neither two fields
        //    certified together, nor a certify paired with a retire of another
        //    name, because a burn is a second pair of this policy in `tx.mint`.
        //    That is the deliberate trade. Every extra name admitted here would
        //    have to be proved here too, and the proof is per-certificate: its
        //    own tx-id re-derivation, its own witness set, its own chunk vector
        //    resolved from its own reference inputs. A batching form would
        //    carry a list of those and fail whole, while certification is a
        //    once-per-field operation on a permissionless, healable object
        //    (§8.7) — so the saving would be one transaction's overhead,
        //    against a policy whose single-pair shape is checkable by reading
        //    four lines. Retirement loses nothing either: `Retire` is a
        //    separate redeemer and burns batch freely under it.
        expect [Pair(minted_name, minted_quantity)] = tx.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
        expect minted_name == asset_name
        expect minted_quantity == 1
        // 8. And the certificate output carries that token and nothing else of
        //    this policy. Stated as the whole per-policy pair list rather than
        //    as `quantity_of` the one name: `quantity_of` says nothing about
        //    the names it was not asked about, so a second name of this policy
        //    arriving from an input could ride into the proved output and be
        //    read there by anything that trusts "a token of the certificate
        //    policy" without also checking which one.
        let carried_tokens = value
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
        expect carried_tokens == [Pair(asset_name, 1)]

        // 9. §8.3 bounds the ladder at three chunks; refusing a longer index
        //    list up front keeps a redeemer from making the policy resolve
        //    reference inputs it could never legally need.
        expect list.length(chunk_ref_input_indices) <= max_tier3_chunk_count
        let chunks = list.map(
          chunk_ref_input_indices,
          fn(index) { raw_chunk_bytes(tx.reference_inputs, index) },
        )
        // 10. Everything §8.6 requires of the certificate's content — the tx-id
        //     binding, the split shape, the digest vector, the reconstruction
        //     against the positionally-extracted field commitment.
        verify_field_preimage_certificate_v1(
          certificate,
          compact_cbor,
          witness_set_compact_cbor,
          chunks,
        )
      }

      // Burning. The owner's authority is checked by the `spend` handler that
      // releases the token's UTxO; all this branch owes is that a burn redeemer
      // cannot mint. Every quantity of this policy in the transaction must be
      // negative, so `Retire` can never bring a certificate into existence
      // without the content proof above.
      Retire ->
        tx.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
          |> list.all(fn(minted) { minted.2nd < 0 })
    }
  }

  /// Min-Ada reclaim (§8.5/§8.7). Cleanup is owner-discretionary: no time-lock,
  /// no forced cleanup, and a mid-game yank is self-healing, so the only
  /// question here is whether the owner authorised it and whether the token
  /// stops existing.
  spend(
    datum: Option<FieldPreimageCertificateV1>,
    _redeemer: Data,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    // 1. A datumless UTxO at this address is not a certificate — nothing names
    //    the asset to burn or the owner who may authorise it — so there is
    //    nothing this handler could check and it refuses instead of guessing.
    expect Some(certificate) = datum
    // 2. The spent UTxO is resolved from the ledger's own `own_ref` rather than
    //    from a redeemer index, so the value and address checked below are the
    //    ones actually being consumed.
    expect Some(own_input) = list.find(
      tx.inputs,
      fn(input) { input.output_reference == own_ref },
    )
    // 3. Mint and spend are handlers of one validator, so the script hash
    //    guarding this UTxO *is* the certificate policy id.
    expect Script(own_policy_id) = own_input.output.address.payment_credential
    // 4. The same §8.6 constant name the mint handler pinned (#606). The
    //    datum still names the burn's authority (`owner` below); the asset
    //    name no longer varies with it.
    let asset_name = field_preimage_certificate_asset_name

    // 5. The token must not survive the transaction. Stated over outputs rather
    // than over `tx.mint`, because that is the form that cannot be
    // double-satisfied: a burn count is a sum over the whole transaction, so
    // two certificates of the same name spent together could each point at the
    // same single `-1` and let one token survive — and a surviving token is a
    // certificate whose datum the minting policy never saw, i.e. a forgery.
    // "No output carries it" is per-token and order-independent: if every input
    // holding the name is consumed and no output holds it, the ledger's own
    // value balance says all of them were burnt. That form is what makes the
    // discipline same-name-safe by construction under #606's constant name
    // (invariant a of the ruling): every certificate token is now the same
    // asset, and a token still cannot survive its UTxO or migrate onto a fresh
    // datum, because no output of this transaction may carry the name at all.
    expect
      list.all(
        tx.outputs,
        fn(output) {
          assets.quantity_of(output.value, own_policy_id, asset_name) == 0
        },
      )
    utils.has_signed(certificate.owner, tx.extra_signatories)
  }

  else(_) {
    fail
  }
}
