import {
  computeHash32,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
} from "@al-ft/midgard-core";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "@/index.js";

import {
  acceptedVerdictSubject,
  DoubleSpendStep01Datum,
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  DoubleSpendStep02SpendRedeemer,
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
  EMPTY_SPEND_INPUTS_HASH,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  InvalidRangeStep01Datum,
  InvalidRangeStep01SpendRedeemer,
  InvalidRangeStep02Datum,
  InvalidRangeStep02SpendRedeemer,
  invalidRangeViolationReason,
  MidgardTxInputList,
  NativeTxBodyCompact,
  nativeTxBodyHasZeroInputViolation,
  NormalizedTimeRange,
  normalizeNativeTxValidityRange,
  ZeroInputStep01Datum,
  ZeroInputStep01SpendRedeemer,
  ZeroInputStep02Datum,
  ZeroInputStep02SpendRedeemer,
} from "../src/index.js";
import {
  doubleSpentInput,
  h28,
  h32,
  h32b,
  nativeTxBody,
  proof,
  roundTrip,
  spendInputs,
  txInclusionArgs,
} from "./fault-proof.publication-tx-overhead-bytes.js";

describe("fault-proof ABI", () => {
  it("round-trips computation-thread mint redeemers", () => {
    expect(
      roundTrip(
        {
          Init: {
            first_step_output_index: 0n,
            fraud_category_id: "00000000",
            fraud_category: h28,
            fraud_category_membership_proof: proof,
            fraud_proof_catalogue_ref_input_index: 1n,
            inclusion_proof_script_redeemer_index: 2n,
            hub_oracle_ref_input_index: 3n,
            fraudulent_block_ref_input_index: 4n,
          },
        },
        FraudProofComputationThreadRedeemer,
      ),
    ).toMatchObject({ Init: { fraud_category: h28 } });
    expect(
      roundTrip(
        { Success: { burning_token_asset_name: "abcd" } },
        FraudProofComputationThreadRedeemer,
      ),
    ).toEqual({ Success: { burning_token_asset_name: "abcd" } });
    expect(
      roundTrip(
        { BurnForCancellation: { burning_token_asset_name: "abcd" } },
        FraudProofComputationThreadRedeemer,
      ),
    ).toEqual({ BurnForCancellation: { burning_token_asset_name: "abcd" } });
  });

  it("round-trips fraud-proof token mint redeemer", () => {
    const redeemer = {
      computation_thread_token_asset_name: "00000000" + h28,
      computation_thread_mint_redeemer_index: 1n,
    };
    expect(roundTrip(redeemer, FraudProofTokenMintRedeemer)).toEqual(redeemer);
  });

  it("round-trips double-spend step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, DoubleSpendStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        DoubleSpendStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        verified_tx1_id: h32,
      },
    };
    expect(roundTrip(step02Datum, DoubleSpendStep02Datum)).toEqual(step02Datum);
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        DoubleSpendStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        verified_tx1_id: h32,
        verified_tx2_id: h32b,
      },
    };
    expect(roundTrip(step03Datum, DoubleSpendStep03Datum)).toEqual(step03Datum);
    expect(roundTrip(spendInputs, MidgardTxInputList)).toEqual(spendInputs);

    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              tx1_spend_inputs_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "a1b2c3",
                  carriage: { RawUtxo: { ref_input_index: 1n } },
                },
              },
              double_spent_input_index: 0n,
            },
          ],
        },
        DoubleSpendStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          tx1_spend_inputs_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "a1b2c3",
              carriage: { RawUtxo: { ref_input_index: 1n } },
            },
          },
        },
      ],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        verified_tx2_id: h32b,
        double_spent_input: doubleSpentInput,
      },
    };
    expect(roundTrip(step04Datum, DoubleSpendStep04Datum)).toEqual(step04Datum);
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 1n,
              tx2_spend_inputs_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "c3b2a1",
                  carriage: { RawUtxo: { ref_input_index: 2n } },
                },
              },
              double_spent_input_index: 0n,
            },
          ],
        },
        DoubleSpendStep04SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ fraud_proof_mint_redeemer_index: 1n }],
    });
  });

  it("round-trips invalid-range step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, InvalidRangeStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip(
        {
          Continue: [
            {
              source: {
                AcceptedSource: {
                  inclusion: { RedeemerCarriedInclusion: [txInclusionArgs] },
                },
              },
            },
          ],
        },
        InvalidRangeStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          source: {
            AcceptedSource: {
              inclusion: { RedeemerCarriedInclusion: [{ native_tx_id: h32 }] },
            },
          },
        },
      ],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        subject: acceptedVerdictSubject(h32),
        block_slot: 10n,
        bad_tx_normalized_validity_range: {
          ClosedRange: { lower: 11n, upper: 19n },
        },
      },
    };
    expect(roundTrip(step02Datum, InvalidRangeStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 1n,
            },
          ],
        },
        InvalidRangeStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ fraud_proof_mint_redeemer_index: 1n }],
    });
  });

  it("detects an invalid address witness and leaves valid ones alone", () => {
    const txId = "ab".repeat(32);
    const signingKey = CML.PrivateKey.generate_ed25519();
    const verificationKey = Buffer.from(
      signingKey.to_public().to_raw_bytes(),
    ).toString("hex");
    const goodSignature = Buffer.from(
      signingKey.sign(Buffer.from(txId, "hex")).to_raw_bytes(),
    ).toString("hex");

    const goodWitness = {
      verification_key: verificationKey,
      signature: goodSignature,
    };
    // Flip the leading byte of the signature: still structurally a 64-byte
    // Ed25519 signature, but it no longer verifies against the tx id.
    const badWitness = {
      verification_key: verificationKey,
      signature:
        (goodSignature.startsWith("00") ? "11" : "00") + goodSignature.slice(2),
    };

    expect(
      SDK.findInvalidAddressWitnessIndex({ txId, addrTxWits: [goodWitness] }),
    ).toBeNull();
    expect(
      SDK.nativeTxHasInvalidSignatureViolation({
        txId,
        addrTxWits: [goodWitness],
      }),
    ).toBe(false);
    expect(
      SDK.findInvalidAddressWitnessIndex({
        txId,
        addrTxWits: [goodWitness, badWitness],
      }),
    ).toBe(1);
    // A witness that verifies against a *different* message is still invalid
    // for this transaction.
    expect(
      SDK.findInvalidAddressWitnessIndex({
        txId: "cd".repeat(32),
        addrTxWits: [goodWitness],
      }),
    ).toBe(0);
  });

  it("round-trips the address-witness preimage the node commits to", () => {
    const witnesses = [
      { verification_key: "aa".repeat(32), signature: "bb".repeat(64) },
      { verification_key: "cc".repeat(32), signature: "dd".repeat(64) },
    ];
    // The node stores field 7 as a CBOR array of raw per-witness
    // `[vkey, signature]` encodings; that is what the on-chain
    // `encode_midgard_address_witness` reproduces per item.
    const preimageCbor = encodeCbor(
      witnesses.map((witness) =>
        encodeCbor([
          Buffer.from(witness.verification_key, "hex"),
          Buffer.from(witness.signature, "hex"),
        ]),
      ),
    );

    expect(SDK.decodeAddressWitnessPreimage(preimageCbor)).toEqual(witnesses);
    expect(SDK.encodeAddressWitnessPreimage(witnesses)).toEqual(preimageCbor);
    // Malformed witness lengths are rejected, matching the on-chain
    // `expect bytearray.length(...) == 32 / 64`.
    expect(() =>
      SDK.encodeMidgardAddressWitnessCanonical({
        verification_key: "aa".repeat(31),
        signature: "bb".repeat(64),
      }),
    ).toThrow("must be 32 bytes");
  });

  it("commits the address witnesses as the §4 flat hash of their §5.1 preimage", () => {
    const witnesses = [
      { verification_key: "aa".repeat(32), signature: "bb".repeat(64) },
      { verification_key: "cc".repeat(32), signature: "dd".repeat(64) },
    ];
    const items = witnesses.map((witness) =>
      encodeMidgardAddressWitnessItem({
        verificationKey: Buffer.from(witness.verification_key, "hex"),
        signature: Buffer.from(witness.signature, "hex"),
      }),
    );
    // Twin of `native_tx_field_access_v1.field_commitment(encode_address_witness_preimage(...))`.
    expect(SDK.invalidSignatureAddressWitnessesCommitment(witnesses)).toBe(
      midgardFieldCommitmentFromItems(items).toString("hex"),
    );
    // The commitment is over the assembled preimage bytes and nothing else:
    // envelope-then-hash and hash-of-envelope are the same value, and no field
    // index enters either. §4 is plain hashing, so the field index is **not**
    // load-bearing here — the retired counted scheme salted each item leaf with
    // it, and this test used to assert the resulting inequality against field 0.
    // What separates the fields now is positional (§4's positional-identity
    // invariant): step-01 takes its expected hash from
    // `witness_set.addr_tx_wits_hash` in the committed compact structure.
    expect(midgardFieldCommitmentFromItems(items)).toEqual(
      midgardFieldCommitment(encodeMidgardFieldPreimage(items)),
    );
  });

  it("recomputes the witness set hash step 01 opens", () => {
    const witnessSet = {
      addr_tx_wits_hash: h32,
      script_tx_wits_hash: h32b,
      redeemer_tx_wits_hash: "77".repeat(32),
    };
    // Mirrors `blake2b_256(encode_native_tx_witness_set_compact(...))` as the
    // on-chain §8.8 field-access door computes it, in positional order. The
    // standalone `verify_native_tx_witness_set` helper was deleted by #575; the
    // check now lives inside `authenticated_field_view`.
    expect(SDK.invalidSignatureWitnessSetCommitment(witnessSet)).toBe(
      computeHash32(
        encodeCbor([
          Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
          Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
          Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
        ]),
      ).toString("hex"),
    );
  });

  it("round-trips invalid-signature step datums and redeemers", () => {
    expect(
      roundTrip(
        { fraud_prover: h28, data: null },
        SDK.InvalidSignatureStep01Datum,
      ),
    ).toEqual({ fraud_prover: h28, data: null });

    // #575 collapsed step-01's arguments to the shared inclusion carriage, and
    // #604 followed it off-chain. The witness-set *preimage*
    // no longer travels here: step-02 opens field 7 through the §8.8 door and
    // re-derives it from the prover's carriage. What step-01 still owes the
    // thread is the witness-set *hash*, and that goes into `step_02.State`
    // below, not into these arguments.
    expect(
      roundTrip(
        {
          Continue: [
            {
              source: {
                AcceptedSource: {
                  inclusion: { RedeemerCarriedInclusion: [txInclusionArgs] },
                },
              },
            },
          ],
        },
        SDK.InvalidSignatureStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          source: {
            AcceptedSource: {
              inclusion: { RedeemerCarriedInclusion: [{ native_tx_id: h32 }] },
            },
          },
        },
      ],
    });
    // The retired two-field wrapper must not encode any more. Left as an
    // explicit negative because emitting it produced a redeemer the validator
    // decoded positionally into the wrong fields rather than refusing —
    // `Spend[0] unexpected empty list`, which reads like a fixture defect.
    expect(() =>
      Data.to(
        {
          Continue: [
            {
              tx_inclusion_args: txInclusionArgs,
              bad_tx_witness_set_compact: {
                addr_tx_wits_hash: h32,
                script_tx_wits_hash: h32b,
                redeemer_tx_wits_hash: "77".repeat(32),
              },
            },
          ],
        } as never,
        SDK.InvalidSignatureStep01SpendRedeemer,
      ),
    ).toThrow();

    const step02Datum = {
      fraud_prover: h28,
      data: {
        subject: SDK.acceptedVerdictSubject(h32),
        bad_tx_witness_set_hash: h32b,
      },
    };
    expect(roundTrip(step02Datum, SDK.InvalidSignatureStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      SDK.invalidSignatureStep02StateFromBadTx({
        badTxId: h32.toUpperCase(),
        badTxWitnessSetHash: h32b.toUpperCase(),
      }),
    ).toEqual(step02Datum.data);

    const step02Args = {
      input_index: 0n,
      output_index: 0n,
      fraud_proof_mint_redeemer_index: 1n,
      // Field 7 is a witness-set field, so the opening is the `WitnessFieldOpening`
      // arm and carries the transaction's compact witness set. Tier 3 is
      // admissible for this arm since #606's welded-hash repair, which is
      // asserted below.
      addr_tx_wits_opening: {
        WitnessFieldOpening: {
          native_tx_compact_cbor: "a1b2c3",
          witness_set: {
            addr_tx_wits_hash: h32,
            script_tx_wits_hash: h32b,
            redeemer_tx_wits_hash: h32,
          },
          carriage: { Inline: { preimage: "80" } },
        },
      },
      bad_addr_tx_wit_index: 1n,
    };
    expect(
      roundTrip(
        { Continue: [step02Args] },
        SDK.InvalidSignatureStep02SpendRedeemer,
      ),
    ).toEqual({ Continue: [step02Args] });

    // The witness-set family is where E2 limit 3 used to bite, so the lifted
    // refusal (#606) is asserted at the family rather than only in the shared
    // module's tests: a tier-3 opening of field 7 is emitted, not thrown.
    expect(
      SDK.fieldOpeningForField({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX.addressWitnesses,
        nativeTxCompactCbor: "a1b2c3",
        carriage: {
          Certified: {
            cert_ref_input_index: 0n,
            chunk_ref_input_indices: [1n],
          },
        },
        witnessSet: {
          addr_tx_wits_hash: h32,
          script_tx_wits_hash: h32b,
          redeemer_tx_wits_hash: h32,
        },
      }),
    ).toEqual({
      WitnessFieldOpening: {
        native_tx_compact_cbor: "a1b2c3",
        witness_set: {
          addr_tx_wits_hash: h32,
          script_tx_wits_hash: h32b,
          redeemer_tx_wits_hash: h32,
        },
        carriage: {
          Certified: {
            cert_ref_input_index: 0n,
            chunk_ref_input_indices: [1n],
          },
        },
      },
    });
  });

  it("round-trips zero-input step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, ZeroInputStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip(
        {
          Continue: [
            {
              source: {
                AcceptedSource: {
                  inclusion: { RedeemerCarriedInclusion: [txInclusionArgs] },
                },
              },
            },
          ],
        },
        ZeroInputStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          source: {
            AcceptedSource: {
              inclusion: { RedeemerCarriedInclusion: [{ native_tx_id: h32 }] },
            },
          },
        },
      ],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: { subject: acceptedVerdictSubject(h32) },
    };
    expect(roundTrip(step02Datum, ZeroInputStep02Datum)).toEqual(step02Datum);
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 1n,
              // §5.1's empty field is exactly one byte, so a genuinely empty
              // field 0 always fits tier 1.
              spend_inputs_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "a1b2c3",
                  carriage: { Inline: { preimage: "80" } },
                },
              },
            },
          ],
        },
        ZeroInputStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ fraud_proof_mint_redeemer_index: 1n }],
    });
  });

  it("round-trips input-no-idx step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, SDK.InputNoIdxStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        SDK.InputNoIdxStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    // #604: the thread carries the §2.5 anchor, and the step redeemer carries a
    // `FieldOpeningV1` rather than a reproduced `inputs_preimage`. The retired
    // `Direct`/`Folding` state, the `PublishedSpendInputsV1` publication datum
    // and the four-arm `Complete`/`CompletePublished`/`FoldStart`/`FoldNext`
    // redeemer are gone from the validator, so their round-trips are gone here.
    const step02Datum = {
      fraud_prover: h28,
      data: { verified_tx_id: h32 },
    };
    expect(roundTrip(step02Datum, SDK.InputNoIdxStep02Datum)).toEqual(
      step02Datum,
    );
    expect(SDK.inputNoIdxStep02StateFromBadTx(h32)).toEqual({
      verified_tx_id: h32,
    });

    const spendInputsOpening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: "a1b2c3",
        carriage: { Inline: { preimage: "80" } },
      },
    };
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              spend_inputs_opening: spendInputsOpening,
              bad_inputs_index: 0n,
            },
          ],
        },
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toEqual({
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          spend_inputs_opening: spendInputsOpening,
          bad_inputs_index: 0n,
        },
      ],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: { bad_input_tx_id: h32b, bad_input_output_index: 7n },
    };
    expect(roundTrip(step03Datum, SDK.InputNoIdxStep03Datum)).toEqual(
      step03Datum,
    );
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        SDK.InputNoIdxStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: { producing_tx_id: h32, bad_input_output_index: 7n },
    };
    expect(roundTrip(step04Datum, SDK.InputNoIdxStep04Datum)).toEqual(
      step04Datum,
    );
    const outputsOpening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: "c3b2a1",
        carriage: { RawUtxo: { ref_input_index: 2n } },
      },
    };
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 1n,
              outputs_opening: outputsOpening,
            },
          ],
        },
        SDK.InputNoIdxStep04SpendRedeemer,
      ),
    ).toEqual({
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 1n,
          outputs_opening: outputsOpening,
        },
      ],
    });
  });

  it("round-trips reference-input-no-idx step datums and redeemers", () => {
    const referenceInputs = [
      { tx_id: h32b, output_index: 0n },
      { tx_id: h32, output_index: 5n },
    ];
    const badReferenceInput = referenceInputs[1]!;

    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        SDK.ReferenceInputNoIdxStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: { verified_tx_id: h32b },
    };
    expect(roundTrip(step02Datum, SDK.ReferenceInputNoIdxStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              reference_inputs_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "a1b2c3",
                  carriage: { Inline: { preimage: "80" } },
                },
              },
              bad_reference_input_index: 1n,
            },
          ],
        },
        SDK.ReferenceInputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          reference_inputs_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "a1b2c3",
              carriage: { Inline: { preimage: "80" } },
            },
          },
          bad_reference_input_index: 1n,
        },
      ],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        bad_reference_input_tx_id: badReferenceInput.tx_id,
        bad_reference_input_output_index: badReferenceInput.output_index,
      },
    };
    expect(roundTrip(step03Datum, SDK.ReferenceInputNoIdxStep03Datum)).toEqual(
      step03Datum,
    );
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        SDK.ReferenceInputNoIdxStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        producing_tx_id: h32b,
        bad_reference_input_output_index: badReferenceInput.output_index,
      },
    };
    expect(roundTrip(step04Datum, SDK.ReferenceInputNoIdxStep04Datum)).toEqual(
      step04Datum,
    );
    // Step 04 carries the producing tx's outputs as structured `MidgardTxOutput`
    // PlutusData, because the on-chain step re-encodes each item with
    // `encode_midgard_tx_output` before re-committing under field 2.
    const referenceOutput: SDK.MidgardTxOutput = {
      address: {
        protected: false,
        network_id: 0n,
        payment_credential: { PubKeyCredential: [h28] },
        stake_credential: null,
      },
      value: { lovelace: 5_000_000n, assets: new Map<string, bigint>() },
      datum_cbor: null,
      script_ref: null,
    };
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 2n,
              outputs_opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "c3b2a1",
                  carriage: { Inline: { preimage: "80" } },
                },
              },
            },
          ],
        },
        SDK.ReferenceInputNoIdxStep04SpendRedeemer,
      ),
    ).toEqual({
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 2n,
          outputs_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "c3b2a1",
              carriage: { Inline: { preimage: "80" } },
            },
          },
        },
      ],
    });

    // §4 puts no field index in the hash input, so fields 0 and 1 — which share
    // the §5.3 item encoder — commit identical content to the same value. The
    // retired counted scheme separated them by salting each leaf with its field
    // index, and this assertion used to pin that inequality. Substitution is
    // prevented positionally instead: each step reads its expected hash out of the
    // committed compact structure (`body.reference_inputs_hash` versus
    // `body.spend_inputs_hash`), which is §4's positional-identity invariant.
    expect(
      SDK.referenceInputNoIdxReferenceInputsCommitment(referenceInputs),
    ).toBe(SDK.inputNoIdxSpendInputsCommitment(referenceInputs));
    expect(SDK.referenceInputNoIdxOutputsCommitment([referenceOutput])).toBe(
      SDK.inputNoIdxOutputsCommitment([referenceOutput]),
    );
  });

  it("pins the rebound input-no-idx step-02 wire ABI", () => {
    // The regression this exists for is subtler than an arity change, and it is
    // why the stale builders failed as `Spend[0] the validator crashed` rather
    // than as a clean decode error: the retired four-arm enum's `Complete` arm
    // sat at tag 0 with arity 4, and the rebound flat `Args` record is ALSO tag
    // 0 with arity 4. Only field 2 moved — a `List<MidgardTxInput>` became a
    // `FieldOpeningV1` constructor. A test that checked tag and arity alone
    // would pass against a builder that is still completely wrong, so the
    // assertions below pin the *shape of field 2*.
    const opening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: "a1b2c3",
        carriage: { Inline: { preimage: "80" } },
      },
    };
    const args = {
      input_index: 0n,
      output_index: 0n,
      spend_inputs_opening: opening,
      bad_inputs_index: 0n,
    };
    const redeemer = { Continue: [args] };
    const cbor = Data.to(redeemer as never, SDK.InputNoIdxStep02SpendRedeemer);
    const outer = Data.from(cbor);

    expect(outer).toBeInstanceOf(Constr);
    const continueConstr = outer as Constr<unknown>;
    expect(continueConstr.index).toBe(1);
    expect(continueConstr.fields).toHaveLength(1);
    const argsConstr = continueConstr.fields[0] as Constr<unknown>;
    expect(argsConstr).toBeInstanceOf(Constr);
    // A flat record, not a sum: tag 0 because that is what a single-constructor
    // Aiken record encodes to, and four fields in declaration order.
    expect(argsConstr.index).toBe(0);
    expect(argsConstr.fields).toHaveLength(4);
    expect(typeof argsConstr.fields[0]).toBe("bigint");
    expect(typeof argsConstr.fields[1]).toBe("bigint");
    expect(typeof argsConstr.fields[3]).toBe("bigint");

    // Field 2 is the whole of the #575 divergence. Under the retired scheme it
    // was a *list* of inputs; it is now a `FieldOpeningV1` constructor whose own
    // field 1 is a `FieldCarriageV1` constructor. Asserting `not an array` is
    // what makes a re-stalened builder fail here instead of at a validator.
    const openingConstr = argsConstr.fields[2];
    expect(Array.isArray(openingConstr)).toBe(false);
    expect(openingConstr).toBeInstanceOf(Constr);
    const bodyOpening = openingConstr as Constr<unknown>;
    expect(bodyOpening.index).toBe(0);
    expect(bodyOpening.fields).toHaveLength(2);
    expect(bodyOpening.fields[0]).toBe("a1b2c3");
    const carriage = bodyOpening.fields[1] as Constr<unknown>;
    expect(carriage).toBeInstanceOf(Constr);
    expect(carriage.index).toBe(0);
    expect(carriage.fields).toEqual(["80"]);

    expect(cbor).toBe("d87a9fd8799f0000d8799f43a1b2c3d8799f4180ffff00ffff");
    expect(Data.from(cbor, SDK.InputNoIdxStep02SpendRedeemer)).toEqual(
      redeemer,
    );

    const fields = [...argsConstr.fields];
    const invalid = [
      [
        "retired Complete arm: a reproduced input list where the opening goes",
        new Constr(1, [
          new Constr(0, [0n, 0n, [new Constr(0, [h32b, 7n])], 0n]),
        ]),
      ],
      [
        "obsolete nested CompleteArgs wrapper",
        new Constr(1, [new Constr(0, [new Constr(0, fields)])]),
      ],
      [
        "args under an adjacent tag the flat record does not have",
        new Constr(1, [new Constr(1, fields)]),
      ],
      ["args wrong arity", new Constr(1, [new Constr(0, fields.slice(0, 3))])],
      ["Continue wrong arity", new Constr(1, [])],
    ] as const;

    for (const [label, malformed] of invalid) {
      const malformedCbor = Data.to(malformed as never);
      expect(
        () => Data.from(malformedCbor, SDK.InputNoIdxStep02SpendRedeemer),
        label,
      ).toThrow();
    }

    // The §2.5 pairing is deliberately NOT a decode-time property, and that is
    // worth pinning rather than assuming: `WitnessFieldOpening` is a legitimate
    // arm of `FieldOpeningV1`, so a witness opening aimed at a body field
    // decodes cleanly and is refused later — off-chain by
    // `fieldOpeningForField`, on-chain by `field_pairs_with`. A reader who
    // expected the schema to catch it (this test's first draft did) would
    // otherwise conclude the guard was somewhere it is not.
    const witnessOpeningAtBodyField = new Constr(1, [
      new Constr(0, [
        0n,
        0n,
        new Constr(1, [
          "a1b2c3",
          new Constr(0, [h32, h32b, h32]),
          new Constr(0, ["80"]),
        ]),
        0n,
      ]),
    ]);
    expect(() =>
      Data.from(
        Data.to(witnessOpeningAtBodyField as never),
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).not.toThrow();
    expect(() =>
      SDK.fieldOpeningForField({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX.spendInputs,
        nativeTxCompactCbor: "a1b2c3",
        carriage: { Inline: { preimage: "80" } },
        witnessSet: {
          addr_tx_wits_hash: h32,
          script_tx_wits_hash: h32b,
          redeemer_tx_wits_hash: h32,
        },
      }),
    ).toThrow(SDK.MidgardFieldOpeningError);
  });

  it("detects an input-no-idx violation from the producing outputs count", () => {
    expect(
      SDK.isInputNoIdxViolation({
        badInputOutputIndex: 7n,
        producingTxOutputCount: 1,
      }),
    ).toBe(true);
    // A valid block: the spent index exists in its producing transaction.
    expect(
      SDK.isInputNoIdxViolation({
        badInputOutputIndex: 0n,
        producingTxOutputCount: 1,
      }),
    ).toBe(false);
    const evidence = SDK.inputNoIdxEvidenceFromCommittedTransactions({
      badTxId: h32,
      badInputsIndex: 0,
      badInput: { tx_id: h32b, output_index: 7n },
      producingTxOutputCount: 1,
    });
    expect(evidence.violationId).toBe(SDK.INPUT_NO_IDX_VIOLATION_ID);
    expect(evidence.producingTxId).toBe(h32b);
    expect(evidence.isViolation).toBe(true);
    expect(SDK.inputNoIdxStep03StateFromEvidence(evidence)).toEqual({
      bad_input_tx_id: h32b,
      bad_input_output_index: 7n,
    });
    expect(
      SDK.inputNoIdxStep04StateFromEvidence({
        evidence,
        producingTxId: h32,
      }),
    ).toEqual({
      producing_tx_id: h32,
      bad_input_output_index: 7n,
    });
  });

  it("detects a zero-input violation from the native spend-inputs hash", () => {
    // §4's flat commitment of the empty §5.1 field — `blake2b_256(#"80")` — which
    // is what `fraud_proofs/zero_input/step_02` pins as
    // `native_tx_field_access_v1.empty_field_commitment`. It carries no field
    // index, so it is the empty commitment of all nine fields, not of field 0
    // alone (§4's positional identity).
    expect(EMPTY_SPEND_INPUTS_HASH).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
    expect(
      nativeTxBodyHasZeroInputViolation({
        txBody: {
          ...nativeTxBody,
          spend_inputs_hash: EMPTY_SPEND_INPUTS_HASH,
        },
      }),
    ).toBe(true);
    expect(
      nativeTxBodyHasZeroInputViolation({
        txBody: { ...nativeTxBody, spend_inputs_hash: h32b },
      }),
    ).toBe(false);
  });

  it("normalizes native invalid-range validity bounds", () => {
    expect(
      roundTrip(
        normalizeNativeTxValidityRange(nativeTxBody),
        NormalizedTimeRange,
      ),
    ).toBe("Always");
    expect(
      normalizeNativeTxValidityRange({
        ...nativeTxBody,
        validity_interval_end: 20n,
      }),
    ).toEqual({ FromNegInf: { upper: 19n } });
    expect(
      normalizeNativeTxValidityRange({
        ...nativeTxBody,
        validity_interval_start: 10n,
      }),
    ).toEqual({ ToPosInf: { lower: 10n } });
    expect(
      normalizeNativeTxValidityRange({
        ...nativeTxBody,
        validity_interval_start: 10n,
        validity_interval_end: 11n,
      }),
    ).toEqual({ ClosedRange: { lower: 10n, upper: 10n } });
    expect(
      normalizeNativeTxValidityRange({
        ...nativeTxBody,
        validity_interval_start: 10n,
        validity_interval_end: 21n,
      }),
    ).toEqual({ ClosedRange: { lower: 10n, upper: 20n } });
    expect(
      normalizeNativeTxValidityRange({
        ...nativeTxBody,
        validity_interval_start: 10n,
        validity_interval_end: 10n,
      }),
    ).toBe("InvalidRange");
    expect(roundTrip(nativeTxBody, NativeTxBodyCompact)).toEqual(nativeTxBody);
  });

  it("classifies invalid-range violations with validator boundary semantics", () => {
    const classify = (normalizedRange: NormalizedTimeRange) =>
      invalidRangeViolationReason({
        blockSlot: 10n,
        normalizedRange,
      });

    expect(classify("Always")).toBeNull();
    expect(classify("InvalidRange")).toBe("invalid-range");
    expect(classify({ ClosedRange: { lower: 10n, upper: 10n } })).toBeNull();
    expect(classify({ ClosedRange: { lower: 11n, upper: 19n } })).toBe(
      "starts-after-block-slot",
    );
    expect(classify({ ClosedRange: { lower: 1n, upper: 9n } })).toBe(
      "ends-before-block-slot",
    );
    expect(classify({ FromNegInf: { upper: 10n } })).toBeNull();
    expect(classify({ FromNegInf: { upper: 9n } })).toBe(
      "ends-before-block-slot",
    );
    expect(classify({ ToPosInf: { lower: 10n } })).toBeNull();
    expect(classify({ ToPosInf: { lower: 11n } })).toBe(
      "starts-after-block-slot",
    );
  });
});
