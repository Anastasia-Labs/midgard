import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeHash32,
  encodeCbor,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardFieldPreimageV1,
  midgardFieldCommitmentFromItemsV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  applyParamsToScript,
  CML,
  Constr,
  Data,
  type SpendingValidator as LucidSpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as SDK from "@/index.js";

import {
  AddressData,
  addressDataFromBech32,
  buildDoubleSpendFaultProofContracts,
  buildFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
  deriveValidationTraceDeploymentIdV1,
  DOUBLE_SPEND_FAULT_PROOF_TITLES,
  DoubleSpendStep01Datum,
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  DoubleSpendStep02SpendRedeemer,
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
  EMPTY_SPEND_INPUTS_HASH,
  FAULT_PROOF_SHARED_TITLES,
  type FaultProofBlueprint,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  INVALID_RANGE_FAULT_PROOF_TITLES,
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
  parseFaultProofBlueprint,
  type Proof,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_RESOLVER_COUNT_V1,
  ZERO_INPUT_FAULT_PROOF_TITLES,
  ZeroInputStep01Datum,
  ZeroInputStep01SpendRedeemer,
  ZeroInputStep02Datum,
  ZeroInputStep02SpendRedeemer,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const currentTreeBlueprintPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
const blueprintPath =
  currentTreeBlueprintPath ?? resolve(repoRoot, "onchain/aiken/plutus.json");

const h32 = "00".repeat(32);
const h32b = "11".repeat(32);
const h28 = "22".repeat(28);
const h28b = "33".repeat(28);
const h28c = "44".repeat(28);
const nativeTxBody = {
  spend_inputs_hash: h32,
  reference_inputs_hash: h32,
  outputs_hash: h32,
  fee: 0n,
  validity_interval_start: -1n,
  validity_interval_end: -1n,
  required_observers_hash: h32,
  required_signers_hash: h32,
  mint_hash: h32,
  script_integrity_hash: h32,
  auxiliary_data_hash: h32,
  network_id: 0n,
};

const proof: Proof = [];
const nativeTxCompactCbor = "840182005820" + "55".repeat(32) + "00";
const spendInputs = [
  { tx_id: "aa".repeat(32), output_index: 0n },
  { tx_id: "bb".repeat(32), output_index: 1n },
];
const doubleSpentInput = spendInputs[0]!;
const txInclusionArgs = {
  input_index: 0n,
  output_index: 0n,
  hub_ref_input_index: 1n,
  state_queue_node_ref_input_index: 2n,
  native_tx_id: h32,
  native_tx_compact_cbor: nativeTxCompactCbor,
  transactions_phas_root: h32,
  tx_membership_proof: proof,
  inclusion_proof_script_withdraw_redeemer_index: 3n,
};

const roundTrip = <A>(value: A, schema: Parameters<typeof Data.to>[1]): A =>
  Data.from(Data.to(value, schema), schema) as A;

const loadBlueprint = (): FaultProofBlueprint =>
  parseFaultProofBlueprint(
    JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
  );

const filterBlueprint = (
  blueprint: FaultProofBlueprint,
  titles: readonly string[],
): FaultProofBlueprint => {
  const titleSet = new Set(titles);
  return {
    validators: blueprint.validators.filter((validator) =>
      titleSet.has(validator.title),
    ),
  };
};

const compiledScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) {
    throw new Error(`Missing validator ${title}`);
  }
  return validator.compiledCode;
};

const spendingScript = (script: string): LucidSpendingValidator => ({
  type: "PlutusV3",
  script,
});

const spendingScriptHash = (script: string): string =>
  validatorToScriptHash(spendingScript(script));

/**
 * The §8.6 certificate policy id, derived here from the blueprint rather than
 * read off the builder's output, so these expectations stay an independent
 * re-derivation. The certificate validator declares no parameters, so its
 * compiled script is its deployed script and the script hash of that script is
 * the policy id.
 */
const certificatePolicyId = (blueprint: FaultProofBlueprint): string =>
  spendingScriptHash(
    compiledScript(
      blueprint,
      SDK.FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    ),
  );

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
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        InvalidRangeStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        block_valid_from: 10n,
        block_valid_to: 20n,
        bad_tx_normalized_validity_range: {
          ClosedRange: { lower: 9n, upper: 19n },
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
      SDK.encodeMidgardAddressWitnessCanonicalV1({
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
      encodeMidgardAddressWitnessItemV1({
        verificationKey: Buffer.from(witness.verification_key, "hex"),
        signature: Buffer.from(witness.signature, "hex"),
      }),
    );
    // Twin of `native_tx_field_access_v1.field_commitment(encode_address_witness_preimage(...))`.
    expect(SDK.invalidSignatureAddressWitnessesCommitmentV1(witnesses)).toBe(
      midgardFieldCommitmentFromItemsV1(items).toString("hex"),
    );
    // The commitment is over the assembled preimage bytes and nothing else:
    // envelope-then-hash and hash-of-envelope are the same value, and no field
    // index enters either. §4 is plain hashing, so the field index is **not**
    // load-bearing here — the retired counted scheme salted each item leaf with
    // it, and this test used to assert the resulting inequality against field 0.
    // What separates the fields now is positional (§4's positional-identity
    // invariant): step-01 takes its expected hash from
    // `witness_set.addr_tx_wits_hash` in the committed compact structure.
    expect(midgardFieldCommitmentFromItemsV1(items)).toEqual(
      midgardFieldCommitmentV1(encodeMidgardFieldPreimageV1(items)),
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
    expect(SDK.invalidSignatureWitnessSetCommitmentV1(witnessSet)).toBe(
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

    // #575 collapsed step-01's arguments to a bare `NativeTxInclusionArgs`
    // (`2fec6b0fb`), and #604 followed it off-chain. The witness-set *preimage*
    // no longer travels here: step-02 opens field 7 through the §8.8 door and
    // re-derives it from the prover's carriage. What step-01 still owes the
    // thread is the witness-set *hash*, and that goes into `step_02.State`
    // below, not into these arguments.
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        SDK.InvalidSignatureStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ native_tx_id: h32 }],
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
      data: { bad_tx_id: h32, bad_tx_witness_set_hash: h32b },
    };
    expect(roundTrip(step02Datum, SDK.InvalidSignatureStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      SDK.invalidSignatureStep02StateFromBadTxV1({
        badTxId: h32.toUpperCase(),
        badTxWitnessSetHash: h32b.toUpperCase(),
      }),
    ).toEqual(step02Datum.data);

    const step02Args = {
      input_index: 0n,
      output_index: 0n,
      // Field 7 is a witness-set field, so the opening is the `WitnessFieldOpening`
      // arm and carries the transaction's compact witness set. Tier 3 is refused
      // for this arm (§8.3 erratum E2 limit 3), which is asserted below.
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
      fraud_proof_mint_redeemer_index: 1n,
    };
    expect(
      roundTrip(
        { Continue: [step02Args] },
        SDK.InvalidSignatureStep02SpendRedeemer,
      ),
    ).toEqual({ Continue: [step02Args] });

    // The witness-set family is where E2 limit 3 actually bites, so the refusal
    // is asserted at the family rather than only in the shared module's tests.
    expect(() =>
      SDK.fieldOpeningV1ForField({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.addressWitnesses,
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
    ).toThrow(/erratum E2 limit 3/u);
  });

  it("round-trips zero-input step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, ZeroInputStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip(
        { Continue: [{ RedeemerCarriedInclusion: [txInclusionArgs] }] },
        ZeroInputStep01SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ RedeemerCarriedInclusion: [{ native_tx_id: h32 }] }],
    });

    const step02Datum = {
      fraud_prover: h28,
      data: { bad_tx_id: h32 },
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
        { Continue: [txInclusionArgs] },
        SDK.InputNoIdxStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

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
    expect(SDK.inputNoIdxStep02StateFromBadTxV1(h32)).toEqual({
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
        { Continue: [txInclusionArgs] },
        SDK.InputNoIdxStep03SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

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
        { Continue: [txInclusionArgs] },
        SDK.ReferenceInputNoIdxStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

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
        { Continue: [txInclusionArgs] },
        SDK.ReferenceInputNoIdxStep03SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

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
      SDK.referenceInputNoIdxReferenceInputsCommitmentV1(referenceInputs),
    ).toBe(SDK.inputNoIdxSpendInputsCommitmentV1(referenceInputs));
    expect(SDK.referenceInputNoIdxOutputsCommitmentV1([referenceOutput])).toBe(
      SDK.inputNoIdxOutputsCommitmentV1([referenceOutput]),
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
    // `fieldOpeningV1ForField`, on-chain by `field_pairs_with`. A reader who
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
      SDK.fieldOpeningV1ForField({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.spendInputs,
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
      SDK.isInputNoIdxViolationV1({
        badInputOutputIndex: 7n,
        producingTxOutputCount: 1,
      }),
    ).toBe(true);
    // A valid block: the spent index exists in its producing transaction.
    expect(
      SDK.isInputNoIdxViolationV1({
        badInputOutputIndex: 0n,
        producingTxOutputCount: 1,
      }),
    ).toBe(false);
    const evidence = SDK.inputNoIdxEvidenceFromCommittedTransactionsV1({
      badTxId: h32,
      badInputsIndex: 0,
      badInput: { tx_id: h32b, output_index: 7n },
      producingTxOutputCount: 1,
    });
    expect(evidence.violationId).toBe(SDK.INPUT_NO_IDX_VIOLATION_ID_V1);
    expect(evidence.producingTxId).toBe(h32b);
    expect(evidence.isViolation).toBe(true);
    expect(SDK.inputNoIdxStep03StateFromEvidenceV1(evidence)).toEqual({
      bad_input_tx_id: h32b,
      bad_input_output_index: 7n,
    });
    expect(
      SDK.inputNoIdxStep04StateFromEvidenceV1({
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
        blockValidFrom: 10n,
        blockValidTo: 20n,
        normalizedRange,
      });

    expect(classify("Always")).toBeNull();
    expect(classify("InvalidRange")).toBe("invalid-range");
    expect(classify({ ClosedRange: { lower: 10n, upper: 19n } })).toBeNull();
    expect(classify({ ClosedRange: { lower: 9n, upper: 19n } })).toBe(
      "lower-before-block",
    );
    expect(classify({ ClosedRange: { lower: 10n, upper: 20n } })).toBe(
      "upper-at-or-after-block",
    );
    expect(classify({ FromNegInf: { upper: 19n } })).toBeNull();
    expect(classify({ FromNegInf: { upper: 20n } })).toBe(
      "upper-at-or-after-block",
    );
    expect(classify({ ToPosInf: { lower: 10n } })).toBeNull();
    expect(classify({ ToPosInf: { lower: 9n } })).toBe("lower-before-block");
  });
});

describe("double-spend fault-proof contract builder", () => {
  it("builds four distinct ordered validators from the Aiken blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildDoubleSpendFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.doubleSpend.firstStep).toBe(
      contracts.doubleSpend.steps[0],
    );
    expect(contracts.doubleSpend.steps).toHaveLength(4);
    expect(
      new Set(
        contracts.doubleSpend.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(4);
  });

  it("does not require unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(DOUBLE_SPEND_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildDoubleSpendFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.doubleSpend.steps).toHaveLength(4);
  });
});

describe("fault-proof contract builder", () => {
  it("builds every implemented fault-proof chain from the Aiken blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.doubleSpend.steps).toHaveLength(4);
    expect(contracts.nonExistentInput.firstStep).toBe(
      contracts.nonExistentInput.steps[0],
    );
    expect(contracts.nonExistentInput.steps).toHaveLength(4);
    expect(contracts.nonExistentInputNoIndex.firstStep).toBe(
      contracts.nonExistentInputNoIndex.steps[0],
    );
    expect(contracts.nonExistentInputNoIndex.steps).toHaveLength(4);
    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
    // `reference_input_no_idx` is the reference-input mirror of `input_no_idx`.
    // Steps 01 and 02 genuinely differ — step 01 commits the bad tx's
    // reference-inputs hash instead of its spend-inputs hash, and step 02 opens
    // consensus field 1 with a flat `Args` rather than the spend side's
    // Complete/Published/Fold enum. Steps 03 and 04 differ only in record field
    // *names*, which PlutusData erases, so they compile to the same UPLC and the
    // two chains share those two scripts — exactly like the
    // `no_input`/`no_reference_input` pair. The threads stay distinguishable
    // because the computation-thread token asset name binds each thread to its
    // own category and block.
    expect(
      new Set([
        ...contracts.referenceInputNoIdx.steps.map(
          (step) => step.spendingScriptHash,
        ),
        ...contracts.nonExistentInputNoIndex.steps.map(
          (step) => step.spendingScriptHash,
        ),
      ]).size,
    ).toBe(6);
    expect(
      contracts.referenceInputNoIdx.steps
        .slice(2)
        .map((step) => step.spendingScriptHash),
    ).toEqual(
      contracts.nonExistentInputNoIndex.steps
        .slice(2)
        .map((step) => step.spendingScriptHash),
    );
    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(9);
    expect(contracts.validationTraceDispute.firstStep).toBe(
      contracts.validationTraceDispute.steps[0],
    );
    expect(contracts.validationTraceDispute.steps).toHaveLength(106);
    expect(contracts.validationTraceDispute.resolvers).toHaveLength(
      VALIDATION_TRACE_RESOLVER_COUNT_V1,
    );
    expect(
      new Set(
        [
          ...contracts.doubleSpend.steps,
          ...contracts.nonExistentInput.steps,
          ...contracts.nonExistentInputNoIndex.steps,
          ...contracts.referenceInputNoIdx.steps,
          ...contracts.invalidRange.steps,
          ...contracts.invalidSignature.steps,
          ...contracts.zeroInput.steps,
          ...contracts.transitionTrace.steps,
          ...contracts.validationTraceDispute.steps,
        ].map((step) => step.spendingScriptHash),
      ).size,
      // The split stage-one route contributes the envelope resolver plus five
      // internal stage hashes to the applied proof surface; the four
      // `reference_input_no_idx` steps add two more distinct hashes, since its
      // steps 03-04 are the same UPLC as `input_no_idx`'s; the two
      // `invalid_signature` steps are distinct from every other family.
    ).toBe(135);
  });

  it("builds invalid-range with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(
      new Set(
        contracts.invalidRange.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, INVALID_RANGE_FAULT_PROOF_TITLES.step02),
      [
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, INVALID_RANGE_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.invalidRange.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.invalidRange.steps[1].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep02Cbor),
    );
    expect(contracts.invalidRange.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.invalidRange.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.invalidRange.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds invalid-range without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(INVALID_RANGE_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
  });

  it("builds zero-input with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
    expect(
      new Set(contracts.zeroInput.steps.map((step) => step.spendingScriptHash))
        .size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step02),
      [
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        contracts.computationThread.policyId,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.zeroInput.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.zeroInput.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.zeroInput.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds zero-input without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(ZERO_INPUT_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
  });

  it("builds invalid-signature with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
    expect(
      new Set(
        contracts.invalidSignature.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(2);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    // Note the parameter order differs from zero-input/invalid-range: this
    // chain's final step takes the computation-thread policy first, matching
    // the aiken `validator main(...)` signature.
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES.step02,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES.step01,
      ),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.invalidSignature.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.invalidSignature.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.invalidSignature.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds invalid-signature without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(SDK.INVALID_SIGNATURE_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidSignature.firstStep).toBe(
      contracts.invalidSignature.steps[0],
    );
    expect(contracts.invalidSignature.steps).toHaveLength(2);
  });

  it("builds reference-input-no-idx with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
    expect(
      new Set(
        contracts.referenceInputNoIdx.steps.map(
          (step) => step.spendingScriptHash,
        ),
      ).size,
    ).toBe(4);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    // Same applied-parameter order as input-no-idx, taken from the blueprint.
    const expectedStep04Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep03Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
      ),
      [
        spendingScriptHash(expectedStep04Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
      ),
      [
        spendingScriptHash(expectedStep03Cbor),
        contracts.computationThread.policyId,
        certificatePolicyId(blueprint),
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
      ),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.referenceInputNoIdx.steps[3].spendingScriptCBOR).toBe(
      expectedStep04Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[2].spendingScriptCBOR).toBe(
      expectedStep03Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.referenceInputNoIdx.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds reference-input-no-idx without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(SDK.REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
  });

  it("builds transition-trace with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(9);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const finalNames = [
      "control",
      "source",
      "withdrawal",
      "forced",
      "accepted",
      "deposit",
      "l1Event",
      "duplicate",
    ] as const;
    const expectedFinalCbors = finalNames.map((name) =>
      applyParamsToScript(
        compiledScript(blueprint, TRANSITION_TRACE_FAULT_PROOF_TITLES[name]),
        [
          contracts.computationThread.policyId,
          contracts.fraudProof.policyId,
          fraudProofTokenAddressData,
          ...(name === "deposit" || name === "l1Event" ? [h28b] : []),
        ],
      ),
    );
    expect(
      contracts.transitionTrace.finals.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedFinalCbors);
    const finalHashesSchema = Data.Array(Data.Bytes());
    type FinalHashes = Data.Static<typeof finalHashesSchema>;
    const FinalHashes = finalHashesSchema as unknown as FinalHashes;
    const finalHashesData = Data.from(
      Data.to(
        expectedFinalCbors.map((cbor) => spendingScriptHash(cbor)),
        FinalHashes,
      ),
    );
    const expectedRouteCbor = applyParamsToScript(
      compiledScript(blueprint, TRANSITION_TRACE_FAULT_PROOF_TITLES.route),
      [finalHashesData, contracts.computationThread.policyId],
    );
    expect(contracts.transitionTrace.route.spendingScriptCBOR).toBe(
      expectedRouteCbor,
    );
    expect(contracts.transitionTrace.route.spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedRouteCbor)),
    );
  });

  it("builds validation-trace dispute with its exact shared-policy parameter order", async () => {
    if (currentTreeBlueprintPath === undefined) {
      return;
    }
    const rawBlueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
      readonly validators?: readonly {
        readonly title?: string;
        readonly parameters?: readonly unknown[];
      }[];
    };
    expect(
      rawBlueprint.validators?.find(
        ({ title }) =>
          title ===
          VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers.cek,
      )?.parameters,
    ).toHaveLength(4);
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.proofItem,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
      ...Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.canonicalDecodeItemStages,
      ),
      ...Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.scriptSourcesStageOneRedeemerStages,
      ),
      ...Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares),
      ...Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics),
      ...Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers,
      ),
      CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
    ]);

    const contracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );
    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedAward = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const deploymentId = deriveValidationTraceDeploymentIdV1(h28c);
    const expectedStageOneRedeemerFoldMapExecutor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.foldMapExecutor,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerFinalizeFrameExecutor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.finalizeFrameExecutor,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerOuterNormalizer = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.outerNormalizer,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerTraversalNormalizer = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.traversalNormalizer,
      ),
      [deploymentId, contracts.computationThread.policyId],
    );
    const expectedStageOneRedeemerSettlement = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.settlement,
      ),
      [
        deploymentId,
        spendingScriptHash(expectedStageOneRedeemerTraversalNormalizer),
        spendingScriptHash(expectedStageOneRedeemerOuterNormalizer),
        spendingScriptHash(expectedStageOneRedeemerFoldMapExecutor),
        spendingScriptHash(expectedStageOneRedeemerFinalizeFrameExecutor),
        spendingScriptHash(expectedAward),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStageOneRedeemerEnvelope = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.envelope,
      ),
      [
        deploymentId,
        spendingScriptHash(expectedStageOneRedeemerTraversalNormalizer),
        spendingScriptHash(expectedStageOneRedeemerOuterNormalizer),
        spendingScriptHash(expectedStageOneRedeemerFoldMapExecutor),
        spendingScriptHash(expectedStageOneRedeemerFinalizeFrameExecutor),
        spendingScriptHash(expectedStageOneRedeemerSettlement),
        contracts.computationThread.policyId,
      ],
    );
    const expectedBaseSemanticResolvers = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
    ).map((title, index) =>
      applyParamsToScript(
        compiledScript(blueprint, title),
        index === 1
          ? [
              contracts.validationTraceDispute.canonicalDecodeItemStages.source
                .spendingScriptHash,
              contracts.computationThread.policyId,
              contracts.validationTraceDispute.proofItem.spendingScriptHash,
            ]
          : [
              spendingScriptHash(expectedAward),
              contracts.computationThread.policyId,
            ],
      ),
    );
    const expectedSemanticResolvers = [
      ...expectedBaseSemanticResolvers,
      expectedStageOneRedeemerEnvelope,
    ];
    const expectedSemanticResolverGroups = [
      [expectedSemanticResolvers[0]!, expectedSemanticResolvers[1]!],
      [expectedSemanticResolvers[2]!],
      [expectedSemanticResolvers[3]!],
      [expectedSemanticResolvers[4]!, expectedSemanticResolvers[5]!],
      [
        expectedSemanticResolvers[6]!,
        expectedSemanticResolvers[7]!,
        expectedSemanticResolvers[8]!,
        expectedSemanticResolvers[9]!,
      ],
      [
        expectedSemanticResolvers[10]!,
        expectedSemanticResolvers[11]!,
        expectedSemanticResolvers[12]!,
        expectedSemanticResolvers[13]!,
        expectedSemanticResolvers[14]!,
        expectedSemanticResolvers[15]!,
        expectedSemanticResolvers[16]!,
        expectedSemanticResolvers[17]!,
        expectedSemanticResolvers[18]!,
        expectedSemanticResolvers[19]!,
        expectedSemanticResolvers[20]!,
        expectedSemanticResolvers[21]!,
        expectedSemanticResolvers[22]!,
        expectedSemanticResolvers[23]!,
      ],
      [expectedSemanticResolvers[24]!, expectedSemanticResolvers[25]!],
      [
        expectedSemanticResolvers[26]!,
        expectedSemanticResolvers[27]!,
        expectedSemanticResolvers[28]!,
        expectedSemanticResolvers[29]!,
        expectedSemanticResolvers[30]!,
        expectedSemanticResolvers[31]!,
      ],
      [
        expectedSemanticResolvers[32]!,
        expectedSemanticResolvers[33]!,
        expectedSemanticResolvers[34]!,
        expectedSemanticResolvers[35]!,
        expectedSemanticResolvers[36]!,
        expectedSemanticResolvers[37]!,
        expectedSemanticResolvers[38]!,
        expectedSemanticResolvers[39]!,
        expectedSemanticResolvers[40]!,
        expectedSemanticResolvers[41]!,
        expectedSemanticResolvers[42]!,
        expectedSemanticResolvers[43]!,
        expectedSemanticResolvers[44]!,
        expectedSemanticResolvers[45]!,
        expectedSemanticResolvers[46]!,
        expectedSemanticResolvers[47]!,
        expectedSemanticResolvers[48]!,
        expectedSemanticResolvers[49]!,
        expectedSemanticResolvers[50]!,
        expectedSemanticResolvers[51]!,
        expectedSemanticResolvers[52]!,
        expectedSemanticResolvers[53]!,
        expectedSemanticResolvers[54]!,
        expectedSemanticResolvers[55]!,
        expectedSemanticResolvers[56]!,
        expectedSemanticResolvers[57]!,
        expectedSemanticResolvers[58]!,
        expectedSemanticResolvers[59]!,
        expectedSemanticResolvers[75]!,
      ],
      [
        expectedSemanticResolvers[60]!,
        expectedSemanticResolvers[61]!,
        expectedSemanticResolvers[62]!,
      ],
      [
        expectedSemanticResolvers[63]!,
        expectedSemanticResolvers[64]!,
        expectedSemanticResolvers[65]!,
        expectedSemanticResolvers[66]!,
      ],
      [
        expectedSemanticResolvers[67]!,
        expectedSemanticResolvers[68]!,
        expectedSemanticResolvers[69]!,
        expectedSemanticResolvers[70]!,
        expectedSemanticResolvers[71]!,
        expectedSemanticResolvers[72]!,
        expectedSemanticResolvers[73]!,
        expectedSemanticResolvers[74]!,
      ],
    ] as const;
    const resolverHashesSchema = Data.Array(Data.Bytes());
    type ResolverHashes = Data.Static<typeof resolverHashesSchema>;
    const ResolverHashes = resolverHashesSchema as unknown as ResolverHashes;
    const expectedSemanticResolverHashParams =
      expectedSemanticResolverGroups.map(
        (group) =>
          Data.from(
            Data.to(group.map(spendingScriptHash), ResolverHashes),
          ) as Data,
      );
    const expectedPrepareResolvers = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
    ).map((title, index) =>
      applyParamsToScript(compiledScript(blueprint, title), [
        expectedSemanticResolverHashParams[index]!,
        contracts.computationThread.policyId,
      ]),
    );
    const expectedCekProgramMaterial = compiledScript(
      blueprint,
      CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
    );
    const expectedDirectResolvers = [
      applyParamsToScript(
        compiledScript(
          blueprint,
          VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers.cek,
        ),
        [
          contracts.computationThread.policyId,
          contracts.fraudProof.policyId,
          fraudProofTokenAddressData,
          spendingScriptHash(expectedCekProgramMaterial),
        ],
      ),
      applyParamsToScript(
        compiledScript(
          blueprint,
          VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers
            .valueAndMint,
        ),
        [
          contracts.computationThread.policyId,
          contracts.fraudProof.policyId,
          fraudProofTokenAddressData,
        ],
      ),
    ];
    const expectedResolvers = [
      expectedPrepareResolvers[0]!,
      expectedPrepareResolvers[1]!,
      expectedPrepareResolvers[2]!,
      expectedPrepareResolvers[3]!,
      expectedPrepareResolvers[4]!,
      expectedPrepareResolvers[5]!,
      expectedPrepareResolvers[6]!,
      expectedPrepareResolvers[7]!,
      expectedPrepareResolvers[8]!,
      expectedPrepareResolvers[9]!,
      expectedPrepareResolvers[10]!,
      expectedDirectResolvers[0]!,
      expectedDirectResolvers[1]!,
      expectedPrepareResolvers[11]!,
    ];
    expect(expectedResolvers).toHaveLength(VALIDATION_TRACE_RESOLVER_COUNT_V1);
    const resolverHashesData = Data.from(
      Data.to(
        expectedResolvers.map((cbor) => spendingScriptHash(cbor)),
        ResolverHashes,
      ),
    );
    const expectedBoundaryCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
      ),
      [resolverHashesData, contracts.computationThread.policyId],
    );
    const expectedTimeoutCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const expectedGameCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
      ),
      [
        spendingScriptHash(expectedBoundaryCbor),
        spendingScriptHash(expectedTimeoutCbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedSourceCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
      ),
      [
        spendingScriptHash(expectedGameCbor),
        spendingScriptHash(expectedAward),
        contracts.computationThread.policyId,
      ],
    );
    const expectedCbor = applyParamsToScript(
      compiledScript(
        blueprint,
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
      ),
      [
        spendingScriptHash(expectedSourceCbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );
    for (const [label, cbor] of [
      ["opener", expectedCbor],
      ["source", expectedSourceCbor],
      ["game", expectedGameCbor],
      ["boundary", expectedBoundaryCbor],
      ["timeout", expectedTimeoutCbor],
      ["award", expectedAward],
      ...expectedPrepareResolvers.map(
        (cbor, prepareIndex) =>
          [`prepare-${prepareIndex.toString()}`, cbor] as const,
      ),
    ] as const) {
      expect(
        cbor.length / 2,
        `${label} parameterized script bytes`,
      ).toBeLessThan(14 * 1024);
    }

    expect(contracts.validationTraceDispute.steps).toHaveLength(106);
    expect(contracts.validationTraceDispute.award.spendingScriptCBOR).toBe(
      expectedAward,
    );
    expect(
      contracts.validationTraceDispute.semanticResolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedSemanticResolvers);
    expect(
      Object.values(
        contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages,
      ).map(({ spendingScriptCBOR }) => spendingScriptCBOR),
    ).toEqual([
      expectedStageOneRedeemerEnvelope,
      expectedStageOneRedeemerTraversalNormalizer,
      expectedStageOneRedeemerOuterNormalizer,
      expectedStageOneRedeemerFoldMapExecutor,
      expectedStageOneRedeemerFinalizeFrameExecutor,
      expectedStageOneRedeemerSettlement,
    ]);
    expect(
      contracts.validationTraceDispute.prepareResolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedPrepareResolvers);
    expect(
      contracts.validationTraceDispute.directResolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedDirectResolvers);
    expect(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptCBOR,
    ).toBe(expectedCekProgramMaterial);
    expect(
      contracts.validationTraceDispute.resolvers.map(
        ({ spendingScriptCBOR }) => spendingScriptCBOR,
      ),
    ).toEqual(expectedResolvers);
    expect(contracts.validationTraceDispute.boundary.spendingScriptCBOR).toBe(
      expectedBoundaryCbor,
    );
    expect(contracts.validationTraceDispute.timeout.spendingScriptCBOR).toBe(
      expectedTimeoutCbor,
    );
    expect(contracts.validationTraceDispute.game.spendingScriptCBOR).toBe(
      expectedGameCbor,
    );
    expect(contracts.validationTraceDispute.source.spendingScriptCBOR).toBe(
      expectedSourceCbor,
    );
    expect(contracts.validationTraceDispute.firstStep.spendingScriptCBOR).toBe(
      expectedCbor,
    );
    expect(contracts.validationTraceDispute.firstStep.spendingScriptHash).toBe(
      spendingScriptHash(expectedCbor),
    );
    expect(
      contracts.validationTraceDispute.firstStep.spendingScriptAddress,
    ).toBe(validatorToAddress("Preprod", spendingScript(expectedCbor)));
  });
});
