import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
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
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

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
        { Continue: [txInclusionArgs] },
        DoubleSpendStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        verified_tx1_id: h32,
        verified_tx1_spend_inputs_hash: h32b,
      },
    };
    expect(roundTrip(step02Datum, DoubleSpendStep02Datum)).toEqual(step02Datum);
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        DoubleSpendStep02SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        verified_tx1_spend_inputs_hash: h32,
        verified_tx2_spend_inputs_hash: h32b,
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
              tx1_spend_inputs_ref_input_index: 1n,
              double_spent_input_index: 0n,
            },
          ],
        },
        DoubleSpendStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ tx1_spend_inputs_ref_input_index: 1n }],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        verified_tx2_spend_inputs_hash: h32b,
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
              tx2_spend_inputs_ref_input_index: 2n,
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
        { Continue: [txInclusionArgs] },
        InvalidRangeStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

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

  it("round-trips zero-input step datums and redeemers", () => {
    expect(
      roundTrip({ fraud_prover: h28, data: null }, ZeroInputStep01Datum),
    ).toEqual({ fraud_prover: h28, data: null });
    expect(
      roundTrip({ Continue: [txInclusionArgs] }, ZeroInputStep01SpendRedeemer),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: { bad_tx_spend_inputs_hash: EMPTY_SPEND_INPUTS_HASH },
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

    const step02Datum = {
      fraud_prover: h28,
      data: { Direct: { verified_tx_inputs_hash: h32 } },
    };
    expect(roundTrip(step02Datum, SDK.InputNoIdxStep02Datum)).toEqual(
      step02Datum,
    );
    const publishedInputs = {
      version: 1n,
      computation_thread_policy_id: h28,
      computation_thread_asset_name: h28b,
      fraud_prover: h28,
      verified_tx_inputs_hash: h32,
      item_count: 1n,
      inputs: [{ tx_id: h32b, output_index: 7n }],
    };
    expect(roundTrip(publishedInputs, SDK.PublishedSpendInputsV1)).toEqual(
      publishedInputs,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              Complete: {
                input_index: 0n,
                output_index: 0n,
                inputs_preimage: [{ tx_id: h32b, output_index: 7n }],
                bad_inputs_index: 0n,
              },
            },
          ],
        },
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ Complete: { inputs_preimage: [{ output_index: 7n }] } }],
    });
    expect(
      roundTrip(
        {
          Continue: [
            {
              CompletePublished: {
                input_index: 1n,
                output_index: 0n,
                publication_reference_input_index: 2n,
                bad_inputs_index: 0n,
              },
            },
          ],
        },
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toEqual({
      Continue: [
        {
          CompletePublished: {
            input_index: 1n,
            output_index: 0n,
            publication_reference_input_index: 2n,
            bad_inputs_index: 0n,
          },
        },
      ],
    });

    const foldInputs = Array.from({ length: 20 }, (_, index) => ({
      tx_id: index % 2 === 0 ? h32 : h32b,
      output_index: BigInt(index),
    }));
    expect(SDK.INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1).toBe(19);
    expect(SDK.inputNoIdxStep02ExecutionModeV1(19)).toBe("direct");
    expect(SDK.inputNoIdxStep02ExecutionModeV1(20)).toBe("fold");
    const openings = SDK.buildInputNoIdxSpendInputFoldOpeningsV1(foldInputs);
    expect(openings).toHaveLength(20);
    expect(
      SDK.verifyInputNoIdxSpendInputFoldOpeningV1({
        inputs: foldInputs,
        opening: openings[0]!,
      }),
    ).toBe(true);
    expect(
      SDK.verifyInputNoIdxSpendInputFoldOpeningV1({
        inputs: foldInputs,
        opening: { ...openings[0]!, inputCbor: "00" },
      }),
    ).toBe(false);
    const opening = openings[7]!;
    const malformedOpenings = [
      {
        ...opening,
        collectionProof: { ...opening.collectionProof, field_index: 1n },
      },
      {
        ...opening,
        collectionProof: { ...opening.collectionProof, item_count: 21n },
      },
      {
        ...opening,
        collectionProof: { ...opening.collectionProof, item_index: 8n },
      },
      {
        ...opening,
        collectionProof: {
          ...opening.collectionProof,
          item_length: opening.collectionProof.item_length + 1n,
        },
      },
      {
        ...opening,
        collectionProof: {
          ...opening.collectionProof,
          item_commitment: "00".repeat(32),
        },
      },
      {
        ...opening,
        collectionProof: { ...opening.collectionProof, frontier: [] },
      },
      {
        ...opening,
        collectionProof: {
          ...opening.collectionProof,
          siblings: [...opening.collectionProof.siblings, "00".repeat(32)],
        },
      },
      { ...opening, inputCbor: openings[8]!.inputCbor },
      { ...opening, inputCbor: `${opening.inputCbor}00` },
    ];
    for (const malformed of malformedOpenings) {
      expect(
        SDK.verifyInputNoIdxSpendInputFoldOpeningV1({
          inputs: foldInputs,
          opening: malformed,
        }),
      ).toBe(false);
    }
    for (const alteredInputs of [
      foldInputs.slice(0, -1),
      [...foldInputs, foldInputs[7]!],
      [...foldInputs].reverse(),
      foldInputs.map((input, index) =>
        index === 19 ? { ...input, output_index: 99n } : input,
      ),
    ]) {
      expect(
        SDK.verifyInputNoIdxSpendInputFoldOpeningV1({
          inputs: alteredInputs,
          opening,
        }),
      ).toBe(false);
    }
    expect(
      roundTrip(
        {
          Continue: [
            {
              FoldStart: {
                input_index: 0n,
                output_index: 0n,
                bad_inputs_index: 7n,
                input_cbor: openings[0]!.inputCbor,
                collection_proof: openings[0]!.collectionProof,
              },
            },
          ],
        },
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ FoldStart: { collection_proof: { item_count: 20n } } }],
    });
    expect(
      roundTrip(
        {
          Continue: [
            {
              FoldNext: {
                input_index: 0n,
                output_index: 0n,
                input_cbor: openings[1]!.inputCbor,
                collection_proof: openings[1]!.collectionProof,
              },
            },
          ],
        },
        SDK.InputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ FoldNext: { collection_proof: { item_index: 1n } } }],
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
      data: { producing_tx_outputs_hash: h32, bad_input_output_index: 7n },
    };
    expect(roundTrip(step04Datum, SDK.InputNoIdxStep04Datum)).toEqual(
      step04Datum,
    );
    const output = {
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
              fraud_proof_mint_redeemer_index: 1n,
              outputs_preimage: [output],
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
          outputs_preimage: [output],
        },
      ],
    });
  });

  it("pins the flattened input-no-idx step-02 wire ABI", () => {
    const inputs = [
      { tx_id: h32b, output_index: 7n },
      { tx_id: h32, output_index: 8n },
    ];
    const openings = SDK.buildInputNoIdxSpendInputFoldOpeningsV1(inputs);
    const complete = {
      Complete: {
        input_index: 0n,
        output_index: 0n,
        inputs_preimage: [inputs[0]!],
        bad_inputs_index: 0n,
      },
    };
    const variants = [
      ["Complete", 0, 4, complete],
      [
        "CompletePublished",
        1,
        4,
        {
          CompletePublished: {
            input_index: 1n,
            output_index: 0n,
            publication_reference_input_index: 2n,
            bad_inputs_index: 0n,
          },
        },
      ],
      [
        "FoldStart",
        2,
        5,
        {
          FoldStart: {
            input_index: 0n,
            output_index: 0n,
            bad_inputs_index: 1n,
            input_cbor: openings[0]!.inputCbor,
            collection_proof: openings[0]!.collectionProof,
          },
        },
      ],
      [
        "FoldNext",
        3,
        4,
        {
          FoldNext: {
            input_index: 0n,
            output_index: 0n,
            input_cbor: openings[1]!.inputCbor,
            collection_proof: openings[1]!.collectionProof,
          },
        },
      ],
    ] as const;
    let completeFields: readonly unknown[] | undefined;

    for (const [label, tag, arity, args] of variants) {
      const redeemer = { Continue: [args] };
      const cbor = Data.to(
        redeemer as never,
        SDK.InputNoIdxStep02SpendRedeemer,
      );
      const outer = Data.from(cbor);

      expect(outer, label).toBeInstanceOf(Constr);
      const continueConstr = outer as Constr<unknown>;
      expect(continueConstr.index, label).toBe(1);
      expect(continueConstr.fields, label).toHaveLength(1);
      expect(continueConstr.fields[0], label).toBeInstanceOf(Constr);
      const argsConstr = continueConstr.fields[0] as Constr<unknown>;
      expect(argsConstr.index, label).toBe(tag);
      expect(argsConstr.fields, label).toHaveLength(arity);
      expect(
        typeof argsConstr.fields[0],
        `${label} has no wrapper constructor`,
      ).toBe("bigint");
      expect(Data.from(cbor, SDK.InputNoIdxStep02SpendRedeemer), label).toEqual(
        redeemer,
      );

      if (label === "Complete") {
        completeFields = argsConstr.fields;
        expect(cbor).toBe(`d87a9fd8799f00009fd8799f5820${h32b}07ffff00ffff`);
      }
    }

    expect(completeFields).toBeDefined();
    const fields = [...completeFields!];
    const invalid = [
      [
        "obsolete nested CompleteArgs wrapper",
        new Constr(1, [new Constr(0, [new Constr(0, fields)])]),
      ],
      [
        "Complete payload under adjacent CompletePublished tag",
        new Constr(1, [new Constr(1, fields)]),
      ],
      [
        "Complete wrong arity",
        new Constr(1, [new Constr(0, fields.slice(0, 3))]),
      ],
      [
        "args adjacent out-of-range tag",
        new Constr(1, [new Constr(4, fields)]),
      ],
      ["Continue wrong arity", new Constr(1, [])],
    ] as const;

    for (const [label, malformed] of invalid) {
      const cbor = Data.to(malformed as never);
      expect(
        () => Data.from(cbor, SDK.InputNoIdxStep02SpendRedeemer),
        label,
      ).toThrow();
    }
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
        producingTxOutputsHash: h32,
      }),
    ).toEqual({
      producing_tx_outputs_hash: h32,
      bad_input_output_index: 7n,
    });
  });

  it("detects a zero-input violation from the native spend-inputs hash", () => {
    // The empty spend-inputs list uses the native V1 bounded-collection
    // commitment for field zero, which the step-02 validator pins.
    expect(EMPTY_SPEND_INPUTS_HASH).toBe(
      "eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec76",
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
    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
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
          ...contracts.invalidRange.steps,
          ...contracts.zeroInput.steps,
          ...contracts.transitionTrace.steps,
          ...contracts.validationTraceDispute.steps,
        ].map((step) => step.spendingScriptHash),
      ).size,
      // The split stage-one route contributes the envelope resolver plus five
      // internal stage hashes to the applied proof surface.
    ).toBe(131);
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
    const expectedDirectResolvers = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers,
    ).map((title) =>
      applyParamsToScript(compiledScript(blueprint, title), [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ]),
    );
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
