import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Data,
  type SpendingValidator as LucidSpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  AddressData,
  addressDataFromBech32,
  buildDoubleSpendFaultProofContracts,
  buildFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildZeroInputFaultProofContracts,
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
  INPUT_NO_IDX_FAULT_PROOF_TITLES,
  InputNoIdxStep01SpendRedeemer,
  InputNoIdxStep02Datum,
  InputNoIdxStep02SpendRedeemer,
  InputNoIdxStep03Datum,
  InputNoIdxStep03SpendRedeemer,
  InputNoIdxStep04Datum,
  InputNoIdxStep04SpendRedeemer,
  INVALID_RANGE_FAULT_PROOF_TITLES,
  InvalidRangeStep01Datum,
  InvalidRangeStep01SpendRedeemer,
  InvalidRangeStep02Datum,
  InvalidRangeStep02SpendRedeemer,
  invalidRangeViolationReason,
  MidgardTxInputList,
  NativeTxBodyCompact,
  nativeTxBodyHasZeroInputViolation,
  NO_REFERENCE_INPUT_FAULT_PROOF_TITLES,
  NoReferenceInputStep01SpendRedeemer,
  NoReferenceInputStep02Datum,
  NoReferenceInputStep02SpendRedeemer,
  NoReferenceInputStep03Datum,
  NoReferenceInputStep03SpendRedeemer,
  NoReferenceInputStep04Datum,
  NoReferenceInputStep04SpendRedeemer,
  NormalizedTimeRange,
  normalizeNativeTxValidityRange,
  parseFaultProofBlueprint,
  type Proof,
  REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES,
  ReferenceInputNoIdxStep01SpendRedeemer,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  ReferenceInputNoIdxStep03SpendRedeemer,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
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
const outOfRangeInput = spendInputs[1]!;
const referenceInputs = [
  { tx_id: "cc".repeat(32), output_index: 0n },
  { tx_id: "dd".repeat(32), output_index: 2n },
];
const missingReferenceInput = referenceInputs[1]!;
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

  it("round-trips no-reference-input step datums and redeemers", () => {
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        NoReferenceInputStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        bad_tx_reference_inputs_hash: h32,
        blocks_prev_utxos_root: h32b,
        blocks_transactions_root: h32,
      },
    };
    expect(roundTrip(step02Datum, NoReferenceInputStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              reference_inputs_preimage: referenceInputs,
              bad_reference_input_index: 1n,
            },
          ],
        },
        NoReferenceInputStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          reference_inputs_preimage: referenceInputs,
          bad_reference_input_index: 1n,
        },
      ],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        missing_reference_input: missingReferenceInput,
        blocks_prev_utxos_root: h32,
        blocks_transactions_root: h32b,
      },
    };
    expect(roundTrip(step03Datum, NoReferenceInputStep03Datum)).toEqual(
      step03Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              non_membership_proof_in_ledger: proof,
              non_membership_proof_script_redeemer_index: 1n,
            },
          ],
        },
        NoReferenceInputStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ non_membership_proof_script_redeemer_index: 1n }],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        missing_reference_input_tx_id: missingReferenceInput.tx_id,
        blocks_transactions_root: h32b,
      },
    };
    expect(roundTrip(step04Datum, NoReferenceInputStep04Datum)).toEqual(
      step04Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              non_membership_proof_in_txs: proof,
              non_membership_proof_script_redeemer_index: 1n,
              fraud_proof_mint_redeemer_index: 2n,
            },
          ],
        },
        NoReferenceInputStep04SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ fraud_proof_mint_redeemer_index: 2n }],
    });
  });

  it("round-trips input-no-idx step datums and redeemers", () => {
    expect(
      roundTrip({ Continue: [txInclusionArgs] }, InputNoIdxStep01SpendRedeemer),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        verified_tx_inputs_hash: h32,
      },
    };
    expect(roundTrip(step02Datum, InputNoIdxStep02Datum)).toEqual(step02Datum);
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              inputs_preimage: spendInputs,
              bad_inputs_index: 1n,
            },
          ],
        },
        InputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ inputs_preimage: spendInputs, bad_inputs_index: 1n }],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        bad_input_tx_id: outOfRangeInput.tx_id,
        bad_input_output_index: outOfRangeInput.output_index,
      },
    };
    expect(roundTrip(step03Datum, InputNoIdxStep03Datum)).toEqual(step03Datum);
    expect(
      roundTrip({ Continue: [txInclusionArgs] }, InputNoIdxStep03SpendRedeemer),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        producing_tx_outputs_hash: h32b,
        bad_input_output_index: outOfRangeInput.output_index,
      },
    };
    expect(roundTrip(step04Datum, InputNoIdxStep04Datum)).toEqual(step04Datum);
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 2n,
              outputs_preimage: ["ee".repeat(20), "ff".repeat(20)],
            },
          ],
        },
        InputNoIdxStep04SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          fraud_proof_mint_redeemer_index: 2n,
          outputs_preimage: ["ee".repeat(20), "ff".repeat(20)],
        },
      ],
    });
  });

  it("round-trips reference-input-no-idx step datums and redeemers", () => {
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        ReferenceInputNoIdxStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        verified_tx_reference_inputs_hash: h32,
      },
    };
    expect(roundTrip(step02Datum, ReferenceInputNoIdxStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              reference_inputs_preimage: referenceInputs,
              bad_reference_input_index: 1n,
            },
          ],
        },
        ReferenceInputNoIdxStep02SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          reference_inputs_preimage: referenceInputs,
          bad_reference_input_index: 1n,
        },
      ],
    });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        bad_reference_input_tx_id: missingReferenceInput.tx_id,
        bad_reference_input_output_index: missingReferenceInput.output_index,
      },
    };
    expect(roundTrip(step03Datum, ReferenceInputNoIdxStep03Datum)).toEqual(
      step03Datum,
    );
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        ReferenceInputNoIdxStep03SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step04Datum = {
      fraud_prover: h28,
      data: {
        producing_tx_outputs_hash: h32b,
        bad_reference_input_output_index: missingReferenceInput.output_index,
      },
    };
    expect(roundTrip(step04Datum, ReferenceInputNoIdxStep04Datum)).toEqual(
      step04Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              fraud_proof_mint_redeemer_index: 2n,
              outputs_preimage: ["ee".repeat(20), "ff".repeat(20)],
            },
          ],
        },
        ReferenceInputNoIdxStep04SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [
        {
          fraud_proof_mint_redeemer_index: 2n,
          outputs_preimage: ["ee".repeat(20), "ff".repeat(20)],
        },
      ],
    });
  });

  it("detects a zero-input violation from the native spend-inputs hash", () => {
    // The empty spend-inputs list hashes the definite-length empty CBOR array,
    // which is what the step-02 validator's `empty_spend_inputs_hash` pins.
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
    expect(contracts.noReferenceInput.firstStep).toBe(
      contracts.noReferenceInput.steps[0],
    );
    expect(contracts.noReferenceInput.steps).toHaveLength(4);
    expect(contracts.inputNoIdx.firstStep).toBe(contracts.inputNoIdx.steps[0]);
    expect(contracts.inputNoIdx.steps).toHaveLength(4);
    expect(contracts.referenceInputNoIdx.firstStep).toBe(
      contracts.referenceInputNoIdx.steps[0],
    );
    expect(contracts.referenceInputNoIdx.steps).toHaveLength(4);
    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(contracts.zeroInput.firstStep).toBe(contracts.zeroInput.steps[0]);
    expect(contracts.zeroInput.steps).toHaveLength(2);
    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(1);
    // `no_reference_input` steps 02-04 compile to the same UPLC as `no_input`
    // steps 02-04 — the aiken sources differ only in datum field names and
    // comments — so the two chains share those three scripts (and their
    // addresses). The families stay distinguishable by step 01, which commits
    // the bad tx's reference-inputs hash instead of its spend-inputs hash, and
    // by the category id in the computation-thread token minted at Init.
    expect(
      contracts.noReferenceInput.steps
        .slice(1)
        .map((step) => step.spendingScriptHash),
    ).toEqual(
      contracts.nonExistentInput.steps
        .slice(1)
        .map((step) => step.spendingScriptHash),
    );
    expect(contracts.noReferenceInput.firstStep.spendingScriptHash).not.toBe(
      contracts.nonExistentInput.firstStep.spendingScriptHash,
    );
    // `reference_input_no_idx` is the reference-input mirror of `input_no_idx`:
    // its steps 02-04 only ever see an input-list preimage and a producing
    // transaction, so they compile to the same UPLC and the two chains share
    // those three scripts (and their addresses) — exactly like the
    // `no_input`/`no_reference_input` pair. Step 01 differs, since it commits
    // the bad tx's reference-inputs hash instead of its spend-inputs hash.
    expect(
      contracts.referenceInputNoIdx.steps
        .slice(1)
        .map((step) => step.spendingScriptHash),
    ).toEqual(
      contracts.inputNoIdx.steps
        .slice(1)
        .map((step) => step.spendingScriptHash),
    );
    expect(contracts.referenceInputNoIdx.firstStep.spendingScriptHash).not.toBe(
      contracts.inputNoIdx.firstStep.spendingScriptHash,
    );
    // `input_no_idx` shares no script with any other family: its steps 02-04
    // carry different state/args than the `no_input` chain they resemble.
    expect(
      new Set(
        [
          ...contracts.doubleSpend.steps,
          ...contracts.nonExistentInput.steps,
          ...contracts.noReferenceInput.steps,
          ...contracts.inputNoIdx.steps,
          ...contracts.referenceInputNoIdx.steps,
          ...contracts.invalidRange.steps,
          ...contracts.zeroInput.steps,
          ...contracts.transitionTrace.steps,
        ].map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(19);
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

  it("builds no-reference-input with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildNoReferenceInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.noReferenceInput.firstStep).toBe(
      contracts.noReferenceInput.steps[0],
    );
    expect(contracts.noReferenceInput.steps).toHaveLength(4);
    expect(
      new Set(
        contracts.noReferenceInput.steps.map((step) => step.spendingScriptHash),
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
    const expectedStep04Cbor = applyParamsToScript(
      compiledScript(blueprint, NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step04),
      [
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep03Cbor = applyParamsToScript(
      compiledScript(blueprint, NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03),
      [
        spendingScriptHash(expectedStep04Cbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02),
      [
        spendingScriptHash(expectedStep03Cbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.noReferenceInput.steps[3].spendingScriptCBOR).toBe(
      expectedStep04Cbor,
    );
    expect(contracts.noReferenceInput.steps[2].spendingScriptCBOR).toBe(
      expectedStep03Cbor,
    );
    expect(contracts.noReferenceInput.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.noReferenceInput.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.noReferenceInput.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.noReferenceInput.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds no-reference-input without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(NO_REFERENCE_INPUT_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildNoReferenceInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.noReferenceInput.firstStep).toBe(
      contracts.noReferenceInput.steps[0],
    );
    expect(contracts.noReferenceInput.steps).toHaveLength(4);
  });

  it("builds input-no-idx with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.inputNoIdx.firstStep).toBe(contracts.inputNoIdx.steps[0]);
    expect(contracts.inputNoIdx.steps).toHaveLength(4);
    expect(
      new Set(contracts.inputNoIdx.steps.map((step) => step.spendingScriptHash))
        .size,
    ).toBe(4);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    // Step 04 takes (computation-thread, fraud-proof policy, fraud-proof
    // address) — the reverse of every other family's step-04 — and step 03
    // takes the hub-oracle policy because it binds the producing native tx.
    const expectedStep04Cbor = applyParamsToScript(
      compiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step04),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const expectedStep03Cbor = applyParamsToScript(
      compiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step03),
      [
        spendingScriptHash(expectedStep04Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );
    const expectedStep02Cbor = applyParamsToScript(
      compiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step02),
      [
        spendingScriptHash(expectedStep03Cbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step01),
      [
        spendingScriptHash(expectedStep02Cbor),
        contracts.computationThread.policyId,
        h28b,
      ],
    );

    expect(contracts.inputNoIdx.steps[3].spendingScriptCBOR).toBe(
      expectedStep04Cbor,
    );
    expect(contracts.inputNoIdx.steps[2].spendingScriptCBOR).toBe(
      expectedStep03Cbor,
    );
    expect(contracts.inputNoIdx.steps[1].spendingScriptCBOR).toBe(
      expectedStep02Cbor,
    );
    expect(contracts.inputNoIdx.steps[0].spendingScriptCBOR).toBe(
      expectedStep01Cbor,
    );
    expect(contracts.inputNoIdx.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedStep01Cbor),
    );
    expect(contracts.inputNoIdx.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedStep01Cbor)),
    );
  });

  it("builds input-no-idx without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(INPUT_NO_IDX_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildInputNoIdxFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.inputNoIdx.firstStep).toBe(contracts.inputNoIdx.steps[0]);
    expect(contracts.inputNoIdx.steps).toHaveLength(4);
  });

  it("builds reference-input-no-idx with the validator parameter order from the blueprint", async () => {
    const blueprint = loadBlueprint();

    const contracts = await Effect.runPromise(
      buildReferenceInputNoIdxFaultProofContracts({
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
    // Same parameter order as input-no-idx — the two chains share their
    // step-02..04 scripts, so a divergent order here would fork those hashes.
    const expectedStep04Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
      ),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
    );
    const expectedStep03Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
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
        REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
      ),
      [
        spendingScriptHash(expectedStep03Cbor),
        contracts.computationThread.policyId,
      ],
    );
    const expectedStep01Cbor = applyParamsToScript(
      compiledScript(
        blueprint,
        REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
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
      ...Object.values(REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildReferenceInputNoIdxFaultProofContracts({
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
    expect(contracts.transitionTrace.steps).toHaveLength(1);

    const fraudProofTokenAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const expectedProofCbor = applyParamsToScript(
      compiledScript(blueprint, TRANSITION_TRACE_FAULT_PROOF_TITLES.proof),
      [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofTokenAddressData,
        h28b,
      ],
    );

    expect(contracts.transitionTrace.steps[0].spendingScriptCBOR).toBe(
      expectedProofCbor,
    );
    expect(contracts.transitionTrace.steps[0].spendingScriptHash).toBe(
      spendingScriptHash(expectedProofCbor),
    );
    expect(contracts.transitionTrace.steps[0].spendingScriptAddress).toBe(
      validatorToAddress("Preprod", spendingScript(expectedProofCbor)),
    );
  });
});
