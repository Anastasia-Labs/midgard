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
  buildInvalidRangeFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  DOUBLE_SPEND_FAULT_PROOF_TITLES,
  DoubleSpendStep01Datum,
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  DoubleSpendStep02SpendRedeemer,
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
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
  NormalizedTimeRange,
  normalizeNativeTxValidityRange,
  parseFaultProofBlueprint,
  type Proof,
  TRANSITION_TRACE_FAULT_PROOF_TITLES,
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
  it("builds double-spend, invalid-range, and transition-trace chains from the Aiken blueprint", async () => {
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
    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
    expect(contracts.transitionTrace.firstStep).toBe(
      contracts.transitionTrace.steps[0],
    );
    expect(contracts.transitionTrace.steps).toHaveLength(1);
    expect(
      new Set(
        [
          ...contracts.doubleSpend.steps,
          ...contracts.invalidRange.steps,
          ...contracts.transitionTrace.steps,
        ].map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(7);
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
