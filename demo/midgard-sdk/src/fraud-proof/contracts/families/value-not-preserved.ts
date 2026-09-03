import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/value_not_preserved/step_01.main.spend",
  step02: "fraud_proofs/value_not_preserved/step_02.main.spend",
  step03: "fraud_proofs/value_not_preserved/step_03.main.spend",
  step04: "fraud_proofs/value_not_preserved/step_04.main.spend",
} as const;

export type ValueNotPreservedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly valueNotPreserved: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildValueNotPreservedFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildValueNotPreservedChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ValueNotPreservedFaultProofContracts["valueNotPreserved"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build value-not-preserved step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build value-not-preserved step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildValueNotPreservedFaultProofContracts = (
  params: BuildValueNotPreservedFaultProofContractsParams,
): Effect.Effect<ValueNotPreservedFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const valueNotPreserved = yield* buildValueNotPreservedChain({
      ...params,
      ...shared,
    });
    return { ...shared, valueNotPreserved };
  });
