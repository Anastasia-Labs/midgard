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

export const INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/input_set_uniqueness/step_01.main.spend",
  step02: "fraud_proofs/input_set_uniqueness/step_02.main.spend",
  step03: "fraud_proofs/input_set_uniqueness/step_03.main.spend",
  step04: "fraud_proofs/input_set_uniqueness/step_04.main.spend",
} as const;

export type InputSetUniquenessFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly inputSetUniqueness: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildInputSetUniquenessFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildInputSetUniquenessChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  InputSetUniquenessFaultProofContracts["inputSetUniqueness"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build input-set-uniqueness step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build input-set-uniqueness step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build input-set-uniqueness step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        step03.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build input-set-uniqueness step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

export const buildInputSetUniquenessFaultProofContracts = (
  params: BuildInputSetUniquenessFaultProofContractsParams,
): Effect.Effect<InputSetUniquenessFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const inputSetUniqueness = yield* buildInputSetUniquenessChain({
      ...params,
      ...shared,
    });
    return { ...shared, inputSetUniqueness };
  });
