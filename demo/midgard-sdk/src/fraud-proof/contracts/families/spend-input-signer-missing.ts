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

export const SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
  step02: "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
  step03: "fraud_proofs/spend_input_signer_missing/step_03.main.spend",
  step04: "fraud_proofs/spend_input_signer_missing/step_04.main.spend",
  step05: "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
} as const;

export type SpendInputSignerMissingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly spendInputSignerMissing: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildSpendInputSignerMissingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildSpendInputSignerMissingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  SpendInputSignerMissingFaultProofContracts["spendInputSignerMissing"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build spend-input-signer-missing step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build spend-input-signer-missing step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build spend-input-signer-missing step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build spend-input-signer-missing step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build spend-input-signer-missing step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildSpendInputSignerMissingFaultProofContracts = (
  params: BuildSpendInputSignerMissingFaultProofContractsParams,
): Effect.Effect<SpendInputSignerMissingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const spendInputSignerMissing = yield* buildSpendInputSignerMissingChain({
      ...params,
      ...shared,
    });
    return { ...shared, spendInputSignerMissing };
  });
