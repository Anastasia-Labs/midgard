import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const SPEND_INPUT_SIGNER_MISSING_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofSpendInputSignerMissing"),
  step02: deployedTitle("fraudProofSpendInputSignerMissingStep02"),
  step03: deployedTitle("fraudProofSpendInputSignerMissingStep03"),
  step04: deployedTitle("fraudProofSpendInputSignerMissingStep04"),
  step05: deployedTitle("fraudProofSpendInputSignerMissingStep05"),
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
}: CertifiedFaultProofChainInputs): Effect.Effect<
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
        step05.spendingScriptHash,
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
