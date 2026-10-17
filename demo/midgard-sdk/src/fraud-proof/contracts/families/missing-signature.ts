import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
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

export const MISSING_SIGNATURE_FAULT_PROOF_TITLES = {
  forcedStep: deployedTitle("fraudProofMissingSignatureForcedStep"),
  forcedSigner: deployedTitle("fraudProofMissingSignatureForcedSigner"),
  forcedWitness: deployedTitle("fraudProofMissingSignatureForcedWitness"),
  step01: deployedTitle("fraudProofMissingSignature"),
  step02: deployedTitle("fraudProofMissingSignatureStep02"),
  step03: deployedTitle("fraudProofMissingSignatureStep03"),
  step04: deployedTitle("fraudProofMissingSignatureStep04"),
} as const;

export type MissingSignatureFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly missingSignature: FraudProofChain & {
    readonly forcedStep: SpendingValidator;
    readonly forcedSigner: SpendingValidator;
    readonly forcedWitness: SpendingValidator;
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMissingSignatureFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMissingSignatureChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingSignatureFaultProofContracts["missingSignature"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-signature step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature step 02",
    );
    const forcedWitness = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.forcedWitness,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature forced witness",
    );
    const forcedSigner = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.forcedSigner,
      [
        forcedWitness.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature forced signer",
    );
    const forcedStep = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.forcedStep,
      [forcedSigner.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-signature forced step",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        forcedStep.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-signature step 01",
    );
    return {
      firstStep: step01,
      forcedStep,
      forcedSigner,
      forcedWitness,
      steps: [step01, step02, step03, step04],
    };
  });

export const buildMissingSignatureFaultProofContracts = (
  params: BuildMissingSignatureFaultProofContractsParams,
): Effect.Effect<MissingSignatureFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingSignature = yield* buildMissingSignatureChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingSignature };
  });
