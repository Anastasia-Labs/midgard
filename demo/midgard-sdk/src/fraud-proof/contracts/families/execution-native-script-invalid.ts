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

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofExecutionNativeScriptInvalid"),
  acceptedReconstructionInit: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
  ),
  acceptedSpendPrefix: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
  ),
  acceptedMintPrefix: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
  ),
  acceptedObserverPrefix: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
  ),
  acceptedReceivePrefix: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
  ),
  acceptedInlineSource: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
  ),
  acceptedReferenceSource: deployedTitle(
    "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
  ),
  step02: deployedTitle("fraudProofExecutionNativeScriptInvalidStep02"),
  step03: deployedTitle("fraudProofExecutionNativeScriptInvalidStep03"),
  step04: deployedTitle("fraudProofExecutionNativeScriptInvalidStep04"),
  step05: deployedTitle("fraudProofExecutionNativeScriptInvalidStep05"),
  step06: deployedTitle("fraudProofExecutionNativeScriptInvalidStep06"),
} as const;

export type ExecutionNativeScriptInvalidFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly executionNativeScriptInvalid: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildExecutionNativeScriptInvalidFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildExecutionNativeScriptInvalidChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  ExecutionNativeScriptInvalidFaultProofContracts["executionNativeScriptInvalid"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build execution-native-script-invalid step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step05,
      [
        step06.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-native-script-invalid step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-native-script-invalid step 02",
    );
    const acceptedReferenceSource = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedReferenceSource,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted reference source",
    );
    const acceptedInlineSource = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedInlineSource,
      [
        acceptedReferenceSource.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted inline source",
    );
    const acceptedReceivePrefix = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedReceivePrefix,
      [
        acceptedInlineSource.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted receive prefix",
    );
    const acceptedObserverPrefix = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedObserverPrefix,
      [
        acceptedReceivePrefix.spendingScriptHash,
        acceptedInlineSource.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted observer prefix",
    );
    const acceptedMintPrefix = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedMintPrefix,
      [
        acceptedObserverPrefix.spendingScriptHash,
        acceptedInlineSource.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted mint prefix",
    );
    const acceptedSpendPrefix = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedSpendPrefix,
      [
        acceptedMintPrefix.spendingScriptHash,
        acceptedInlineSource.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build execution-native-script-invalid accepted spend prefix",
    );
    const acceptedReconstructionInit = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.acceptedReconstructionInit,
      [acceptedSpendPrefix.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-native-script-invalid accepted reconstruction init",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step01,
      [
        acceptedReconstructionInit.spendingScriptHash,
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build execution-native-script-invalid step 01",
    );
    return {
      firstStep: step01,
      steps: [
        step01,
        step02,
        step03,
        step04,
        step05,
        step06,
        acceptedReconstructionInit,
        acceptedSpendPrefix,
        acceptedMintPrefix,
        acceptedObserverPrefix,
        acceptedReceivePrefix,
        acceptedInlineSource,
        acceptedReferenceSource,
      ],
    };
  });

export const buildExecutionNativeScriptInvalidFaultProofContracts = (
  params: BuildExecutionNativeScriptInvalidFaultProofContractsParams,
): Effect.Effect<ExecutionNativeScriptInvalidFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const executionNativeScriptInvalid =
      yield* buildExecutionNativeScriptInvalidChain({ ...params, ...shared });
    return { ...shared, executionNativeScriptInvalid };
  });
