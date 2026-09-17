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

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/execution_native_script_invalid/step_01.main.spend",
  acceptedReconstructionInit:
    "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
  acceptedSpendPrefix:
    "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
  acceptedMintPrefix:
    "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
  acceptedObserverPrefix:
    "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
  acceptedReceivePrefix:
    "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
  acceptedInlineSource:
    "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
  acceptedReferenceSource:
    "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
  step02: "fraud_proofs/execution_native_script_invalid/step_02.main.spend",
  step03: "fraud_proofs/execution_native_script_invalid/step_03.main.spend",
  step04: "fraud_proofs/execution_native_script_invalid/step_04.main.spend",
  step05: "fraud_proofs/execution_native_script_invalid/step_05.main.spend",
  step06: "fraud_proofs/execution_native_script_invalid/step_06.main.spend",
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
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
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
        step03.spendingScriptHash,
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
