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

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/execution_source_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/execution_source_script_decoding/step_02.main.spend",
  step03: "fraud_proofs/execution_source_script_decoding/step_03.main.spend",
  step04: "fraud_proofs/execution_source_script_decoding/step_04.main.spend",
  step05: "fraud_proofs/execution_source_script_decoding/step_05.main.spend",
} as const;

export type ExecutionSourceScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly executionSourceScriptDecoding: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildExecutionSourceScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildExecutionSourceScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ExecutionSourceScriptDecodingFaultProofContracts["executionSourceScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step05,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build execution-source-script-decoding step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-source-script-decoding step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-source-script-decoding step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build execution-source-script-decoding step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      EXECUTION_SOURCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build execution-source-script-decoding step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildExecutionSourceScriptDecodingFaultProofContracts = (
  params: BuildExecutionSourceScriptDecodingFaultProofContractsParams,
): Effect.Effect<ExecutionSourceScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const executionSourceScriptDecoding =
      yield* buildExecutionSourceScriptDecodingChain({ ...params, ...shared });
    return { ...shared, executionSourceScriptDecoding };
  });
