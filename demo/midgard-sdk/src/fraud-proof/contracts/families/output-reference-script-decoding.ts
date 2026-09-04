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

export const OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
  step03: "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
  step04: "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
  step05: "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
  step06: "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
} as const;

export type OutputReferenceScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly outputReferenceScriptDecoding: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildOutputReferenceScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildOutputReferenceScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  OutputReferenceScriptDecodingFaultProofContracts["outputReferenceScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step06,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build output-reference-script-decoding step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build output-reference-script-decoding step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build output-reference-script-decoding step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build output-reference-script-decoding step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build output-reference-script-decoding step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build output-reference-script-decoding step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06],
    };
  });

export const buildOutputReferenceScriptDecodingFaultProofContracts = (
  params: BuildOutputReferenceScriptDecodingFaultProofContractsParams,
): Effect.Effect<OutputReferenceScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const outputReferenceScriptDecoding =
      yield* buildOutputReferenceScriptDecodingChain({ ...params, ...shared });
    return { ...shared, outputReferenceScriptDecoding };
  });
