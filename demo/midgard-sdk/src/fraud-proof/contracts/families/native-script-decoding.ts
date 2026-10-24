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

export const NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofNativeScriptDecoding"),
  step02: deployedTitle("fraudProofNativeScriptDecodingStep02"),
  step03OpenSubject: deployedTitle(
    "fraudProofNativeScriptDecodingStep03OpenSubject",
  ),
  step03BindDescriptor: deployedTitle(
    "fraudProofNativeScriptDecodingStep03BindDescriptor",
  ),
  step03AdvanceOrClose: deployedTitle(
    "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
  ),
  step04: deployedTitle("fraudProofNativeScriptDecodingStep04"),
} as const;

export type NativeScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nativeScriptDecoding: FraudProofChain & {
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

export type BuildNativeScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildNativeScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  NativeScriptDecodingFaultProofContracts["nativeScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step04,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build native-script-decoding step 04",
    );
    const step03AdvanceOrClose = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03AdvanceOrClose,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build native-script-decoding step 03 advance-or-close",
    );
    const step03BindDescriptor = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03BindDescriptor,
      [
        step03AdvanceOrClose.spendingScriptHash,
        step04.spendingScriptHash,
        computationThread.policyId,
      ],
      "Failed to build native-script-decoding step 03 bind-descriptor",
    );
    const step03OpenSubject = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03OpenSubject,
      [
        step03BindDescriptor.spendingScriptHash,
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build native-script-decoding step 03 open-subject",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step02,
      [step03OpenSubject.spendingScriptHash, computationThread.policyId],
      "Failed to build native-script-decoding step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build native-script-decoding step 01",
    );
    return {
      firstStep: step01,
      steps: [
        step01,
        step02,
        step03OpenSubject,
        step03BindDescriptor,
        step03AdvanceOrClose,
        step04,
      ],
    };
  });

export const buildNativeScriptDecodingFaultProofContracts = (
  params: BuildNativeScriptDecodingFaultProofContractsParams,
): Effect.Effect<NativeScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nativeScriptDecoding = yield* buildNativeScriptDecodingChain({
      ...params,
      ...shared,
    });
    return { ...shared, nativeScriptDecoding };
  });
