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

export const NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/native_script_invalid/step_01.main.spend",
  step02: "fraud_proofs/native_script_invalid/step_02.main.spend",
  step03: "fraud_proofs/native_script_invalid/step_03.main.spend",
  step04: "fraud_proofs/native_script_invalid/step_04.main.spend",
  step05: "fraud_proofs/native_script_invalid/step_05.main.spend",
} as const;

export type NativeScriptInvalidFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly nativeScriptInvalid: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildNativeScriptInvalidFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildNativeScriptInvalidChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  NativeScriptInvalidFaultProofContracts["nativeScriptInvalid"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step05,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build native-script-invalid step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build native-script-invalid step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build native-script-invalid step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build native-script-invalid step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_INVALID_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build native-script-invalid step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildNativeScriptInvalidFaultProofContracts = (
  params: BuildNativeScriptInvalidFaultProofContractsParams,
): Effect.Effect<NativeScriptInvalidFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nativeScriptInvalid = yield* buildNativeScriptInvalidChain({
      ...params,
      ...shared,
    });
    return { ...shared, nativeScriptInvalid };
  });
