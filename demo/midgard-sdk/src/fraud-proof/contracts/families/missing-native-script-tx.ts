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

export const MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_native_script_tx/step_01.main.spend",
  step02: "fraud_proofs/missing_native_script_tx/step_02.main.spend",
  step03: "fraud_proofs/missing_native_script_tx/step_03.main.spend",
  step04: "fraud_proofs/missing_native_script_tx/step_04.main.spend",
  step05: "fraud_proofs/missing_native_script_tx/step_05.main.spend",
  step06: "fraud_proofs/missing_native_script_tx/step_06.main.spend",
  step07: "fraud_proofs/missing_native_script_tx/step_07.main.spend",
  step08: "fraud_proofs/missing_native_script_tx/step_08.main.spend",
} as const;

export type MissingNativeScriptTxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly missingNativeScriptTx: FraudProofChain & {
    readonly steps: readonly [
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

export type BuildMissingNativeScriptTxFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMissingNativeScriptTxChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingNativeScriptTxFaultProofContracts["missingNativeScriptTx"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step08 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step08,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 08",
    );
    const step07 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step07,
      [
        step08.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 07",
    );
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step06,
      [
        step07.spendingScriptHash,
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-native-script-tx step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-native-script-tx step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-native-script-tx step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06, step07, step08],
    };
  });

export const buildMissingNativeScriptTxFaultProofContracts = (
  params: BuildMissingNativeScriptTxFaultProofContractsParams,
): Effect.Effect<MissingNativeScriptTxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingNativeScriptTx = yield* buildMissingNativeScriptTxChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingNativeScriptTx };
  });
