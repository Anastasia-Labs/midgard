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

export const MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_native_script_utxo/step_01.main.spend",
  step02: "fraud_proofs/missing_native_script_utxo/step_02.main.spend",
  step03: "fraud_proofs/missing_native_script_utxo/step_03.main.spend",
  step04: "fraud_proofs/missing_native_script_utxo/step_04.main.spend",
  step05: "fraud_proofs/missing_native_script_utxo/step_05.main.spend",
  step06: "fraud_proofs/missing_native_script_utxo/step_06.main.spend",
  step07: "fraud_proofs/missing_native_script_utxo/step_07.main.spend",
} as const;

export type MissingNativeScriptUtxoFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly missingNativeScriptUtxo: FraudProofChain & {
    readonly steps: readonly [
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

export type BuildMissingNativeScriptUtxoFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMissingNativeScriptUtxoChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingNativeScriptUtxoFaultProofContracts["missingNativeScriptUtxo"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step07 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step07,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-utxo step 07",
    );
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step06,
      [
        step07.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-utxo step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step05,
      [
        step06.spendingScriptHash,
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-utxo step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-native-script-utxo step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-native-script-utxo step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-utxo step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_UTXO_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-native-script-utxo step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06, step07],
    };
  });

export const buildMissingNativeScriptUtxoFaultProofContracts = (
  params: BuildMissingNativeScriptUtxoFaultProofContractsParams,
): Effect.Effect<MissingNativeScriptUtxoFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingNativeScriptUtxo = yield* buildMissingNativeScriptUtxoChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingNativeScriptUtxo };
  });
