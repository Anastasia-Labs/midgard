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

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
  step02: "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend",
  step03: "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend",
  step04: "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend",
  step05: "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
} as const;

export type ScriptIntegrityHashMismatchFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly scriptIntegrityHashMismatch: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildScriptIntegrityHashMismatchFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildScriptIntegrityHashMismatchChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ScriptIntegrityHashMismatchFaultProofContracts["scriptIntegrityHashMismatch"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step05,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build script-integrity-hash-mismatch step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build script-integrity-hash-mismatch step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build script-integrity-hash-mismatch step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build script-integrity-hash-mismatch step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISMATCH_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build script-integrity-hash-mismatch step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildScriptIntegrityHashMismatchFaultProofContracts = (
  params: BuildScriptIntegrityHashMismatchFaultProofContractsParams,
): Effect.Effect<ScriptIntegrityHashMismatchFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const scriptIntegrityHashMismatch =
      yield* buildScriptIntegrityHashMismatchChain({ ...params, ...shared });
    return { ...shared, scriptIntegrityHashMismatch };
  });
