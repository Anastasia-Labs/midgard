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

export const UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/unused_script_witness/step_01.main.spend",
  step02: "fraud_proofs/unused_script_witness/step_02.main.spend",
  step03: "fraud_proofs/unused_script_witness/step_03.main.spend",
  step04: "fraud_proofs/unused_script_witness/step_04.main.spend",
  step05: "fraud_proofs/unused_script_witness/step_05.main.spend",
  step06: "fraud_proofs/unused_script_witness/step_06.main.spend",
} as const;

export type UnusedScriptWitnessFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly unusedScriptWitness: FraudProofChain & {
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

export type BuildUnusedScriptWitnessFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildUnusedScriptWitnessChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  UnusedScriptWitnessFaultProofContracts["unusedScriptWitness"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build unused-script-witness step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-script-witness step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-script-witness step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-script-witness step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-script-witness step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build unused-script-witness step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06],
    };
  });

export const buildUnusedScriptWitnessFaultProofContracts = (
  params: BuildUnusedScriptWitnessFaultProofContractsParams,
): Effect.Effect<UnusedScriptWitnessFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const unusedScriptWitness = yield* buildUnusedScriptWitnessChain({
      ...params,
      ...shared,
    });
    return { ...shared, unusedScriptWitness };
  });
