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

export const UNUSED_REDEEMER_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/unused_redeemer/step_01.main.spend",
  step02: "fraud_proofs/unused_redeemer/step_02.main.spend",
  step02a: "fraud_proofs/unused_redeemer/step_02a.main.spend",
  step02b: "fraud_proofs/unused_redeemer/step_02b.main.spend",
  step02c: "fraud_proofs/unused_redeemer/step_02c.main.spend",
  step03: "fraud_proofs/unused_redeemer/step_03.main.spend",
  step04: "fraud_proofs/unused_redeemer/step_04.main.spend",
  step05: "fraud_proofs/unused_redeemer/step_05.main.spend",
  step06: "fraud_proofs/unused_redeemer/step_06.main.spend",
} as const;

export type UnusedRedeemerFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly unusedRedeemer: FraudProofChain & {
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
    ];
  };
};

export type BuildUnusedRedeemerFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildUnusedRedeemerChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  UnusedRedeemerFaultProofContracts["unusedRedeemer"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build unused-redeemer step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 03",
    );
    const step02c = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02c,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02c",
    );
    const step02b = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02b,
      [step02c.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02b",
    );
    const step02a = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02a,
      [step02b.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02a",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02,
      [step02a.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build unused-redeemer step 01",
    );
    return {
      firstStep: step01,
      steps: [
        step01,
        step02,
        step02a,
        step02b,
        step02c,
        step03,
        step04,
        step05,
        step06,
      ],
    };
  });

export const buildUnusedRedeemerFaultProofContracts = (
  params: BuildUnusedRedeemerFaultProofContractsParams,
): Effect.Effect<UnusedRedeemerFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const unusedRedeemer = yield* buildUnusedRedeemerChain({
      ...params,
      ...shared,
    });
    return { ...shared, unusedRedeemer };
  });
