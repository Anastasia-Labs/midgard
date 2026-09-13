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

export const MISSING_REDEEMER_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_redeemer/step_01.main.spend",
  step02: "fraud_proofs/missing_redeemer/step_02.main.spend",
  step02a: "fraud_proofs/missing_redeemer/step_02a.main.spend",
  step02b: "fraud_proofs/missing_redeemer/step_02b.main.spend",
  step03: "fraud_proofs/missing_redeemer/step_03.main.spend",
  step04: "fraud_proofs/missing_redeemer/step_04.main.spend",
  step05: "fraud_proofs/missing_redeemer/step_05.main.spend",
} as const;

export type MissingRedeemerFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly missingRedeemer: FraudProofChain & {
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

export type BuildMissingRedeemerFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMissingRedeemerChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingRedeemerFaultProofContracts["missingRedeemer"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build missing-redeemer step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-redeemer step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-redeemer step 03",
    );
    const step02b = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step02b,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-redeemer step 02b",
    );
    const step02a = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step02a,
      [step02b.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-redeemer step 02a",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step02,
      [step02a.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-redeemer step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-redeemer step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step02a, step02b, step03, step04, step05],
    };
  });

export const buildMissingRedeemerFaultProofContracts = (
  params: BuildMissingRedeemerFaultProofContractsParams,
): Effect.Effect<MissingRedeemerFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingRedeemer = yield* buildMissingRedeemerChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingRedeemer };
  });
