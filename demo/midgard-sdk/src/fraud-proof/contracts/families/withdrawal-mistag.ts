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

export const WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/withdrawal_mistag/step_01.main.spend",
  step02: "fraud_proofs/withdrawal_mistag/step_02.main.spend",
  step03: "fraud_proofs/withdrawal_mistag/step_03.main.spend",
  step04: "fraud_proofs/withdrawal_mistag/step_04.main.spend",
  step05: "fraud_proofs/withdrawal_mistag/step_05.main.spend",
} as const;

export type WithdrawalMistagFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly withdrawalMistag: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildWithdrawalMistagFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildWithdrawalMistagChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  WithdrawalMistagFaultProofContracts["withdrawalMistag"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step05,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build withdrawal-mistag step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build withdrawal-mistag step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildWithdrawalMistagFaultProofContracts = (
  params: BuildWithdrawalMistagFaultProofContractsParams,
): Effect.Effect<WithdrawalMistagFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const withdrawalMistag = yield* buildWithdrawalMistagChain({
      ...params,
      ...shared,
    });
    return { ...shared, withdrawalMistag };
  });
