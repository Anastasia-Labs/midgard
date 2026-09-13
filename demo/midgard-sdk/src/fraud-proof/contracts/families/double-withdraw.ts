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

export const DOUBLE_WITHDRAW_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_withdraw/step_01.main.spend",
  step02: "fraud_proofs/double_withdraw/step_02.main.spend",
} as const;

export type DoubleWithdrawFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleWithdraw: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildDoubleWithdrawFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildDoubleWithdrawChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  DoubleWithdrawFaultProofContracts["doubleWithdraw"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      DOUBLE_WITHDRAW_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build double-withdraw step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      DOUBLE_WITHDRAW_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build double-withdraw step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildDoubleWithdrawFaultProofContracts = (
  params: BuildDoubleWithdrawFaultProofContractsParams,
): Effect.Effect<DoubleWithdrawFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleWithdraw = yield* buildDoubleWithdrawChain({
      ...params,
      ...shared,
    });
    return { ...shared, doubleWithdraw };
  });
