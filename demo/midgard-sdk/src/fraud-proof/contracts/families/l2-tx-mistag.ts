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

export const L2_TX_MISTAG_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
  step02: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
} as const;

export type L2TxMistagFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly l2TxMistag: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildL2TxMistagFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildL2TxMistagChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  L2TxMistagFaultProofContracts["l2TxMistag"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      L2_TX_MISTAG_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build l2-tx-mistag step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      L2_TX_MISTAG_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build l2-tx-mistag step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildL2TxMistagFaultProofContracts = (
  params: BuildL2TxMistagFaultProofContractsParams,
): Effect.Effect<L2TxMistagFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const l2TxMistag = yield* buildL2TxMistagChain({ ...params, ...shared });
    return { ...shared, l2TxMistag };
  });
