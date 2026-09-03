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

export const CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/cross_block_duplicate_event/step_01.main.spend",
  step02: "fraud_proofs/cross_block_duplicate_event/step_02.main.spend",
} as const;

export type CrossBlockDuplicateEventFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly crossBlockDuplicateEvent: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildCrossBlockDuplicateEventFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildCrossBlockDuplicateEventChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CrossBlockDuplicateEventFaultProofContracts["crossBlockDuplicateEvent"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build cross-block-duplicate-event step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build cross-block-duplicate-event step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildCrossBlockDuplicateEventFaultProofContracts = (
  params: BuildCrossBlockDuplicateEventFaultProofContractsParams,
): Effect.Effect<CrossBlockDuplicateEventFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const crossBlockDuplicateEvent = yield* buildCrossBlockDuplicateEventChain({
      ...params,
      ...shared,
    });
    return { ...shared, crossBlockDuplicateEvent };
  });
