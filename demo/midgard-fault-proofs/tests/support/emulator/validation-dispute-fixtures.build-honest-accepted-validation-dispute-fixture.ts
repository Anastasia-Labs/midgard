import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
} from "@al-ft/midgard-core";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import {
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";

import {
  type ForcedValidationDisputeFixture,
  rejectingTerminalWorkRoot,
} from "./validation-dispute-fixtures.build-accepted-claim-over-rejecting-transaction-fixture.js";
import { buildForcedValidationDisputeCommitments } from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildNativeTransactionTrace } from "./validation-dispute-fixtures.build-native-transaction-trace.js";

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
export const buildHonestAcceptedValidationDisputeFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedPhase: string }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildNativeTransactionTrace({
    now,
    txOrderSeed: "e3",
  });
  const disputedLowIndex = operatorTrace.states.findIndex(
    (state) => state.phase === "inputSets",
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      "honest accepted validation trace is missing its inputSets instruction",
    );
  }
  const preState = operatorTrace.states[disputedLowIndex]!;
  const forgedRejectionCode = RejectCodes.EmptyInputs;
  const forgedTerminal = {
    ...preState,
    phase: "terminal" as const,
    programCounter: preState.programCounter + 1,
    workRoot: rejectingTerminalWorkRoot({
      programCounter: preState.programCounter + 1,
      rejectionCode: forgedRejectionCode,
      priorLedgerRoot: preState.priorLedgerRoot,
    }),
    verdict: "rejected" as const,
    rejectionCodeHash: Buffer.from(
      hashMidgardValidationRejectionCode(forgedRejectionCode),
    ),
  };
  const challengerStates = operatorTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const challengerTrace: DeterministicValidationMachineTrace = {
    ...operatorTrace,
    states: challengerStates,
    tree: buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineState),
      "rejected",
      forgedTerminal.rejectionCodeHash,
    ),
    verdict: "rejected",
    rejectionCode: forgedRejectionCode,
  };
  const evidence = buildValidationDisputeEvidenceBundle({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase: preState.phase,
  };
};
