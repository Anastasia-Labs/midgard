/** Honest re-execution of one committed normal transaction from DA replay, for
 * the validation-trace dispute (category 00000006). The challenger commits its
 * own trace, so nothing here reads a trace from the DA payload. */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  commitValidationTraceV1,
  RejectCodes,
  replayValidationVerdictEvent,
  requiresRealValidationDescriptorV1,
  type ValidationMachineReplayInput,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { SourceEventRecord } from "../transition-trace/reconstruct.js";

export type CommittedL2Event = Extract<
  SourceEventRecord,
  { readonly phase: "L2Transaction" }
>;

/**
 * Rule R-DESC, read from the authenticated source bytes: only an accepted
 * normal transaction with a non-empty redeemer field runs Plutus.
 */
export const eventRunsPlutus = (
  event: SourceEventRecord,
): event is CommittedL2Event =>
  event.phase === "L2Transaction" &&
  event.entry.validity === "TxIsValid" &&
  requiresRealValidationDescriptorV1(event.entry.fullTransactionCbor, "normal");

/**
 * The challenger's replay input for `event` over its event pre-state ledger.
 * The claim derives the trace context from the header, so the context's time
 * element is `header.end_time`; validity is decided by `header.block_slot`.
 * The honest trace rejects, so the ledger is unchanged.
 */
export const plutusFailureReplayInput = ({
  header,
  event,
  ledger,
  l1SlotConfig,
}: {
  readonly header: SDK.Header;
  readonly event: CommittedL2Event;
  readonly ledger: CheckedLedger;
  readonly l1SlotConfig: L1SlotConfig;
}): ValidationMachineReplayInput => ({
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from(Data.to(event.eventKey, SDK.EventKey), "hex"),
  transactionId: Buffer.from(event.entry.txId, "hex"),
  canonicalTransactionCbor: Buffer.from(event.entry.fullTransactionCbor),
  sourceKind: "normal",
  priorUtxosRoot: ledger.utxosRoot,
  postUtxosRoot: ledger.utxosRoot,
  ledgerWitnessEntries: ledger.entries.map(({ outRef, output }) => ({
    outRef,
    output,
  })),
  expectedLedgerOps: [],
  ledgerMutationSteps: [],
  expectedVerdict: "rejected",
  expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
  blockTimeMs: Number(header.endTime),
  expectedNetworkId: header.expectedNetworkId,
  minFeeA: header.minFeeA,
  minFeeB: header.minFeeB,
  blockSlot: header.blockSlot,
  l1SlotConfig,
});

export type PlutusExecutionReplay =
  | { readonly kind: "no_failure" }
  | { readonly kind: "fails"; readonly stepCount: number }
  /** The verdict replay found a failed script run, but the trace replay did
   * not reproduce it. No claim is built on it. */
  | { readonly kind: "trace_disagrees"; readonly detail: string };

/**
 * Replays `event` by verdict first and builds the trace only for a failed
 * script run, so a block of passing transactions runs no proof-mode replay.
 */
export const replayPlutusExecution = async ({
  evidence,
  event,
  ledger,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly event: CommittedL2Event;
  readonly ledger: CheckedLedger;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<PlutusExecutionReplay> => {
  const input = plutusFailureReplayInput({
    header: evidence.header,
    event,
    ledger,
    l1SlotConfig,
  });
  const verdict = await Effect.runPromise(
    replayValidationVerdictEvent({
      consensusProfile: input.consensusProfile,
      canonicalTransactionCbor: input.canonicalTransactionCbor,
      ledgerWitnessEntries: input.ledgerWitnessEntries,
      blockTimeMs: input.blockTimeMs,
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      blockSlot: input.blockSlot,
      l1SlotConfig,
      sourceKind: "normal",
    }),
  );
  if (
    verdict.verdict !== "rejected" ||
    verdict.rejection?.subject?.arm !== "PlutusExecutionFailed"
  )
    return { kind: "no_failure" };
  const committed = await Effect.runPromise(
    Effect.either(commitValidationTraceV1(input)),
  );
  if (committed._tag === "Left") {
    if (committed.left instanceof ValidationTraceStopped)
      return { kind: "trace_disagrees", detail: committed.left.message };
    throw committed.left;
  }
  if (committed.right.descriptor.verdict !== "rejected")
    return {
      kind: "trace_disagrees",
      detail: "the trace replay did not end Rejected",
    };
  return { kind: "fails", stepCount: committed.right.descriptor.stepCount };
};
