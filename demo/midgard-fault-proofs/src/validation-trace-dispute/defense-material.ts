/** The defender's side of a validation-trace case: our honest trace of the
 * case's event, replayed from our own retained DA over our own ledger, at the
 * case's step count. It defends when our state there is not Terminal, or is
 * Terminal Accepted; an event our block accepted that the replay rejects is
 * our own fault, and is never defended. */
import {
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildTraceAtStepCountV1,
  DirectValidationTraceUnavailable,
  openValidationTraceV1,
  type RejectedTx,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerBeforeTransactionEvent } from "../input-existence/event-ledger.js";
import type { CheckedLedger } from "../ledger-at.js";
import {
  acceptedLedgerMutation,
  eventRunsPlutus,
  plutusReplayInput,
  replayPlutusVerdict,
} from "./replay.js";

export type ValidationTraceDefenseMaterial =
  /** Our state at the case's step count may defend the case. */
  | Readonly<{
      kind: "defend";
      /** Our trace root at the case's step count. */
      root: string;
      /** S, the case's step count. */
      stepCount: number;
      /** Our state at S, and its proof under `root`. */
      stateAtS: MidgardValidationMachineState;
      proofAtS: MidgardValidationTraceProof;
      /** The proof of any of our states under `root`, for the reveals. */
      proofAt: (index: number) => MidgardValidationTraceProof;
      /** Our state 0's hash: the case's agreed initial hash, when honest. */
      initialStateHash: Buffer;
      /** T, the length of our honest trace. */
      honestStepCount: number;
    }>
  /** Our block accepted an event that the replay rejects. */
  | Readonly<{ kind: "operator_fault"; rejection: RejectedTx | undefined }>
  /** The verdict replay accepts, but the trace replay deterministically
   * does not reproduce an accepted run. Never retried. */
  | Readonly<{ kind: "trace_disagrees"; detail: string }>;

/**
 * Our defense material for a case on `evidence`'s event `eventKey` at
 * `stepCount` steps, over the event's pre-state ledger walked from
 * `predecessor` (our landed ledger at the parent header, already checked
 * against its root).
 *
 * Only a deterministic outcome is returned. Anything else throws, including
 * a trace build stopped as `unavailable`, so the caller retries.
 */
export const buildValidationTraceDefenseMaterial = async ({
  evidence,
  predecessor,
  eventKey,
  stepCount,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly eventKey: SDK.EventKey;
  readonly stepCount: number;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<ValidationTraceDefenseMaterial> => {
  const keyCbor = Data.to(eventKey, SDK.EventKey);
  const event = evidence.reconstruction.sourceEvents.find(
    (candidate) => Data.to(candidate.eventKey, SDK.EventKey) === keyCbor,
  );
  if (event === undefined || !eventRunsPlutus(event))
    throw new Error(
      "validation defense: the case's event is not an accepted normal transaction of the block that runs Plutus",
    );
  const opening = evidence.reconstruction.eventToStepByFingerprint.get(
    event.fingerprint,
  );
  if (opening === undefined || opening.value.phase !== event.phase)
    throw new Error(
      "validation defense: the case's event has no committed event-to-step opening",
    );
  const ledger = await ledgerBeforeTransactionEvent({
    block: evidence,
    eventKey: event.eventKey,
    stepIndex: opening.value.step_index,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const replay = {
    header: evidence.header,
    event,
    ledger,
    l1SlotConfig,
  } as const;
  const verdict = await replayPlutusVerdict(
    plutusReplayInput({ ...replay, verdict: { verdict: "rejected" } }),
  );
  if (verdict.verdict !== "accepted")
    return { kind: "operator_fault", rejection: verdict.rejection };
  const input = plutusReplayInput({
    ...replay,
    verdict: {
      verdict: "accepted",
      mutation: await acceptedLedgerMutation(ledger, verdict.statePatch),
    },
  });
  const opened = await Effect.runPromise(
    Effect.either(openValidationTraceV1(input, { revealStates: [stepCount] })),
  );
  if (opened._tag === "Left") {
    const stop = opened.left;
    if (
      (stop instanceof ValidationTraceStopped &&
        stop.reason === "disagreement") ||
      stop instanceof DirectValidationTraceUnavailable
    )
      return { kind: "trace_disagrees", detail: stop.message };
    throw stop;
  }
  const leaves = opened.right;
  const stateAtS = leaves.states.get(stepCount)!;
  if (stateAtS.phase === "terminal" && stateAtS.verdict !== "accepted")
    return {
      kind: "trace_disagrees",
      detail: "the accepted trace replay ended other than Accepted",
    };
  const trace = buildTraceAtStepCountV1(leaves, stepCount);
  return {
    kind: "defend",
    root: trace.descriptor.traceRoot.toString("hex"),
    stepCount,
    stateAtS,
    proofAtS: trace.proof(stepCount),
    proofAt: trace.proof,
    initialStateHash: trace.stateHash(0),
    honestStepCount: leaves.stepCount,
  };
};
