/** The challenger's claim for a validation-trace dispute: the committed claim
 * witness that names and authenticates the event, and the challenger's own
 * trace root, step count and Terminal Rejected leaf, built from DA replay. */
import {
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildTraceAtStepCountV1,
  DirectValidationTraceUnavailable,
  openValidationTraceStepsV1,
  openValidationTraceV1,
  type ValidationTraceSteps,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
  membershipProof,
  sourceEventOrThrow,
} from "../transition-trace/witnesses.build-source-membership-proof.js";
import { WorkflowArtifactPendingError } from "../workflow/workflow-artifact-pending.js";
import { type CommittedL2Event, plutusReplayInput } from "./replay.js";
import {
  recordedSelectionOf,
  selectedScriptItemReader,
  type ValidationTraceSelection,
  type ValidationTraceSelectionSource,
} from "./selected-scripts.js";

export const VALIDATION_CLAIM_WITNESS_VERSION = 1n;

/** The readiness state of a claim whose build failed without a result. */
export const VALIDATION_TRACE_CLAIM_BUILD_HELD =
  "validation-trace-claim-build-held";

/**
 * The claim's re-execution or pre-state read failed without a result (the
 * evaluator's own failure, such as running out of memory): not a verdict on
 * the claim, so the run builds it again on the next observation. It is an
 * alarm once the case open window is at risk (stage-unbuilt.ts).
 */
export const validationTraceClaimBuildHeld = (
  cause: unknown,
): WorkflowArtifactPendingError =>
  new WorkflowArtifactPendingError(
    `${VALIDATION_TRACE_CLAIM_BUILD_HELD}: ${cause instanceof Error ? cause.message : String(cause)}`,
    Object.freeze({ name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: false }),
    { cause },
  );

/**
 * The claim witness for `eventKey`: its transition step and event-to-step
 * memberships, and its source membership under the forced or transactions
 * root. The step is located through the event-to-step map, so the two
 * memberships name one step.
 */
export const buildValidationClaimWitness = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.ValidationClaimWitness> => {
  const eventToStep = await buildEventToStepMembershipProof({
    reconstruction,
    eventKey,
  });
  const transitionStep = await buildIndexedTraceProof({
    reconstruction,
    stepIndex: eventToStep.value.step_index,
  });
  const event = sourceEventOrThrow(reconstruction, eventKey);
  let source: SDK.ValidationClaimWitness["source_membership"];
  if (event.phase === "ForcedTransaction")
    source = {
      ForcedValidationSource: {
        membership: await buildForcedTransactionLeafMembershipProof({
          reconstruction,
          eventKey,
        }),
      },
    };
  else if (event.phase === "L2Transaction")
    source = {
      NormalValidationSource: {
        membership: await membershipProof({
          root: reconstruction.rootData.transactions,
          entry: {
            key: event.entry.txId,
            value: event.entry.value,
            keyBytes: event.entry.keyBytes,
            valueBytes: event.entry.valueBytes,
          },
        }),
      },
    };
  else
    throw new Error(
      `validation claim: a ${event.phase} event has no validation source`,
    );
  return {
    version: VALIDATION_CLAIM_WITNESS_VERSION,
    transition_step_membership: transitionStep,
    event_to_step_membership: eventToStep,
    source_membership: source,
  };
};

export type ValidationTraceClaimMaterial = {
  readonly claimWitness: SDK.ValidationClaimWitness;
  /** The challenger's trace root over states `0..stepCount`. */
  readonly challengerRoot: string;
  readonly stepCount: bigint;
  /** State `stepCount`: Terminal Rejected. */
  readonly terminal: MidgardValidationMachineState;
  readonly terminalLeafProof: MidgardValidationTraceProof;
  /** State 0, which VerifySource re-derives from committed data (N5). */
  readonly initialState: MidgardValidationMachineState;
  /** The proof of any leaf under `challengerRoot`, for later moves. */
  readonly leafProof: (index: number) => MidgardValidationTraceProof;
  /** The last index of a state of ours hashing to `stateHashHex`. */
  readonly stateIndexOf: (stateHashHex: string) => number | undefined;
  /**
   * States `stateIndex` and `stateIndex + 1` of the same trace and their
   * work witnesses, rebuilt from the same replay on first use: a one-step
   * resolution reads its evidence from them. Only these are held, not the
   * whole witnessed trace.
   */
  readonly steps: (stateIndex: number) => Promise<ValidationTraceSteps>;
  /**
   * `steps` of every index in `stateIndices`, in their order, from ONE
   * replay over the indices not yet read: a pass planning several one-step
   * resolutions pays one replay, and later `steps` reads are served from it.
   */
  readonly stepsAt: (
    stateIndices: readonly number[],
  ) => Promise<readonly ValidationTraceSteps[]>;
  /** Every CEK script selection of the trace that rests on a decode record,
   * in trace order (`recordedSelectionOf`), rebuilt on first use. */
  readonly selections: () => Promise<readonly ValidationTraceSelection[]>;
  /**
   * The versioned item of the script a CEK script selection of the trace
   * runs, from the disputed transaction or its resolved inputs; throws
   * unless it is the item the selection commits to.
   */
  readonly scriptItem: (selection: ValidationTraceSelectionSource) => Buffer;
};

/** States `stateIndex` and `stateIndex + 1` of `trace`, as `steps` holds them. */
const windowOf = (
  trace: ValidationTraceSteps,
  stateIndex: number,
): ValidationTraceSteps => {
  const pick = <Value>(byIndex: {
    readonly [programCounter: number]: Value | undefined;
  }) =>
    Object.fromEntries(
      [stateIndex, stateIndex + 1]
        .filter((index) => index in byIndex)
        .map((index) => [index, byIndex[index]]),
    );
  return { states: pick(trace.states), witnesses: pick(trace.witnesses) };
};

/**
 * The claim material for a committed normal transaction whose honest
 * re-execution over its event pre-state `ledger` ends Terminal Rejected. It
 * fails unless the trace replay reproduces that rejection.
 */
export const buildValidationTraceClaimMaterial = async ({
  evidence,
  event,
  ledger,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly event: CommittedL2Event;
  readonly ledger: CheckedLedger;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<ValidationTraceClaimMaterial> => {
  const input = plutusReplayInput({
    header: evidence.header,
    event,
    ledger,
    l1SlotConfig,
    verdict: { verdict: "rejected" },
  });
  const opened = await Effect.runPromise(
    Effect.either(openValidationTraceV1(input, { revealStates: [0] })),
  ).catch((defect: unknown) => {
    throw validationTraceClaimBuildHeld(defect);
  });
  if (opened._tag === "Left") {
    const stop = opened.left;
    // The same deterministic outcomes the detector names a disagreement.
    if (
      (stop instanceof ValidationTraceStopped &&
        stop.reason === "disagreement") ||
      stop instanceof DirectValidationTraceUnavailable
    )
      throw new Error(
        `validation claim: the trace replay disagrees: ${stop.message}`,
      );
    throw validationTraceClaimBuildHeld(stop);
  }
  const leaves = opened.right;
  const terminal = leaves.states.get(leaves.stepCount);
  const initialState = leaves.states.get(0);
  if (
    terminal === undefined ||
    initialState === undefined ||
    terminal.phase !== "terminal" ||
    terminal.verdict !== "rejected"
  )
    throw new Error(
      "validation claim: the honest trace is not Terminal Rejected",
    );
  const trace = buildTraceAtStepCountV1(leaves, leaves.stepCount);
  const steps = new Map<number, Promise<ValidationTraceSteps>>();
  // Every index of `stateIndices` not yet read is read by one replay that
  // keeps it and its successor, then split into per-index windows.
  const readSteps = (stateIndices: readonly number[]) => {
    const unread = [...new Set(stateIndices)].filter(
      (index) => !steps.has(index),
    );
    if (unread.length > 0) {
      const kept = new Set(unread.flatMap((index) => [index, index + 1]));
      const replay = Effect.runPromise(
        openValidationTraceStepsV1(input, (index) => kept.has(index)),
      );
      for (const index of unread)
        steps.set(
          index,
          replay.then((trace) => windowOf(trace, index)),
        );
    }
    return stateIndices.map((index) => steps.get(index)!);
  };
  let selections: Promise<readonly ValidationTraceSelection[]> | undefined;
  if (!trace.descriptor.traceRoot.equals(leaves.descriptor.traceRoot))
    throw new Error(
      "validation claim: trace root does not match its commitment",
    );
  return {
    claimWitness: await buildValidationClaimWitness({
      reconstruction: evidence.reconstruction,
      eventKey: event.eventKey,
    }),
    challengerRoot: trace.descriptor.traceRoot.toString("hex"),
    stepCount: BigInt(leaves.stepCount),
    terminal,
    terminalLeafProof: trace.proof(leaves.stepCount),
    initialState,
    leafProof: trace.proof,
    stateIndexOf: (stateHashHex) => {
      const hash = Buffer.from(stateHashHex, "hex");
      for (let index = leaves.stepCount; index >= 0; index -= 1)
        if (
          hash.equals(leaves.stateHashes.subarray(index * 32, index * 32 + 32))
        )
          return index;
      return undefined;
    },
    steps: (stateIndex) => readSteps([stateIndex])[0]!,
    stepsAt: (stateIndices) => Promise.all(readSteps(stateIndices)),
    selections: () =>
      (selections ??= Effect.runPromise(
        openValidationTraceStepsV1(
          input,
          (_, witness) => recordedSelectionOf(witness) !== undefined,
        ),
      ).then(({ witnesses }) =>
        Object.values(witnesses).flatMap((witness) => {
          const selection = recordedSelectionOf(witness);
          return selection === undefined ? [] : [selection];
        }),
      )),
    scriptItem: selectedScriptItemReader({
      transactionCbor: event.entry.fullTransactionCbor,
      ledger,
    }),
  };
};
