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
  buildChallengerDescriptorV1,
  buildDeterministicValidationMachineTrace,
  type DeterministicValidationMachineTrace,
  openValidationTraceV1,
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
import { type CommittedL2Event, plutusFailureReplayInput } from "./replay.js";
import {
  selectedScriptItemReader,
  type ValidationTraceSelectionSource,
} from "./selected-scripts.js";

export const VALIDATION_CLAIM_WITNESS_VERSION = 1n;

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
  /**
   * The same trace in full, its states and work witnesses, rebuilt from the
   * same replay on first use: a one-step resolution reads its evidence from
   * it.
   */
  readonly trace: () => Promise<DeterministicValidationMachineTrace>;
  /**
   * The versioned item of the script a CEK script selection of the trace
   * runs, from the disputed transaction or its resolved inputs; throws
   * unless it is the item the selection commits to.
   */
  readonly scriptItem: (selection: ValidationTraceSelectionSource) => Buffer;
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
  const input = plutusFailureReplayInput({
    header: evidence.header,
    event,
    ledger,
    l1SlotConfig,
  });
  const leaves = await Effect.runPromise(
    openValidationTraceV1(input, { revealStates: [0] }),
  );
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
  const trace = buildChallengerDescriptorV1(leaves, leaves.stepCount);
  let full: Promise<DeterministicValidationMachineTrace> | undefined;
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
    trace: () =>
      (full ??= Effect.runPromise(
        buildDeterministicValidationMachineTrace(input),
      )),
    scriptItem: selectedScriptItemReader({
      transactionCbor: event.entry.fullTransactionCbor,
      ledger,
    }),
  };
};
