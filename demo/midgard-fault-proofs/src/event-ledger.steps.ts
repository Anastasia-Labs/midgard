/**
 * The checks every event-ledger walk makes of a block's committed steps, and
 * the refusals they give: the step's trace opening and source event, its
 * phase order, the entries it spends and creates, and the selected step's
 * opening. The ledger store walk (`event-ledger.ledger.ts`) makes them.
 */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "./evidence/canonical-block-evidence.js";
import { type LedgerEntry, ledgerEntryLeaf } from "./ledger-at.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./transition-trace/reconstruct.transition-trace-reconstruction.js";
import {
  type ReplayPrerequisite,
  replayPrerequisiteFailure,
} from "./workflow/replay-prerequisite.js";

type EventLedgerRefusalReason =
  | "step_out_of_range"
  | "transition_link_changed"
  | "source_event_changed"
  | "phase_order_changed"
  | "withdrawal_input_absent"
  | "transaction_input_absent"
  | "transaction_output_already_present"
  | "post_root_changed"
  | "selected_pre_root_changed";

/** A committed prefix cannot supply the selected event's ledger authority.
 * Detectors surface a gap; no missing-output conviction is inferred from it.
 * `step` is the step the refusal is about: the step that failed to apply,
 * the step of a later phase placed before an earlier phase's step, or the
 * requested step when its own index or opening is refused. */
export class EventLedgerReplayRefusal extends Error {
  constructor(
    readonly reason: EventLedgerRefusalReason,
    readonly step: bigint,
  ) {
    super(`event ledger: replay refused (${reason})`);
    this.name = "EventLedgerReplayRefusal";
  }
}

/** Step `step` creates an output that is not a canonical ledger entry, so
 * no ledger after it can hold that output: the step's own output is the
 * fault. It is not a refusal, so a reader that cannot name that fault meets
 * it as the corruption it is. */
export class EventLedgerOutputNotCanonical extends Error {
  constructor(
    readonly step: bigint,
    cause: Error,
  ) {
    super(cause.message, { cause });
  }
}

export const refused = (
  reason: EventLedgerRefusalReason,
  step: number | bigint,
) => new EventLedgerReplayRefusal(reason, BigInt(step));

export const PHASES: readonly SDK.TransitionPhase[] = [
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
  "Deposit",
];

const phaseRank = (phase: SDK.TransitionPhase): number => PHASES.indexOf(phase);

const acceptedEffects = (
  event: SourceEventRecord,
): Readonly<{
  transactionId: string;
  inputs: readonly Buffer[];
  outputs: readonly Buffer[];
}> | null => {
  if (event.phase === "ForcedTransaction") {
    if (event.entry.value.verdict !== "ForcedTxValid") return null;
    const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
      event.entry.fullTransactionCbor,
    );
    return {
      transactionId: event.entry.value.tx_id,
      inputs: decodeMidgardFieldPreimage(tx.body.spendInputsPreimageCbor),
      outputs: decodeMidgardFieldPreimage(tx.body.outputsPreimageCbor),
    };
  }
  if (event.phase === "L2Transaction") {
    // A rejected normal transaction changes no entry, so its step must leave
    // the ledger as it found it. Its leaf is a block fault of its own: a
    // normal transactions_root leaf is an acceptance claim and must use
    // validity code zero, and l2TxMistag convicts any other code
    // (docs/fault-proofs/rejection-reason-catalogue-v1.md).
    if (event.entry.validity !== "TxIsValid") return null;
    return {
      transactionId: event.entry.txId,
      inputs: decodeMidgardFieldPreimage(event.entry.spendInputsPreimage),
      outputs: decodeMidgardFieldPreimage(event.entry.outputsPreimage),
    };
  }
  return null;
};

/** Step `index`'s trace opening and source event, refused unless the step
 * links from `preRoot`, the ledger before it. */
export const openStep = (
  reconstruction: TransitionTraceReconstruction,
  index: number,
  preRoot: string,
) => {
  const opening = reconstruction.traceByStepIndex.get(BigInt(index));
  const step = opening?.value;
  if (
    opening === undefined ||
    step === undefined ||
    opening.key !== BigInt(index) ||
    step.step_index !== BigInt(index) ||
    step.schema_version !== 1n ||
    step.pre_utxos_root !== preRoot
  )
    throw refused("transition_link_changed", index);
  const event = reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(step.event_key),
  );
  if (
    event === undefined ||
    event.phase !== step.phase ||
    Data.to(event.eventKey, SDK.EventKey) !==
      Data.to(step.event_key, SDK.EventKey)
  )
    throw refused("source_event_changed", index);
  return { step, event };
};

/** A walk's phase order so far: `firstAfter[rank]` is the first applied
 * step of a phase after `rank`, `priorRank` the last applied step's. */
export type PhaseOrder = {
  readonly firstAfter: (number | undefined)[];
  readonly priorRank: number;
};

/**
 * Step `index`'s phase rank, refused out of phase order. The refusal names
 * the step of the later phase that comes first: under the header's counts
 * its index lies in an earlier phase's band. A deposit applies before an
 * earlier phase's requested event only as that first step. Records the step
 * as the first after each earlier phase before its effects can fail.
 */
export const orderStep = (
  order: PhaseOrder,
  phase: SDK.TransitionPhase,
  index: number,
): number => {
  const stepRank = phaseRank(phase);
  if (phase === "Deposit") throw refused("phase_order_changed", index);
  if (stepRank < order.priorRank)
    throw refused("phase_order_changed", order.firstAfter[stepRank]!);
  for (let later = 0; later < stepRank; later += 1)
    order.firstAfter[later] ??= index;
  return stepRank;
};

/** An input step `index` spends, and its refusal when the ledger lacks it. */
export type StepSpend = Readonly<{
  key: string;
  refusal: EventLedgerReplayRefusal;
}>;

/** What step `index` of `event` spends, in order, and the entries it
 * creates. */
export const stepEffects = (
  event: SourceEventRecord,
  index: number,
): Readonly<{ spends: StepSpend[]; outputs: LedgerEntry[] }> => {
  if (
    event.phase === "Withdrawal" &&
    event.entry.value.validity === "WithdrawalIsValid"
  ) {
    const ref = event.entry.value.body.l2_outref;
    const key = encodeMidgardSpendInputItem({
      txId: Buffer.from(ref.transactionId, "hex"),
      outputIndex: Number(ref.outputIndex),
    }).toString("hex");
    return {
      spends: [{ key, refusal: refused("withdrawal_input_absent", index) }],
      outputs: [],
    };
  }
  const effects = acceptedEffects(event);
  if (effects === null) return { spends: [], outputs: [] };
  return {
    spends: effects.inputs.map((input) => ({
      key: input.toString("hex"),
      refusal: refused("transaction_input_absent", index),
    })),
    outputs: effects.outputs.map((output, outputIndex) => ({
      outRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(effects.transactionId, "hex"),
        outputIndex,
      }),
      output: Buffer.from(output),
    })),
  };
};

/** The entries step `index` creates, with their trie leaves: the ledger's
 * sole full-output -> descriptor conversion, including its existing
 * malformed-reference-script representation. */
export const createdLeaves = (outputs: readonly LedgerEntry[], index: number) =>
  outputs.map((entry) => {
    try {
      return ledgerEntryLeaf(entry, `event ledger earlier event ${index}`);
    } catch (cause) {
      throw new EventLedgerOutputNotCanonical(BigInt(index), cause as Error);
    }
  });

/** The phase rank of a request for the ledger before `eventKey`, the step
 * `stepIndex` of a block of `stepCount` steps. */
export const requestRank = (
  eventKey: SDK.EventKey,
  stepIndex: bigint,
  stepCount: bigint,
): number => {
  if ("DepositEventKey" in eventKey)
    throw new Error(
      "event ledger: subject is not a withdrawal or transaction event",
    );
  if (stepIndex < 0n || stepIndex >= stepCount)
    throw refused("step_out_of_range", stepIndex);
  return phaseRank(eventKeyPhase(eventKey));
};

/** Refuses a request at step `target` of phase rank `rank` once its walk
 * advanced: a later phase's step or a failed step before it. A step records
 * its phase before it can fail on its effects, so a later-phase step is
 * never past the walk's failure. */
export const requireReached = (
  walk: PhaseOrder & {
    readonly failure?: Readonly<{ step: number; error: unknown }>;
  },
  rank: number,
  target: number,
): void => {
  const later = walk.firstAfter[rank];
  if (later !== undefined && later < target)
    throw refused("phase_order_changed", later);
  const { failure } = walk;
  if (failure !== undefined && failure.step < target) throw failure.error;
};

/** Refuses the selected step unless it opens `eventKey` at `stepIndex`
 * from `preRoot`, the ledger the walk reached before it. */
export const requireSelectedStep = (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
  stepIndex: bigint,
  preRoot: string,
): void => {
  const opening = reconstruction.traceByStepIndex.get(stepIndex);
  if (
    opening === undefined ||
    opening.key !== stepIndex ||
    opening.value.step_index !== stepIndex ||
    opening.value.schema_version !== 1n ||
    opening.value.phase !== eventKeyPhase(eventKey) ||
    Data.to(opening.value.event_key, SDK.EventKey) !==
      Data.to(eventKey, SDK.EventKey) ||
    opening.value.pre_utxos_root !== preRoot
  )
    throw refused("selected_pre_root_changed", stepIndex);
};

/** The reconstruction of canonical `block` a walk may read, once the block
 * is evidence-grade with an authenticated source root. */
export const walkableReconstruction = (
  block: CanonicalBlockEvidence,
): TransitionTraceReconstruction => {
  // These are the existing evidence-grade and source-root authorities. A
  // diagnostic/operator-private reconstruction cannot produce proof material.
  blockTransactionsFromCanonicalEvidence(block);
  SDKFraudProof.assertTransactionSourceInclusionRootAuthenticated(
    block.inclusionRootAuthentication,
  );
  // The walk reads the reconstruction's header; it must be the block's.
  const { header } = block.reconstruction;
  if (
    header.prevUtxosRoot !== block.header.prevUtxosRoot ||
    header.transitionStepCount !== block.header.transitionStepCount
  )
    fatal("block header differs from its reconstruction's");
  return block.reconstruction;
};

function fatal(message: string): never {
  throw new Error(`event ledger: ${message}`);
}

/** What a refused walk means to a trace-relative family: a prerequisite at
 * the event of the step the refusal names, or a fatal corruption of the
 * committed trace. */
const REFUSAL_OUTCOME: Readonly<
  Record<
    EventLedgerRefusalReason,
    Readonly<{ prerequisite: ReplayPrerequisite } | { fatal: string }>
  >
> = {
  withdrawal_input_absent: { prerequisite: "present_spend_input" },
  transaction_input_absent: { prerequisite: "present_spend_input" },
  transaction_output_already_present: {
    prerequisite: "prior_transition_effect",
  },
  post_root_changed: { prerequisite: "prior_transition_effect" },
  phase_order_changed: { prerequisite: "prior_transition_effect" },
  transition_link_changed: { fatal: "prior transition mismatch" },
  source_event_changed: { fatal: "prior event mismatch" },
  step_out_of_range: { fatal: "prior transition mismatch" },
  selected_pre_root_changed: { fatal: "event prior ledger changed" },
};

/**
 * `before` the event of step `stepIndex` of a trace-relative family's
 * reconstruction. A step the walk cannot apply is a prerequisite at the
 * event of the step it names: an absent input wants that event's spend-input
 * proof, an output that is not a canonical ledger entry its field-shape
 * proof, and any other unopenable effect its transition proof.
 */
export const traceRelative = async <T>(
  current: TransitionTraceReconstruction,
  stepIndex: bigint,
  before: (eventKey: SDK.EventKey) => Promise<T>,
): Promise<T> => {
  const target = current.traceByStepIndex.get(stepIndex)?.value;
  if (target === undefined) fatal("prior transition mismatch");
  try {
    return await before(target.event_key);
  } catch (cause) {
    // Every step a prerequisite names opened when the walk applied it.
    const at = (step: bigint, prerequisite: ReplayPrerequisite) =>
      replayPrerequisiteFailure(
        current.headerHash,
        current.traceByStepIndex.get(step)!.value.event_key,
        prerequisite,
      );
    if (cause instanceof EventLedgerOutputNotCanonical)
      throw at(cause.step, "representable_field_shape");
    if (!(cause instanceof EventLedgerReplayRefusal)) throw cause;
    const outcome = REFUSAL_OUTCOME[cause.reason];
    if ("fatal" in outcome) fatal(outcome.fatal);
    throw at(cause.step, outcome.prerequisite);
  }
};
