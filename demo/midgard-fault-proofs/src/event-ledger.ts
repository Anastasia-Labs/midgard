/**
 * The ledger before each event of a block, from the authenticated
 * predecessor ledger and the block's committed earlier steps. Every
 * intermediate root is checked.
 *
 * A walk applies the block's steps in order, once, to one trie it edits in
 * place, and keeps every entry a step creates or spends. The ledger before an
 * event is a view over the predecessor and those changes, so the ledgers
 * before every event of a block cost one pass over its steps. A request's
 * result does not depend on the requests before it: it is what applying the
 * steps before its event from the predecessor gives.
 */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { latestBefore, type Pass, record, view } from "./event-ledger.view.js";
import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "./evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  type LedgerEntry,
  ledgerEntryLeaf,
  requireLedgerOpening,
} from "./ledger-at.js";
import { mutableKeyValuePhasTrie } from "./transition-trace/phas.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./transition-trace/reconstruct.js";
import { requireReplayPredecessorLedger } from "./workflow/replay-predecessor-ledger.js";

export type EventLedgerRefusalReason =
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
    super(`input-existence: event ledger replay refused (${reason})`);
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

const refused = (reason: EventLedgerRefusalReason, step: number | bigint) =>
  new EventLedgerReplayRefusal(reason, BigInt(step));

const PHASES: readonly SDK.TransitionPhase[] = [
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

export type EventLedgerWalk = Readonly<{
  /** The checked ledger before the withdrawal or transaction event
   * `eventKey`, the block's step `stepIndex`. */
  before: (eventKey: SDK.EventKey, stepIndex: bigint) => Promise<CheckedLedger>;
}>;

/**
 * One block's committed steps, applied in order and each at most once: the
 * ledgers before several events of the block cost one walk. A step whose
 * phase follows the requested event's refuses that request only, and the
 * walk goes no further for it. A step that fails to apply stops the walk
 * there, and every request past it meets that failure.
 */
const walkSteps = ({
  reconstruction,
  stepCount,
  open,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepCount: bigint;
  /** The admitted ledger the block starts from. */
  readonly open: () => CheckedLedger;
}): EventLedgerWalk => {
  let pass: Pass | undefined;
  const start = (): Pass => {
    const ledger = open();
    return {
      predecessor: ledger,
      changes: new Map(),
      roots: [ledger.utxosRoot],
      sizes: [ledger.entries.length],
      firstAfter: PHASES.map(() => undefined),
      next: 0,
      priorRank: -1,
    };
  };

  const current = (key: string): boolean => {
    const change = latestBefore(pass!, key, pass!.next + 1);
    return change === undefined
      ? pass!.predecessor.outputs.has(key)
      : change.created !== undefined;
  };

  /** Applies step `pass.next`, or records why it fails. */
  const apply = async (walk: Pass): Promise<void> => {
    const index = walk.next;
    try {
      const opening = reconstruction.traceByStepIndex.get(BigInt(index));
      const step = opening?.value;
      if (
        opening === undefined ||
        step === undefined ||
        opening.key !== BigInt(index) ||
        step.step_index !== BigInt(index) ||
        step.schema_version !== 1n ||
        step.pre_utxos_root !== walk.roots[index]
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
      // Out of phase order, the refusal names the step of the later phase
      // that comes first: under the header's counts its index lies in an
      // earlier phase's band. A deposit applies before an earlier phase's
      // requested event only as that first step.
      const stepRank = phaseRank(event.phase);
      if (event.phase === "Deposit")
        throw refused("phase_order_changed", index);
      if (stepRank < walk.priorRank)
        throw refused("phase_order_changed", walk.firstAfter[stepRank]!);
      for (let later = 0; later < stepRank; later += 1)
        walk.firstAfter[later] ??= index;
      const spent: string[] = [];
      const created: { entry: LedgerEntry; leaf: Buffer }[] = [];
      const spend = (key: string, refusal: EventLedgerReplayRefusal) => {
        if (!current(key)) throw refusal;
        record(walk, key, { step: index });
        spent.push(key);
      };
      if (
        event.phase === "Withdrawal" &&
        event.entry.value.validity === "WithdrawalIsValid"
      ) {
        const ref = event.entry.value.body.l2_outref;
        spend(
          encodeMidgardSpendInputItem({
            txId: Buffer.from(ref.transactionId, "hex"),
            outputIndex: Number(ref.outputIndex),
          }).toString("hex"),
          refused("withdrawal_input_absent", index),
        );
      } else {
        const effects = acceptedEffects(event);
        if (effects !== null) {
          for (const input of effects.inputs)
            spend(
              input.toString("hex"),
              refused("transaction_input_absent", index),
            );
          const outputs = effects.outputs.map((output, outputIndex) => {
            const outRef = encodeMidgardSpendInputItem({
              txId: Buffer.from(effects.transactionId, "hex"),
              outputIndex,
            });
            // A ledger holding this out-ref already applied the same
            // transaction, so its inputs were spent and the spends above
            // refuse first. Only a hash collision, or an accepted
            // transaction with no spend input (itself a fault: validation
            // rejects it as EmptyInputs), reaches here. The entry is
            // refused, never overwritten.
            if (current(outRef.toString("hex")))
              throw refused("transaction_output_already_present", index);
            return { outRef, output: Buffer.from(output) };
          });
          // The ledger's sole full-output -> descriptor conversion, including
          // its existing malformed-reference-script representation.
          for (const entry of outputs)
            try {
              created.push(
                ledgerEntryLeaf(
                  entry,
                  `input-existence earlier event ${index}`,
                ),
              );
            } catch (cause) {
              throw new EventLedgerOutputNotCanonical(
                BigInt(index),
                cause as Error,
              );
            }
          for (const leaf of created)
            record(walk, leaf.entry.outRef.toString("hex"), {
              step: index,
              created: leaf,
            });
        }
      }
      const trie = await (walk.trie ??= mutableKeyValuePhasTrie(
        walk.predecessor.trie.entries,
      ));
      for (const key of spent) await trie.delete(Buffer.from(key, "hex"));
      for (const { entry, leaf } of created)
        await trie.insert(entry.outRef, leaf);
      if (trie.root() !== step.post_utxos_root)
        throw refused("post_root_changed", index);
      walk.roots.push(step.post_utxos_root);
      walk.sizes.push(walk.sizes[index]! - spent.length + created.length);
      walk.priorRank = stepRank;
      walk.next += 1;
      if (walk.next === Number(stepCount)) delete walk.trie;
    } catch (error) {
      walk.failure = { step: index, error };
      delete walk.trie;
    }
  };

  let applying = Promise.resolve();
  /** Applies steps until step `stepIndex`, a failure, or a step of a phase
   * after `rank`; one request at a time. */
  const advance = (walk: Pass, stepIndex: number, rank: number) =>
    (applying = applying.then(async () => {
      while (
        walk.next < stepIndex &&
        walk.failure === undefined &&
        walk.firstAfter[rank] === undefined
      )
        await apply(walk);
    }));

  return Object.freeze({
    before: async (eventKey: SDK.EventKey, stepIndex: bigint) => {
      if ("DepositEventKey" in eventKey)
        throw new Error(
          "event ledger: subject is not a withdrawal or transaction event",
        );
      if (stepIndex < 0n || stepIndex >= stepCount)
        throw refused("step_out_of_range", stepIndex);
      const walk = (pass ??= start());
      const target = Number(stepIndex);
      const rank = phaseRank(eventKeyPhase(eventKey));
      await advance(walk, target, rank);
      // A step records its phase before it can fail on its effects, so a
      // later-phase step is never past the walk's failure.
      const later = walk.firstAfter[rank];
      if (later !== undefined && later < target)
        throw refused("phase_order_changed", later);
      const { failure } = walk;
      if (failure !== undefined && failure.step < target) throw failure.error;
      const opening = reconstruction.traceByStepIndex.get(stepIndex);
      if (
        opening === undefined ||
        opening.key !== stepIndex ||
        opening.value.step_index !== stepIndex ||
        opening.value.schema_version !== 1n ||
        opening.value.phase !== eventKeyPhase(eventKey) ||
        Data.to(opening.value.event_key, SDK.EventKey) !==
          Data.to(eventKey, SDK.EventKey) ||
        opening.value.pre_utxos_root !== walk.roots[target]
      )
        throw refused("selected_pre_root_changed", stepIndex);
      return view(walk, target);
    },
  });
};

/** The walk of one canonical block from its admitted predecessor ledger. */
export const createEventLedgerWalk = ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}): EventLedgerWalk => {
  // These are the existing evidence-grade and source-root authorities. A
  // diagnostic/operator-private reconstruction cannot produce proof material.
  blockTransactionsFromCanonicalEvidence(block);
  SDK.assertTransactionSourceInclusionRootAuthenticated(
    block.inclusionRootAuthentication,
  );
  return walkSteps({
    reconstruction: block.reconstruction,
    stepCount: block.header.transitionStepCount,
    open: () =>
      requireReplayPredecessorLedger({
        block,
        predecessor,
        label: "input-existence",
      }),
  });
};

const sharedWalks = new WeakMap<
  TransitionTraceReconstruction,
  Map<CheckedLedger | undefined, EventLedgerWalk>
>();

/**
 * The one walk of an authenticated reconstruction from `predecessor`, or
 * from the genesis ledger its header must then commit, shared by every
 * family that asks: a request's result does not depend on the requests
 * before it, so the families' ledgers of one block cost one pass.
 */
export const reconstructionEventLedgerWalk = (
  reconstruction: TransitionTraceReconstruction,
  predecessor?: CheckedLedger,
): EventLedgerWalk => {
  let walks = sharedWalks.get(reconstruction);
  if (walks === undefined) sharedWalks.set(reconstruction, (walks = new Map()));
  let walk = walks.get(predecessor);
  if (walk === undefined) {
    walk = walkSteps({
      reconstruction,
      stepCount: reconstruction.header.transitionStepCount,
      open: () =>
        requireLedgerOpening({
          ledger: predecessor,
          utxosRoot: reconstruction.header.prevUtxosRoot,
          label: "event ledger predecessor ledger",
        }),
    });
    walks.set(predecessor, walk);
  }
  return walk;
};

/** `createEventLedgerWalk`, created at its first request: a caller whose
 * events may need no ledger meets the walk's evidence checks only when one
 * does. */
export const lazyEventLedgerWalk = (
  input: Parameters<typeof createEventLedgerWalk>[0],
): EventLedgerWalk => {
  let walk: EventLedgerWalk | undefined;
  return Object.freeze({
    before: async (eventKey: SDK.EventKey, stepIndex: bigint) =>
      await (walk ??= createEventLedgerWalk(input)).before(eventKey, stepIndex),
  });
};

/** The checked ledger before one event of `block`. */
export const ledgerBeforeTransactionEvent = async ({
  block,
  eventKey,
  stepIndex,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
  readonly stepIndex: bigint;
  readonly predecessor?: CheckedLedger;
}): Promise<CheckedLedger> =>
  await createEventLedgerWalk({
    block,
    ...(predecessor === undefined ? {} : { predecessor }),
  }).before(eventKey, stepIndex);
