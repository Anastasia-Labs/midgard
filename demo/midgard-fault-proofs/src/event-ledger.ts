/**
 * The ledger before each event of a block, from the authenticated
 * predecessor ledger and the block's committed earlier steps. Every
 * intermediate root is checked.
 *
 * A walk applies the block's steps in order, once, to one trie it edits in
 * place, and keeps every entry a step creates or spends. The ledger before an
 * event is a view over the predecessor and those changes, so the ledgers
 * before every event of a block cost one pass over its steps. Every reader of
 * a block from one predecessor shares its one walk, which keeps the
 * membership tries of the first few ledgers proved against, so a claim proves
 * against its detected event's ledger without building that trie again. A
 * request's result does not depend on the requests before it: it is what
 * applying the steps before its event from the predecessor gives.
 */
import type * as SDK from "@al-ft/midgard-sdk/core";

import {
  createdLeaves,
  EventLedgerOutputNotCanonical,
  EventLedgerReplayRefusal,
  openStep,
  orderStep,
  PHASES,
  refused,
  requestRank,
  requireReached,
  requireSelectedStep,
  stepEffects,
  traceRelative,
  walkableReconstruction,
} from "./event-ledger.steps.js";
import { latestBefore, type Pass, record, view } from "./event-ledger.view.js";
import type { CanonicalBlockEvidence } from "./evidence/canonical-block-evidence.js";
import { type CheckedLedger, requireLedgerOpening } from "./ledger-at.js";
import { mutableKeyValuePhasTrie } from "./transition-trace/phas.js";
import type { TransitionTraceReconstruction } from "./transition-trace/reconstruct.transition-trace-reconstruction.js";

export {
  EventLedgerOutputNotCanonical,
  EventLedgerReplayRefusal,
} from "./event-ledger.steps.js";

type EventLedgerWalk = Readonly<{
  /** The checked ledger before the withdrawal or transaction event
   * `eventKey`, the block's step `stepIndex`. */
  before: (eventKey: SDK.EventKey, stepIndex: bigint) => Promise<CheckedLedger>;
}>;

/**
 * One block's committed steps, applied in order and each at most once: the
 * ledgers before several events of the block cost one walk. A step whose
 * phase follows the requested event's refuses that request only, and the
 * walk goes no further for it. A step that fails stops it there. What the
 * walk finds of the block, a refusal or an output no ledger can hold, every
 * request past that step meets; any other failure is not the block's, and
 * `evict` takes the walk from the shared walks so the next reader walks
 * afresh.
 */
const walkSteps = ({
  reconstruction,
  stepCount,
  open,
  evict,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepCount: bigint;
  readonly evict: () => void;
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
      proofTries: new Map(),
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
      const { step, event } = openStep(
        reconstruction,
        index,
        walk.roots[index]!,
      );
      const stepRank = orderStep(walk, event.phase, index);
      const { spends, outputs } = stepEffects(event, index);
      const spent: string[] = [];
      for (const { key, refusal } of spends) {
        if (!current(key)) throw refusal;
        record(walk, key, { step: index });
        spent.push(key);
      }
      // A ledger holding this out-ref already applied the same transaction,
      // so its inputs were spent and the spends above refuse first. Only a
      // hash collision, or an accepted transaction with no spend input
      // (itself a fault: validation rejects it as EmptyInputs), reaches here.
      // The entry is refused, never overwritten.
      for (const { outRef } of outputs)
        if (current(outRef.toString("hex")))
          throw refused("transaction_output_already_present", index);
      const created = createdLeaves(outputs, index);
      for (const leaf of created)
        record(walk, leaf.entry.outRef.toString("hex"), {
          step: index,
          created: leaf,
        });
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
      if (
        !(error instanceof EventLedgerReplayRefusal) &&
        !(error instanceof EventLedgerOutputNotCanonical)
      )
        evict();
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
      const rank = requestRank(eventKey, stepIndex, stepCount);
      const walk = (pass ??= start());
      const target = Number(stepIndex);
      await advance(walk, target, rank);
      requireReached(walk, rank, target);
      requireSelectedStep(
        reconstruction,
        eventKey,
        stepIndex,
        walk.roots[target]!,
      );
      return view(walk, target);
    },
  });
};

/** The key of a reconstruction's walk from the genesis ledger. */
const GENESIS = Object.freeze({});

/** Held weakly by reconstruction and by predecessor ledger. */
const sharedWalks = new WeakMap<
  TransitionTraceReconstruction,
  WeakMap<object, EventLedgerWalk>
>();

/**
 * The one walk of an authenticated reconstruction from `predecessor`, or
 * from the genesis ledger its header must then commit, shared by every
 * reader: a request's result does not depend on the requests before it, so
 * every ledger of one block from one predecessor costs one pass.
 */
const sharedWalk = (
  reconstruction: TransitionTraceReconstruction,
  predecessor: CheckedLedger | undefined,
): EventLedgerWalk => {
  const walks = sharedWalks.get(reconstruction) ?? new WeakMap();
  sharedWalks.set(reconstruction, walks);
  const key = predecessor ?? GENESIS;
  const known = walks.get(key);
  if (known !== undefined) return known;
  const walk: EventLedgerWalk = walkSteps({
    reconstruction,
    stepCount: reconstruction.header.transitionStepCount,
    open: () =>
      requireLedgerOpening({
        ledger: predecessor,
        utxosRoot: reconstruction.header.prevUtxosRoot,
        label: "event ledger predecessor",
      }),
    evict: () => {
      if (walks.get(key) === walk) walks.delete(key);
    },
  });
  walks.set(key, walk);
  return walk;
};

/** The checked ledger before the withdrawal or transaction event
 * `eventKey`, the step `stepIndex` of canonical `block`, from its admitted
 * predecessor ledger. */
export const eventLedgerBefore = async ({
  block,
  eventKey,
  stepIndex,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
  readonly stepIndex: bigint;
  readonly predecessor?: CheckedLedger | undefined;
}): Promise<CheckedLedger> =>
  await sharedWalk(walkableReconstruction(block), predecessor).before(
    eventKey,
    stepIndex,
  );

/** The checked ledger before step `stepIndex` of a trace-relative family's
 * reconstruction, from the block's one shared walk (see `traceRelative`). */
export const eventLedgerBeforeStep = async (
  current: TransitionTraceReconstruction,
  stepIndex: bigint,
  predecessor?: CheckedLedger,
): Promise<CheckedLedger> =>
  await traceRelative(
    current,
    stepIndex,
    async (eventKey) =>
      await sharedWalk(current, predecessor).before(eventKey, stepIndex),
  );
