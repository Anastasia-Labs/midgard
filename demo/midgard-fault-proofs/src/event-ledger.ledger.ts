/**
 * The ledger before each event of a block as a ledger store `Ledger`: the
 * predecessor ledger with the block's committed earlier steps applied, each
 * step one `apply`, and every intermediate root checked. Step ledgers are
 * in-memory overlays over the predecessor, never persisted.
 *
 * It refuses what the checked-ledger walk (`event-ledger.ts`) refuses, for
 * the same reasons: both make the checks of `event-ledger.steps.ts`. A
 * predecessor that stops being readable while a walk reads it (its root
 * dropped by `retain`, a node or an output gone) says nothing of the block:
 * it is this process's own fault, a `ReplayLocalFault` (not held), and the
 * next reader walks afresh.
 *
 * Every reader of a block from one predecessor ledger shares one walk, as
 * the checked-ledger walk's readers do: a family that asks for the ledger
 * before each of a block's events (withdrawal-mistag asks once per
 * withdrawal, up to the profile's 10,000) would otherwise apply the steps
 * before each one again, quadratic in the block's steps.
 */
import type * as SDK from "@al-ft/midgard-sdk/core";

import { ReplayLocalFault } from "./block-replay.faults.js";
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
import type { CanonicalBlockEvidence } from "./evidence/canonical-block-evidence.js";
import { LedgerRootMismatchError, requireLedgerOpening } from "./ledger-at.js";
import {
  EMPTY_LEDGER_ROOT,
  type Ledger,
  ledgerAt,
  LedgerNodeMissing,
  LedgerOutputMissing,
} from "./ledger-store/index.js";
import type { TransitionTraceReconstruction } from "./transition-trace/reconstruct.transition-trace-reconstruction.js";

/**
 * `run()`, a read of a ledger a walk derived, with the store no longer
 * holding what it reads (`LedgerNodeMissing`, `LedgerOutputMissing`) named
 * this process's own fault: a `ReplayLocalFault`, which its caller retries.
 * Any other throw passes through. (`LedgerNotRetained` comes only from
 * `LedgerStore.at`, before a ledger exists to read.)
 */
export const readingLedger = async <T>(
  what: string,
  run: () => Promise<T>,
): Promise<T> => {
  try {
    return await run();
  } catch (cause) {
    if (
      cause instanceof LedgerNodeMissing ||
      cause instanceof LedgerOutputMissing
    )
      throw new ReplayLocalFault({
        message: `${what}: the ledger is no longer readable: ${cause.message}`,
        cause,
      });
    throw cause;
  }
};

/** The genesis ledger: empty, with nothing to read. */
const emptyLedger = (): Ledger =>
  ledgerAt(EMPTY_LEDGER_ROOT, {
    node: async () => undefined,
    outputs: async (outRefs) => outRefs.map(() => undefined),
  });

type LedgerWalk = Readonly<{
  /** The ledger before the withdrawal or transaction event `eventKey`, the
   * block's step `stepIndex`. */
  before: (eventKey: SDK.EventKey, stepIndex: bigint) => Promise<Ledger>;
}>;

type LedgerPass = {
  /** `ledgers[k]`: the ledger before step `k`. */
  readonly ledgers: Ledger[];
  readonly firstAfter: (number | undefined)[];
  priorRank: number;
  /** The step that failed to apply, and why. */
  failure?: Readonly<{ step: number; error: unknown }>;
};

/**
 * One block's committed steps, applied in order and each at most once, as
 * the checked-ledger walk applies them (`walkSteps` in `event-ledger.ts`): a
 * step whose phase follows the requested event's refuses that request only,
 * and a step that fails stops the walk there. A refusal or an output no
 * ledger can hold is the block's, and every request past that step meets it;
 * any other failure is not, and `evict` takes the walk from the shared walks
 * so the next reader walks afresh.
 */
const walkLedgers = ({
  reconstruction,
  open,
  evict,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  /** The predecessor ledger, refused unless it opens the block's start. */
  readonly open: () => Ledger;
  readonly evict: () => void;
}): LedgerWalk => {
  const stepCount = reconstruction.header.transitionStepCount;
  let pass: LedgerPass | undefined;

  /** Applies the step after the last ledger, or records why it fails. */
  const apply = async (walk: LedgerPass): Promise<void> => {
    const index = walk.ledgers.length - 1;
    const ledger = walk.ledgers[index]!;
    try {
      const { step, event } = openStep(reconstruction, index, ledger.root);
      const stepRank = orderStep(walk, event.phase, index);
      const { spends, outputs } = stepEffects(event, index);
      const read = await readingLedger(
        `event ledger step ${index}`,
        async () =>
          await ledger.read([
            ...spends.map(({ key }) => Buffer.from(key, "hex")),
            ...outputs.map(({ outRef }) => outRef),
          ]),
      );
      const held = (key: string) => read.get(key)?.output !== undefined;
      const spent = new Set<string>();
      for (const { key, refusal } of spends) {
        // An input this step already spent is absent the second time.
        if (!held(key) || spent.has(key)) throw refusal;
        spent.add(key);
      }
      for (const { outRef } of outputs)
        if (held(outRef.toString("hex")))
          throw refused("transaction_output_already_present", index);
      const created = createdLeaves(outputs, index);
      const after = await readingLedger(
        `event ledger step ${index}`,
        async () =>
          await ledger.apply([
            ...[...spent].map((key) => ({
              type: "delete" as const,
              outRef: Buffer.from(key, "hex"),
            })),
            ...created.map(({ entry }) => ({
              type: "insert" as const,
              ...entry,
            })),
          ]),
      );
      if (after.root !== step.post_utxos_root)
        throw refused("post_root_changed", index);
      walk.ledgers.push(after);
      walk.priorRank = stepRank;
    } catch (error) {
      walk.failure = { step: index, error };
      if (
        !(error instanceof EventLedgerReplayRefusal) &&
        !(error instanceof EventLedgerOutputNotCanonical)
      )
        evict();
    }
  };

  let applying = Promise.resolve();
  /** Applies steps until step `target`, a failure, or a step of a phase
   * after `rank`; one request at a time. */
  const advance = (walk: LedgerPass, target: number, rank: number) =>
    (applying = applying.then(async () => {
      while (
        walk.ledgers.length - 1 < target &&
        walk.failure === undefined &&
        walk.firstAfter[rank] === undefined
      )
        await apply(walk);
    }));

  return Object.freeze({
    before: async (eventKey: SDK.EventKey, stepIndex: bigint) => {
      const rank = requestRank(eventKey, stepIndex, stepCount);
      const walk = (pass ??= {
        ledgers: [open()],
        firstAfter: PHASES.map(() => undefined),
        priorRank: -1,
      });
      const target = Number(stepIndex);
      await advance(walk, target, rank);
      requireReached(walk, rank, target);
      const ledger = walk.ledgers[target]!;
      requireSelectedStep(reconstruction, eventKey, stepIndex, ledger.root);
      return ledger;
    },
  });
};

/** The key of a reconstruction's walk from the genesis ledger. */
const GENESIS = Object.freeze({});

/** Held weakly by reconstruction and by predecessor ledger. */
const sharedWalks = new WeakMap<
  TransitionTraceReconstruction,
  WeakMap<object, LedgerWalk>
>();

/** The one walk of an authenticated reconstruction from `predecessor`, or
 * from the genesis ledger its header must then commit, shared by every
 * reader. */
const sharedWalk = (
  reconstruction: TransitionTraceReconstruction,
  predecessor: Ledger | undefined,
): LedgerWalk => {
  const walks = sharedWalks.get(reconstruction) ?? new WeakMap();
  sharedWalks.set(reconstruction, walks);
  const key = predecessor ?? GENESIS;
  const known = walks.get(key);
  if (known !== undefined) return known;
  const label = "event ledger predecessor";
  const { prevUtxosRoot } = reconstruction.header;
  const walk = walkLedgers({
    reconstruction,
    open: () => {
      if (predecessor === undefined) {
        requireLedgerOpening({
          ledger: undefined,
          utxosRoot: prevUtxosRoot,
          label,
        });
        return emptyLedger();
      }
      if (predecessor.root !== prevUtxosRoot)
        throw new LedgerRootMismatchError(
          label,
          prevUtxosRoot,
          predecessor.root,
        );
      return predecessor;
    },
    evict: () => {
      if (walks.get(key) === walk) walks.delete(key);
    },
  });
  walks.set(key, walk);
  return walk;
};

/** The ledger before the withdrawal or transaction event `eventKey`, the
 * step `stepIndex` of canonical `block`, from its predecessor ledger. */
export const ledgerBeforeStep = async ({
  block,
  eventKey,
  stepIndex,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
  readonly stepIndex: bigint;
  readonly predecessor?: Ledger | undefined;
}): Promise<Ledger> =>
  await sharedWalk(walkableReconstruction(block), predecessor).before(
    eventKey,
    stepIndex,
  );

/** The ledger before step `stepIndex` of a trace-relative family's
 * reconstruction, from the block's one shared walk (see `traceRelative`). */
export const ledgerBeforeTraceStep = async (
  current: TransitionTraceReconstruction,
  stepIndex: bigint,
  predecessor?: Ledger,
): Promise<Ledger> =>
  await traceRelative(
    current,
    stepIndex,
    async (eventKey) =>
      await sharedWalk(current, predecessor).before(eventKey, stepIndex),
  );
