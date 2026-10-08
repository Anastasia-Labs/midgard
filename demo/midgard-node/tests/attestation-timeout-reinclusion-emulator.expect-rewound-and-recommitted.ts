import { inspect } from "node:util";

import { expect } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  commitNextBlock,
  type Lifecycle,
  openCorrectionRewindScenario,
  readDeposits,
  readJournal,
  readObserver,
  readRecoveryPlans,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";

export const C = Pending.Columns;

export const MARKER_REFUSAL =
  "Architecture G durable marker differs from the selected commit base";

export const REWIND_DOMAIN = "midgard-history-correction-rewind-intent-v1";

export type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;

type Handle = Scenario["h"] | Awaited<ReturnType<Lifecycle["restartRuntime"]>>;

/** Every message on a refusal's cause chain, through Effect fiber failures
 * and tagged errors, whose `cause` fields `inspect` does not always print. */
export const failureText = async (promise: Promise<unknown>) => {
  const caught = await promise.then(
    () => undefined,
    (error: unknown) => error,
  );
  if (caught === undefined) throw new Error("Expected a refusal");
  const seen = new Set<unknown>();
  const texts: string[] = [inspect(caught, { depth: 40 })];
  const walk = (value: unknown, depth: number) => {
    if (depth > 40 || value === null || typeof value !== "object") return;
    if (seen.has(value)) return;
    seen.add(value);
    if (value instanceof Error) texts.push(value.message);
    for (const key of Reflect.ownKeys(value))
      walk((value as Record<PropertyKey, unknown>)[key], depth + 1);
  };
  walk(caught, 0);
  return texts.join("\n");
};

export const nativeRoot = async (handle: Pick<Handle, "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The removed blocks' journals, captured before the rewind. */
export const captureRemoved = async (headers: readonly string[]) => {
  const journals = await Promise.all(headers.map(readJournal));
  for (const journal of journals) {
    expect(journal[C.STATUS]).toBe(Pending.Status.LocallyApplied);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST] ?? null).toBeNull();
  }
  return journals.map((journal) => ({
    headerHash: journal[C.HEADER_HASH].toString("hex"),
    base: journal[C.BASE_UTXOS_ROOT],
    expected: journal[C.EXPECTED_UTXOS_ROOT],
    deposits: journal.depositEventIds.map((id) => id.toString("hex")),
  }));
};

/** The canonical end state of a rewind of `removed` (earliest first), after
 * the next commit carried every reincluded deposit on the rewound base. */
export const expectRewoundAndRecommitted = async ({
  scenario,
  removed,
  rewoundNativeRoot,
  rewoundSqlRoot,
  next,
  laterDeposits = 0,
  observerAtRewind,
}: {
  readonly scenario: Scenario;
  readonly removed: Awaited<ReturnType<typeof captureRemoved>>;
  readonly rewoundNativeRoot: string;
  readonly rewoundSqlRoot: string | null;
  readonly next: Awaited<ReturnType<typeof commitNextBlock>>;
  /** Deposits submitted after the removal, carried by the same next block. */
  readonly laterDeposits?: number;
  /** The observer state the rewind consumed, if it was replayed since. */
  readonly observerAtRewind?: Awaited<ReturnType<typeof readObserver>>;
}) => {
  const target = removed[0]!.base;
  // Rewound to the replay base of the EARLIEST removed block, before the
  // next commit selected its base.
  expect(rewoundNativeRoot).toBe(target);
  expect(rewoundSqlRoot).toBe(target);
  // Each removed journal is abandoned under its own admitted correction.
  const observer = observerAtRewind ?? (await readObserver());
  const admitted = new Map(
    observer.admitted.map(({ transactionHash, transitionDigest }) => [
      transactionHash,
      transitionDigest,
    ]),
  );
  for (const [index, block] of removed.entries()) {
    const removal = scenario.removals.find(
      ({ checkpoint }) =>
        checkpoint.terminalTransition?.removedHeaderHashes.includes(
          block.headerHash,
        ) ?? false,
    );
    expect(removal, `removal of block ${index.toString()}`).toBeDefined();
    const journal = await readJournal(block.headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      admitted.get(removal!.accepted.transaction.txHash),
    );
  }
  // One applied rewind plan naming exactly the removed chain.
  const plans = await readRecoveryPlans();
  expect(plans).toHaveLength(1);
  expect(plans[0]!.state).toBe("applied");
  expect(plans[0]!.intent.domain).toBe(REWIND_DOMAIN);
  expect(plans[0]!.intent.headerHash).toBe(removed[0]!.headerHash);
  expect(plans[0]!.intent.members?.map(({ headerHash }) => headerHash)).toEqual(
    removed.map(({ headerHash }) => headerHash),
  );
  expect(plans[0]!.intent.targetRoot).toBe(target);
  // The next block was accepted on L1, on the rewound base, and carries every
  // reincluded deposit.
  const committed = await readJournal(next.submittedHeaderHash);
  expect(committed[C.BASE_UTXOS_ROOT]).toBe(target);
  expect(committed[C.EXPECTED_UTXOS_ROOT]).toBe(next.submittedUtxosRoot);
  expect(committed[C.BASE_TAIL_HEADER_HASH].toString("hex")).not.toBe(
    removed.at(-1)!.headerHash,
  );
  const carried = committed.depositEventIds.map((id) => id.toString("hex"));
  expect(carried).toHaveLength(removed.length + laterDeposits);
  expect(carried).toEqual(
    expect.arrayContaining(removed.flatMap(({ deposits }) => deposits)),
  );
  expect(removed.every(({ deposits }) => deposits.length === 1)).toBe(true);
  // Reopened by the reinclusion; projected again only when the new block is
  // confirmed.
  expect(await readDeposits()).toEqual(
    carried.map(() => ({ status: "projected", projectedHeader: null })),
  );
  const queue = await scenario.readQueue();
  expect(queue.at(-1)!.headerHash).toBe(next.submittedHeaderHash);
};

/** Stop the node, then let `whileDown` act on L1 only; the node restarts
 * after every one of its services has closed. */
export const restartAfter = (
  scenario: Awaited<ReturnType<typeof openCorrectionRewindScenario>>,
  whileDown: () => Promise<void>,
) => scenario.h.restartRuntime({ afterStop: whileDown });

/** The durable state of a node that never observed its block's removal. */
export const expectUnobservedRemoval = async (
  removed: Awaited<ReturnType<typeof captureRemoved>>,
  handle: Pick<Handle, "evidence">,
  cursorQueue?: unknown,
) => {
  expect(await nativeRoot(handle)).toBe(removed[0]!.expected);
  expect((await readSqlLedgerRoot()).root_hex).toBe(removed[0]!.expected);
  if (cursorQueue !== undefined)
    expect((await readObserver()).cursorQueue).toEqual(cursorQueue);
  const journal = await readJournal(removed[0]!.headerHash);
  expect(journal[C.STATUS]).toBe(Pending.Status.LocallyApplied);
  expect(journal[C.CORRECTION_TRANSITION_DIGEST] ?? null).toBeNull();
  expect((await readDeposits())[0]).toEqual({
    status: "projected",
    projectedHeader: removed[0]!.headerHash,
  });
  expect(await readRecoveryPlans()).toEqual([]);
};

/** Two blocks, both removed: a two-member rewind to the first one's base. */
export const openRemovedTwoBlockSuffix = async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 2 });
  const removed = await captureRemoved(scenario.headers);
  const second = await scenario.removeTail(scenario.headers[1]!);
  const first = await scenario.removeTail(scenario.headers[0]!);
  await scenario.awaitRemovalFinality();
  expect(
    (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
  ).toEqual([
    second.accepted.transaction.txHash,
    first.accepted.transaction.txHash,
  ]);
  return { scenario, removed };
};

/** Two blocks, only the tail removed: the rewind target is the first block's
 * (non-empty, retained) root. */
export const openRemovedTailOverRetainedBlock = async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 2 });
  const removedAll = await captureRemoved(scenario.headers);
  const removed = removedAll.slice(1);
  expect(removed[0]!.base).toBe(removedAll[0]!.expected);
  const removal = await scenario.removeTail(scenario.headers[1]!);
  await scenario.awaitRemovalFinality();
  expect(
    (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
  ).toEqual([removal.accepted.transaction.txHash]);
  return { scenario, removed };
};

/** The durable state of a rewind interrupted before its SQL transaction. */
export const expectOwedAndUnreincluded = async (
  removed: Awaited<ReturnType<typeof captureRemoved>>,
) => {
  for (const block of removed) {
    const journal = await readJournal(block.headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.LocallyApplied);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST] ?? null).toBeNull();
  }
  expect((await readDeposits()).slice(-removed.length)).toEqual(
    removed.map(({ headerHash }) => ({
      status: "projected",
      projectedHeader: headerHash,
    })),
  );
  const plans = await readRecoveryPlans();
  expect(plans).toHaveLength(1);
  expect(plans[0]!.state).toBe("prepared");
  expect(plans[0]!.intent.domain).toBe(REWIND_DOMAIN);
  expect(plans[0]!.intent.targetRoot).toBe(removed[0]!.base);
  expect(plans[0]!.intent.expectedRoot).toBe(removed.at(-1)!.expected);
};
