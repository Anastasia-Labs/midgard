import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { Level } from "level";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  closeLifecycle,
  commitNextBlock,
  type Lifecycle,
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readObserver,
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";

const C = Pending.Columns;
const MARKER_REFUSAL =
  "Architecture G durable marker differs from the selected commit base";
const REWIND_DOMAIN = "midgard-history-correction-rewind-intent-v1";

type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;
type Handle = Scenario["h"] | Awaited<ReturnType<Lifecycle["restartRuntime"]>>;

/** Every message on a refusal's cause chain, through Effect fiber failures
 * and tagged errors, whose `cause` fields `inspect` does not always print. */
const failureText = async (promise: Promise<unknown>) => {
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

const nativeRoot = async (handle: Pick<Handle, "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The removed blocks' journals, captured before the rewind. */
const captureRemoved = async (headers: readonly string[]) => {
  const journals = await Promise.all(headers.map(readJournal));
  for (const journal of journals) {
    expect(journal[C.STATUS]).toBe(Pending.Status.Finalized);
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
const expectRewoundAndRecommitted = async ({
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

it("rewinds the native ledger and commits a removed block's reincluded deposit on the correct base in the same process", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    const rewoundNativeRoot = await nativeRoot(h);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(h);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
    await h.synchronize();
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("does not rewind before the removal is final, and until then the next commit fails closed instead of committing on the removed block's root", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    // Depth 1 < release depth: nothing is admitted, owed or rewound, even
    // across forward appends.
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual(
      [],
    );
    await scenario.nextSourceBlock();
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect(await readRecoveryPlans()).toEqual([]);
    expect((await readJournal(removed[0]!.headerHash))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    // A new deposit gives the next block content. Without an admitted
    // removal nothing is owed or rewound.
    const later = await scenario.deposit(14_000_000n);
    await h.deployment.chain.awaitLedgerTime(later + 1000);
    await scenario.nextSourceBlock();
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect(await readRecoveryPlans()).toEqual([]);
    const queueBefore = await scenario.readQueue();
    // The native root still holds the removed block's outputs, which no
    // canonical block carries: the commit is refused, nothing is submitted.
    expect(await failureText(commitNextBlock(h))).toContain(MARKER_REFUSAL);
    expect(await scenario.readQueue()).toEqual(queueBefore);
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect((await readDeposits())[0]).toEqual({
      status: "projected",
      projectedHeader: removed[0]!.headerHash,
    });
    // Once final, the rewind happens and the same commit succeeds.
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    const rewoundNativeRoot = await nativeRoot(h);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(h);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
      laterDeposits: 1,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("rewinds after a restart that preceded local reconciliation (the live state), and replays an unsaved observer idempotently", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    const observerBefore = await readObserverRow();
    // Removal final on L1; journal finalized; observer cursor pre-removal;
    // native root at the removed block's root. The process restarts.
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(removed[0]!.expected);
    const admitted = await scenario.tick(restarted.globals);
    expect(admitted.admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    const saved = await readObserverRow();
    // Crash before the observer save: the replayed tick admits the same
    // transition and saves the same state.
    await restoreObserverRow(observerBefore);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    expect(await readObserverRow()).toEqual(saved);
    await scenario.nextSourceBlock(restarted);
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    // An observer replay after the rewind (at a greater depth, so with a new
    // transition digest) re-admits the removal but owes nothing: the removed
    // journal keeps the digest it was abandoned under, and no second rewind
    // is planned.
    const observerAtRewind = await readObserver();
    const abandonedUnder = (await readJournal(removed[0]!.headerHash))[
      C.CORRECTION_TRANSITION_DIGEST
    ];
    expect(abandonedUnder).not.toBeNull();
    await restoreObserverRow(observerBefore);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([]);
    await scenario.nextSourceBlock(restarted);
    expect(await readRecoveryPlans()).toHaveLength(1);
    expect(
      (await readJournal(removed[0]!.headerHash))[
        C.CORRECTION_TRANSITION_DIGEST
      ],
    ).toBe(abandonedUnder);
    expect(await nativeRoot(restarted)).toBe(rewoundNativeRoot);
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
      observerAtRewind,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

/** Stop the node, then let `whileDown` act on L1 only; the node restarts
 * after every one of its services has closed. */
const restartAfter = (
  scenario: Awaited<ReturnType<typeof openCorrectionRewindScenario>>,
  whileDown: () => Promise<void>,
) => scenario.h.restartRuntime({ afterStop: whileDown });

/** The durable state of a node that never observed its block's removal. */
const expectUnobservedRemoval = async (
  removed: Awaited<ReturnType<typeof captureRemoved>>,
  handle: Pick<Handle, "evidence">,
  cursorQueue?: unknown,
) => {
  expect(await nativeRoot(handle)).toBe(removed[0]!.expected);
  expect((await readSqlLedgerRoot()).root_hex).toBe(removed[0]!.expected);
  if (cursorQueue !== undefined)
    expect((await readObserver()).cursorQueue).toEqual(cursorQueue);
  const journal = await readJournal(removed[0]!.headerHash);
  expect(journal[C.STATUS]).toBe(Pending.Status.Finalized);
  expect(journal[C.CORRECTION_TRANSITION_DIGEST] ?? null).toBeNull();
  expect((await readDeposits())[0]).toEqual({
    status: "projected",
    projectedHeader: removed[0]!.headerHash,
  });
  expect(await readRecoveryPlans()).toEqual([]);
};

it("recovers at startup when its block was removed and the removal became final on L1 while the node was down, then commits the reincluded deposit", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const cursorQueue = (await readObserver()).cursorQueue;
    let removal: Awaited<ReturnType<typeof scenario.removeTail>> | undefined;
    // No node service runs from before the removal until after its finality:
    // the node first sees the removal on L1 when it starts.
    const restarted = await restartAfter(scenario, async () => {
      removal = await scenario.removeTail(scenario.headers[0]!, {
        observe: false,
      });
      await scenario.awaitRemovalFinality(undefined, { observe: false });
    });
    h = restarted;
    await expectUnobservedRemoval(removed, restarted, cursorQueue);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal!.accepted.transaction.txHash]);
    await scenario.nextSourceBlock(restarted);
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("does not rewind at startup for a removal accepted while the node was down but not yet final, and until then the next commit fails closed", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const cursorQueue = (await readObserver()).cursorQueue;
    let removal: Awaited<ReturnType<typeof scenario.removeTail>> | undefined;
    const restarted = await restartAfter(scenario, async () => {
      removal = await scenario.removeTail(scenario.headers[0]!, {
        observe: false,
      });
    });
    h = restarted;
    await expectUnobservedRemoval(removed, restarted, cursorQueue);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([]);
    await scenario.nextSourceBlock(restarted);
    // The observer now tracks the removal as pending; nothing is admitted,
    // owed or rewound.
    expect((await readObserver()).admitted).toEqual([]);
    await expectUnobservedRemoval(removed, restarted);
    const later = await scenario.deposit(14_000_000n, restarted);
    await restarted.deployment.chain.awaitLedgerTime(later + 1000);
    await scenario.nextSourceBlock(restarted);
    const queueBefore = await scenario.readQueue();
    expect(await failureText(commitNextBlock(restarted))).toContain(
      MARKER_REFUSAL,
    );
    expect(await scenario.readQueue()).toEqual(queueBefore);
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await nativeRoot(restarted)).toBe(removed[0]!.expected);
    // Once final, the same node rewinds and the same commit succeeds.
    await scenario.awaitRemovalFinality(restarted);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal!.accepted.transaction.txHash]);
    await scenario.nextSourceBlock(restarted);
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
      laterDeposits: 1,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("rewinds a removed two-block suffix to the earliest block's base and commits both reincluded deposits in order", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 2 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    expect(removed[1]!.base).toBe(removed[0]!.expected);
    expect(removed[0]!.base).not.toBe(removed[1]!.base);
    // A timed-out suffix is removed tail first.
    const second = await scenario.removeTail(scenario.headers[1]!);
    const first = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      second.accepted.transaction.txHash,
      first.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    const rewoundNativeRoot = await nativeRoot(h);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(h);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

/** Two blocks, both removed: a two-member rewind to the first one's base. */
const openRemovedTwoBlockSuffix = async () => {
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
const openRemovedTailOverRetainedBlock = async () => {
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
const expectOwedAndUnreincluded = async (
  removed: Awaited<ReturnType<typeof captureRemoved>>,
) => {
  for (const block of removed) {
    const journal = await readJournal(block.headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.Finalized);
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

it("resumes a two-block rewind interrupted after its plan was persisted and before the native root moved", async () => {
  const { scenario, removed } = await openRemovedTwoBlockSuffix();
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const owner = await Effect.runPromise(
      Ref.get(scenario.h.globals.NATIVE_MPF_OWNER),
    );
    if (owner === undefined) throw new Error("Native owner is not open");
    // The process dies at the first native mutation.
    owner.restoreCanonicalRoot = () =>
      Promise.reject(new Error("injected crash before native restore"));
    expect(await failureText(scenario.nextSourceBlock())).toContain(
      "injected crash before native restore",
    );
    expect(await nativeRoot(scenario.h)).toBe(removed.at(-1)!.expected);
    await expectOwedAndUnreincluded(removed);
    // The restarted process resumes the persisted plan at startup.
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("resumes a rewind to a retained non-empty base interrupted after the native root moved and before reinclusion committed", async () => {
  const { scenario, removed } = await openRemovedTailOverRetainedBlock();
  let h: Pick<Lifecycle, "close"> = scenario.h;
  const dropProbe = () =>
    read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DROP TRIGGER IF EXISTS rewind_crash_probe
          ON pending_block_finalizations`;
        yield* sql`DROP FUNCTION IF EXISTS rewind_crash_probe()`;
      }),
    );
  try {
    // The SQL reinclusion transaction dies at its first journal abandonment,
    // after the native restore acknowledged.
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe(`CREATE FUNCTION rewind_crash_probe() RETURNS trigger
          LANGUAGE plpgsql AS $$ BEGIN
            RAISE EXCEPTION 'injected crash between rewind and reinclusion';
          END $$`);
        yield* sql.unsafe(`CREATE TRIGGER rewind_crash_probe
          BEFORE UPDATE ON pending_block_finalizations FOR EACH ROW
          WHEN (NEW.status = 'abandoned' AND OLD.status <> 'abandoned')
          EXECUTE FUNCTION rewind_crash_probe()`);
      }),
    );
    expect(await failureText(scenario.nextSourceBlock())).toContain(
      "injected crash between rewind and reinclusion",
    );
    // Native rewound; journal, deposit and SQL marker untouched; plan retained.
    const target = removed[0]!.base;
    expect(await nativeRoot(scenario.h)).toBe(target);
    expect((await readSqlLedgerRoot()).root_hex).not.toBe(target);
    await expectOwedAndUnreincluded(removed);
    await dropProbe();
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(target);
    const engine = await readSqlLedgerRoot();
    expect(engine.root_hex).toBe(target);
    // The replay base's own payload aggregate, from the retained journal.
    expect(engine.utxo_payload_entry_count).not.toBeNull();
    const journal = await readJournal(removed[0]!.headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      (await readObserver()).admitted[0]!.transitionDigest,
    );
    const plans = await readRecoveryPlans();
    expect(plans.map(({ state }) => state)).toEqual(["applied"]);
    expect((await readDeposits()).at(-1)).toEqual({
      status: "projected",
      projectedHeader: null,
    });
    // The retained predecessor is unattested and, in this profile, timed out
    // before its successor could be removed: the node correctly refuses to
    // commit on it until it is removed too.
    expect(await failureText(commitNextBlock(restarted))).toContain(
      "Commit paused until expired unattested suffix is corrected",
    );
  } finally {
    try {
      await dropProbe();
    } finally {
      await closeLifecycle(h);
    }
  }
}, 600_000);

it("fails closed with a specific error when the rewind base is not retained, and never moves the marker or reincludes", async () => {
  const { scenario, removed } = await openRemovedTailOverRetainedBlock();
  const levelPath = scenario.h.production.nodeConfig.LEDGER_MPF_DB_PATH;
  const target = removed[0]!.base;
  const readMarker = async () => {
    const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
    await db.open();
    try {
      return await db.get("__root__");
    } finally {
      await db.close();
    }
  };
  try {
    // Drop the base root's own record while no service holds the store.
    const failure = await failureText(
      scenario.h.restartRuntime({
        afterStop: async () => {
          const db = new Level<string, unknown>(levelPath, {
            valueEncoding: "json",
          });
          await db.open();
          try {
            expect(await db.get(target)).toBeDefined();
            await db.del(target);
          } finally {
            await db.close();
          }
        },
      }),
    );
    expect(failure).toContain(
      `Native MPF canonical recovery target root ${target} is not retained in full; refusing to restore`,
    );
    expect(await readMarker()).toBe(removed[0]!.expected);
    expect((await readSqlLedgerRoot()).root_hex).not.toBe(target);
    await expectOwedAndUnreincluded(removed);
  } finally {
    await closeLifecycle(scenario.h);
  }
}, 600_000);
