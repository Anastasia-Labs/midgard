/**
 * A landed own block whose event left the chain holds commits and its own
 * merge until it leaves the landed queue, on the node database:
 *
 * - own landed block B (built on own landed block A, with own landed block
 *   C built on it) includes deposit D, and the follower rewinds past D's
 *   admission without re-admitting it. The driver's recompute publishes its
 *   view (no pending write gate); the commit tick refuses with
 *   `l1_own_block_event_orphaned`; B and C are not merged while A is; an
 *   unrelated transaction is admitted while a spend of D's output is
 *   rejected as a missing input; DA recovery runs. A rollback or a landed
 *   correction that removes B ends the hold and commits resume;
 * - D re-admitted: no hold;
 * - the same for a withdrawal B includes;
 * - a foreign landed block with an orphan keeps the base behaviour, the gate
 *   pending as `l1_events_orphan_recovery` (`follower-driver-recompute.test.ts`).
 */
import { type FactStore, type OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import {
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { reconcileFollowerEvents } from "../src/database/follower-events.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import {
  poisonedHeaderHold,
  readPoisonedOwnHeaders,
} from "../src/database/poisoned-own-headers.js";
import {
  L1_OWN_BLOCK_EVENT_ORPHANED,
  refuseCommitForOrphanedOwnBlockEvent,
} from "../src/fibers/block-commitment.own-block-event-orphaned.js";
import type { IngestionPlan } from "../src/l1-events/driver.js";
import { deleteRows, setState } from "../src/landed-blocks/store.js";
import {
  readFollowerWriteGate,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { classifyOldestQueuedBlockReadiness } from "../src/transactions/state-queue/merge-readiness.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../src/workers/commit-block-header/da-payload-backfill.js";
import {
  DRIVER_TEST_SLOT,
  testDriverRecompute,
  testWrite,
} from "./helpers/driver-recompute.js";
import {
  modelSlotTime,
  rewindFollowerKey,
  writeFollowerView,
} from "./helpers/follower-view.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  DROP_ALL_TIMEOUT_MS,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  BLOCK,
  freshNative,
  FRONTIER,
  land,
  processOf,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";
import { snapshot } from "./mempool-ledger-cache.row.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

/** Own landed block B (`BLOCK`) is built on A; C is built on B. */
const A = "a1".repeat(28);
const C = "c1".repeat(28);
/** A working-ledger output no event projects. */
const UNRELATED = outRefFromByte(0x71);

/** One deposit admitted on a simulated chain, as the follower's projection reads it. */
const projectedDeposit = async (): Promise<ProjectedEvent> => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const chain = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await chain.init();
  const nonce: OutRef = { txHash: Buffer.alloc(32, 0xd6), index: 1 };
  await chain.forward([
    admissionTx(eventOrder("deposit", nonce, { inclusionTime: 1_000n }), 1),
  ]);
  const read = await eventsAt(store, listOf("deposit"), chain.tip.point);
  if (read.kind !== "ok" || read.value.length !== 1)
    throw new Error(JSON.stringify(read));
  return read.value[0]!;
};

/** A, B and C land as processed own blocks, built locally (applied). */
const landOwnChain = async (
  globals: Globals,
  included: { depositIds?: Buffer[] } = {},
) => {
  await seed(globals, {
    kind: "own",
    applied: true,
    parentHeaderHash: A,
    depositIds: included.depositIds ?? [],
  });
  await land(globals, {
    headerHash: A,
    parentHeaderHash: FRONTIER,
    kind: "own",
  });
  await land(globals, { headerHash: C, parentHeaderHash: BLOCK, kind: "own" });
};

/**
 * Own landed block B includes deposit D (assigned to B, its output in the
 * working ledger), next to an unrelated working-ledger output. Then the
 * follower rewinds past D's admission; `relands` re-admits it at the view
 * the recompute applies.
 */
const arrangeDeposit = async (relands: boolean) => {
  const deposit = await projectedDeposit();
  const eventId = Buffer.from(deposit.idCbor, "hex");
  const globals = await processOf(freshNative());
  await landOwnChain(globals, { depositIds: [eventId] });
  const view = { events: [deposit] as readonly ProjectedEvent[] };
  const plan = Effect.suspend(() =>
    writeFollowerView(DRIVER_TEST_SLOT, view.events),
  );
  const depositOutRef = await run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const at: IngestionPlan = yield* plan;
        yield* reconcileFollowerEvents(at, {
          network: "Preprod",
          slotToUnixTime: modelSlotTime,
          cutoffMs: modelSlotTime(DRIVER_TEST_SLOT),
        });
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET status = 'projected',
          projected_header_hash = ${Buffer.from(BLOCK, "hex")}`;
        yield* MempoolLedgerDB.insert([
          {
            tx_id: Buffer.alloc(32, 0x71),
            outref: UNRELATED,
            output: makeOutput(FUNDED_OUTPUT_LOVELACE),
            address: "addr_test1_unrelated",
            source_event_id: null,
          },
        ]);
        const [row] = yield* sql<{ outref: Buffer }>`SELECT outref
          FROM mempool_ledger WHERE source_event_id = ${eventId}`;
        if (row === undefined)
          return yield* Effect.die("the deposit projects no output");
        if (!relands) {
          // Funded like the candidate that spends it, so only its absence
          // from the spendable ledger can reject that spend.
          yield* sql`UPDATE mempool_ledger
            SET output = ${makeOutput(FUNDED_OUTPUT_LOVELACE)}
            WHERE source_event_id = ${eventId}`;
          view.events = [];
        }
        yield* rewindFollowerKey(deposit);
        return Buffer.from(row.outref);
      }),
    ),
  );
  return { globals, plan, view, deposit, depositOutRef };
};

/**
 * Own landed block B includes withdrawal W whose admission the follower no
 * longer holds (its key is not in `l1_event_keys`).
 */
const arrangeWithdrawal = async () => {
  const globals = await processOf(freshNative());
  await landOwnChain(globals);
  await run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const bytes = Buffer.from("00", "hex");
        const id = Buffer.alloc(32, 0x57);
        yield* sql`INSERT INTO withdrawal_utxos
          (event_id, raw_event_info, settlement_event_info, inclusion_time,
            withdrawal_l1_tx_hash, withdrawal_l1_output_index, asset_name,
            l2_outref, l2_owner, l2_value, l1_address, l1_datum,
            refund_address, refund_datum, validity, projected_header_hash,
            status, l1_event_key, l1_origin_outref)
          VALUES (${id}, ${bytes}, ${bytes}, ${new Date(1_000)}, ${id}, 0,
            ${bytes}, ${bytes}, ${Buffer.alloc(28, 4)}, ${bytes}, ${bytes},
            ${bytes}, ${bytes}, ${bytes}, 'WithdrawalIsValid',
            ${Buffer.from(BLOCK, "hex")}, 'finalized', ${Buffer.alloc(32, 0x58)},
            ${Buffer.concat([Buffer.alloc(32, 0x59), Buffer.alloc(2)])})`;
      }),
    ),
  );
  return { globals, plan: writeFollowerView(DRIVER_TEST_SLOT, []) };
};

/** One driver recompute run at `plan`. */
const recompute = (
  globals: Globals,
  plan: NonNullable<Parameters<typeof testDriverRecompute>[0]>["plan"],
) =>
  run(
    globals,
    Effect.flatMap(testDriverRecompute({ plan }), (driver) =>
      driver.run("the follower rewound"),
    ),
  );

/** Phase B admission of `candidate` over the spendable working ledger, under a follower write permit. */
const admit = (
  globals: Globals,
  candidate: ReturnType<typeof makePhaseBCandidate>,
) =>
  run(
    globals,
    Effect.either(
      runAtFollowerView(
        Effect.gen(function* () {
          const rows = yield* MempoolLedgerDB.retrieveSpendable;
          const cache = yield* makeMempoolLedgerCacheService(
            globals,
            Effect.succeed(rows),
          );
          const state = yield* cache.withPhaseBLock(
            cache.currentState.pipe(Effect.map(snapshot)),
          );
          return yield* runPhaseBValidationWithPatch([candidate], state, {
            nowCardanoSlotNo: 0n,
            bucketConcurrency: 1,
          });
        }),
      ),
    ),
  );

const spending = (outref: Buffer) =>
  makePhaseBCandidate({
    spent: [outref],
    outputLovelace: FUNDED_OUTPUT_LOVELACE,
  });

/** The commit tick's refusal, the reason it raises and the poisoned headers. */
const commitTick = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const refused = yield* refuseCommitForOrphanedOwnBlockEvent;
      const reasons = yield* activeLivenessReasons(globals);
      return {
        refused,
        reason: reasons.find(
          (active) => active.reason === L1_OWN_BLOCK_EVENT_ORPHANED,
        ),
        poisoned: yield* readPoisonedOwnHeaders,
      };
    }),
  );

const PUBLISHED = {
  Published: { terminal_commitment: "22".repeat(32) },
} satisfies SDK.DaAvailabilityStateQueueStatus;

/** The oldest-queued-block merge decision for `headerHash`, mature and published. */
const mergeOf = (
  poisoned: Parameters<typeof poisonedHeaderHold>[0],
  headerHash: string,
) =>
  classifyOldestQueuedBlockReadiness({
    headerHash,
    currentDaAvailability: PUBLISHED,
    provenFraud: null,
    eventOrphaned: poisonedHeaderHold(poisoned, headerHash),
    readyAfterUnixTime: 0,
    nowUnixTime: 1,
  }).status;

describe("a landed own block whose deposit left the chain", () => {
  it("holds commits and its own merge, while admissions, older merges and DA recovery continue; a rollback ends the hold", async () => {
    const { globals, plan, depositOutRef } = await arrangeDeposit(false);
    const outcome = await recompute(globals, plan);

    // An unrelated transaction is admitted: no pending write gate.
    const unrelated = await admit(globals, spending(UNRELATED));
    expect(Either.isRight(unrelated)).toBe(true);
    if (Either.isRight(unrelated)) {
      expect(unrelated.right.rejected).toEqual([]);
      expect(unrelated.right.accepted).toHaveLength(1);
    }
    expect(outcome).toMatchObject({ published: true, hold: undefined });
    expect((await run(globals, readFollowerWriteGate)).pending).toBeUndefined();

    // A spend of D's output is rejected as a missing input.
    const spend = await admit(globals, spending(depositOutRef));
    expect(Either.isRight(spend)).toBe(true);
    if (Either.isRight(spend)) {
      expect(spend.right.accepted).toEqual([]);
      expect(spend.right.rejected.map((tx) => tx.code)).toEqual([
        RejectCodes.InputNotFound,
      ]);
    }

    // DA recovery runs.
    expect(
      await run(
        globals,
        runAtFollowerView(
          backfillMissingDaPayloadsFromFinalizedJournals({ limit: 100 }),
        ),
      ),
    ).toMatchObject({ scanned: 0 });

    // No commit; B and C are not merged, A is.
    const held = await commitTick(globals);
    expect(held.refused).toBe(true);
    expect(held.reason).toBeDefined();
    expect(held.poisoned.orphaned).toEqual([{ headerHash: BLOCK, orphans: 1 }]);
    expect([...held.poisoned.headers].sort()).toEqual([BLOCK, C].sort());
    expect(mergeOf(held.poisoned, A)).toBe("ready");
    expect(mergeOf(held.poisoned, BLOCK)).toBe(
      "skipped_oldest_block_event_orphaned",
    );
    expect(mergeOf(held.poisoned, C)).toBe(
      "skipped_oldest_block_event_orphaned",
    );

    // A rollback removes B and C: the hold ends and commits resume.
    await run(globals, testWrite(setState([BLOCK, C], "removed")));
    const resumed = await commitTick(globals);
    expect(resumed.refused).toBe(false);
    expect(resumed.reason).toBeUndefined();
    expect(resumed.poisoned.headers.size).toBe(0);
    expect(mergeOf(resumed.poisoned, BLOCK)).toBe("ready");
  });

  it("holds nothing when the deposit is admitted again", async () => {
    const { globals, plan, depositOutRef } = await arrangeDeposit(true);
    expect(await recompute(globals, plan)).toMatchObject({
      published: true,
      hold: undefined,
    });
    const tick = await commitTick(globals);
    expect(tick.refused).toBe(false);
    expect(tick.reason).toBeUndefined();
    expect(tick.poisoned.headers.size).toBe(0);
    expect(mergeOf(tick.poisoned, BLOCK)).toBe("ready");
    const spendable = await run(globals, MempoolLedgerDB.retrieveSpendable);
    expect(
      spendable.map((row) => Buffer.from(row.outref).toString("hex")),
    ).toContain(depositOutRef.toString("hex"));
  });
});

describe("a landed own block whose withdrawal left the chain", () => {
  it("holds commits and its own merge without a pending write gate; a landed correction ends the hold", async () => {
    const { globals, plan } = await arrangeWithdrawal();
    const outcome = await recompute(globals, plan);
    const unrelated = await admit(globals, spending(UNRELATED));
    expect(Either.isRight(unrelated)).toBe(true);
    expect(outcome).toMatchObject({ published: true, hold: undefined });

    const held = await commitTick(globals);
    expect(held.refused).toBe(true);
    expect(held.reason).toBeDefined();
    expect(held.poisoned.orphaned).toEqual([{ headerHash: BLOCK, orphans: 1 }]);
    expect(mergeOf(held.poisoned, A)).toBe("ready");
    expect(mergeOf(held.poisoned, BLOCK)).toBe(
      "skipped_oldest_block_event_orphaned",
    );
    expect(mergeOf(held.poisoned, C)).toBe(
      "skipped_oldest_block_event_orphaned",
    );

    // A correction replaces B (and C, built on it) in the landed queue.
    const corrected = "b2".repeat(28);
    await run(globals, testWrite(deleteRows([BLOCK, C])));
    await land(globals, { headerHash: corrected, parentHeaderHash: A });
    const resumed = await commitTick(globals);
    expect(resumed.refused).toBe(false);
    expect(resumed.reason).toBeUndefined();
    expect(resumed.poisoned.headers.size).toBe(0);
  });
});
