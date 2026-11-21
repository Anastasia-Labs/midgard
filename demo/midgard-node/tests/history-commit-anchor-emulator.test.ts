import "./helpers/follower-emulator-installed.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { Database } from "../src/services/database.js";
import {
  readFollowerWriteGate,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import {
  commitEventHorizon,
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
} from "../src/services/history-commit-window.js";
import {
  assertCommitUserEventSourceCompleteness,
  COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE,
} from "../src/workers/commit-block-header/submission.commit-event-sources.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  runCommitWorkerUntilSubmitted,
  SDK,
  stateQueueFetchConfig,
} from "./deposit-flow-emulator-shared.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

type Lifecycle = Awaited<ReturnType<typeof openProductionLifecycle>>;

/**
 * The header-end cap of the anchor d below a view at `viewHeight`, read
 * straight from the follower's blocks: time(block at height - d) +
 * event_wait - 1, computed here apart from the code under test.
 */
const anchorCapBelow = (h: Lifecycle, viewHeight: number, depth: number) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const [anchor] = yield* sql<{
        slot: string;
      }>`SELECT slot::text AS slot FROM l1_blocks WHERE height = ${viewHeight - depth}`;
      return (
        h.fixture.operatorLucid.slotToUnixTime(Number(anchor!.slot)) +
        SDK.EVENT_WAIT_DURATION_MS -
        1
      );
    }).pipe(Effect.provide(Database.layer)),
  );

/** The view the driver applied (the write permit's view). */
const appliedView = async (h: Lifecycle) => {
  const gate = await h.runWithoutSynchronizing(readFollowerWriteGate);
  if (gate.applied === undefined) throw new Error("no applied view");
  return gate.applied;
};

/** The planner's end-time horizon at the applied view, at depth `depth`. */
const horizonAt = (h: Lifecycle, depth: number) =>
  h.runWithoutSynchronizing(
    Effect.gen(function* () {
      const gate = yield* readFollowerWriteGate;
      return yield* commitEventHorizon({
        view: gate.applied,
        depth,
        slotToUnixTime: Effect.succeed((slot: number) =>
          h.fixture.operatorLucid.slotToUnixTime(slot),
        ),
      });
    }),
  );

/** Advance one emulator block and let the owner and follower take it. */
const followNextBlock = async (h: Lifecycle) => {
  h.fixture.emulator.awaitBlock(1);
  vi.setSystemTime(h.fixture.emulator.now());
  await h.synchronize();
};

/**
 * The commit anchor (plan §8.1) through the production owner and the node's
 * follower store (`follower-emulator.host.ts`, one follower block per
 * emulator block). A commit planned at a view P is capped at
 * time(A) + event_wait - 1, A the follower block d below P: an event whose
 * validity ends in the block d + 1 below P is due by then and committed;
 * one whose validity ends in A (d below P) is not. A deposit commit built
 * with d = 1 lands with its end at that cap, and the gated journal recheck
 * refuses an end one millisecond above it. Each test opens its own
 * lifecycle: the shared emulator harness resets the node runtime after
 * every test.
 */
const withLifecycle = async (test: (h: Lifecycle) => Promise<void>) => {
  const h = await openProductionLifecycle();
  try {
    await test(h);
  } finally {
    try {
      await h.close();
    } finally {
      vi.useRealTimers();
    }
  }
};

it("lands a deposit commit built with d = 1, its end at the cap of the block one below its planning view", () =>
  withLifecycle(async (h) => {
    const { fixture } = h;
    const wallet = fixture.depositorLucid;
    const address = await wallet.wallet().address();
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await h.synchronize();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    expect(await wallet.awaitTx(await signed.submit())).toBe(true);
    await h.deployment.chain.awaitLedgerTime(
      built.metadata.inclusionTime + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService: h.lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    // The anchor block, then one block above it.
    await h.synchronize();
    fixture.emulator.awaitBlock(1);
    vi.setSystemTime(fixture.emulator.now());

    // The owned commit worker runs under the production fixture's config.
    const nodeConfig = { ...h.production.nodeConfig, COMMIT_EVENT_DEPTH: 1 };
    const output = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService: h.lucidService,
      latestBlock: await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      ),
      nodeConfig,
      production: { ...h.production, nodeConfig, globals: h.globals },
      alignScheduler: false,
    });
    expect(await fixture.operatorLucid.awaitTx(output.submittedTxHash)).toBe(
      true,
    );
    const attempts = (await h.evidence()).commitAttempts;
    const planned = attempts.at(-1)!.permit.view;
    const cap = await anchorCapBelow(h, planned.height, 1);
    const queue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queue.at(-1)!.datum),
    );
    // The landed header end is the anchor's cap; the state queue accepted
    // it as the tx's validity upper bound, and the deposit is due by it.
    expect(header.endTime).toBe(BigInt(cap));
    expect(header.endTime).toBeGreaterThanOrEqual(
      BigInt(built.metadata.inclusionTime),
    );
  }));

it("commits an event whose validity ends d + 1 blocks below the view, and not one whose validity ends d blocks below it", () =>
  withLifecycle(async (h) => {
    const { fixture } = h;
    const wallet = fixture.depositorLucid;
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await h.synchronize();
    // The deposit's validity ends one slot into the tip block V's window,
    // before the next block: its inclusion time is time(V) + 1 slot +
    // event_wait at most, past V's cap time(V) + event_wait - 1.
    const validity = await appliedView(h);
    const validTo = fixture.depositorLucid.slotToUnixTime(
      fixture.emulator.slot + 1,
    );
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: await wallet.wallet().address(),
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
        validity: { validFrom: validTo - 60_000, validTo },
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    expect(await wallet.awaitTx(await signed.submit())).toBe(true);
    await h.synchronize();
    const inclusion = built.metadata.inclusionTime;
    const timeOf = (slot: number) =>
      fixture.depositorLucid.slotToUnixTime(slot);
    // The deposit's validity ends in V: at or after V's start, before the
    // start of the block that holds it.
    const landed = await appliedView(h);
    expect(landed.height).toBe(validity.height + 1);
    expect(inclusion - SDK.EVENT_WAIT_DURATION_MS).toBeGreaterThanOrEqual(
      timeOf(validity.slot),
    );
    expect(inclusion - SDK.EVENT_WAIT_DURATION_MS).toBeLessThan(
      timeOf(landed.slot),
    );

    // V is d = 1 below the view: V is the anchor, the deposit is not due.
    const atDepth = await horizonAt(h, 1);
    expect(atDepth?.anchor.height).toBe(validity.height);
    expect(atDepth!.horizonMs).toBe(await anchorCapBelow(h, landed.height, 1));
    expect(atDepth!.horizonMs).toBeLessThan(inclusion);
    // At d = 0 the anchor is the view itself, above V: due.
    expect((await horizonAt(h, 0))!.horizonMs).toBeGreaterThanOrEqual(
      inclusion,
    );

    // One more block: V is d + 1 = 2 below the view, the deposit is due.
    await followNextBlock(h);
    const view = await appliedView(h);
    expect(view.height).toBe(validity.height + 2);
    const belowDepth = await horizonAt(h, 1);
    expect(belowDepth?.anchor.height).toBe(validity.height + 1);
    expect(belowDepth!.horizonMs).toBeGreaterThanOrEqual(inclusion);
    // At d = 2 V is the anchor again: not due.
    expect((await horizonAt(h, 2))!.horizonMs).toBeLessThan(inclusion);
  }));

it("refuses an end above the anchor's cap in the gated journal recheck", () =>
  withLifecycle(async (h) => {
    await followNextBlock(h);
    await followNextBlock(h);
    const view = await appliedView(h);
    const horizon = await horizonAt(h, 1);
    const cap = await anchorCapBelow(h, view.height, 1);
    expect(horizon?.horizonMs).toBe(cap);
    const recheck = (end: number) =>
      h.runWithoutSynchronizing(
        Effect.either(
          withFollowerWrite(
            assertCommitUserEventSourceCompleteness({
              blockEndTimeMs: end,
              commitAnchor: horizon!.anchor,
              depth: 1,
              slotToUnixTime: (slot) =>
                h.fixture.operatorLucid.slotToUnixTime(slot),
              includedDepositEntries: [],
              includedForcedTransactionEntries: [],
              includedWithdrawalEntries: [],
            }),
          ),
        ),
      );
    const refused = await recheck(cap + 1);
    expect(refused._tag).toBe("Left");
    expect(refused._tag === "Left" && refused.left.message).toBe(
      COMMIT_END_ABOVE_ANCHOR_CAP_MESSAGE,
    );
    const accepted = await recheck(cap);
    expect(accepted._tag).toBe("Right");
    const stored = accepted._tag === "Right" ? accepted.right : undefined;
    expect(stored?.height).toBe(horizon!.anchor.height);
    expect(stored?.slot).toBe(horizon!.anchor.slot);
    expect(Buffer.from(stored!.hash).equals(horizon!.anchor.hash)).toBe(true);
  }));
