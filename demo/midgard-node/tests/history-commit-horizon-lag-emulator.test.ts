import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import {
  assertIncludedEventsDeep,
  COMMIT_EVENT_NOT_DEEP_MESSAGE,
} from "../src/database/commit-event-depth.js";
import { Database } from "../src/services/database.js";
import {
  type CommitHorizonLag,
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
} from "../src/services/history-commit-window.js";
import { refreshCommitUserEventSourcesThroughBlockEnd } from "../src/workers/commit-block-header/submission.js";
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
 * The follower's covered tip, the block one below it and the ingested view
 * time, read straight from the follower tables: the expected caps are
 * computed here, apart from the code under test.
 */
const readFollowerHorizon = (h: Lifecycle) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const [tip] = yield* sql<{
        height: string;
      }>`SELECT height::text AS height FROM l1_follower_cursor`;
      const [below] = yield* sql<{
        slot: string;
      }>`SELECT slot::text AS slot FROM l1_blocks WHERE height = ${Number(tip!.height) - 1}`;
      const [ingested] = yield* sql<{
        ms: string;
      }>`SELECT ingested_through_ms::text AS ms FROM follower_event_ingestion`;
      const wait = SDK.EVENT_WAIT_DURATION_MS;
      return {
        /** slot(the block d = 1 below the covered tip) + W - 1. */
        laggedCap:
          h.fixture.operatorLucid.slotToUnixTime(Number(below!.slot)) +
          wait -
          1,
        /** The follower's unlagged horizon: its ingested view + W - 1. */
        unlagged: Number(ingested!.ms) + wait - 1,
      };
    }).pipe(Effect.provide(Database.layer)),
  );

const lagOf = (h: Lifecycle, lagBlocks: number): CommitHorizonLag => ({
  lagBlocks,
  slotToUnixTime: Effect.succeed((slot: number) =>
    h.fixture.operatorLucid.slotToUnixTime(slot),
  ),
});

/** Advance one emulator block and let the owner and follower take it. */
const followNextBlock = async (h: Lifecycle) => {
  h.fixture.emulator.awaitBlock(1);
  vi.setSystemTime(h.fixture.emulator.now());
  await h.synchronize();
};

/**
 * The horizon lag d (U3) through the production owner and the follower
 * stand-in (`emulator-l1-follower.ts`, one block per synced tip). Honest: a
 * deposit commit built with d = 1 lands, its end at the follower block one
 * below the covered tip. Adversarial: an end above that lagged cap is
 * refused by the final recheck before submission
 * (`refreshCommitUserEventSourcesThroughBlockEnd`), while d = 0 accepts
 * it. A commit including a deposit admitted fewer than d blocks below the
 * view is refused by the journal-preparation depth check
 * (`assertIncludedEventsDeep`). Each test opens its own lifecycle: the shared emulator harness
 * resets the node runtime after every test.
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

it("lands a deposit commit built with d = 1, its end at the lagged cap", () =>
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
    // The block the lag measures from, then one block above it.
    await h.synchronize();
    fixture.emulator.awaitBlock(1);
    vi.setSystemTime(fixture.emulator.now());

    // The owned commit worker runs under the production fixture's config.
    const nodeConfig = {
      ...h.production.nodeConfig,
      HISTORY_COMMIT_HORIZON_LAG_BLOCKS: 1,
    };
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
    const expected = await readFollowerHorizon(h);
    expect(await fixture.operatorLucid.awaitTx(output.submittedTxHash)).toBe(
      true,
    );
    const queue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queue.at(-1)!.datum),
    );
    // The landed header end is the lagged cap, short of the unlagged one;
    // the state queue accepted it as the tx's validity upper bound.
    expect(expected.laggedCap).toBeLessThan(expected.unlagged);
    expect(header.endTime).toBe(BigInt(expected.laggedCap));
    expect(header.endTime).toBeGreaterThanOrEqual(
      BigInt(built.metadata.inclusionTime),
    );
  }));

it("refuses an end above the lagged cap at the final recheck", () =>
  withLifecycle(async (h) => {
    await followNextBlock(h);
    await followNextBlock(h);
    const { laggedCap, unlagged } = await readFollowerHorizon(h);
    expect(laggedCap).toBeLessThan(unlagged);
    const recheck = (end: number, lagBlocks: number) =>
      h.runWithoutSynchronizing(
        Effect.either(
          refreshCommitUserEventSourcesThroughBlockEnd(
            end,
            lagOf(h, lagBlocks),
          ),
        ),
      );
    const refused = await recheck(laggedCap + 1, 1);
    expect(refused._tag).toBe("Left");
    expect(refused._tag === "Left" && refused.left.message).toBe(
      "Final commitment end time exceeds the ingested event horizon",
    );
    expect((await recheck(laggedCap, 1))._tag).toBe("Right");
    // Unlagged, the same end is inside the horizon: the lag refused it.
    expect((await recheck(laggedCap + 1, 0))._tag).toBe("Right");
  }));

it("refuses to journal a commit including a deposit admitted fewer than d blocks below the view", () =>
  withLifecycle(async (h) => {
    const { fixture } = h;
    const wallet = fixture.depositorLucid;
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await h.synchronize();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: await wallet.wallet().address(),
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    expect(await wallet.awaitTx(await signed.submit())).toBe(true);
    // The follower admits the deposit at the next block it follows.
    await followNextBlock(h);
    await followNextBlock(h);
    const depth = await h.runWithoutSynchronizing(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const [row] = yield* sql<{
          event_id: Buffer;
          admitted: string;
          view: string;
        }>`SELECT d.event_id, e.admitted_height::text AS admitted,
            (SELECT height::text FROM l1_follower_cursor) AS view
          FROM deposits_utxos d JOIN node_l1_events e
            ON e.kind = 'deposit' AND e.event_key = d.l1_event_key`;
        return {
          eventId: Buffer.from(row!.event_id),
          blocks: Number(row!.view) - Number(row!.admitted),
        };
      }),
    );
    expect(depth.blocks).toBeGreaterThan(0);
    const check = (lagBlocks: number) =>
      h.runWithoutSynchronizing(
        Effect.either(
          assertIncludedEventsDeep({
            lagBlocks,
            depositIds: [depth.eventId],
            forcedIds: [],
            withdrawalIds: [],
          }),
        ),
      );
    // One block short of d deep: refused before anything is journaled.
    const refused = await check(depth.blocks + 1);
    expect(refused._tag).toBe("Left");
    expect(refused._tag === "Left" && refused.left.message).toBe(
      COMMIT_EVENT_NOT_DEEP_MESSAGE,
    );
    // Exactly d deep, and d = 0, are accepted.
    expect((await check(depth.blocks))._tag).toBe("Right");
    expect((await check(0))._tag).toBe("Right");
    // One more block makes the refused event deep enough.
    await followNextBlock(h);
    expect((await check(depth.blocks + 1))._tag).toBe("Right");
  }));
