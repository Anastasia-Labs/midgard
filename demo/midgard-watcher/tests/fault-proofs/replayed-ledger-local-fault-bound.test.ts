/**
 * The replayed ledger's advance holds a local replay fault on its confirmed
 * head under `replayed_ledger_local_fault` and replays it again once its
 * backoff (1 s doubling to 60 s) passed, for as long as the fault recurs:
 * after `LOCAL_FAULT_RUN_LIMIT` identical faults in a row the hold is named
 * `replayed_ledger_local_fault_stuck`, logged once at error level, and still
 * retried. It never ends the watcher: the decision driver keeps running its
 * passes past the bound. An advance that completes releases the hold.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  type LedgerCheckpoint,
  LOCAL_FAULT_RUN_LIMIT,
  ReplayLocalFault,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  WATCHER_REPLAY_LOCAL_FAULT,
  WATCHER_REPLAY_LOCAL_FAULT_STUCK,
} from "../../src/fault-proofs/replay-local-fault-bound.js";
import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import {
  createWatcherDecisionDriver,
  WATCHER_DECISION_PASS_FAILED,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import {
  applyAll,
  AUTHORITY,
  chainEvents,
  collaborators,
  openStore,
  RELEASE_DEPTH,
  SOURCE_ID,
} from "../support/l1-follower-store-reset.js";
import { observation } from "./fault-decision-bridge.observation.js";

const availabilityAuthority = () => ({
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
});

const FAULT = "the script evaluator failed locally";

const directories: string[] = [];
const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  vi.restoreAllMocks();
  for (const close of closers.splice(0).reverse()) await close();
  for (const directory of directories.splice(0))
    await rm(directory, { recursive: true, force: true });
});

/**
 * A keeper whose confirmed head is one block on genesis, whose replay fails
 * locally while `faulting` holds, on the clock `state.nowMs`. Its checkpoint
 * store never restores, so a new follower generation replays the head again.
 */
const scene = async () => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-local-fault-"));
  directories.push(directory);
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  const state = { faulting: true, epoch: 0, nowMs: 0, replays: 0 };
  const written: LedgerCheckpoint[] = [];
  const sources: RetainedDaPayloadSource[] = [
    {
      sourceId: "local-fault",
      fetchPayloadByHeaderHash: async () => ({
        ok: true,
        sourceId: "local-fault",
        sourcePeerId: "peer",
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        provenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "local-fault/peer",
          grade: "security",
        },
        attempts: [],
      }),
    },
  ];
  const keeper = createWatcherReplayedLedger({
    follower: {
      epoch: () => state.epoch,
      confirmed: async () => ({
        state: {
          ...SDK.makeGenesisConfirmedState(1000n),
          headerHash: block.headerHash,
          utxoRoot: block.header.utxosRoot,
        },
        current: () => true,
      }),
      materialFor: async (payload) => {
        state.replays += 1;
        if (state.faulting)
          throw new ReplayLocalFault({ message: FAULT, cause: undefined });
        return {
          blockTimeMs: Number(payload.block_body.header.endTime),
          replayUserEvent: () => Effect.die("the block has no user event"),
          verifyForcedSource: () => Effect.die("the block has no forced order"),
        };
      },
      availabilityStatus: async () => "Unattested",
      checkpointDurable: () => undefined,
      close: () => undefined,
    },
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority,
    checkpointPath: join(directory, "ledger.json"),
    unsafeCheckpointStoreForTest: {
      read: async () => undefined,
      write: async (checkpoint) => {
        written.push(checkpoint);
      },
    },
    nowMs: () => state.nowMs,
  });
  const advance = () => keeper.advance(observation([]), sources);
  /** `count` advances, each one replay held on the local fault, the clock
   * moved past the longest backoff before each. */
  const held = async (count: number) => {
    const reasons: string[] = [];
    for (let attempt = 0; attempt < count; attempt += 1) {
      state.nowMs += 60_000;
      const replays = state.replays;
      const outcome = await advance();
      expect(state.replays).toBe(replays + 1);
      expect(outcome).toHaveLength(1);
      expect(outcome[0]!.detail).toBe(FAULT);
      reasons.push(outcome[0]!.reason);
    }
    return reasons;
  };
  return { block, state, written, advance, held, keeper, sources };
};

const stderrLines = () => {
  const lines: string[] = [];
  vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
    lines.push(String(chunk));
    return true;
  });
  return lines;
};

describe("the replayed ledger's local-fault hold", () => {
  it("holds past the bound, names the hold stuck with one error-level alarm, and keeps retrying", async () => {
    const s = await scene();
    const logged = stderrLines();
    const reasons = await s.held(LOCAL_FAULT_RUN_LIMIT + 2);
    expect(reasons).toEqual([
      ...Array.from(
        { length: LOCAL_FAULT_RUN_LIMIT - 1 },
        () => WATCHER_REPLAY_LOCAL_FAULT,
      ),
      WATCHER_REPLAY_LOCAL_FAULT_STUCK,
      WATCHER_REPLAY_LOCAL_FAULT_STUCK,
      WATCHER_REPLAY_LOCAL_FAULT_STUCK,
    ]);
    const alarms = logged
      .filter((line) => line.includes(WATCHER_REPLAY_LOCAL_FAULT_STUCK))
      .map((line) => JSON.parse(line) as Record<string, unknown>);
    expect(alarms).toEqual([
      expect.objectContaining({
        level: "error",
        detail: `${s.block.headerHash}: ${FAULT}`,
      }),
    ]);
    expect(s.written).toEqual([]);
    // Still retried: the head advances once the fault clears.
    s.state.faulting = false;
    s.state.nowMs += 60_000;
    expect(await s.advance()).toEqual([]);
    expect(s.written.map(({ headerHash }) => headerHash)).toEqual([
      s.block.headerHash,
    ]);
  });

  it("waits out the backoff, doubling from 1 s to 60 s, before replaying the head again", async () => {
    const s = await scene();
    stderrLines();
    const delays: number[] = [];
    for (let attempt = 0; attempt < 9; attempt += 1) {
      const replays = s.state.replays;
      let waited = 0;
      while (s.state.replays === replays) {
        const outcome = await s.advance();
        expect(outcome).toHaveLength(1);
        if (s.state.replays === replays) {
          waited += 100;
          s.state.nowMs += 100;
        }
      }
      delays.push(waited);
    }
    expect(delays).toEqual([
      0, 1_000, 2_000, 4_000, 8_000, 16_000, 32_000, 60_000, 60_000,
    ]);
  });

  it("releases the hold once an advance toward the head completes", async () => {
    const s = await scene();
    stderrLines();
    await s.held(LOCAL_FAULT_RUN_LIMIT - 1);
    s.state.faulting = false;
    s.state.nowMs += 60_000;
    expect(await s.advance()).toEqual([]);
    // A new follower generation replays the head again, at once.
    s.state.epoch += 1;
    s.state.faulting = true;
    const replays = s.state.replays;
    expect(await s.advance()).toEqual([
      { reason: WATCHER_REPLAY_LOCAL_FAULT, detail: FAULT },
    ]);
    expect(s.state.replays).toBe(replays + 1);
  });

  it("keeps the decision driver's passes running past the bound", async () => {
    const s = await scene();
    stderrLines();
    const store = openStore(":memory:");
    closers.push(() => store.close());
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, chainEvents(8));
    const c = collaborators();
    const logged: string[] = [];
    const driver = createWatcherDecisionDriver(
      {
        store,
        onFollowerChange: () => () => undefined,
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        replayedLedger: {
          depth: RELEASE_DEPTH,
          advance: (current) => s.keeper.advance(current, s.sources),
        },
        retryDelayMs: 10,
        log: (line) => logged.push(line),
      },
      { atTip: () => true, started: () => true },
    );
    closers.push(() => driver.close());
    while (s.state.replays < LOCAL_FAULT_RUN_LIMIT + 2) {
      s.state.nowMs += 60_000;
      driver.wake();
      await driver.idle();
    }
    const dispatched = c.seen.dispatched.length;
    driver.wake();
    await driver.idle();
    expect(c.seen.dispatched.length).toBeGreaterThan(dispatched);
    const reasons = driver.readiness().map(({ reason }) => reason);
    expect(reasons).toContain(WATCHER_REPLAY_LOCAL_FAULT_STUCK);
    expect(reasons).not.toContain(WATCHER_DECISION_PASS_FAILED);
    // No pass failed on the fault: the driver never stopped serving.
    expect(logged).toEqual([]);
    expect(driver.status().lastError).toBeNull();
    s.state.faulting = false;
    s.state.nowMs += 60_000;
    driver.wake();
    await driver.idle();
    expect(driver.readiness()).toEqual([]);
    expect(s.written.map(({ headerHash }) => headerHash)).toEqual([
      s.block.headerHash,
    ]);
  });
});
