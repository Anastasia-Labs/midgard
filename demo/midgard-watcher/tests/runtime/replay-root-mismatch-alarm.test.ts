/**
 * A replayed ledger that does not open the authenticated root reaches the
 * operator: the confirmed head's mismatch raises the `root_mismatch` alert
 * (an `active_alert` readiness reason) until the ledger advances again, every newly observed refusal is counted by phase
 * and kind, logged once, and listed in `/v1/status`, and `/metrics` serves
 * both to Prometheus.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type {
  LedgerCheckpoint,
  RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherReplayRefusals } from "../../src/fault-proofs/replay-refusals.js";
import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { observation } from "../fault-proofs/fault-decision-bridge.observation.js";
import { supervisor } from "./operations-observability.supervisor.js";

const MANIFEST = "11".repeat(32);

const availabilityAuthority = () => ({
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
});

const directories: string[] = [];
afterEach(async () => {
  for (const directory of directories.splice(0))
    await rm(directory, { recursive: true, force: true });
});

const sample = (
  text: string,
  name: string,
  labels: Readonly<Record<string, string>>,
): number | undefined => {
  const wanted = `${name}{${Object.entries(labels)
    .map(([key, value]) => `${key}="${value}"`)
    .join(",")}}`;
  const line = text.split("\n").find((entry) => entry.startsWith(`${wanted} `));
  return line === undefined ? undefined : Number(line.split(" ").at(-1));
};

/** A watcher's replay keeper with its refusals wired to operations. */
const watcher = async (follower: WatcherFollowerReplaySource) => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-replay-alarm-"));
  directories.push(directory);
  const logged: Record<string, unknown>[] = [];
  const refusals = createWatcherReplayRefusals({
    nowMs: () => 100_000,
    log: (line) => logged.push(JSON.parse(line) as Record<string, unknown>),
  });
  let stored: LedgerCheckpoint | undefined;
  const keeper = createWatcherReplayedLedger({
    follower,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority,
    checkpointPath: join(directory, "ledger.json"),
    unsafeCheckpointStoreForTest: {
      read: async () => stored,
      write: async (checkpoint) => {
        stored = checkpoint;
      },
    },
    refusals,
  });
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: MANIFEST,
    supervisor: supervisor().runtime,
    launchScopeStatus: () => ({
      installedCategoryCount: 32,
      requiredCategoryCount: 32,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 1,
      oldestQueuedAtMs: "99000",
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    replayRefusals: refusals.status,
    nowMs: () => 100_000n,
  });
  refusals.attach(operations.sink);
  operations.sink.recordL1Source({
    sourceIdentityDigest: "22".repeat(32),
    sourceMode: "local_node",
    status: "consistent",
    blockHash: "33".repeat(32),
    blockNo: "50",
    slot: "500",
    observedAtMs: "100000",
  });
  const scrape = async () => {
    const response = await operations.handleHttpRequest(
      new Request("http://127.0.0.1/metrics"),
    );
    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("text/plain");
    return await response.text();
  };
  return { keeper, operations, logged, scrape };
};

describe("the replayed ledger's root mismatch alarm", () => {
  it("raises root_mismatch while the confirmed head does not open its root, and clears it once the ledger advances", async () => {
    let state = SDK.makeGenesisConfirmedState(1000n);
    const genesisRoot = state.utxoRoot;
    const w = await watcher({
      epoch: () => 0,
      confirmed: async () => ({ state, current: () => true }),
      materialFor: async () => {
        throw new Error("genesis never needs replay material");
      },
      availabilityStatus: async () => "Unattested",
      checkpointDurable: () => undefined,
      close: () => undefined,
    });

    expect(await w.keeper.advance(observation([]), [])).toEqual([]);
    const healthy = await w.scrape();
    expect(
      sample(healthy, "midgard_watcher_active_alerts", {
        code: "root_mismatch",
      }),
    ).toBe(0);
    expect(
      sample(healthy, "midgard_watcher_replay_refusals_total", {
        phase: "confirmed",
        kind: "root-mismatch",
      }),
    ).toBe(0);
    expect(w.operations.api.status().replayRefusals.rootMismatch).toBeNull();

    // The confirmed head now binds another root under the same hash.
    state = { ...state, utxoRoot: "00".repeat(32) };
    expect(await w.keeper.advance(observation([]), [])).toMatchObject([
      { reason: "replayed_ledger_checkpoint_pending" },
    ]);
    const mismatched = await w.scrape();
    expect(
      sample(mismatched, "midgard_watcher_active_alerts", {
        code: "root_mismatch",
      }),
    ).toBe(1);
    expect(
      sample(mismatched, "midgard_watcher_replay_refusals_total", {
        phase: "confirmed",
        kind: "root-mismatch",
      }),
    ).toBe(1);
    const status = w.operations.api.status();
    expect(status.replayRefusals.rootMismatch).toMatchObject({
      phase: "confirmed",
      kind: "root-mismatch",
      headerHash: state.headerHash,
      observedAtMs: "100000",
    });
    expect(status.activeAlerts.map(({ code }) => code)).toEqual([
      "root_mismatch",
    ]);
    expect(status.readiness).toBe("not_ready");
    expect(status.readinessReasons).toContain("active_alert");
    expect(w.logged).toEqual([
      expect.objectContaining({
        level: "error",
        reason: "replayed_ledger_root_mismatch",
        headerHash: state.headerHash,
      }),
    ]);

    // The same mismatch again is not a new observation.
    await w.keeper.advance(observation([]), []);
    expect(
      sample(await w.scrape(), "midgard_watcher_replay_refusals_total", {
        phase: "confirmed",
        kind: "root-mismatch",
      }),
    ).toBe(1);
    expect(w.logged).toHaveLength(1);

    // The root is back: the ledger advances and the alarm clears.
    state = { ...state, utxoRoot: genesisRoot };
    expect(await w.keeper.advance(observation([]), [])).toEqual([]);
    const resolved = await w.scrape();
    expect(
      sample(resolved, "midgard_watcher_active_alerts", {
        code: "root_mismatch",
      }),
    ).toBe(0);
    expect(
      sample(resolved, "midgard_watcher_replay_refusals_total", {
        phase: "confirmed",
        kind: "root-mismatch",
      }),
    ).toBe(1);
    const cleared = w.operations.api.status();
    expect(cleared.replayRefusals.rootMismatch).toBeNull();
    expect(cleared.activeAlerts).toEqual([]);
    expect(cleared.readinessReasons).not.toContain("active_alert");
  });

  it("counts a classified block whose replay does not open its root without raising the alarm", async () => {
    // An empty block over genesis whose header claims another root: the
    // bytes are canonical, the transition does not open its root.
    const honest = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const header = { ...honest.header, utxosRoot: "99".repeat(32) };
    const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
    const block = {
      headerHash,
      payloadEnvelopeCbor: await wrapDaPayload(
        SDK.encodeDaPayload({
          ...honest.payload,
          block_body: {
            ...honest.payload.block_body,
            header,
            header_hash: headerHash,
          },
        }),
        { mode: "identity" },
      ),
    };
    const commitment = SDK.buildDaAvailabilityCommitment({
      ...availabilityAuthority(),
      headerHash: block.headerHash,
      payload: block.payloadEnvelopeCbor,
    });
    const w = await watcher({
      epoch: () => 0,
      confirmed: async () => ({
        state: SDK.makeGenesisConfirmedState(1000n),
        current: () => true,
      }),
      materialFor: async (payload) => ({
        blockTimeMs: Number(payload.block_body.header.endTime),
        replayUserEvent: () => Effect.die("the block has no user event"),
        verifyForcedSource: () => Effect.die("the block has no forced order"),
      }),
      availabilityStatus: async () => ({
        Attested: {
          commitment_hash: SDK.daAvailabilityCommitmentHash(commitment),
        },
      }),
      checkpointDurable: () => undefined,
      close: () => undefined,
    });
    const sources: RetainedDaPayloadSource[] = [
      {
        sourceId: "s0",
        fetchPayloadByHeaderHash: async () => ({
          ok: true,
          sourceId: "s0",
          sourcePeerId: "peer",
          payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "s0/peer",
            grade: "security",
          },
          attempts: [],
        }),
      },
    ];

    // The classifier still gets the unchecked post-ledger.
    expect(await w.keeper.ledgerAt(sources)(block.headerHash)).toEqual([]);
    const text = await w.scrape();
    expect(
      sample(text, "midgard_watcher_replay_refusals_total", {
        phase: "classification",
        kind: "root-mismatch",
      }),
    ).toBe(1);
    expect(
      sample(text, "midgard_watcher_active_alerts", { code: "root_mismatch" }),
    ).toBe(0);
    const status = w.operations.api.status();
    expect(status.replayRefusals.rootMismatch).toBeNull();
    expect(status.replayRefusals.recent).toMatchObject([
      {
        phase: "classification",
        kind: "root-mismatch",
        headerHash: block.headerHash,
      },
    ]);
    expect(w.logged).toEqual([
      expect.objectContaining({
        level: "warn",
        reason: "replayed_ledger_refused",
        phase: "classification",
        kind: "root-mismatch",
        headerHash: block.headerHash,
      }),
    ]);
  });
});
