import type {
  ChainSyncEvent,
  ChainSyncStream,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import { decodeBlock, openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import {
  encodeUtxoAnswer,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  openWatcherFollowerRuntime,
  watcherSecurityParameter,
} from "../../src/l1-follower/follower-runtime.js";
import {
  readWatcherObservation,
  type WatcherObservationAuthority,
} from "../../src/l1-follower/observation.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import { WATCHER_UNIT_HISTORY_TABLE } from "../../src/l1-follower/tables.js";
import { L1_TX_INPUTS_UNRESOLVABLE } from "../../src/l1-follower/tx-inputs.js";
import { startWatcherOperationsHttpServer } from "../../src/runtime/operations-http.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { createWatcherDecisionDriver } from "../../src/runtime/watcher-runtime.decision-driver.js";
import { createWatcherL1Readiness } from "../../src/runtime/watcher-runtime.l1-readiness.js";
import { supervisor } from "../runtime/operations-observability.supervisor.js";
import {
  commitTx,
  initTx,
  queueState,
  SIM_HUB_ORACLE_ONE_SHOT,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";

// W3/L3 acceptance (ticket W1): the watcher's decisions follow the
// follower through rollbacks. A rollback below k, even past the release
// depth, rewinds the store and the next pass recomputes every decision
// in-process: nothing is quarantined and nothing restarts. A rollback
// beyond k stops the follower with an intervention: /readyz names it, the
// process stays up and the driver keeps serving the last facts.

const D = SIM_WATCHER_DEPLOYMENT;
/** automaticRecoveryMaxDepth: the store's k is this plus two. */
const RECOVERY_DEPTH = 4;
const K = watcherSecurityParameter(RECOVERY_DEPTH);
const RELEASE_DEPTH = 2;
const SOURCE_ID = "decision-driver-fork-sim";
const ONE_SHOT: SimUtxo = {
  outRef: SIM_HUB_ORACLE_ONE_SHOT,
  output: { address: simUniverse().untrackedAddress, lovelace: 5_000_000n },
};

const AUTHORITY: WatcherObservationAuthority = {
  authorityDigest: "a1".repeat(32),
  deploymentFingerprint: "a2".repeat(32),
  protocolScriptHashes: {
    hubOracleMint: D.hubOracleMint,
    stateQueueSpend: D.stateQueueSpend,
    stateQueueMint: D.stateQueueMint,
    correctionLockSpend: D.correctionLockSpend,
    fraudProofSpend: D.fraudProofSpend,
    fraudProofMint: D.fraudProofMint,
    referenceScriptAuthMint: "b1".repeat(28),
    availabilityChallengeSpend: D.availabilityChallengeSpend,
    availabilityChallengeMint: D.availabilityChallengeMint,
    daBondPoolSpend: D.daBondPoolSpend,
    daBondPoolMint: "b2".repeat(28),
    daAttestationMint: D.daAttestationMint,
    availabilityChallengeOpenWithdraw: "b3".repeat(28),
    availabilityChallengeSettleWithdraw: "b4".repeat(28),
    availabilityChallengeCloseWithdraw: "b5".repeat(28),
    availabilityChallengeTimeoutWithdraw: "b6".repeat(28),
  },
};

type Pause = Readonly<{
  /** Events served up to here. */
  events: number;
  /** The canonical chain's raw blocks at this pause. */
  blocks: readonly Buffer[];
}>;

/** A node script: its events, the pauses, and the rollbacks it serves. */
const nodeScript = (
  build: (chain: SimChain, events: ChainSyncEvent[], pause: () => void) => void,
) => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const events: ChainSyncEvent[] = [];
  const pauses: Pause[] = [];
  build(chain, events, () =>
    pauses.push({ events: events.length, blocks: chain.rawBlocks() }),
  );
  return { events, pauses };
};

const forward = (
  chain: SimChain,
  events: ChainSyncEvent[],
  blocks: number,
  withCommit = false,
) => {
  for (let i = 0; i < blocks; i += 1) {
    const state = withCommit && i === 0 ? queueState(chain, D) : null;
    events.push(
      chain.forward(state === null ? [] : [commitTx(state, D)]).event,
    );
  }
};

const until = async (what: string, holds: () => boolean, ms = 20_000) => {
  const deadline = Date.now() + ms;
  while (!holds()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};

/**
 * A node serving the script up to `limit`: every stream resumes after the
 * last acknowledged event, as a node intersecting at the follower's cursor.
 */
const scriptedNode = (
  events: readonly ChainSyncEvent[],
  ledger: readonly SimUtxo[] = [ONE_SHOT],
) => {
  const state = { limit: 0, acked: 0 };
  const transport = {
    openChainSync: (): ChainSyncStream => {
      let position = state.acked;
      let closed = false;
      return {
        opened: Promise.resolve(),
        next: async () => {
          for (;;) {
            if (closed) return undefined;
            if (position < Math.min(state.limit, events.length))
              return events[(position += 1) - 1];
            await new Promise((resolve) => setTimeout(resolve, 2));
          }
        },
        ack: (seq: bigint) => {
          const index = events.findIndex((event) => event.seq === seq);
          if (index >= 0) state.acked = Math.max(state.acked, index + 1);
        },
        close: () => {
          closed = true;
          return Promise.resolve();
        },
      } as unknown as ChainSyncStream;
    },
    // Its ledger state holds the one-shot the init spends (created before
    // the scripted chain), which the follower stores at ingest.
    withLedgerState: (
      _at: unknown,
      use: (session: { query: () => Promise<Uint8Array> }) => unknown,
    ) => use({ query: () => Promise.resolve(encodeUtxoAnswer([...ledger])) }),
    // The scripted node is always reachable: no readiness change to report.
    readiness: { ready: true, nodeToClientVersion: 32784 },
    onReadiness: (): (() => void) => () => undefined,
    close: () => Promise.resolve(),
  } as unknown as L1NodeTransport;
  return { state, transport };
};

/** The tip observation a fresh store replaying `blocks` reads. */
const freshObservation = async (
  blocks: readonly Buffer[],
): Promise<WatcherAuthenticatedStateQueueObservation> => {
  const store = openSqliteFactStore({
    ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
    path: ":memory:",
  });
  try {
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    for (const raw of blocks)
      expect((await store.applyBlock(decodeBlock(raw))).kind).toBe("applied");
    const read = await readWatcherObservation(store, {
      authority: AUTHORITY,
      sourceId: SOURCE_ID,
      depth: 1,
      releaseDepth: RELEASE_DEPTH,
    });
    if (read.kind !== "ok") throw new Error(`${read.reason}: ${read.detail}`);
    return read.observation;
  } finally {
    await store.close();
  }
};

/** The follower runtime over the scripted node, with the decision driver. */
const watcherOver = (
  events: readonly ChainSyncEvent[],
  ledger?: readonly SimUtxo[],
) => {
  const node = scriptedNode(events, ledger);
  const follower = openWatcherFollowerRuntime({
    deployment: D,
    storePath: ":memory:",
    automaticRecoveryMaxDepth: RECOVERY_DEPTH,
    origin: {
      origin: SIM_ORIGIN.point,
      hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
    },
    node: {
      binaryPath: "unused",
      socketPath: "unused",
      networkMagic: 42,
      requestTimeoutMs: 1_000,
    },
    walletAddresses: [],
    unsafeTransportForTest: node.transport,
  });
  const seen = {
    bridgeInvalidations: 0,
    availabilityInvalidations: 0,
    dispatched: [] as string[],
    inclusionMissing: 0,
  };
  const driver = createWatcherDecisionDriver(
    {
      store: follower.store,
      onFollowerChange: (listener) => follower.onChange(() => listener()),
      authority: AUTHORITY,
      sourceId: SOURCE_ID,
      releaseDepth: RELEASE_DEPTH,
      bridge: {
        prepareForRecovery: (observation) =>
          Promise.resolve({
            observationDigest: observation.observationDigest,
            decisionDigests: [],
            target: null,
          }),
        recoverExisting: () => Promise.resolve(0),
        reconcileAndDispatch: (observation) => {
          seen.dispatched.push(observation.observationDigest);
          return Promise.resolve({
            observationDigest: observation.observationDigest,
            decisionDigests: [],
            target: null,
          });
        },
        invalidateForRollback: () => {
          seen.bridgeInvalidations += 1;
        },
      },
      availability: {
        // The availability actor reads its L1 payloads at the pass's tip.
        reconcile: () => {
          if (driver.inclusion() === null) seen.inclusionMissing += 1;
          return Promise.resolve();
        },
        invalidateForRollback: () => {
          seen.availabilityInvalidations += 1;
        },
      },
      retryDelayMs: 10,
    },
    { atTip: () => follower.status()?.atTip ?? false },
  );
  const l1 = createWatcherL1Readiness({ follower, driver: () => driver });
  const serve = async (count: number) => {
    node.state.limit = count;
    await until(
      `the follower to apply ${count.toString()} events`,
      () => (follower.status()?.events ?? 0) >= count,
    );
  };
  return { node, follower, driver, l1, seen, serve };
};

const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0)) await close();
  vi.restoreAllMocks();
});

describe("decision driver over the follower: rollbacks (W3/L3)", () => {
  it("recomputes every decision in-process after rollbacks below k, the deep ones past the release depth included", async () => {
    const { events, pauses } = nodeScript((chain, events, pause) => {
      events.push(chain.forward([initTx(D)]).event);
      forward(chain, events, 6, true);
      pause();
      // Past the release depth (2), below k (6).
      events.push(chain.backward(3));
      forward(chain, events, 4, true);
      pause();
      // As deep as the store allows below k.
      events.push(chain.backward(K - 1));
      forward(chain, events, K + 1, true);
      pause();
    });
    const exit = vi.spyOn(process, "exit");
    const watcher = watcherOver(events);
    opened.push(async () => {
      await watcher.driver.close();
      await watcher.follower.close();
    });
    for (const [index, pause] of pauses.entries()) {
      await watcher.serve(pause.events);
      const expected = await freshObservation(pause.blocks);
      await until(
        `pause ${index.toString()} decided at the canonical tip`,
        () =>
          watcher.driver.readiness().length === 0 &&
          watcher.driver.current().nativePoint.blockHash ===
            expected.nativePoint.blockHash,
      );
      await watcher.driver.idle();
      expect(watcher.driver.current().observationDigest).toBe(
        expected.observationDigest,
      );
      expect(watcher.driver.status().rewinds).toBe(index);
      expect(watcher.seen.bridgeInvalidations).toBe(index);
      expect(watcher.seen.availabilityInvalidations).toBe(index);
    }
    // The last decision was dispatched from the canonical facts.
    expect(watcher.seen.dispatched.at(-1)).toBe(
      watcher.driver.current().observationDigest,
    );
    expect(watcher.seen.inclusionMissing).toBe(0);
    expect(watcher.follower.status()?.interventions ?? []).toEqual([]);
    expect(await watcher.follower.readiness()).toEqual([]);
    expect(exit).not.toHaveBeenCalled();
  });

  it("holds /readyz with rollback_beyond_k on a rollback deeper than k, and the process stays live", async () => {
    const { events, pauses } = nodeScript((chain, events, pause) => {
      events.push(chain.forward([initTx(D)]).event);
      forward(chain, events, K + 3, true);
      pause();
      events.push(chain.backward(K + 1));
      forward(chain, events, 2);
      pause();
    });
    const exit = vi.spyOn(process, "exit");
    const watcher = watcherOver(events);
    const proofSupervisor = supervisor();
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: AUTHORITY.deploymentFingerprint,
      supervisor: proofSupervisor.runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 0,
        oldestQueuedAtMs: null,
      }),
      l1Readiness: () => watcher.l1.read(),
    });
    const server = await startWatcherOperationsHttpServer({
      endpoint: "http://127.0.0.1:0",
      observability,
      unsafeAllowEphemeralPortForTest: true,
    });
    opened.push(async () => {
      await server.close();
      await watcher.driver.close();
      await watcher.follower.close();
    });

    await watcher.serve(pauses[0]!.events);
    await until("the first decision", () => {
      try {
        return watcher.driver.current() !== undefined;
      } catch {
        return false;
      }
    });
    await watcher.driver.idle();
    const before = watcher.driver.current();

    watcher.node.state.limit = pauses[1]!.events;
    await until("the follower to refuse the rollback", () =>
      (watcher.follower.status()?.interventions ?? []).some(
        (entry) => entry.reason === "rollback_beyond_k",
      ),
    );
    await watcher.l1.refresh();
    expect(watcher.l1.read().map(({ reason }) => reason)).toContain(
      "rollback_beyond_k",
    );
    const readyz = await fetch(`${server.endpoint}/readyz`);
    expect(readyz.status).toBe(503);
    const body = (await readyz.json()) as {
      ready: boolean;
      l1: { reason: string }[];
    };
    expect(body.ready).toBe(false);
    expect(body.l1.map(({ reason }) => reason)).toContain("rollback_beyond_k");
    const status = await fetch(`${server.endpoint}/v1/status`);
    expect(status.status).toBe(200);
    expect(await status.json()).toMatchObject({ liveness: "live" });
    // No rewind reached the driver; it keeps serving the last facts.
    expect(watcher.driver.status().rewinds).toBe(0);
    expect(watcher.driver.current().observationDigest).toBe(
      before.observationDigest,
    );
    expect(exit).not.toHaveBeenCalled();
  });

  it("reports an unresolvable input as a degradation while /readyz stays ready, and fails /readyz by name only while a proof pin holds it", async () => {
    // A node whose ledger lacks the one-shot the init spends: the init's
    // input can never resolve.
    const { events, pauses } = nodeScript((chain, events, pause) => {
      events.push(chain.forward([initTx(D)]).event);
      forward(chain, events, 3);
      pause();
    });
    const watcher = watcherOver(events, []);
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: AUTHORITY.deploymentFingerprint,
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      // As the supervisor stub reports, so /v1/metrics reads.
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "0",
      }),
      l1Readiness: () => watcher.l1.read(),
      l1Degradations: () => watcher.l1.degradations(),
    });
    const server = await startWatcherOperationsHttpServer({
      endpoint: "http://127.0.0.1:0",
      observability,
      unsafeAllowEphemeralPortForTest: true,
    });
    opened.push(async () => {
      await server.close();
      await watcher.driver.close();
      await watcher.follower.close();
    });
    const l1Reasons = async () => {
      await watcher.l1.refresh();
      const body = (await (
        await fetch(`${server.endpoint}/readyz`)
      ).json()) as {
        l1: { reason: string }[];
      };
      return body.l1.map(({ reason }) => reason);
    };

    await watcher.serve(pauses[0]!.events);
    const deadline = Date.now() + 20_000;
    while ((await watcher.follower.degradations()).length === 0) {
      if (Date.now() > deadline) throw new Error("no degradation reported");
      await new Promise((resolve) => setTimeout(resolve, 5));
    }
    expect(await watcher.follower.readiness()).toEqual([]);
    expect(await l1Reasons()).not.toContain(L1_TX_INPUTS_UNRESOLVABLE);
    const metrics = (await (
      await fetch(`${server.endpoint}/v1/metrics`)
    ).json()) as { l1Degradations: Record<string, string> };
    expect(metrics.l1Degradations).toEqual({
      [L1_TX_INPUTS_UNRESOLVABLE]: "1",
    });

    // A proof pin holding the init's hub-oracle history makes it blocking.
    const [hubOracle] = await watcher.follower.store.transaction("read", (tx) =>
      tx.query(`SELECT DISTINCT unit FROM ${WATCHER_UNIT_HISTORY_TABLE}`),
    );
    const target = { category: "doubleSpend", headerHash: "ab".repeat(28) };
    await watcher.follower.proofRetention.pin(target);
    await watcher.follower.proofRetention.holdUnits(target.headerHash, [
      Buffer.from(hubOracle!.unit as Uint8Array).toString("hex"),
    ]);
    expect(await l1Reasons()).toContain(L1_TX_INPUTS_UNRESOLVABLE);
    expect((await fetch(`${server.endpoint}/readyz`)).status).toBe(503);

    await watcher.follower.proofRetention.release(target);
    expect(await l1Reasons()).not.toContain(L1_TX_INPUTS_UNRESOLVABLE);
    expect(watcher.l1.degradations()).toMatchObject([
      { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 },
    ]);
  });
});
