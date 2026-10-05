import { createServer, type Socket } from "node:net";

import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  makeWatcherFinalityBootstrapState,
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  createWatcherLocalKupmiosNativeObservationRuntime,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
} from "../../src/l1/native-chain-sync.js";
import {
  initializeWatcherRollbackDurableAuthority,
  persistWatcherRollbackDurableObservation,
  persistWatcherRollbackDurableObservations,
  readWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthority,
} from "../../src/l1/rollback-engine.js";
import { makeEmptyWatcherDurableStore } from "../../src/storage/durable-store.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
  type SyntheticUserEventOriginFixture,
} from "../support/user-event-origin-fixture.js";
import {
  MemoryRollbackAuthorityBackend,
  rollbackAuthorityKey,
} from "./rollback-engine.test-tls-identities.js";

// Real native exact-point queries and local Kupmios admission over a synthetic
// chain of empty (quiet) blocks.
const openTcpPeer = async () => {
  const sockets = new Set<Socket>();
  const server = createServer((socket) => {
    sockets.add(socket);
    socket.on("error", () => undefined);
    socket.on("close", () => sockets.delete(socket));
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.removeListener("error", reject);
      resolve();
    });
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("Missing fixture TCP port");
  return {
    port: address.port,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      );
    },
  };
};

const RUN_LENGTH = 6;
let peers: Awaited<ReturnType<typeof openTcpPeer>>[] = [];
let fixture: SyntheticUserEventOriginFixture;
let policy: WatcherFinalityPolicy;
let entries: WatcherLocalKupmiosNativeObservation[];

// Transport attestations stay valid only while their native queries and the
// local runtime are open, so both live until the suite closes.
const closers: (() => unknown)[] = [];

const observeAll = async (
  blocks: readonly SyntheticUserEventBlock[],
): Promise<WatcherLocalKupmiosNativeObservation[]> => {
  let local:
    | Awaited<
        ReturnType<typeof createWatcherLocalKupmiosNativeObservationRuntime>
      >
    | undefined;
  const observed = [];
  for (const block of blocks) {
    const query = await openWatcherNativeExactPointQuery({
      binaryPath: fixture.nativeChainSyncBinaryPath,
      watcherConfig: fixture.watcherConfig,
      predecessor: {
        blockHash: block.parentPoint.blockHash,
        blockNo: block.parentPoint.blockNo,
        slot: block.parentPoint.slot,
      },
      target: {
        blockHash: block.point.blockHash,
        blockNo: block.point.blockNo,
        slot: block.point.slot,
      },
      timeoutMs: 60_000,
    });
    closers.push(() => query.close());
    const captured = readWatcherNativeExactPointQuery(query.receipt);
    if (local === undefined) {
      const opened = await createWatcherLocalKupmiosNativeObservationRuntime({
        watcherConfig: fixture.watcherConfig,
        deploymentIdentity: fixture.deploymentIdentity,
        nativeAuthority: captured.authority,
      });
      closers.unshift(() => opened.close());
      local = opened;
    }
    observed.push(
      await local.observe({
        block: admitWatcherNativeRollForwardBlock(captured.event),
        depth: captured.depthAtObservedTip,
      }),
    );
  }
  return observed;
};

beforeAll(async () => {
  peers = [await openTcpPeer(), await openTcpPeer()];
  fixture = await createSyntheticUserEventOriginFixture({
    queryEndpoints: {
      kupo: `http://127.0.0.1:${peers[0]!.port}`,
      ogmios: `ws://127.0.0.1:${peers[1]!.port}`,
    },
    nativeTipBaseDepth: 40,
  });
  const admitted = makeWatcherFinalityPolicy(
    fixture.watcherConfig,
    fixture.deploymentIdentity,
  );
  if (admitted === null) throw new Error("Fixture finality policy rejected");
  policy = admitted;
  const blocks = [];
  for (let index = 0; index < RUN_LENGTH; index += 1)
    blocks.push(await fixture.makeBlock({ transactions: [] }));
  entries = await observeAll(blocks);
}, 120_000);

afterAll(async () => {
  for (const close of closers) await close();
  await fixture?.close();
  for (const peer of peers) await peer.close();
});

const initialize = async () => {
  const backend = new MemoryRollbackAuthorityBackend();
  const bootstrapFinalityState = makeWatcherFinalityBootstrapState(policy);
  if (bootstrapFinalityState === null)
    throw new Error("Expected bootstrap finality");
  const { authority } = await initializeWatcherRollbackDurableAuthority({
    backend,
    policy,
    authenticationKey: rollbackAuthorityKey,
    trustedHead: null,
    bootstrapStore: makeEmptyWatcherDurableStore(policy.deploymentMarker),
    bootstrapFinalityState,
  });
  return { backend, authority };
};

const committed = (
  result: Awaited<ReturnType<typeof persistWatcherRollbackDurableObservations>>,
): WatcherRollbackDurableAuthority => {
  if (result.persistence !== "committed")
    throw new Error(`Expected a committed run, got ${result.persistence}`);
  return result.authority;
};

const evidenceOf = (authority: WatcherRollbackDurableAuthority) => {
  const read = readWatcherRollbackDurableAuthority(authority);
  return {
    observations: read.currentStore.l1Observations
      .map(({ observationId }) => observationId)
      .sort(),
    chainPoints: read.currentStore.chainPoints
      .map(({ chainPointId }) => chainPointId)
      .sort(),
    history: read.authenticatedConsistencyHistory
      .map(({ consistencyDigest }) => consistencyDigest)
      .sort(),
    finality: read.currentFinalityState,
  };
};

describe("journaling a run of authenticated quiet blocks", () => {
  it("commits the run once with exactly the evidence of block-by-block journaling", async () => {
    const batched = await initialize();
    const initialFinality = readWatcherRollbackDurableAuthority(
      batched.authority,
    ).currentFinalityState;
    const writesBefore = batched.backend.writes;
    const batchedAuthority = committed(
      await persistWatcherRollbackDurableObservations({
        authority: batched.authority,
        entries,
      }),
    );
    expect(batched.backend.writes - writesBefore).toBe(1);

    const sequential = await initialize();
    let sequentialAuthority = sequential.authority;
    for (const single of entries)
      sequentialAuthority = committed(
        await persistWatcherRollbackDurableObservation({
          ...single,
          authority: sequentialAuthority,
        }),
      );
    expect(sequential.backend.writes - writesBefore).toBe(RUN_LENGTH);

    const evidence = evidenceOf(batchedAuthority);
    expect(evidence.history).toHaveLength(RUN_LENGTH);
    expect(evidence).toEqual(evidenceOf(sequentialAuthority));
    // Quiet evidence never advances finality.
    expect(evidence.finality).toEqual(initialFinality);

    const writesAfter = batched.backend.writes;
    expect(
      (
        await persistWatcherRollbackDurableObservations({
          authority: batchedAuthority,
          entries,
        })
      ).persistence,
    ).toBe("unchanged");
    expect(batched.backend.writes).toBe(writesAfter);
  });

  it("refuses the whole run when any block is unauthenticated or repeated", async () => {
    const { backend, authority } = await initialize();
    const before = readWatcherRollbackDurableAuthority(authority);
    const writes = backend.writes;
    await expect(
      persistWatcherRollbackDurableObservations({
        authority,
        entries: entries.map((candidate, index) =>
          index === 2 ? { ...candidate, transportAttestations: [] } : candidate,
        ),
      }),
    ).rejects.toThrow(/authenticated/u);
    await expect(
      persistWatcherRollbackDurableObservations({ authority, entries: [] }),
    ).rejects.toThrow(/authenticated/u);
    await expect(
      persistWatcherRollbackDurableObservations({
        authority,
        entries: [entries[0]!, entries[1]!, entries[0]!],
      }),
    ).rejects.toThrow(/repeats a block/u);
    expect(backend.writes).toBe(writes);
    expect(readWatcherRollbackDurableAuthority(authority)).toEqual(before);
  });
});
