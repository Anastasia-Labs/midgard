import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
} from "@al-ft/midgard-fault-proofs";
import { beforeEach, vi } from "vitest";

import { createWatcherStateQueueObservationSource } from "../../src/indexers/authenticated-state-queue-observation.js";
import { createWatcherStateQueueReadScopes } from "../../src/indexers/authenticated-state-queue-observation.read-scopes.js";
import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { config } from "../l1/native-chain-sync.config.js";
import { fixture } from "./authenticated-state-queue-observation.fixture.js";

const upstream = vi.hoisted(() => ({ live: true, kupo: {}, ogmios: {} }));
vi.mock("@al-ft/midgard-fault-proofs", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-fault-proofs")>()),
  readAdmittedLocalKupmiosBoundary: vi.fn(),
  readAdmittedLocalKupmiosRawBlockAtPoint: vi.fn(),
  readAdmittedLocalKupmiosRawTransaction: vi.fn(),
}));
vi.mock(
  "../../src/l1/local-kupmios-native-observation.js",
  async (original) => ({
    ...(await original<
      typeof import("../../src/l1/local-kupmios-native-observation.js")
    >()),
    assertWatcherLocalKupmiosNativeObservation: () => {
      if (!upstream.live) throw new Error("native admission revoked");
    },
  }),
);
vi.mock("../../src/l1/l1-adapter.js", async (original) => ({
  ...(await original<typeof import("../../src/l1/l1-adapter.js")>()),
  watcherL1TransportAttestationDetails: (context: object) => {
    if (!upstream.live) return null;
    if (context === upstream.kupo)
      return {
        provider: { source: { sourceMode: "local_node", surface: "kupo" } },
        transportEndpoint: "http://127.0.0.1:1442",
      };
    if (context === upstream.ogmios)
      return {
        provider: { source: { sourceMode: "local_node", surface: "ogmios" } },
        transportEndpoint: "ws://127.0.0.1:1337",
      };
    return null;
  },
}));
export const io = {
  boundary: vi.mocked(readAdmittedLocalKupmiosBoundary),
  block: vi.mocked(readAdmittedLocalKupmiosRawBlockAtPoint),
  transaction: vi.mocked(readAdmittedLocalKupmiosRawTransaction),
};
beforeEach(() => {
  upstream.live = true;
  io.boundary.mockReset();
  io.block.mockReset();
  io.transaction.mockReset();
});
export const deferred = <T = void>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((yes) => {
    resolve = yes;
  });
  return { promise, resolve };
};
export const nextTurn = async () => {
  for (let i = 0; i < 30; i++) await Promise.resolve();
};
export const setup = (requestTimeoutMs = 5000) => {
  const initial = fixture();
  const base = config();
  const watcherConfig = parseWatcherConfig({
    ...base,
    da: {
      ...base.da,
      peers: [
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ],
    },
    l1: {
      ...base.l1,
      requestTimeoutMs,
      finality: {
        ...base.l1.finality,
        depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
        },
      },
    },
  });
  const deploymentIdentity = initial.deployment.result;
  const scopes = createWatcherStateQueueReadScopes({
    watcherConfig,
    deploymentIdentity,
  });
  const rawSource = createWatcherLocalKupmiosRawSource({
    watcherConfig,
    deploymentIdentity,
  });
  const includedSource = createWatcherLocalKupmiosRawSource({
    watcherConfig,
    deploymentIdentity,
    observationDepth: "inclusion",
  });
  const localObservation = {
    ...initial.localObservation,
    transportAttestations: [upstream.kupo, upstream.ogmios],
  } as unknown as WatcherLocalKupmiosNativeObservation;
  const rawBlock = {
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: "fixture",
    point: initial.raw.inclusionPoint,
    parentBlockHash: initial.nativeBlock.prevHash,
    kupoCheckpoint: {
      slot: Number(initial.nativeBlock.slot),
      blockHash: initial.nativeBlock.blockHash,
    },
    transactions: initial.nativeBlock.transactionIds.map((txHash, index) => ({
      txHash,
      transactionCbor: initial.nativeBlock.transactionCbors[index]!,
    })),
  };
  io.boundary.mockResolvedValue({
    kupoCheckpoint: initial.raw.inclusionPoint,
    ogmiosTip: initial.raw.inclusionPoint,
    confirmationDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  });
  io.block.mockResolvedValue(rawBlock);
  io.transaction.mockResolvedValue(initial.raw);
  const source = createWatcherStateQueueObservationSource({
    deploymentIdentity,
    rawSource,
    inclusionRawSource: includedSource,
    readScopes: scopes,
  });
  return {
    source,
    scopes,
    rawSource,
    includedSource,
    deploymentIdentity,
    rawBlock,
    initial,
    input: {
      nativeBlock: initial.nativeBlock,
      localObservation,
      previous: null,
    },
  };
};
