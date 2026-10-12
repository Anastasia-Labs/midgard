import { createServer } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { afterEach } from "vitest";

import { DaLibp2pAttestationExchange } from "../src/da/libp2p/attestations.js";
import { DaLibp2pNode } from "../src/da/libp2p/DaLibp2pNode.js";
import type {
  DaStoredPayloadRootSet,
  Header,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { type PostgresCommitteeStore } from "../src/store/postgres.js";
import {
  openTestCommitteeStore,
  saveHealthyL1SourceState,
} from "./helpers/committee-store.js";

// The request deadline the live committee runs with. Every pull must finish
// far inside it; a response stream the server never closes ends only there.
export const REQUEST_TIMEOUT_MS = 5_000;

export const PROMPT_RESPONSE_MS = 2_000;

// Twenty pulls on one connection exceed this several times over, so any
// stream left half-open would exhaust the muxer.
export const MAX_STREAMS_PER_PEER = 8;

export const runningNodes: DaLibp2pNode[] = [];

const openedStores: PostgresCommitteeStore[] = [];

afterEach(async () => {
  await Promise.all(runningNodes.splice(0).map((node) => node.stop()));
  await Promise.all(openedStores.splice(0).map((store) => store.close()));
});

export type MemberOptions = {
  readonly ingestGossip: boolean;
  readonly serveAttestations: boolean;
};

export type RunningMember = {
  readonly peerId: string;
  readonly node: DaLibp2pNode;
  readonly exchange: DaLibp2pAttestationExchange;
  readonly gossipErrors: unknown[];
};

/**
 * Opens `count` fresh stores. A store a test writes signatures to directly
 * gets the durable source state a node saves at startup; a store a
 * `CommitteeService` will initialize (`serviceOwned`) is left for it to save.
 */
export const openStores = async (
  count: number,
  { serviceOwned = false }: { readonly serviceOwned?: boolean } = {},
): Promise<PostgresCommitteeStore[]> => {
  const stores = await Promise.all(
    Array.from({ length: count }, async () => {
      const store = await openTestCommitteeStore();
      return serviceOwned ? store : saveHealthyL1SourceState(store);
    }),
  );
  openedStores.push(...stores);
  return stores;
};

export type GossipSubscribers = {
  readonly pubsub: {
    getSubscribers(topic: string): readonly { toString(): string }[];
  };
};

export const stateQueueRecord = ({
  deploymentFingerprint,
  headerHash,
  header,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly header: Header;
}): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: "state-queue#0",
  blockAssetName: `block-${headerHash}`,
  header,
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "aa".repeat(32),
    depth: 10,
    providerSource: "fixture",
  },
  finalized: true,
  status: "unattested",
  validationErrors: [],
  updatedAt: new Date().toISOString(),
});

export const rootSummaryFromHeader = (
  header: Header,
): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

export const reserveLoopbackPort = (): Promise<number> =>
  new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(new Error("failed to reserve loopback port"));
        return;
      }
      const port = address.port;
      server.close((error) =>
        error === undefined ? resolve(port) : reject(error),
      );
    });
  });

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
