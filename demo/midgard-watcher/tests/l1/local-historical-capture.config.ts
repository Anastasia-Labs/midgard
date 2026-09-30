import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";

export const GENESIS_BYTES = JSON.stringify({ networkMagic: 1 });

export const GENESIS = createHash("sha256").update(GENESIS_BYTES).digest("hex");

export const config = (NODE_CONFIG_PATH: string, GENESIS_CONFIG_PATH: string) =>
  Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: Object.freeze({
      source: Object.freeze({
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: Object.freeze({
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: NODE_CONFIG_PATH,
          genesisConfigPath: GENESIS_CONFIG_PATH,
          genesisIdentitySha256: GENESIS,
        }),
        queryServices: Object.freeze([
          Object.freeze({
            kind: "ogmios",
            identity: "local-ogmios",
            endpoint: "ws://127.0.0.1:1337",
          }),
          Object.freeze({
            kind: "kupo",
            identity: "local-kupo",
            endpoint: "http://127.0.0.1:1442",
          }),
        ]),
      }),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
      finality: Object.freeze({
        depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
        rollback: Object.freeze({
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
        }),
      }),
    }),
    da: Object.freeze({
      peers: Object.freeze([
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ]),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      }),
    }),
    proverWallet: Object.freeze({
      keySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      }),
    }),
    deadlines: Object.freeze({
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    }),
  });

// Metadata and bytes of the unchanged ordinary Conway fixture. The scalar
// provider predecessor below is test data, not another block encoded in it.
export const metadata = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});

export const target = Object.freeze({
  blockHash: metadata.blockHash,
  blockNo: metadata.blockNo,
  slot: metadata.slot,
  pointId: computeFraudProofRawL1PointId(metadata),
});

export const parent = {
  blockHash: metadata.prevHash,
  blockNo: (BigInt(metadata.blockNo) - 1n).toString(),
  slot: (BigInt(metadata.slot) - 1n).toString(),
};

export const earlier = {
  blockHash: "ee".repeat(32),
  slot: Number(metadata.slot) - 2,
};

export type ReferenceFixture = Readonly<{
  schemaVersion: string;
  provenance: Readonly<{
    dataNetwork: string;
    transactionDatasetSha256: string;
    predecessorDatasetSha256: string;
  }>;
  transactions: readonly Readonly<{
    txHash: string;
    transactionCbor: string;
    creatingPoint: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
    }>;
    predecessorPoint: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
    }>;
    creatingTransactionIndex: number;
  }>[];
}>;

export const delay = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

export type Log = {
  kind: string;
  query: number;
  value?: Record<string, unknown>;
};

export type FixtureOptions = Readonly<{
  modes?: readonly string[];
  tipOffsets?: readonly number[];
  secondBlockType?: string;
  recheckChangesHead?: boolean | "always";
  reverseTransactions?: boolean;
  pendingHttp?: boolean;
  socketMode?: "opening" | "request";
  onLastInitialCheckpoint?: () => void;
  referenceMode?:
    | "missing"
    | "wrong_frame"
    | "wrong_index"
    | "head_changed"
    | "wait";
}>;
