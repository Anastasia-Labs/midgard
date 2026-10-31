import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "midgard-watcher";
import { describe, expect, it } from "vitest";

import {
  lazyReplayedLedger,
  stackJourneyReplayWatcherConfig,
} from "./stack-replayed-ledger.js";

const stackWatcherConfig = parseWatcherConfig({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      chainSync: {
        kind: "cardano_node_socket",
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/config.json",
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: "11".repeat(32),
      },
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: { depth: 30 },
  },
  da: {
    peers: [
      {
        identity: "committee",
        multiaddr:
          "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_TEST_ROLLBACK_KEY",
    },
  },
  proverWallet: {
    keySource: { kind: "environment", variable: "MIDGARD_TEST_PROVER_KEY" },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

describe("the attached stack journey's replay configuration", () => {
  it("replays as the stack's watcher does, through the host's end of the node socket", () => {
    const replay = stackJourneyReplayWatcherConfig(
      stackWatcherConfig,
      "/home/run/stack/node.socket",
    );

    expect(replay.l1.source.chainSync.socketPath).toBe(
      "/home/run/stack/node.socket",
    );
    expect({
      ...replay,
      l1: {
        ...replay.l1,
        source: {
          ...replay.l1.source,
          chainSync: {
            ...replay.l1.source.chainSync,
            socketPath: stackWatcherConfig.l1.source.chainSync.socketPath,
          },
        },
      },
    }).toEqual(stackWatcherConfig);
  });
});

describe("a replayed ledger opened on its first read", () => {
  const replay = () => {
    const events: string[] = [];
    const ledger = lazyReplayedLedger(async () => {
      events.push("open");
      return {
        ledgerAt: async (headerHash) => {
          events.push(`read ${headerHash}`);
          return [];
        },
        close: async () => {
          events.push("close");
        },
      };
    });
    return { events, ledger };
  };

  it("opens nothing for a session that reads nothing", async () => {
    const { events, ledger } = replay();
    await ledger.close();
    expect(events).toEqual([]);
  });

  it("opens once across reads and closes what it opened", async () => {
    const { events, ledger } = replay();
    await Promise.all([ledger.ledgerAt("aa"), ledger.ledgerAt("bb")]);
    await ledger.ledgerAt("cc");
    await ledger.close();
    expect(events).toEqual(["open", "read aa", "read bb", "read cc", "close"]);
  });

  it("refuses a read after it is closed", async () => {
    const { events, ledger } = replay();
    await ledger.close();
    await expect(ledger.ledgerAt("aa")).rejects.toThrow(/closed/u);
    expect(events).toEqual([]);
  });

  it("opens again after a failed open, and closes quietly when nothing opened", async () => {
    const events: string[] = [];
    let attempts = 0;
    const ledger = lazyReplayedLedger(async () => {
      attempts += 1;
      events.push(`open ${attempts.toString()}`);
      if (attempts === 1) throw new Error("follower not reachable");
      return {
        ledgerAt: async () => [],
        close: async () => {
          events.push("close");
        },
      };
    });
    await expect(ledger.ledgerAt("aa")).rejects.toThrow(
      "follower not reachable",
    );
    await ledger.ledgerAt("bb");
    await ledger.close();
    expect(events).toEqual(["open 1", "open 2", "close"]);

    const never = lazyReplayedLedger(() =>
      Promise.reject(new Error("follower not reachable")),
    );
    await expect(never.ledgerAt("aa")).rejects.toThrow(
      "follower not reachable",
    );
    await expect(never.close()).resolves.toBeUndefined();
  });
});
