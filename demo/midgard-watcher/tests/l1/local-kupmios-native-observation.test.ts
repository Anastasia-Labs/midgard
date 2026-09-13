import { readFile } from "node:fs/promises";

import {
  computeFraudProofRawL1PointId,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosExactPointNotCanonicalError,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosPredecessorPoint,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  assertWatcherLocalKupmiosNativeObservation,
  captureExactBlockWithKupoLag,
  unsafeAssertNativeKupmiosAgreementForTest,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncRollForward,
} from "../../src/l1/native-chain-sync.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const captureWatcherConfig = {
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
        nodeConfigPath: "/etc/cardano/node-config.json",
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: "66".repeat(32),
      },
      queryServices: [
        {
          kind: "ogmios",
          identity: "local-ogmios",
          endpoint: "ws://127.0.0.1:1337",
        },
        {
          kind: "kupo",
          identity: "local-kupo",
          endpoint: "http://127.0.0.1:1442",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth: 30,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 30,
      },
    },
  },
  da: {
    peers: [
      {
        identity: "da-peer-a",
        multiaddr:
          "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
    },
  },
  proverWallet: {
    keySource: { kind: "environment", variable: "MIDGARD_WATCHER_PROVER_KEY" },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
};

let captureDeployment:
  | ReturnType<typeof makeWatcherDeploymentAuthorityFixture>
  | undefined;
const rawSource = (
  captureBounds?: Parameters<
    typeof createWatcherLocalKupmiosRawSource
  >[0]["captureBounds"],
) => {
  // Existing signed unit fixture only; these resource checks mint no native or
  // historical observation and make no genuine deployment/release claim.
  captureDeployment ??= makeWatcherDeploymentAuthorityFixture();
  return createWatcherLocalKupmiosRawSource({
    watcherConfig: captureWatcherConfig,
    deploymentIdentity: captureDeployment.result,
    ...(captureBounds === undefined ? {} : { captureBounds }),
  });
};

// Exercise HTTP acquisition directly; readBoundary begins with Ogmios
// chain-sync to obtain an atomic tip and its height.
const readHttpPredecessor = (source: ReturnType<typeof rawSource>) => {
  const point = { blockHash: "11".repeat(32), blockNo: "1", slot: "2" };
  return readAdmittedLocalKupmiosPredecessorPoint({
    source,
    point: { ...point, pointId: computeFraudProofRawL1PointId(point) },
  });
};

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

describe("concrete raw-source operational capture bounds", () => {
  it("validates resource bounds without changing the configured default", () => {
    expect(() => rawSource()).not.toThrow();
    for (const bounds of [
      { timeoutMs: 0 },
      { timeoutMs: 10_001 },
      { timeoutMs: 1.5 },
      { blockScanLimit: 0 },
      { blockScanLimit: 2_001 },
      { maxResponseBytes: 0 },
      { maxResponseBytes: 67_108_865 },
    ])
      expect(() => rawSource(bounds)).toThrow("operational bound");
    expect(() =>
      rawSource({
        signal: Object.create(AbortSignal.prototype) as AbortSignal,
      }),
    ).toThrow("platform AbortSignal");
  });

  it("forwards the actual signal and response cap to concrete HTTP acquisition", async () => {
    const fetcher = vi.fn(async () => new Response("{}"));
    vi.stubGlobal("fetch", fetcher);
    const source = rawSource({
      maxResponseBytes: 1,
      blockScanLimit: 1,
      timeoutMs: 100,
    });
    await expect(readHttpPredecessor(source)).rejects.toThrow(
      "exceeds the raw-source byte bound",
    );
    expect(fetcher).toHaveBeenCalledOnce();

    const controller = new AbortController();
    let started!: () => void;
    const ready = new Promise<void>((resolve) => {
      started = resolve;
    });
    let requestSignal: AbortSignal | null | undefined;
    vi.stubGlobal(
      "fetch",
      vi.fn(async (_url: string, init?: RequestInit) => {
        requestSignal = init?.signal;
        started();
        return await new Promise<Response>((_resolve, reject) =>
          requestSignal!.addEventListener(
            "abort",
            () => reject(requestSignal!.reason),
            { once: true },
          ),
        );
      }),
    );
    const cancelled = rawSource({ signal: controller.signal, timeoutMs: 500 });
    const outcome = readHttpPredecessor(cancelled).catch(
      (error: unknown) => error,
    );
    await ready;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(requestSignal?.aborted).toBe(true);
    await expect(readHttpPredecessor(cancelled)).rejects.toThrow("aborted");
  });

  it("uses the smaller supplied timeout and clears it after cancellation", async () => {
    vi.useFakeTimers();
    vi.stubGlobal(
      "fetch",
      vi.fn(
        async (_url: string, init?: RequestInit) =>
          await new Promise<Response>((_resolve, reject) =>
            init!.signal!.addEventListener(
              "abort",
              () => reject(init!.signal!.reason),
              { once: true },
            ),
          ),
      ),
    );
    const outcome = readHttpPredecessor(rawSource({ timeoutMs: 5 })).catch(
      (error: unknown) => error,
    );
    await vi.advanceTimersByTimeAsync(5);
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(vi.getTimerCount()).toBe(0);
  });
});

const METADATA = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});

const fixture = async () => {
  const event: WatcherNativeChainSyncRollForward = Object.freeze({
    ...METADATA,
    kind: "roll_forward",
    rawBlockCbor: (
      await readFile(
        new URL("../support/conway-block.hex", import.meta.url),
        "utf8",
      )
    ).trim(),
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    tip: Object.freeze({
      blockHash: METADATA.blockHash,
      blockNo: METADATA.blockNo,
      kind: "point" as const,
      slot: METADATA.slot,
    }),
  });
  const native = admitWatcherNativeRollForwardBlock(event);
  const point = Object.freeze({
    blockHash: native.blockHash,
    blockNo: native.blockNo,
    slot: native.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: native.blockHash,
      blockNo: native.blockNo,
      slot: native.slot,
    }),
  });
  const raw: LocalKupmiosRawBlockAtPoint = Object.freeze({
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: "fixture",
    point,
    parentBlockHash: native.prevHash.length === 0 ? null : native.prevHash,
    kupoCheckpoint: Object.freeze({
      slot: Number(native.slot),
      blockHash: native.blockHash,
    }),
    transactions: Object.freeze(
      native.transactionIds.map((txHash, index) =>
        Object.freeze({
          txHash,
          transactionCbor: native.transactionCbors[index]!,
        }),
      ),
    ),
  });
  return { native, raw };
};

describe("native Kupo/Ogmios agreement", () => {
  it("rejects a structural local observation that the live source did not admit", async () => {
    const { native } = await fixture();
    expect(() =>
      assertWatcherLocalKupmiosNativeObservation(
        Object.freeze({}) as WatcherLocalKupmiosNativeObservation,
        native,
      ),
    ).toThrow("is not admitted for the native block");
  });

  it("accepts one exact point and ordered transaction-CBOR vector", async () => {
    const { native, raw } = await fixture();
    expect(() =>
      unsafeAssertNativeKupmiosAgreementForTest(native, raw),
    ).not.toThrow();
  });

  it.each(["point", "checkpoint", "tx_hash", "tx_cbor", "tx_order"] as const)(
    "rejects hostile %s substitution",
    async (attack) => {
      const { native, raw } = await fixture();
      const transactions = raw.transactions.map((transaction) => ({
        ...transaction,
      }));
      let candidate: LocalKupmiosRawBlockAtPoint;
      if (attack === "point") {
        candidate = {
          ...raw,
          point: { ...raw.point, blockNo: "12069666" },
        };
      } else if (attack === "checkpoint") {
        candidate = {
          ...raw,
          kupoCheckpoint: { ...raw.kupoCheckpoint, slot: 159835208 },
        };
      } else {
        if (attack === "tx_hash") transactions[0]!.txHash = "00".repeat(32);
        if (attack === "tx_cbor") transactions[0]!.transactionCbor = "80";
        if (attack === "tx_order") {
          [transactions[0], transactions[1]] = [
            transactions[1]!,
            transactions[0]!,
          ];
        }
        candidate = { ...raw, transactions };
      }
      expect(() =>
        unsafeAssertNativeKupmiosAgreementForTest(native, candidate),
      ).toThrow(
        "local Kupo/Ogmios observation differs from the native chain-sync block",
      );
    },
  );
});

describe("exact block capture under Kupo indexing lag", () => {
  const lag = () =>
    new LocalKupmiosExactPointNotCanonicalError(
      "Kupo exact checkpoint does not contain the requested block",
      { requestedSlot: 1_000, checkpointSlot: 980, kupoHeadSlot: 980 },
    );
  // Kupo has a block at the requested slot and it is another block: a real
  // divergence. A checkpoint still before the slot is lag even when Kupo's
  // advertised head already passed it (the head header and the checkpoint
  // query are separate reads and race while Kupo applies a block).
  const divergence = () =>
    new LocalKupmiosExactPointNotCanonicalError(
      "Kupo exact checkpoint does not contain the requested block",
      { requestedSlot: 1_000, checkpointSlot: 1_000, kupoHeadSlot: 1_200 },
    );

  it("waits for Kupo to reach the requested slot with a fresh source each poll", async () => {
    const outcomes = [lag(), lag(), "block"] as const;
    let reads = 0;
    const recreated: number[] = [];
    const slept: number[] = [];
    let clock = 0;
    const raw = await captureExactBlockWithKupoLag({
      read: async () => {
        const outcome = outcomes[reads]!;
        reads += 1;
        if (outcome instanceof Error) throw outcome;
        return outcome;
      },
      recreateSource: () => recreated.push(reads),
      isClosed: () => false,
      lagBudgetMs: 10_000,
      pollMs: 2_000,
      sleep: async (ms) => {
        slept.push(ms);
        clock += ms;
      },
      now: () => clock,
    });
    expect(raw).toBe("block");
    expect(slept).toEqual([2_000, 2_000]);
    expect(recreated).toEqual([1, 2]);
  });

  it("fails closed once the lag budget is spent or when Kupo is not behind", async () => {
    let clock = 0;
    await expect(
      captureExactBlockWithKupoLag({
        read: async () => {
          throw lag();
        },
        recreateSource: () => undefined,
        isClosed: () => false,
        lagBudgetMs: 5_000,
        pollMs: 2_000,
        sleep: async (ms) => {
          clock += ms;
        },
        now: () => clock,
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(clock).toBe(4_000);
    const recreate = vi.fn();
    await expect(
      captureExactBlockWithKupoLag({
        read: async () => {
          throw divergence();
        },
        recreateSource: recreate,
        isClosed: () => false,
        lagBudgetMs: 60_000,
        sleep: async () => {
          throw new Error("must not wait on a divergence");
        },
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(recreate).not.toHaveBeenCalled();
  });

  it("still bounds moving-head retries independently of lag waits", async () => {
    let reads = 0;
    await expect(
      captureExactBlockWithKupoLag({
        read: async () => {
          reads += 1;
          throw new LocalKupmiosCheckpointChangedError("head moved");
        },
        recreateSource: () => undefined,
        isClosed: () => false,
        lagBudgetMs: 60_000,
        sleep: async () => undefined,
      }),
    ).rejects.toThrow("head moved");
    expect(reads).toBe(3);
  });
});
