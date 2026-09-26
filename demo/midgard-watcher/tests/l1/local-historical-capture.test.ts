import { createHash } from "node:crypto";
import { chmod, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofRawL1PointId,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
} from "@al-ft/midgard-fault-proofs";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  admitWatcherLocalBackfillUserEventReferenceEvidence,
  admitWatcherUserEventReferenceEvidence,
  createWatcherLocalBackfillUserEventReferenceAuthority,
  readWatcherUserEventReferenceEvidence,
} from "../../src/indexers/user-event-reference-authority.js";
import {
  admitWatcherLocalBackfillFinality,
  readWatcherLocalBackfillFinality,
  readWatcherLocalBackfillFinalityObservation,
  readWatcherLocalBackfillFinalityOriginalWitness,
} from "../../src/l1/finality-engine.js";
import {
  admitWatcherLocalBackfillObservation,
  normalizeWatcherL1Block,
  readWatcherLocalBackfillObservation,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
} from "../../src/l1/l1-adapter.js";
import {
  openWatcherLocalHistoricalCapture,
  readWatcherLocalHistoricalCapture,
} from "../../src/l1/local-historical-capture.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import {
  evaluateWatcherLocalBackfillConsistency,
  evaluateWatcherMultiProviderConsistency,
} from "../../src/l1/multi-provider-consistency.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/config.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const GENESIS_BYTES = JSON.stringify({ networkMagic: 1 });
const GENESIS = createHash("sha256").update(GENESIS_BYTES).digest("hex");
const config = (NODE_CONFIG_PATH: string, GENESIS_CONFIG_PATH: string) =>
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
const metadata = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});
const target = Object.freeze({
  blockHash: metadata.blockHash,
  blockNo: metadata.blockNo,
  slot: metadata.slot,
  pointId: computeFraudProofRawL1PointId(metadata),
});
const parent = {
  blockHash: metadata.prevHash,
  blockNo: (BigInt(metadata.blockNo) - 1n).toString(),
  slot: (BigInt(metadata.slot) - 1n).toString(),
};
const earlier = { blockHash: "ee".repeat(32), slot: Number(metadata.slot) - 2 };
const rawPath = fileURLToPath(
  new URL("../support/conway-block.hex", import.meta.url),
);
type ReferenceFixture = Readonly<{
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
let referenceFixture: ReferenceFixture;
let rawBlockCbor: string;
let ordinaryBlock: ReturnType<typeof admitWatcherNativeRollForwardBlock>;
let deployment: Awaited<
  ReturnType<typeof makeWatcherDeploymentAuthorityFixture>
>;
beforeAll(async () => {
  referenceFixture = JSON.parse(
    await readFile(
      new URL("../support/conway-reference-transactions.json", import.meta.url),
      "utf8",
    ),
  ) as ReferenceFixture;
  rawBlockCbor = (await readFile(rawPath, "utf8")).trim();
  ordinaryBlock = admitWatcherNativeRollForwardBlock({
    ...metadata,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_forward",
    rawBlockCbor,
    tip: {
      kind: "point",
      blockHash: metadata.blockHash,
      blockNo: metadata.blockNo,
      slot: metadata.slot,
    },
  });
  deployment = makeWatcherDeploymentAuthorityFixture();
}, 60_000);
const cleanup: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of cleanup.splice(0).reverse()) await close();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});
const delay = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));
type Log = { kind: string; query: number; value?: Record<string, unknown> };
type FixtureOptions = Readonly<{
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
const fixture = async (options: FixtureOptions = {}) => {
  const dir = await mkdtemp(join("/var/tmp", "local-historical-capture-"));
  cleanup.push(() => rm(dir, { recursive: true, force: true }));
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  const binaryPath = join(dir, "helper");
  const logPath = join(dir, "events.jsonl");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  await writeFile(logPath, "");
  // The existing executable fixture still owns ready/stdin/stdout/lifecycle.
  // Only its ordinary payload is adapted to existing Conway bytes, without
  // mocking the supervisor, query receipt, native admission or source factory.
  await writeFile(
    binaryPath,
    `#!${process.execPath}
import { appendFileSync, readFileSync } from "node:fs";
const logPath = ${JSON.stringify(logPath)};
const records = readFileSync(logPath, "utf8").trim().split("\\n").filter(Boolean).map(JSON.parse);
const query = records.filter(x => x.kind === "start").length + 1;
const log = (kind, value) => appendFileSync(logPath, JSON.stringify({kind, query, ...(value === undefined ? {} : {value})}) + "\\n");
const previousPid = records.findLast(x => x.kind === "start")?.value?.pid;
let previousProcessAlive = false;
if (previousPid !== undefined) {
  try { process.kill(previousPid, 0); previousProcessAlive = true; }
  catch (error) { if (error.code !== "ESRCH") throw error; }
}
log("start", {pid: process.pid, previousProcessAlive});
process.once("exit", () => log("stop"));
const modes = ${JSON.stringify(options.modes ?? ["query_idle", "query_idle"])};
process.argv[2] = modes[(query - 1) % modes.length];
const metadata = ${JSON.stringify(metadata)};
const raw = readFileSync(${JSON.stringify(rawPath)}, "utf8").trim();
const canonical = value => {
  if (Array.isArray(value)) return value.map(canonical);
  if (value !== null && typeof value === "object") return Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])]));
  return value;
};
const output = process.stdout.write.bind(process.stdout);
process.stdout.write = (chunk, ...args) => {
  const value = JSON.parse(chunk.toString());
  if (value.kind === "ready") value.currentTip = {kind:"point", blockHash: "77".repeat(32), blockNo: (BigInt(metadata.blockNo) + 1n).toString(), slot: (BigInt(metadata.slot) + 1n).toString()};
  if (value.kind === "roll_forward") {
    Object.assign(value, metadata, {rawBlockCbor: raw});
    if (query % 2 === 0 && ${JSON.stringify(options.secondBlockType ?? null)} !== null) value.blockType = ${JSON.stringify(options.secondBlockType ?? null)};
    const offsets = ${JSON.stringify(options.tipOffsets ?? [3000, 5000])};
    const depthOffset = BigInt(offsets[(query - 1) % offsets.length]);
    value.tip = {kind:"point", blockHash: (query % 2 === 0 ? "99" : "88").repeat(32), blockNo: (BigInt(metadata.blockNo) + depthOffset).toString(), slot: (BigInt(metadata.slot) + depthOffset).toString()};
  }
  log(value.kind, value);
  return output(JSON.stringify(canonical(value)) + "\\n", ...args);
};
await import(${JSON.stringify(new URL("../support/native-chain-sync-fixture.mjs", import.meta.url).href)});
`,
  );
  await chmod(binaryPath, 0o700);
  const logs = async (): Promise<Log[]> =>
    (await readFile(logPath, "utf8"))
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line) as Log);
  const referenceReads: { outRef: string; nativeQueryStarts: number }[] = [];
  const creatingLookups: string[] = [];
  const sockets: BoundarySocket[] = [];
  let head = "55".repeat(32);
  let targetReads = 0;
  const requests: { url: string; signal: AbortSignal | null | undefined }[] =
    [];
  class BoundarySocket extends EventTarget {
    intersection: { slot: number; id: string } = {
      slot: Number(parent.slot),
      id: parent.blockHash,
    };
    closed = false;
    constructor() {
      super();
      sockets.push(this);
      if (options.socketMode !== "opening")
        queueMicrotask(() => this.dispatchEvent(new Event("open")));
    }
    send(text: string): void {
      if (options.socketMode === "request") return;
      const request = JSON.parse(text) as {
        id: number;
        method: string;
        params: { points?: { slot: number; id: string }[] };
      };
      let result: unknown;
      if (request.method === "findIntersection") {
        this.intersection = request.params.points![0]!;
        result = { intersection: this.intersection };
      } else {
        const creating = referenceFixture.transactions.filter(
          ({ predecessorPoint }) =>
            predecessorPoint.blockHash === this.intersection.id,
        );
        if (creating.length > 0) {
          const first = creating[0]!;
          creatingLookups.push(first.creatingPoint.blockHash);
          result = {
            direction: "forward",
            block: {
              id: first.creatingPoint.blockHash,
              slot: Number(first.creatingPoint.slot),
              height: Number(first.creatingPoint.blockNo),
              ancestor: first.predecessorPoint.blockHash,
              // These are requested creating-transaction responses, not a claim
              // of a complete native creating block or a synthetic native frame.
              transactions: creating.map((transaction) => ({
                id: transaction.txHash,
                cbor:
                  options.referenceMode === "wrong_frame"
                    ? referenceFixture.transactions.find(
                        ({ txHash }) => txHash !== transaction.txHash,
                      )!.transactionCbor
                    : transaction.transactionCbor,
              })),
            },
          };
        } else {
          const child = this.intersection.id === parent.blockHash;
          const transactions = ordinaryBlock.transactionIds.map(
            (id, index) => ({
              id,
              cbor: ordinaryBlock.transactionCbors[index]!,
            }),
          );
          result = {
            direction: "forward",
            block: {
              id: child ? metadata.blockHash : parent.blockHash,
              slot: child ? Number(metadata.slot) : Number(parent.slot),
              height: child ? Number(metadata.blockNo) : Number(parent.blockNo),
              ancestor: child ? parent.blockHash : earlier.blockHash,
              transactions: child
                ? options.reverseTransactions
                  ? transactions.reverse()
                  : transactions
                : [],
            },
          };
        }
      }
      queueMicrotask(() =>
        this.dispatchEvent(
          new MessageEvent("message", {
            data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
          }),
        ),
      );
    }
    close(): void {
      if (this.closed) return;
      this.closed = true;
      // The source waits for the physical close event before releasing the
      // session, exactly as a real socket reports it.
      queueMicrotask(() =>
        this.dispatchEvent(
          Object.assign(new Event("close"), { code: 1000, wasClean: true }),
        ),
      );
    }
  }
  vi.stubGlobal("WebSocket", BoundarySocket);
  vi.stubGlobal(
    "fetch",
    vi.fn(async (url: string, init?: RequestInit) => {
      requests.push({ url, signal: init?.signal });
      if (options.pendingHttp)
        return await new Promise<Response>((_resolve, reject) =>
          init!.signal!.addEventListener(
            "abort",
            () => reject(new Error("fixture HTTP aborted")),
            { once: true },
          ),
        );
      const path = new URL(url).pathname;
      if (path.startsWith("/matches/")) {
        const [rawIndex, txHash] = decodeURIComponent(
          path.slice("/matches/".length),
        ).split("@");
        referenceReads.push({
          outRef: `${txHash}#${rawIndex}`,
          nativeQueryStarts: (await logs()).filter(
            ({ kind }) => kind === "start",
          ).length,
        });
        if (options.referenceMode === "wait")
          return await new Promise<Response>((_resolve, reject) => {
            init!.signal!.addEventListener(
              "abort",
              () => reject(new Error("reference HTTP aborted")),
              { once: true },
            );
          });
        if (options.referenceMode === "head_changed") head = "66".repeat(32);
        const source = referenceFixture.transactions.find(
          (transaction) => transaction.txHash === txHash,
        );
        if (source === undefined)
          throw new Error("fixture has no imported creating transaction");
        const transaction = CML.Transaction.from_cbor_hex(
          source.transactionCbor,
        );
        const body = transaction.body();
        const outputs = body.outputs();
        const outputIndex = Number(rawIndex);
        const output = outputs.get(outputIndex);
        const address = output.address();
        const datum = output.datum();
        const inlineDatum = datum?.as_datum();
        const datumHash =
          inlineDatum === undefined
            ? output.datum_hash()
            : CML.hash_plutus_data(inlineDatum);
        const script = output.script_ref();
        const scriptHash = script?.hash();
        try {
          const assets = coreToTxOutput(output).assets;
          const match = {
            transaction_index: source.creatingTransactionIndex,
            transaction_id: source.txHash,
            output_index:
              options.referenceMode === "wrong_index"
                ? outputIndex + 1
                : outputIndex,
            address: address.to_bech32(),
            value: {
              coins: assets.lovelace!.toString(),
              assets: Object.fromEntries(
                Object.entries(assets)
                  .filter(([unit]) => unit !== "lovelace")
                  .map(([unit, amount]) => [
                    `${unit.slice(0, 56)}.${unit.slice(56)}`,
                    amount.toString(),
                  ]),
              ),
            },
            datum_hash: datumHash?.to_hex() ?? null,
            ...(datumHash === undefined
              ? {}
              : { datum_type: inlineDatum === undefined ? "hash" : "inline" }),
            script_hash: scriptHash?.to_hex() ?? null,
            created_at: {
              slot_no: Number(source.creatingPoint.slot),
              header_hash: source.creatingPoint.blockHash,
            },
            spent_at: null,
            datum: inlineDatum?.to_canonical_cbor_hex() ?? null,
            script: script?.to_canonical_cbor_hex() ?? null,
          };
          return new Response(
            JSON.stringify(options.referenceMode === "missing" ? [] : [match]),
            {
              headers: {
                "X-Most-Recent-Checkpoint": "159845207",
                ETag: head,
              },
            },
          );
        } finally {
          scriptHash?.free();
          script?.free();
          datumHash?.free();
          inlineDatum?.free();
          datum?.free();
          address.free();
          output.free();
          outputs.free();
          body.free();
          transaction.free();
        }
      }
      const slot = Number(path.split("/").at(-1));
      const point = [
        target,
        parent,
        earlier,
        ...referenceFixture.transactions.flatMap(
          ({ creatingPoint, predecessorPoint }) => [
            creatingPoint,
            predecessorPoint,
          ],
        ),
      ]
        .sort((left, right) => Number(right.slot) - Number(left.slot))
        .find((candidate) => Number(candidate.slot) <= slot);
      if (point === undefined)
        throw new Error("fixture has no imported checkpoint");
      if (point === target) {
        targetReads += 1;
        if (targetReads === 5) options.onLastInitialCheckpoint?.();
      }
      if (
        options.recheckChangesHead &&
        (await logs()).some(({ kind }) => kind === "start")
      )
        head =
          options.recheckChangesHead === "always" && head === "66".repeat(32)
            ? "77".repeat(32)
            : "66".repeat(32);
      return new Response(
        JSON.stringify({
          slot_no: Number(point.slot),
          header_hash: point.blockHash,
        }),
        {
          headers: {
            "X-Most-Recent-Checkpoint": "159845207",
            ETag: head,
          },
        },
      );
    }),
  );
  const args = {
    watcherConfig: config(nodeConfig, genesisConfig),
    deploymentIdentity: deployment.result,
    nativeChainSyncBinaryPath: binaryPath,
    point: target,
    limits: { timeoutMs: 5_000 },
  };
  const open = async (
    overrides: Partial<
      Parameters<typeof openWatcherLocalHistoricalCapture>[0]
    > = {},
  ) => {
    const value = await openWatcherLocalHistoricalCapture({
      ...args,
      ...overrides,
    });
    cleanup.push(value.close);
    return value;
  };
  const waitFor = async (predicate: () => boolean | Promise<boolean>) => {
    const until = performance.now() + 2_000;
    while (!(await predicate())) {
      if (performance.now() > until)
        throw new Error("capture fixture wait timed out");
      await delay(5);
    }
  };
  return {
    args,
    open,
    logs,
    requests,
    sockets,
    referenceReads,
    creatingLookups,
    waitFor,
    changeHead: () => {
      head = "aa".repeat(32);
    },
  };
};

describe("owned local historical capture", () => {
  it.each([false, true])(
    "captures exact bytes with two sequential queries (admitted config: %s)",
    async (admitted) => {
      const f = await fixture();
      const capture = await f.open(
        admitted
          ? { watcherConfig: parseWatcherConfig(f.args.watcherConfig) }
          : {},
      );
      const value = readWatcherLocalHistoricalCapture(capture.receipt);
      expect(value.nativeBlock).toEqual(ordinaryBlock);
      expect(value.nativeBlock.rawBlockCbor).toBe(rawBlockCbor);
      expect(value.point).toEqual(target);
      expect(value.predecessorPoint).toMatchObject(parent);
      expect(value.depthAtObservedTip).toBe("5001");
      expect(value.observedNativeTip.blockHash).toBe("99".repeat(32));
      const records = await f.logs();
      expect(records.filter(({ kind }) => kind === "start")).toHaveLength(2);
      expect(
        records.findIndex(({ kind, query }) => kind === "stop" && query === 1),
      ).toBeLessThan(
        records.findIndex(({ kind, query }) => kind === "start" && query === 2),
      );
      expect(
        records.find(({ kind, query }) => kind === "start" && query === 2)
          ?.value?.previousProcessAlive,
      ).toBe(false);
      const delivered = records.find(
        ({ kind, query }) => kind === "roll_forward" && query === 2,
      )!.value;
      expect(value.targetEventDigest).toBe(
        createHash("sha256")
          .update(watcherCanonicalJson(delivered))
          .digest("hex"),
      );
      expect(value.sourceId).toContain(
        `:watcher-native-crosscheck/${deployment.result.manifestId}/watcher-node`,
      );
      expect(Object.keys(capture).sort()).toEqual(["close", "receipt"]);
      expect(Object.isFrozen(value.nativeBlock.transactionCbors)).toBe(true);
      expect(readWatcherLocalHistoricalCapture(capture.receipt)).toBe(value);
      expect(() =>
        readWatcherLocalHistoricalCapture({ ...capture.receipt }),
      ).toThrow("absent or stale");
      await capture.close();
      await capture.close();
      expect(() => readWatcherLocalHistoricalCapture(capture.receipt)).toThrow(
        /closed/,
      );
      expect(
        (await f.logs()).filter(({ kind }) => kind === "stop"),
      ).toHaveLength(2);
      expect(f.sockets.every(({ closed }) => closed)).toBe(true);
    },
  );

  it("uses a fresh concrete Kupo pin for each independent capture", async () => {
    const f = await fixture();
    const first = await f.open();
    await first.close();
    f.changeHead();
    const second = await f.open();
    expect(
      readWatcherLocalHistoricalCapture(second.receipt).depthAtObservedTip,
    ).toBe("5001");
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(4);
  });

  it("restarts a changed Kupo capture after closing its abandoned query", async () => {
    const f = await fixture({ recheckChangesHead: true });
    const capture = await f.open();
    expect(readWatcherLocalHistoricalCapture(capture.receipt).point).toEqual(
      target,
    );
    await capture.close();
    const records = await f.logs();
    expect(records.filter(({ kind }) => kind === "start")).toHaveLength(3);
    expect(records.filter(({ kind }) => kind === "stop")).toHaveLength(3);
    expect(
      records.findIndex(({ kind, query }) => kind === "stop" && query === 1),
    ).toBeLessThan(
      records.findIndex(({ kind, query }) => kind === "start" && query === 2),
    );
    expect(f.sockets.every(({ closed }) => closed)).toBe(true);
  });

  it("fails closed after bounded attempts when Kupo keeps changing", async () => {
    const f = await fixture({ recheckChangesHead: "always" });
    await expect(f.open()).rejects.toThrow(/Kupo (changed|advanced)/);
    const records = await f.logs();
    expect(
      records.filter(({ kind }) => kind === "start").length,
    ).toBeLessThanOrEqual(3);
    expect(records.filter(({ kind }) => kind === "stop")).toHaveLength(
      records.filter(({ kind }) => kind === "start").length,
    );
    expect(f.sockets.every(({ closed }) => closed)).toBe(true);
  });

  it("compares the complete ordered transaction vector", async () => {
    const f = await fixture({ reverseTransactions: true });
    await expect(f.open()).rejects.toThrow("blocks disagree");
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(1);
  });

  it("independently admits the second delivered event through CML", async () => {
    const f = await fixture({ secondBlockType: "8" });
    await expect(f.open()).rejects.toThrow("block admission failed");
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(2);
    expect((await f.logs()).filter(({ kind }) => kind === "stop")).toHaveLength(
      2,
    );
  });

  it.each(["no_ready", "query_wait"])(
    "cancels native acquisition during %s",
    async (mode) => {
      const f = await fixture({ modes: [mode] });
      const controller = new AbortController();
      const pending = f.open({ signal: controller.signal });
      const outcome = pending.catch((error: unknown) => error);
      await f.waitFor(async () =>
        (await f.logs()).some(
          ({ kind }) => kind === (mode === "no_ready" ? "start" : "ready"),
        ),
      );
      controller.abort();
      expect(await outcome).toMatchObject({
        message: expect.stringMatching(/cancel|aborted/),
      });
      const pid = (await f.logs()).find(({ kind }) => kind === "start")?.value
        ?.pid;
      expect(typeof pid).toBe("number");
      await f.waitFor(() => {
        try {
          process.kill(pid as number, 0);
          return false;
        } catch (error) {
          if ((error as NodeJS.ErrnoException).code !== "ESRCH") throw error;
          return true;
        }
      });
    },
  );

  it("cleans up both owners when the second query cannot deliver a target", async () => {
    const f = await fixture({ modes: ["query_idle", "query_wait"] });
    await expect(f.open({ limits: { timeoutMs: 600 } })).rejects.toThrow(
      /expired|cancel|aborted/,
    );
    await f.waitFor(
      async () =>
        (await f.logs()).filter(({ kind }) => kind === "stop").length === 2,
    );
  });

  it.each(["http", "opening", "request"] as const)(
    "aborts the actual pending %s transport",
    async (stage) => {
      const f = await fixture(
        stage === "http" ? { pendingHttp: true } : { socketMode: stage },
      );
      const controller = new AbortController();
      const pending = f.open({ signal: controller.signal });
      const outcome = expect(pending).rejects.toThrow(/abort|cancel/);
      await f.waitFor(() =>
        stage === "http" ? f.requests.length > 0 : f.sockets.length > 0,
      );
      controller.abort();
      await outcome;
      if (stage === "http") expect(f.requests[0]!.signal!.aborted).toBe(true);
      else expect(f.sockets[0]!.closed).toBe(true);
      expect(await f.logs()).toHaveLength(0);
    },
  );

  it("refuses rather than rounding up less than 100ms remaining", async () => {
    let started: number | undefined;
    let virtual: number | undefined;
    const now = performance.now.bind(performance);
    vi.spyOn(performance, "now").mockImplementation(() => {
      const value = virtual ?? now();
      started ??= value;
      return value;
    });
    const f = await fixture({
      onLastInitialCheckpoint: () => {
        virtual = started! + 5_000 - 50;
      },
    });
    await expect(f.open()).rejects.toThrow("less than 100ms");
    expect(await f.logs()).toHaveLength(0);
  });

  it("revokes on observed helper exit without treating an idle query as monitoring", async () => {
    const f = await fixture({ modes: ["query_idle", "query_exit"] });
    const capture = await f.open();
    await f.waitFor(async () =>
      (await f.logs()).some(
        ({ kind, query }) => kind === "stop" && query === 2,
      ),
    );
    await f.waitFor(() => {
      try {
        readWatcherLocalHistoricalCapture(capture.receipt);
        return false;
      } catch {
        return true;
      }
    });
    expect(() => readWatcherLocalHistoricalCapture(capture.receipt)).toThrow(
      "absent or stale",
    );
  });

  it("expires monotonically when wall time moves backward", async () => {
    const f = await fixture();
    const capture = await f.open({ limits: { timeoutMs: 600 } });
    vi.spyOn(Date, "now").mockReturnValue(0);
    await delay(650);
    expect(() => readWatcherLocalHistoricalCapture(capture.receipt)).toThrow(
      /expired|closed/,
    );
  });

  it("validates bounds, point, signal and deployment before acquisition", async () => {
    const f = await fixture();
    for (const limits of [
      { timeoutMs: 99 },
      { timeoutMs: 120_001 },
      { timeoutMs: 1.5 },
      { maxRawResponseBytes: 0 },
      { maxRawResponseBytes: 67_108_865 },
    ]) {
      await expect(f.open({ limits })).rejects.toThrow("operational bound");
    }
    await expect(
      f.open({ signal: Object.create(AbortSignal.prototype) as AbortSignal }),
    ).rejects.toThrow("platform AbortSignal");
    const controller = new AbortController();
    controller.abort();
    await expect(f.open({ signal: controller.signal })).rejects.toThrow();
    await expect(
      f.open({ point: { ...target, pointId: "00".repeat(32) } }),
    ).rejects.toThrow("pointId");
    await expect(
      f.open({ deploymentIdentity: { ...deployment.result } }),
    ).rejects.toThrow();
    await expect(
      f.open({
        limits: Object.assign({ timeoutMs: 500 }, { maxNativeEvents: 64 }),
      }),
    ).rejects.toThrow("unknown");
    expect(f.requests).toHaveLength(0);
    expect(await f.logs()).toHaveLength(0);
  });

  it("forwards raw response caps and preserves current live finality configuration", async () => {
    const f = await fixture();
    await expect(
      f.open({ limits: { maxRawResponseBytes: 1 } }),
    ).rejects.toThrow("byte bound");
    expect(await f.logs()).toHaveLength(0);
    expect(
      parseWatcherConfig(f.args.watcherConfig).l1.finality.rollback
        .postFinalityRecoveryMaxDepth,
    ).toBe(2160);
    await expect(
      f.open({
        watcherConfig: { ...f.args.watcherConfig, targetNetwork: "Preview" },
      }),
    ).rejects.toThrow();
  });
});

describe("receipt-only local backfill admission", () => {
  it("retains all eight independent native/Ogmios encodings and the original fractional capture start", async () => {
    const f = await fixture();
    const originalNow = performance.now.bind(performance);
    const clockSamples: number[] = [];
    vi.spyOn(performance, "now").mockImplementation(() => {
      const sample = Math.floor(originalNow()) + 0.125;
      clockSamples.push(sample);
      return sample;
    });
    const capture = await f.open();
    const captured = readWatcherLocalHistoricalCapture(capture.receipt);
    expect(captured.startedAtMonotonicMs).toBe(clockSamples[0]);
    expect(captured.startedAtMonotonicMs % 1).toBe(0.125);
    expect(captured.finalityConfig).toEqual(
      parseWatcherConfig(f.args.watcherConfig).l1.finality,
    );
    expect(captured.sourceBinding.queryServices).toEqual([
      {
        kind: "kupo",
        providerId: "local-kupo",
        endpoint: "http://127.0.0.1:1442",
        admittedSourceUrl: "http://127.0.0.1:1442",
      },
      {
        kind: "ogmios",
        providerId: "local-ogmios",
        endpoint: "ws://127.0.0.1:1337",
        admittedSourceUrl: "http://127.0.0.1:1337",
      },
    ]);
    const receipt = admitWatcherLocalBackfillObservation(capture.receipt);
    expect(admitWatcherLocalBackfillObservation(capture.receipt)).toBe(receipt);
    const observed = readWatcherLocalBackfillObservation(receipt);
    expect(observed.capture).toBe(captured);
    expect(observed.capture.startedAtMonotonicMs).toBe(clockSamples[0]);
    expect(observed.native).not.toBe(observed.ogmios);
    expect(observed.native.transactions).toHaveLength(8);
    expect(observed.ogmios.transactions).toHaveLength(8);
    for (const [index, raw] of ordinaryBlock.transactionCbors.entries()) {
      const decoded = CML.Transaction.from_cbor_hex(raw);
      const body = decoded.body();
      try {
        for (const block of [observed.native, observed.ogmios]) {
          const transaction = block.transactions[index]!;
          expect(transaction.transactionIndex).toBe(index.toString());
          expect(transaction.txHash).toBe(ordinaryBlock.transactionIds[index]);
          expect(transaction.fullTransaction.bytesHex).toBe(raw);
          expect(transaction.body.bytesHex).toBe(body.to_cbor_hex());
          expect(
            computeHash32(
              Buffer.from(transaction.body.bytesHex, "hex"),
            ).toString("hex"),
          ).toBe(transaction.txHash);
        }
        expect(captured.rawBlock.transactions[index]).toEqual({
          txHash: ordinaryBlock.transactionIds[index],
          transactionCbor: raw,
        });
      } finally {
        body.free();
        decoded.free();
      }
    }
    expect(observed.kupo.transactions).toHaveLength(0);
    expect(observed.kupo.chainPoint.pointDigest).toBe(
      observed.native.chainPoint.pointDigest,
    );
    expect(observed.ogmios.provider.authentication.kind).toBe(
      "local_capture_identity_v1",
    );
    expect(observed.native.provider.authentication).toEqual({
      kind: "cardano_node_genesis_v1",
      publicIdentitySha256: GENESIS,
    });
    const consistency = evaluateWatcherLocalBackfillConsistency(receipt);
    expect(consistency).toMatchObject({
      status: "agreed",
      protocolDecision: "allowed",
      observationCount: 3,
      independentProviderCount: 1,
      queryObservationCount: 2,
      agreement: { minimumDepth: "5001" },
    });
    for (const value of [
      captured.rawBlock,
      captured.rawBlock.point,
      captured.rawBlock.kupoCheckpoint,
      captured.rawBlock.transactions,
      ...captured.rawBlock.transactions,
      captured.finalityConfig,
      captured.finalityConfig.rollback,
      captured.sourceBinding,
      captured.sourceBinding.queryServices,
      ...captured.sourceBinding.queryServices,
    ])
      expect(Object.isFrozen(value)).toBe(true);
    for (const value of [capture.receipt, receipt, observed, observed.native])
      expect(watcherL1TransportAttestationDetails(value)).toBeNull();
    expect(() =>
      normalizeWatcherL1Block(
        receipt as unknown as WatcherL1TransportAttestationContext,
        observed.native,
      ),
    ).toThrow();
    expect(
      evaluateWatcherMultiProviderConsistency(
        captured.sourceBinding,
        [observed.native, observed.ogmios, observed.kupo],
        [],
      ).protocolDecision,
    ).toBe("quarantined");
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: receipt,
      previous: null,
    });
    expect(first.result).toMatchObject({
      action: "observe_pending",
      state: {
        pending: {
          firstSeenDepth: "5001",
          currentDepth: "5001",
          visibilityCount: "1",
        },
      },
    });
    expect(first.admitted).not.toBeNull();
    expect(
      readWatcherLocalBackfillFinality(first.admitted!).startedAtMonotonicMs,
    ).toBe(captured.startedAtMonotonicMs);
  });

  it("requires two actual captures and accepts only one deeper successor after a closed first capture", async () => {
    const f = await fixture({
      tipOffsets: [3000, 5000, 5000, 5001, 5001, 5002],
    });
    const originalNow = performance.now.bind(performance);
    vi.spyOn(performance, "now").mockImplementation(
      () => Math.floor(originalNow()) + 0.125,
    );
    const captureA = await f.open();
    const observationA = admitWatcherLocalBackfillObservation(captureA.receipt);
    const observedA = readWatcherLocalBackfillObservation(observationA);
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: observationA,
      previous: null,
    });
    expect(first.admitted).not.toBeNull();
    const firstRead = readWatcherLocalBackfillFinality(first.admitted!);
    expect(firstRead.step).toBe(1);
    expect(() =>
      readWatcherLocalBackfillFinalityOriginalWitness({
        finality: first.admitted!,
        observation: observationA,
      }),
    ).toThrow("requires a finalized pair");
    for (const previous of [null, first.admitted]) {
      const duplicate = admitWatcherLocalBackfillFinality({
        ...f.args,
        observation: observationA,
        previous,
      });
      expect(duplicate.result.action).toBe("duplicate");
      expect(duplicate.admitted).toBeNull();
    }
    await captureA.close();
    expect(() => readWatcherLocalBackfillObservation(observationA)).toThrow(
      /closed/,
    );
    expect(() => readWatcherLocalBackfillFinality(first.admitted!)).toThrow(
      /closed/,
    );
    const captureB = await f.open();
    const observationB = admitWatcherLocalBackfillObservation(captureB.receipt);
    expect(
      readWatcherLocalBackfillObservation(observationB).capture
        .startedAtMonotonicMs,
    ).toBeGreaterThan(firstRead.admittedAtMonotonicMs);
    const second = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: observationB,
      previous: first.admitted,
    });
    expect(second.result).toMatchObject({
      action: "finalize",
      protocolDecision: "finality_granted",
      state: {
        finalized: {
          firstSeenDepth: "5001",
          currentDepth: "5002",
          visibilityCount: "2",
        },
      },
    });
    expect(second.admitted).not.toBeNull();
    expect(readWatcherLocalBackfillFinality(second.admitted!).step).toBe(2);
    const currentPair = {
      finality: second.admitted!,
      observation: observationB,
    };
    const witness =
      readWatcherLocalBackfillFinalityOriginalWitness(currentPair);
    expect(witness.first.finality).toBe(firstRead);
    expect(witness.first.observation).toBe(observedA);
    expect(witness.current.finality).toBe(
      readWatcherLocalBackfillFinality(second.admitted!),
    );
    expect(witness.current.observation).toBe(
      readWatcherLocalBackfillObservation(observationB),
    );
    expect(witness.first.observation.capture).toBe(observedA.capture);
    expect(witness.first.observation.capture.creatingTransactionBodies).toBe(
      observedA.capture.creatingTransactionBodies,
    );
    expect(
      witness.first.observation.capture.creatingTransactionBodies,
    ).toHaveLength(6);
    expect(witness.first.finality.consistency.agreement?.minimumDepth).toBe(
      "5001",
    );
    expect(witness.current.finality.consistency.agreement?.minimumDepth).toBe(
      "5002",
    );
    expect(witness.first.finality.startedAtMonotonicMs).toBe(
      observedA.capture.startedAtMonotonicMs,
    );
    expect(witness.first.finality.startedAtMonotonicMs % 1).toBe(0.125);
    expect(witness.first.finality.admittedAtMonotonicMs % 1).toBe(0.125);
    for (const surface of ["native", "ogmios", "kupo"] as const) {
      const original = observedA[surface];
      const retained = witness.first.observation[surface];
      expect(retained).toBe(original);
      expect(retained.provider).toBe(original.provider);
      expect(retained.transactions).toBe(original.transactions);
      expect(retained.chainPoint.depth).toBe("5001");
      expect(witness.current.observation[surface].chainPoint.depth).toBe(
        "5002",
      );
      for (const [index, transaction] of retained.transactions.entries()) {
        expect(transaction.fullTransaction.bytesHex).toBe(
          original.transactions[index]!.fullTransaction.bytesHex,
        );
        expect(transaction.body.bytesHex).toBe(
          original.transactions[index]!.body.bytesHex,
        );
      }
    }
    expect(witness.first.observation.kupo.transactions).toHaveLength(0);
    for (const accepted of [witness.first, witness.current]) {
      const captured = accepted.observation.capture;
      for (const value of [
        witness,
        accepted,
        accepted.finality,
        accepted.finality.result,
        accepted.observation,
        captured,
        captured.sourceBinding,
        captured.sourceBinding.queryServices,
        captured.finalityConfig,
        captured.rawBlock,
        captured.rawBlock.transactions,
        captured.creatingTransactionBodies,
        ...(["native", "ogmios", "kupo"] as const).flatMap((surface) => {
          const block = accepted.observation[surface];
          return [block, block.provider, block.transactions];
        }),
      ])
        expect(Object.isFrozen(value)).toBe(true);
    }
    for (const invalid of [
      { ...currentPair, observation: observationA },
      { ...currentPair, observation: { ...observationB } },
      { ...currentPair, finality: { ...second.admitted! } },
      {
        ...currentPair,
        finality: witness.current as unknown as typeof currentPair.finality,
      },
    ])
      expect(() =>
        readWatcherLocalBackfillFinalityOriginalWitness(invalid),
      ).toThrow("not the identical admitted pair");
    const reread = readWatcherLocalBackfillFinalityOriginalWitness(currentPair);
    expect(reread.first).toBe(witness.first);
    expect(reread.current).toBe(witness.current);
    expect(() => readWatcherLocalBackfillObservation(observationA)).toThrow(
      /closed/,
    );
    expect(() => readWatcherLocalBackfillFinality(first.admitted!)).toThrow(
      /closed/,
    );
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(4);
    expect(
      admitWatcherLocalBackfillFinality({
        ...f.args,
        observation: observationB,
        previous: null,
      }).admitted,
    ).toBeNull();
    await captureB.close();
    expect(() =>
      readWatcherLocalBackfillFinalityOriginalWitness(currentPair),
    ).toThrow(/closed/);
    const captureC = await f.open();
    const observationC = admitWatcherLocalBackfillObservation(captureC.receipt);
    expect(() =>
      readWatcherLocalBackfillFinalityOriginalWitness({
        ...currentPair,
        observation: observationC,
      }),
    ).toThrow("not the identical admitted pair");
    for (const previous of [first.admitted, second.admitted]) {
      const reused = admitWatcherLocalBackfillFinality({
        ...f.args,
        observation: observationC,
        previous,
      });
      expect(reused.admitted).toBeNull();
      expect(reused.result.reasonCodes).toContain("already_finalized");
    }
  });

  it("keeps stable source identity and treats a fresh same-depth capture as duplicate", async () => {
    const f = await fixture();
    const firstCapture = await f.open();
    const a = admitWatcherLocalBackfillObservation(firstCapture.receipt);
    const firstRead = readWatcherLocalBackfillObservation(a);
    const firstConsistency = evaluateWatcherLocalBackfillConsistency(a);
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: a,
      previous: null,
    });
    await firstCapture.close();
    const secondCapture = await f.open();
    const b = admitWatcherLocalBackfillObservation(secondCapture.receipt);
    const secondRead = readWatcherLocalBackfillObservation(b);
    expect(secondRead.sourceIdentityDigest).toBe(
      firstRead.sourceIdentityDigest,
    );
    expect(secondRead.acquisitionDigest).not.toBe(firstRead.acquisitionDigest);
    expect(secondRead.capture.startedAtMonotonicMs).toBeGreaterThan(
      firstRead.capture.startedAtMonotonicMs,
    );
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(4);
    expect(secondRead.ogmios.provider).toEqual(firstRead.ogmios.provider);
    expect(evaluateWatcherLocalBackfillConsistency(b)).toEqual(
      firstConsistency,
    );
    const duplicate = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: b,
      previous: first.admitted,
    });
    expect(duplicate.result.action).toBe("duplicate");
    expect(duplicate.admitted).toBeNull();
  });

  it("refuses a deeper capture that started before the first step was admitted", async () => {
    const f = await fixture({ tipOffsets: [3000, 5000, 5000, 5001] });
    const captureA = await f.open();
    const a = admitWatcherLocalBackfillObservation(captureA.receipt);
    const captureB = await f.open();
    const b = admitWatcherLocalBackfillObservation(captureB.receipt);
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: a,
      previous: null,
    });
    expect(
      readWatcherLocalBackfillObservation(b).capture.startedAtMonotonicMs,
    ).toBeLessThan(
      readWatcherLocalBackfillFinality(first.admitted!).admittedAtMonotonicMs,
    );
    expect(() =>
      admitWatcherLocalBackfillFinality({
        ...f.args,
        observation: b,
        previous: first.admitted,
      }),
    ).toThrow("later-started capture");
  });

  it("refuses finality-policy substitution, copied receipts and copied deployment authority", async () => {
    const f = await fixture();
    const capture = await f.open();
    const observation = admitWatcherLocalBackfillObservation(capture.receipt);
    const args = { ...f.args, observation, previous: null };
    for (const finality of [
      {
        ...f.args.watcherConfig.l1.finality,
        depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth + 1,
      },
      {
        ...f.args.watcherConfig.l1.finality,
        rollback: {
          ...f.args.watcherConfig.l1.finality.rollback,
          maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth - 1,
        },
      },
    ]) {
      const watcherConfig = {
        ...f.args.watcherConfig,
        l1: { ...f.args.watcherConfig.l1, finality },
      };
      expect(() => parseWatcherConfig(watcherConfig)).not.toThrow();
      expect(() =>
        admitWatcherLocalBackfillFinality({ ...args, watcherConfig }),
      ).toThrow("configuration differs");
    }
    expect(() =>
      admitWatcherLocalBackfillFinality({
        ...args,
        deploymentIdentity: { ...deployment.result },
      }),
    ).toThrow();
    expect(() =>
      admitWatcherLocalBackfillObservation({ ...capture.receipt }),
    ).toThrow();
    expect(() =>
      readWatcherLocalBackfillObservation({ ...observation }),
    ).toThrow();
    expect(() =>
      evaluateWatcherLocalBackfillConsistency({ ...observation }),
    ).toThrow();
    expect(() =>
      admitWatcherLocalBackfillFinality({
        ...args,
        observation: { ...observation },
      }),
    ).toThrow();
    const first = admitWatcherLocalBackfillFinality(args);
    expect(() =>
      readWatcherLocalBackfillFinality({ ...first.admitted! }),
    ).toThrow();
    expect(() =>
      admitWatcherLocalBackfillFinality({
        ...args,
        previous: { ...first.admitted! },
      }),
    ).toThrow("not privately admitted");
    const saved = JSON.parse(
      JSON.stringify(readWatcherLocalBackfillFinality(first.admitted!)),
    ) as typeof first.admitted;
    expect(() =>
      admitWatcherLocalBackfillFinality({ ...args, previous: saved }),
    ).toThrow("not privately admitted");
    await capture.close();
    expect(() => admitWatcherLocalBackfillObservation(capture.receipt)).toThrow(
      /closed/,
    );
    expect(() => evaluateWatcherLocalBackfillConsistency(observation)).toThrow(
      /closed/,
    );
    expect(() =>
      admitWatcherLocalBackfillFinality({ ...args, previous: first.admitted }),
    ).toThrow(/closed/);
    expect(() => readWatcherLocalBackfillFinality(first.admitted!)).toThrow(
      /closed/,
    );
  });

  it("leaves below-threshold visibility to the live workflow", async () => {
    const f = await fixture({
      // The admitted second read lands one block short of the release depth.
      tipOffsets: [1, DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth - 2],
    });
    const capture = await f.open();
    const observation = admitWatcherLocalBackfillObservation(capture.receipt);
    expect(() =>
      admitWatcherLocalBackfillFinality({
        ...f.args,
        observation,
        previous: null,
      }),
    ).toThrow("below confirmation depth");
  });

  it("rechecks capture liveness after synchronous transaction derivation", async () => {
    const f = await fixture();
    const capture = await f.open();
    const decode = CML.Transaction.from_cbor_hex;
    let close: Promise<void> | undefined;
    vi.spyOn(CML.Transaction, "from_cbor_hex").mockImplementation((bytes) => {
      const transaction = decode(bytes);
      close ??= capture.close();
      return transaction;
    });
    expect(() => admitWatcherLocalBackfillObservation(capture.receipt)).toThrow(
      /closed/,
    );
    await close;
  });
});

describe("owned reference bodies and current receipt pairing", () => {
  it("releases creating overlays on the same concrete source while preserving its target cache", async () => {
    const f = await fixture();
    const source = createWatcherLocalKupmiosRawSource({
      watcherConfig: f.args.watcherConfig,
      deploymentIdentity: deployment.result,
    });
    const args = { source, point: target };
    const targetBlock = await readAdmittedLocalKupmiosRawBlockAtPoint(args);
    const targetSockets = f.sockets.length;
    const first = await readAdmittedLocalKupmiosReferenceBodiesAtPoint(args);
    expect(first.targetBlock).toEqual(targetBlock);
    expect(first.creatingTransactionBodies).toHaveLength(6);
    expect(f.creatingLookups).toHaveLength(4);
    const second = await readAdmittedLocalKupmiosReferenceBodiesAtPoint(args);
    expect(second).toEqual(first);
    expect(f.creatingLookups).toHaveLength(8);
    expect(f.sockets).toHaveLength(targetSockets + 8);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint(args),
    ).resolves.toEqual(targetBlock);
    expect(f.sockets).toHaveLength(targetSockets + 8);
  });

  it("acquires six exact creating bodies for fourteen references before query two", async () => {
    const f = await fixture();
    const capture = await f.open();
    const observed = admitWatcherLocalBackfillObservation(capture.receipt);
    const current = readWatcherLocalBackfillObservation(observed);
    const expectedBodies = referenceFixture.transactions
      .map(({ txHash, transactionCbor }) => {
        const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
        const body = transaction.body();
        try {
          const cbor = body.to_cbor_hex();
          expect(computeHash32(Buffer.from(cbor, "hex")).toString("hex")).toBe(
            txHash,
          );
          return { txHash, cbor };
        } finally {
          body.free();
          transaction.free();
        }
      })
      .sort((left, right) => left.txHash.localeCompare(right.txHash));
    expect(current.capture.creatingTransactionBodies).toEqual(
      expectedBodies.map(({ cbor }) => cbor),
    );
    expect(Object.isFrozen(current.capture.creatingTransactionBodies)).toBe(
      true,
    );
    expect(f.referenceReads).toHaveLength(6);
    expect(
      f.referenceReads.every(
        ({ nativeQueryStarts }) => nativeQueryStarts === 1,
      ),
    ).toBe(true);
    expect(new Set(f.creatingLookups).size).toBe(4);
    expect(f.creatingLookups).toHaveLength(4);
    expect(
      current.capture.rawBlock.transactions.map(
        ({ transactionCbor }) => transactionCbor,
      ),
    ).toEqual(ordinaryBlock.transactionCbors);
    expect(current.native.transactions).toHaveLength(8);
    expect(referenceFixture.provenance.dataNetwork).toBe("Mainnet");
    const pending = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: observed,
      previous: null,
    });
    expect(pending.admitted).not.toBeNull();
    const pair = { finality: pending.admitted!, observation: observed };
    const paired = readWatcherLocalBackfillFinalityObservation(pair);
    expect(paired.observation).toBe(current);
    expect(paired.finality.result.action).toBe("observe_pending");
    const authority = createWatcherLocalBackfillUserEventReferenceAuthority({
      ...pair,
      deploymentIdentity: deployment.result,
    });
    const evidence = readWatcherUserEventReferenceEvidence(authority);
    expect(evidence.evidenceKind).toBe("creating_bodies");
    expect(evidence.transactions).toHaveLength(8);
    expect(
      evidence.transactions.flatMap(({ referenceInputs }) => referenceInputs),
    ).toHaveLength(14);
    expect(
      admitWatcherLocalBackfillUserEventReferenceEvidence({
        ...pair,
        deploymentIdentity: deployment.result,
        referenceAuthority: authority,
        evidence,
      }),
    ).toBe(evidence);
    expect(
      admitWatcherUserEventReferenceEvidence({
        evidence,
        deploymentIdentity: deployment.result,
        targetBlock: current.native,
        referenceAuthorities: [authority],
      }),
    ).toBeNull();
    const noncanonical = expectedBodies.find(({ txHash }) =>
      txHash.startsWith("0dc17712"),
    )!;
    const preserved = CML.TransactionBody.from_cbor_hex(noncanonical.cbor);
    try {
      expect(preserved.to_canonical_cbor_hex()).not.toBe(noncanonical.cbor);
    } finally {
      preserved.free();
    }
    for (const targetTransaction of evidence.transactions) {
      for (const reference of targetTransaction.referenceInputs) {
        const [txHash, index] = reference.outRef.split("#");
        const creating = CML.Transaction.from_cbor_hex(
          referenceFixture.transactions.find((row) => row.txHash === txHash)!
            .transactionCbor,
        );
        const body = creating.body();
        const outputs = body.outputs();
        const output = outputs.get(Number(index));
        try {
          expect(reference.outputCbor).toBe(output.to_canonical_cbor_hex());
        } finally {
          output.free();
          outputs.free();
          body.free();
          creating.free();
        }
      }
    }
    await capture.close();
    expect(() => readWatcherLocalBackfillFinalityObservation(pair)).toThrow(
      /closed/,
    );
    expect(() => readWatcherUserEventReferenceEvidence(authority)).toThrow(
      /closed/,
    );
    expect(
      admitWatcherLocalBackfillUserEventReferenceEvidence({
        ...pair,
        deploymentIdentity: deployment.result,
        referenceAuthority: authority,
        evidence,
      }),
    ).toBeNull();
  });

  it("requires identical opaque pairing even when target and W11 digests match", async () => {
    const f = await fixture();
    const captureA = await f.open();
    const a = admitWatcherLocalBackfillObservation(captureA.receipt);
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: a,
      previous: null,
    });
    const captureB = await f.open();
    const b = admitWatcherLocalBackfillObservation(captureB.receipt);
    const second = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: b,
      previous: null,
    });
    expect(
      readWatcherLocalBackfillFinality(first.admitted!).consistency
        .consistencyDigest,
    ).toBe(
      readWatcherLocalBackfillFinality(second.admitted!).consistency
        .consistencyDigest,
    );
    const pair = { finality: first.admitted!, observation: a };
    const args = { ...pair, deploymentIdentity: deployment.result };
    const authority =
      createWatcherLocalBackfillUserEventReferenceAuthority(args);
    const evidence = readWatcherUserEventReferenceEvidence(authority);
    expect(() =>
      readWatcherLocalBackfillFinalityObservation({ ...pair, observation: b }),
    ).toThrow("identical admitted pair");
    expect(() =>
      createWatcherLocalBackfillUserEventReferenceAuthority({
        ...args,
        observation: b,
      }),
    ).toThrow("identical admitted pair");
    expect(() =>
      createWatcherLocalBackfillUserEventReferenceAuthority({
        ...args,
        deploymentIdentity: { ...deployment.result },
      }),
    ).toThrow();
    for (const mismatch of [
      { finality: second.admitted!, observation: b },
      { ...pair, observation: { ...a } },
      { ...pair, finality: { ...first.admitted! } },
    ]) {
      expect(
        admitWatcherLocalBackfillUserEventReferenceEvidence({
          ...args,
          ...mismatch,
          referenceAuthority: authority,
          evidence,
        }),
      ).toBeNull();
    }
    expect(
      admitWatcherLocalBackfillUserEventReferenceEvidence({
        ...args,
        referenceAuthority: { ...authority },
        evidence,
      }),
    ).toBeNull();
    expect(
      admitWatcherLocalBackfillUserEventReferenceEvidence({
        ...args,
        referenceAuthority: authority,
        evidence: { ...evidence, transactions: [] },
      }),
    ).toBeNull();
    expect(f.creatingLookups).toHaveLength(8);
  });

  it.each(["missing", "wrong_frame", "wrong_index"] as const)(
    "refuses %s creating evidence before issuing the final native query",
    async (referenceMode) => {
      const f = await fixture({ referenceMode });
      await expect(f.open()).rejects.toThrow();
      expect(
        (await f.logs()).filter(({ kind }) => kind === "start"),
      ).toHaveLength(1);
      expect(f.sockets.every(({ closed }) => closed)).toBe(true);
    },
  );

  it("reacquires all creating bodies after their Kupo checkpoint changes", async () => {
    const f = await fixture({ referenceMode: "head_changed" });
    const capture = await f.open();
    expect(
      readWatcherLocalHistoricalCapture(capture.receipt)
        .creatingTransactionBodies,
    ).toHaveLength(6);
    await capture.close();
    const records = await f.logs();
    expect(records.filter(({ kind }) => kind === "start")).toHaveLength(3);
    expect(records.filter(({ kind }) => kind === "stop")).toHaveLength(3);
    expect(f.sockets.every(({ closed }) => closed)).toBe(true);
  });

  it("cancels a pending creating-body lookup and closes the owned first native query", async () => {
    const f = await fixture({ referenceMode: "wait" });
    const controller = new AbortController();
    const pending = f.open({ signal: controller.signal });
    const outcome = pending.catch((error: unknown) => error);
    await f.waitFor(() => f.referenceReads.length > 0);
    controller.abort();
    expect(await outcome).toMatchObject({
      message: expect.stringMatching(/abort|cancel/),
    });
    expect(
      (await f.logs()).filter(({ kind }) => kind === "start"),
    ).toHaveLength(1);
  });
});
