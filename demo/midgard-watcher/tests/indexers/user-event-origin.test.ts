import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { chmod, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import {
  buildDepositValidators,
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
  buildWithdrawalValidators,
  HubOracleDatum,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  admitWatcherUserEventOrigin,
  readWatcherUserEventOrigin,
  unsafeInspectWatcherUserEventActivationTransactionForTest,
  type WatcherUserEventOriginReceipt,
} from "../../src/indexers/user-event-origin.js";
import {
  admitWatcherLocalBackfillFinality,
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherLocalBackfillFinalityReceipt,
} from "../../src/l1/finality-engine.js";
import {
  admitWatcherLocalBackfillObservation,
  type WatcherLocalBackfillObservationReceipt,
} from "../../src/l1/l1-adapter.js";
import { openWatcherLocalHistoricalCapture } from "../../src/l1/local-historical-capture.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
  type WatcherUserEventScriptBinding,
} from "../../src/runtime/deployment-identity.js";
import {
  makeWatcherAuthorityContracts,
  makeWatcherDeploymentAuthorityFixture,
} from "../support/deployment-authority-fixture.js";
import { createEmulatorInitialization } from "../support/emulator-initialization.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// The unchanged Conway capture adapter below comes from local-historical-capture.test.ts.
// Native/W12 admission stays real; only executable and HTTP/WebSocket peers use local test data.
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
        depth: 30,
        rollback: Object.freeze({
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 30,
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
  emulatorInitialization = await createEmulatorInitialization();
  deployment = makeOriginDeployment();
  scriptBinding = verifyWatcherUserEventScriptBinding({
    deploymentIdentity: deployment.result,
    blueprintBytes,
  });
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
  recheckChangesHead?: boolean;
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
      this.closed = true;
      queueMicrotask(() => this.dispatchEvent(new Event("close")));
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
        head = "66".repeat(32);
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

let emulatorInitialization: Awaited<
  ReturnType<typeof createEmulatorInitialization>
>;
const blueprintBytes = readFileSync(
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
    fileURLToPath(
      new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
    ),
);
const blueprint = parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString("utf8")) as unknown,
);
let scriptBinding: WatcherUserEventScriptBinding;
const makeOriginDeployment = () => {
  const contractSet = makeWatcherAuthorityContracts();
  const hub = buildHubOracleMintingValidator({
    blueprint,
    oneShotOutRef: parseOutRefLabel(
      emulatorInitialization.canonicalOneShotOutRef,
    ),
  });
  const input = {
    blueprint,
    network: "Preprod" as const,
    hubOraclePolicyId: hub.policyId,
  };
  const deposit = buildDepositValidators(input);
  const withdrawal = buildWithdrawalValidators(input);
  const { txOrder, fieldPreimageCertificate } = buildTxOrderValidators(input);
  const scripts = {
    hubOracleMint: hub.mintingScript,
    depositMint: deposit.mintingScript,
    depositSpend: deposit.spendingScript,
    withdrawalMint: withdrawal.mintingScript,
    withdrawalSpend: withdrawal.spendingScript,
    txOrderMint: txOrder.mintingScript,
    txOrderSpend: txOrder.spendingScript,
    fieldPreimageCertificateMint: fieldPreimageCertificate.mintingScript,
  };
  for (const [name, script] of Object.entries(scripts)) {
    contractSet.contracts[name] = {
      ...contractSet.contracts[name],
      contract: { type: script.type, cborHex: script.script },
      scriptHash: validatorToScriptHash(script),
    };
  }
  for (const [role, name] of Object.entries(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  )) {
    contractSet.referenceScripts[role] = {
      ...contractSet.referenceScripts[role],
      scriptHash: contractSet.contracts[name].scriptHash,
    };
  }
  return makeWatcherDeploymentAuthorityFixture({
    contractSet,
    blueprintHash: createHash("sha256").update(blueprintBytes).digest("hex"),
    hubOracleOneShotOutRef: emulatorInitialization.canonicalOneShotOutRef,
  });
};

describe("user-event activation origin", () => {
  it("recognizes the current signed and confirmed emulator initialization without native inclusion authority", () => {
    expect(createHash("sha256").update(blueprintBytes).digest("hex")).toBe(
      emulatorInitialization.blueprintSha256,
    );
    const transaction = CML.Transaction.from_cbor_hex(
      emulatorInitialization.transactionCbor,
    );
    expect(transaction.is_valid()).toBe(true);
    expect(CML.hash_transaction(transaction.body()).to_hex()).toBe(
      emulatorInitialization.transactionId,
    );
    const facts = unsafeInspectWatcherUserEventActivationTransactionForTest({
      deploymentIdentity: deployment.result,
      scriptBinding,
      transactionCbor: emulatorInitialization.transactionCbor,
    });
    expect(facts).not.toBeNull();
    expect(Object.isFrozen(facts)).toBe(true);
    expect(facts!.transactionId).toBe(emulatorInitialization.transactionId);
    expect(facts!.hubOutRef).toBe(
      `${emulatorInitialization.transactionId}#${facts!.hubOutputIndex.toString()}`,
    );
    const hub = transaction.body().outputs().get(facts!.hubOutputIndex);
    expect(hub.datum()!.as_datum()!.to_cbor_hex()).toBe(facts!.hubDatumCbor);
    const datum = Data.from(facts!.hubDatumCbor, HubOracleDatum);
    const scripts = readWatcherUserEventScriptBinding({
      binding: scriptBinding,
      deploymentIdentity: deployment.result,
    });
    expect(datum.deposit).toBe(scripts.deposit.policyId);
    expect(datum.withdrawal).toBe(scripts.withdrawal.policyId);
    expect(datum.tx_order).toBe(scripts.forcedOrder.policyId);
    expect(() =>
      readWatcherUserEventOrigin({
        origin: facts as unknown as WatcherUserEventOriginReceipt,
        deploymentIdentity: deployment.result,
        scriptBinding,
        finality: {} as WatcherLocalBackfillFinalityReceipt,
        observation: {} as WatcherLocalBackfillObservationReceipt,
      }),
    ).toThrow("not privately admitted");
  });

  it("finds no activation in any unchanged ordinary Conway transaction", () => {
    expect(ordinaryBlock.transactionCbors).toHaveLength(8);
    for (const transactionCbor of ordinaryBlock.transactionCbors) {
      expect(
        unsafeInspectWatcherUserEventActivationTransactionForTest({
          deploymentIdentity: deployment.result,
          scriptBinding,
          transactionCbor,
        }),
      ).toBeNull();
    }
  });

  it("refuses structural script-binding and deployment substitutions even at the descriptive test seam", () => {
    expect(() =>
      unsafeInspectWatcherUserEventActivationTransactionForTest({
        deploymentIdentity: { ...deployment.result },
        scriptBinding,
        transactionCbor: emulatorInitialization.transactionCbor,
      }),
    ).toThrow();
    expect(() =>
      unsafeInspectWatcherUserEventActivationTransactionForTest({
        deploymentIdentity: deployment.result,
        scriptBinding: { ...scriptBinding },
        transactionCbor: emulatorInitialization.transactionCbor,
      }),
    ).toThrow();
  });

  it("refuses absent activation through a real live finalized W12 pair and also rejects pending, mixed and closed pairs", async () => {
    const f = await fixture({ tipOffsets: [3000, 5000, 5000, 5001] });
    const captureA = await f.open();
    const observationA = admitWatcherLocalBackfillObservation(captureA.receipt);
    const first = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: observationA,
      previous: null,
    });
    expect(first.result.action).toBe("observe_pending");
    expect(first.admitted).not.toBeNull();
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        finality: first.admitted!,
        observation: observationA,
      }),
    ).toThrow("requires a finalized pair");
    await captureA.close();
    const captureB = await f.open();
    const observationB = admitWatcherLocalBackfillObservation(captureB.receipt);
    const second = admitWatcherLocalBackfillFinality({
      ...f.args,
      observation: observationB,
      previous: first.admitted,
    });
    expect(second.result.action).toBe("finalize");
    expect(second.admitted).not.toBeNull();
    const pair = { finality: second.admitted!, observation: observationB };
    const witness = readWatcherLocalBackfillFinalityOriginalWitness(pair);
    expect(witness.current.observation.capture.nativeBlock.rawBlockCbor).toBe(
      rawBlockCbor,
    );
    expect(witness.first.observation.capture.nativeBlock.rawBlockCbor).toBe(
      rawBlockCbor,
    );
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        ...pair,
      }),
    ).toThrow("no activation for this deployment");
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        ...pair,
        observation: observationA,
      }),
    ).toThrow("identical admitted pair");
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        ...pair,
        finality: { ...pair.finality },
      }),
    ).toThrow();
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        ...pair,
        observation: { ...pair.observation },
      }),
    ).toThrow();
    await captureB.close();
    expect(() =>
      admitWatcherUserEventOrigin({
        deploymentIdentity: deployment.result,
        scriptBinding,
        ...pair,
      }),
    ).toThrow(/closed|stale/);
  });
});

describe("synthetic local native activation origin", () => {
  it("admits a real opaque origin from a live W12 pair and rejects copied, mixed, successor and closed handles", async () => {
    const f = await createSyntheticUserEventOriginFixture();
    cleanup.push(f.close);
    const activation = await f.openFinalizedBlock(f.activationBlock);
    const input = {
      deploymentIdentity: f.deploymentIdentity,
      scriptBinding: f.scriptBinding,
      ...activation,
    };
    const origin = admitWatcherUserEventOrigin(input);
    const facts = readWatcherUserEventOrigin({ ...input, origin });
    expect(facts.block.chainPoint.blockHash).toBe(
      f.activationBlock.point.blockHash,
    );
    expect(facts.activation.transactionId).toBe(
      f.activationBlock.nativeBlock.transactionIds[0],
    );
    expect(facts.parentPoint).toEqual(f.activationBlock.parentPoint);
    expect(Object.isFrozen(facts)).toBe(true);
    expect(() =>
      readWatcherUserEventOrigin({ ...input, origin: { ...origin } }),
    ).toThrow();
    expect(() =>
      readWatcherUserEventOrigin({
        ...input,
        origin,
        scriptBinding: { ...f.scriptBinding },
      }),
    ).toThrow();
    const successor = await f.openFinalizedBlock(f.emptySuccessorBlock);
    expect(f.emptySuccessorBlock.nativeBlock.transactionIds).toHaveLength(0);
    expect(() =>
      readWatcherUserEventOrigin({ ...input, ...successor, origin }),
    ).toThrow();
    expect(() =>
      admitWatcherUserEventOrigin({ ...input, ...successor }),
    ).toThrow("no activation for this deployment");
    await activation.close();
    expect(() => readWatcherUserEventOrigin({ ...input, origin })).toThrow();
  }, 60_000);
});
