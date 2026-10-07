import "node:crypto";
import "node:fs";
import "node:fs/promises";
import "node:path";
import "node:perf_hooks";
import "node:url";
import "@al-ft/l1-node-transport";
import "@al-ft/l1-node-transport/testing/fake-sidecar";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/out-ref";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../../src/indexers/user-event-origin.js";
import "../../src/l1/finality-engine.js";
import "../../src/l1/l1-adapter.js";
import "../../src/l1/local-historical-capture.js";
import "../../src/l1/native-block-admission.js";
import "../../src/l1/native-chain-sync.js";
import "../../src/runtime/config.js";
import "../../src/runtime/deployment-identity.js";
import "../support/deployment-authority-fixture.js";
import "../support/emulator-initialization.js";
import "../support/user-event-origin-fixture.js";
import "./user-event-origin.config.js";

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { mkdtemp, readFile, realpath, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

import { closeSharedL1NodeTransports } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
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
import {
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
  type WatcherUserEventScriptBinding,
} from "../../src/runtime/deployment-identity.js";
import {
  makeWatcherAuthorityContracts,
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_EMULATOR_HISTORY_RECIPE,
} from "../support/deployment-authority-fixture.js";
import { createEmulatorInitialization } from "../support/emulator-initialization.js";
import {
  buildWatcherOriginFixtureHistoryDeployments,
  createSyntheticUserEventOriginFixture,
} from "../support/user-event-origin-fixture.js";
import {
  config,
  delay,
  earlier,
  type FixtureOptions,
  GENESIS_BYTES,
  type Log,
  metadata,
  parent,
  type ReferenceFixture,
  target,
} from "./user-event-origin.config.js";

const rawPath = fileURLToPath(
  new URL("../support/conway-block.hex", import.meta.url),
);
const handlerModule = fileURLToPath(
  new URL("../support/local-historical-capture-handler.mjs", import.meta.url),
);

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
  // Setup publishes every reference script. The whole file runs in about 20 s
  // on an idle machine; the limit leaves room for a fully contended battery.
}, 120_000);

const cleanup: (() => Promise<void>)[] = [];

afterEach(async () => {
  for (const close of cleanup.splice(0).reverse()) await close();
  await closeSharedL1NodeTransports();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

const fixture = async (options: FixtureOptions = {}) => {
  const dir = await realpath(
    await mkdtemp(join("/var/tmp", "local-historical-capture-")),
  );
  cleanup.push(() => rm(dir, { recursive: true, force: true }));
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  const binaryPath = join(dir, "node-transport");
  const logPath = join(dir, "events.jsonl");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  await writeFile(logPath, "");
  // The fake node transport serves each exact-point query the unchanged
  // ordinary Conway block, without mocking the supervisor, query receipt,
  // native admission or source factory.
  await writeFakeSidecar({
    path: binaryPath,
    handlerModule,
    options: {
      logPath,
      rawPath,
      metadata,
      modes: options.modes ?? ["query_idle", "query_idle"],
      tipOffsets: options.tipOffsets ?? [3000, 5000],
      secondBlockType: options.secondBlockType ?? null,
    },
  });
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
                    unit.replace(/^(.{56})(?=.)/u, "$1."),
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
  const history = buildWatcherOriginFixtureHistoryDeployments(
    emulatorInitialization.canonicalOneShotOutRef,
    hub.policyId,
  );
  const deposit = history.deposit.list;
  const withdrawal = history.withdrawal.list;
  const { txOrder, fieldPreimageCertificate } = buildTxOrderValidators(input);
  const scripts = {
    hubOracleMint: hub.mintingScript,
    depositMint: deposit.mintingScript,
    depositSpend: deposit.spendingScript,
    withdrawalMint: withdrawal.mintingScript,
    withdrawalSpend: withdrawal.spendingScript,
    depositHistoryRetentionSpend: history.deposit.retention.spendingScript,
    depositHistoryRetirementWithdraw:
      history.deposit.retirement.withdrawalScript,
    withdrawalHistoryRetentionSpend:
      history.withdrawal.retention.spendingScript,
    withdrawalHistoryRetirementWithdraw:
      history.withdrawal.retirement.withdrawalScript,
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
    eventHistoryRecipe: WATCHER_EMULATOR_HISTORY_RECIPE,
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
    const expectedScripts = readWatcherUserEventScriptBinding({
      binding: scriptBinding,
      deploymentIdentity: deployment.result,
    });
    const candidate = Array.from(
      { length: transaction.body().outputs().len() },
      (_, index) => transaction.body().outputs().get(index),
    ).find(
      (output) =>
        output
          .amount()
          .multi_asset()
          .get(
            CML.ScriptHash.from_hex(expectedScripts.hub.policyId),
            CML.AssetName.from_hex(expectedScripts.hub.assetName),
          ) === 1n,
    )!;
    const openedHub = Data.from(
      candidate.datum()!.as_datum()!.to_cbor_hex(),
      HubOracleDatum,
    );
    expect({
      deposit: openedHub.deposit,
      withdrawal: openedHub.withdrawal,
      tx_order: openedHub.tx_order,
    }).toEqual({
      deposit: expectedScripts.deposit.policyId,
      withdrawal: expectedScripts.withdrawal.policyId,
      tx_order: expectedScripts.forcedOrder.policyId,
    });
    expect(
      CML.PlutusData.from_cbor_hex(
        Data.to(openedHub, HubOracleDatum),
      ).to_canonical_cbor_hex(),
    ).toBe(candidate.datum()!.as_datum()!.to_canonical_cbor_hex());
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

  it("preserves definite and indefinite hub datum bytes and rejects malformed or mismatched data", () => {
    const original = CML.Transaction.from_cbor_hex(
      emulatorInitialization.transactionCbor,
    );
    const baseline = unsafeInspectWatcherUserEventActivationTransactionForTest({
      deploymentIdentity: deployment.result,
      scriptBinding,
      transactionCbor: emulatorInitialization.transactionCbor,
    })!;
    const hub = original.body().outputs().get(baseline.hubOutputIndex);
    const datum = Data.from(baseline.hubDatumCbor, HubOracleDatum);
    // These edited frames exercise the descriptive decoder only; their original
    // signatures no longer authenticate the rebuilt body and confer no L1 authority.
    const withDatum = (cbor: string) => {
      const outputs = CML.TransactionOutputList.new();
      for (let index = 0; index < original.body().outputs().len(); index++)
        outputs.add(
          index === baseline.hubOutputIndex
            ? CML.TransactionOutput.new(
                hub.address(),
                hub.amount(),
                CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(cbor)),
                hub.script_ref(),
              )
            : original.body().outputs().get(index),
        );
      const body = CML.TransactionBody.new(
        original.body().inputs(),
        outputs,
        original.body().fee(),
      );
      body.set_mint(original.body().mint()!);
      return CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      ).to_cbor_hex();
    };
    const inspect = (cbor: string) =>
      unsafeInspectWatcherUserEventActivationTransactionForTest({
        deploymentIdentity: deployment.result,
        scriptBinding,
        transactionCbor: withDatum(cbor),
      });
    const indefinite = Data.to(datum, HubOracleDatum);
    const definite =
      CML.PlutusData.from_cbor_hex(indefinite).to_canonical_cbor_hex();
    expect(indefinite).not.toBe(definite);
    for (const cbor of [definite, indefinite])
      expect(inspect(cbor)?.hubDatumCbor).toBe(cbor);
    expect(() => inspect(Data.to(0n))).toThrow();
    expect(() =>
      inspect(Data.to({ ...datum, deposit: "ff".repeat(28) }, HubOracleDatum)),
    ).toThrow("hub datum differs");
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
