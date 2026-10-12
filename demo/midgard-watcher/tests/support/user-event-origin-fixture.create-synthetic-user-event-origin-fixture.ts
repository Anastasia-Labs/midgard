import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import {
  mkdtemp,
  readFile,
  realpath,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { closeSharedL1NodeTransports } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { CML } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  verifyWatcherUserEventScriptBinding,
  type WatcherUserEventScriptBinding,
} from "../../src/runtime/deployment-identity.js";
import {
  type LedgerProtocolParameterOverrides,
  ledgerProtocolParameters,
} from "./ledger-protocol-parameters.js";
import {
  buildBlock,
  type OriginDeployment,
  type SyntheticNativeQuery,
  type SyntheticNativeTip,
  type SyntheticUserEventOriginFixture,
} from "./user-event-origin-fixture.build-block.js";
import {
  blueprintBytes,
  type Consumption,
  type Creating,
  type CreatingOutput,
  GENESIS_BYTES,
  INITIALIZATION,
  makeConfig,
  makeOriginDeployment,
  pointAt,
  type SyntheticUserEventBlock,
  withFixtureCml,
} from "./user-event-origin-fixture.make-config.js";

const nativeHandlerModule = fileURLToPath(
  new URL("./user-event-origin-fixture.native-handler.mjs", import.meta.url),
);

export const createSyntheticUserEventOriginFixture = async (
  options: Readonly<{
    queryEndpoints?: Readonly<{ ogmios: string; kupo: string }>;
    nativeTipBaseDepth?: number;
    nativeTipMode?: "query_counter" | "controlled";
    blockSlotInterval?: number;
    ruleBundleCommitment?: string;
    protocolParameters?: unknown;
    /**
     * When given, the native node answers the `protocol_params` ledger
     * query with these parameters (the follower's funding read).
     */
    nodeProtocolParameters?: LedgerProtocolParameterOverrides;
    /** Exact accepted transactions and identity from a live emulator deployment. */
    published?: Readonly<{
      deployment: OriginDeployment;
      transactionCbor: string;
      inclusionSlot?: number;
      creatingTransactions: readonly Readonly<{ transactionCbor: string }>[];
    }>;
  }> = {},
): Promise<SyntheticUserEventOriginFixture> => {
  const nativeTipBaseDepth = options.nativeTipBaseDepth ?? 3000;
  const blockSlotInterval = options.blockSlotInterval ?? 1;
  if (
    !Number.isSafeInteger(blockSlotInterval) ||
    blockSlotInterval < 1 ||
    blockSlotInterval > 60
  ) {
    throw new Error("Synthetic block slot interval is outside fixture bounds");
  }
  const nativeTipMode = options.nativeTipMode ?? "query_counter";
  if (nativeTipMode !== "query_counter" && nativeTipMode !== "controlled")
    throw new Error("Unknown synthetic native tip mode");
  if (
    !Number.isSafeInteger(nativeTipBaseDepth) ||
    nativeTipBaseDepth < 30 ||
    nativeTipBaseDepth > 4000
  )
    throw new Error(
      "Synthetic native tip base depth is outside fixture bounds",
    );
  // The transport binary path must be canonical.
  const dir = await realpath(
    await mkdtemp(join("/var/tmp", "synthetic-user-event-origin-")),
  );
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  const binaryPath = join(dir, "node-transport");
  const registryPath = join(dir, "blocks.json");
  const counterPath = join(dir, "counter");
  const tipPath = join(dir, "tip.json");
  const controlPath = join(dir, "native-control.json");
  const queryLogPath = join(dir, "native-queries.jsonl");
  const ledgerPath = join(dir, "ledger-outputs.json");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  await writeFile(counterPath, "0");
  await writeFile(queryLogPath, "");
  const deployment =
    options.published?.deployment ??
    makeOriginDeployment(options.ruleBundleCommitment);
  const deploymentIdentity = deployment.result;
  const scriptBinding: WatcherUserEventScriptBinding =
    verifyWatcherUserEventScriptBinding({
      deploymentIdentity,
      blueprintBytes,
    });
  const watcherConfig = makeConfig(nodeConfig, genesisConfig);
  const blocks: SyntheticUserEventBlock[] = [];
  const creating: Creating[] = [];
  const creatingByHash = new Map<string, Creating>();
  const outputsByAddress = new Map<string, CreatingOutput[]>();
  const outputsByUnit = new Map<string, CreatingOutput[]>();
  const consumptionsByOutRef = new Map<string, Consumption[]>();
  const closed: (() => Promise<void>)[] = [];
  // The node's ledger, for `utxo_by_address`: the outputs the initialization
  // frames leave unspent, and each native block's spends and outputs.
  type LedgerOutput = Readonly<{
    outRef: string;
    address: string;
    cbor: string;
  }>;
  const ledger = {
    preOrigin: new Map<string, LedgerOutput>(),
    blocks: {} as Record<string, { spent: string[]; created: LedgerOutput[] }>,
  };
  let initializing = true;
  const initialization = options.published ?? INITIALIZATION;
  const initializationTransaction = CML.Transaction.from_cbor_hex(
    initialization.transactionCbor,
  );
  const initialSlot =
    options.published?.inclusionSlot === undefined
      ? initializationTransaction.body().validity_interval_start()! + 1n
      : BigInt(options.published.inclusionSlot) + 1n;
  const seededReferenceSpan =
    2n * BigInt(initialization.creatingTransactions.length);
  const anchorSlot = initialSlot - 3n;
  const latestReferenceStartSlot = anchorSlot - seededReferenceSpan;
  if (latestReferenceStartSlot < 0n) {
    await rm(dir, { recursive: true, force: true });
    throw new Error(
      "Synthetic initialization has insufficient prior slots for its creating transactions",
    );
  }
  // Reference-only prehistory must precede Init in both coordinates. A fixed
  // native height of 100 placed later seeded references after live proof blocks.
  const referenceStartSlot =
    latestReferenceStartSlot < 100n ? latestReferenceStartSlot : 100n;
  const anchorHeight = seededReferenceSpan + 1n;
  const anchor = pointAt(
    "ab".repeat(32),
    anchorHeight > 100n ? anchorHeight : 100n,
    anchorSlot,
  );
  const emptyParent = buildBlock([], anchor);
  blocks.push(emptyParent);
  const activationBlock = buildBlock(
    [initialization.transactionCbor],
    emptyParent.point,
  );
  blocks.push(activationBlock);
  const emptySuccessorBlock = buildBlock([], activationBlock.point);
  blocks.push(emptySuccessorBlock);
  type StreamCommand =
    | Readonly<{ kind: "rollback"; point: SyntheticNativeTip | "origin" }>
    | Readonly<{ kind: "exit"; exitCode: number }>;
  const control: {
    mode: "query_counter" | "controlled";
    tip: SyntheticNativeTip;
    commands: StreamCommand[];
    closed: boolean;
    canonicalBranchSelected: boolean;
    exactQueriesHeld: boolean;
  } = {
    canonicalBranchSelected: false,
    exactQueriesHeld: false,
    mode: nativeTipMode,
    tip: {
      blockHash: createHash("sha256")
        .update("synthetic-controlled-tip")
        .digest("hex"),
      blockNo: (
        BigInt(emptySuccessorBlock.point.blockNo) + BigInt(nativeTipBaseDepth)
      ).toString(),
      slot: (
        BigInt(emptySuccessorBlock.point.slot) +
        BigInt(Math.max(600, nativeTipBaseDepth))
      ).toString(),
    },
    commands: [],
    closed: false,
  };
  const writeAtomic = async (path: string, value: unknown) => {
    const temporary = `${path}.next`;
    await writeFile(temporary, JSON.stringify(value));
    await rename(temporary, path);
  };
  const persistControl = () => writeAtomic(controlPath, control);
  await persistControl();
  if (nativeTipMode === "query_counter")
    await writeAtomic(tipPath, { kind: "point", ...control.tip });
  let controlWrites: Promise<void> = Promise.resolve();
  const changeControl = <T>(change: () => Promise<T>): Promise<T> => {
    const result = controlWrites.then(async () => {
      if (control.closed) throw new Error("Synthetic native fixture is closed");
      return await change();
    });
    controlWrites = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  };
  const registerCreating = (
    transactionCbor: string,
    knownBlock?: SyntheticUserEventBlock,
    index = 0,
  ) => {
    const source = withFixtureCml((own) => {
      const transaction = own(CML.Transaction.from_cbor_hex(transactionCbor));
      const body = own(transaction.body());
      const txHash = own(CML.hash_transaction(body)).to_hex();
      const existing = creatingByHash.get(txHash);
      if (existing !== undefined) return existing;
      const position = BigInt(creating.length);
      const predecessorPoint =
        knownBlock?.parentPoint ??
        pointAt(
          createHash("sha256")
            .update(`synthetic-reference-parent-${txHash}`)
            .digest("hex"),
          1n + position * 2n,
          referenceStartSlot + position * 2n,
        );
      const creatingPoint =
        knownBlock?.point ??
        pointAt(
          createHash("sha256")
            .update(`synthetic-reference-child-${txHash}`)
            .digest("hex"),
          2n + position * 2n,
          referenceStartSlot + 1n + position * 2n,
        );
      const inputs = own(body.inputs());
      const inputOutRefs = Array.from({ length: inputs.len() }, (_, index) => {
        const input = own(inputs.get(index));
        return `${own(input.transaction_id()).to_hex()}#${input.index().toString()}`;
      });
      const bodyOutputs = own(body.outputs());
      const outputs = Array.from(
        { length: bodyOutputs.len() },
        (_, outputIndex): CreatingOutput => {
          const output = own(bodyOutputs.get(outputIndex));
          const address = own(output.address()).to_bech32();
          const value = own(output.amount());
          const multiAsset = own(value.multi_asset());
          const policies = own(multiAsset.keys());
          const assets: Record<string, string> = {};
          for (let index = 0; index < policies.len(); index++) {
            const policy = own(policies.get(index));
            const policyAssets = own(multiAsset.get_assets(policy));
            if (policyAssets === undefined)
              throw new Error("Synthetic output lost its asset policy");
            const names = own(policyAssets.keys());
            for (let assetIndex = 0; assetIndex < names.len(); assetIndex++) {
              const name = own(names.get(assetIndex));
              const amount = policyAssets.get(name);
              if (amount === undefined)
                throw new Error("Synthetic output lost its asset quantity");
              // Kupo writes an empty asset name as the bare policy id.
              const key = `${policy.to_hex()}.${name.to_hex()}`;
              assets[key.replace(/\.$/u, "")] = amount.toString();
            }
          }
          const datum = own(output.datum());
          const inline = own(datum?.as_datum());
          const datumHash = own(
            inline === undefined
              ? output.datum_hash()
              : CML.hash_plutus_data(inline),
          );
          const script = own(output.script_ref());
          const scriptHash = own(script?.hash());
          const resolvedScript = (() => {
            if (script === undefined) return null;
            const native = own(script.as_native());
            if (native !== undefined)
              return { language: "native", script: native.to_cbor_hex() };
            const versions = [
              ["plutus:v1", own(script.as_plutus_v1())],
              ["plutus:v2", own(script.as_plutus_v2())],
              ["plutus:v3", own(script.as_plutus_v3())],
            ] as const;
            for (const [language, plutus] of versions) {
              if (plutus !== undefined)
                return {
                  language,
                  script: Buffer.from(plutus.to_raw_bytes()).toString("hex"),
                };
            }
            throw new Error("Unsupported fixture reference script");
          })();
          return Object.freeze({
            transaction_index: index,
            transaction_id: txHash,
            output_index: outputIndex,
            address,
            value: Object.freeze({
              coins: value.coin().toString(),
              assets: Object.freeze(assets),
            }),
            datum_hash: datumHash?.to_hex() ?? null,
            ...(datumHash === undefined
              ? {}
              : {
                  datum_type:
                    inline === undefined
                      ? ("hash" as const)
                      : ("inline" as const),
                }),
            script_hash: scriptHash?.to_hex() ?? null,
            created_at: Object.freeze({
              slot_no: Number(creatingPoint.slot),
              header_hash: creatingPoint.blockHash,
            }),
            datum: inline?.to_cbor_hex() ?? null,
            script: resolvedScript,
          });
        },
      );
      const source: Creating = Object.freeze({
        txHash,
        transactionCbor,
        creatingPoint,
        predecessorPoint,
        creatingTransactionIndex: index,
        inputOutRefs: Object.freeze(inputOutRefs),
        outputs: Object.freeze(outputs),
      });
      creating.push(source);
      const created = Array.from({ length: bodyOutputs.len() }, (_, index) => {
        const output = own(bodyOutputs.get(index));
        return {
          outRef: `${txHash}#${index}`,
          address: Buffer.from(own(output.address()).to_raw_bytes()).toString(
            "hex",
          ),
          cbor: output.to_cbor_hex(),
        };
      });
      if (knownBlock !== undefined) {
        const entry = (ledger.blocks[knownBlock.point.blockHash] ??= {
          spent: [],
          created: [],
        });
        entry.spent.push(...inputOutRefs);
        entry.created.push(...created);
      } else if (initializing) {
        for (const outRef of inputOutRefs) ledger.preOrigin.delete(outRef);
        for (const output of created)
          ledger.preOrigin.set(output.outRef, output);
      }
      creatingByHash.set(txHash, source);
      for (const output of outputs) {
        const addressRows = outputsByAddress.get(output.address) ?? [];
        addressRows.push(output);
        outputsByAddress.set(output.address, addressRows);
        for (const [key, amount] of Object.entries(output.value.assets)) {
          if (amount === "0") continue;
          const unit = key.replace(".", "");
          const unitRows = outputsByUnit.get(unit) ?? [];
          unitRows.push(output);
          outputsByUnit.set(unit, unitRows);
        }
      }
      return source;
    });
    // Reference-only creating frames do not consume inputs in the native chain.
    // Keep every coordinate, including duplicates, so history queries preserve
    // their existing refusal when a synthetic chain spends an outref twice.
    if (knownBlock !== undefined) {
      source.inputOutRefs.forEach((outRef, inputIndex) => {
        const consumptions = consumptionsByOutRef.get(outRef) ?? [];
        consumptions.push(
          Object.freeze({
            transaction_id: source.txHash,
            input_index: inputIndex,
            slot_no: Number(knownBlock.point.slot),
            header_hash: knownBlock.point.blockHash,
          }),
        );
        consumptionsByOutRef.set(outRef, consumptions);
      });
    }
  };
  for (const frame of initialization.creatingTransactions)
    registerCreating(frame.transactionCbor);
  initializing = false;
  registerCreating(initialization.transactionCbor, activationBlock);
  const persistBlocks = async () => {
    await writeAtomic(ledgerPath, {
      preOrigin: [...ledger.preOrigin.values()],
      blocks: ledger.blocks,
    });
    await writeAtomic(registryPath, blocks);
  };
  await persistBlocks();
  await writeFakeSidecar({
    path: binaryPath,
    handlerModule: nativeHandlerModule,
    options: {
      controlPath,
      registryPath,
      counterPath,
      tipPath,
      queryLogPath,
      ledgerPath,
      nativeTipBaseDepth,
      ...(options.nodeProtocolParameters === undefined
        ? {}
        : {
            protocolParametersHex: Buffer.from(
              ledgerProtocolParameters(options.nodeProtocolParameters),
            ).toString("hex"),
          }),
    },
  });
  class BoundarySocket extends EventTarget {
    readyState = 0;
    intersection = { slot: Number(anchor.slot), id: anchor.blockHash };
    constructor() {
      super();
      queueMicrotask(() => {
        if (this.readyState !== 0) return;
        this.readyState = 1;
        this.dispatchEvent(new Event("open"));
      });
    }
    send(text: string): void {
      if (this.readyState !== 1)
        throw new Error("Synthetic Ogmios socket is not open");
      const request = JSON.parse(text) as {
        id: number;
        method: string;
        params: { points?: ("origin" | { slot: number; id: string })[] };
      };
      let result: unknown;
      if (request.method === "findIntersection") {
        const intersection = request.params.points![0]!;
        if (intersection === "origin") {
          const tip: SyntheticNativeTip =
            nativeTipMode === "controlled"
              ? (
                  JSON.parse(readFileSync(controlPath, "utf8")) as {
                    tip: SyntheticNativeTip;
                  }
                ).tip
              : (JSON.parse(
                  readFileSync(tipPath, "utf8"),
                ) as SyntheticNativeTip);
          result = {
            intersection,
            tip: {
              id: tip.blockHash,
              slot: Number(tip.slot),
              height: Number(tip.blockNo),
            },
          };
        } else {
          this.intersection = intersection;
          result = { intersection };
        }
      } else {
        const block = blocks.find(
          (block) => block.parentPoint.blockHash === this.intersection.id,
        );
        const references = creating.filter(
          (row) => row.predecessorPoint.blockHash === this.intersection.id,
        );
        // Controlled tips can also be parents of later test blocks. Kupo
        // advertises those checkpoints, so its Ogmios peer must expose their
        // heights when the boundary search probes them. They carry no txs.
        const parentCheckpoint = blocks
          .map(({ parentPoint }) => parentPoint)
          .filter(
            (point) => BigInt(point.slot) > BigInt(this.intersection.slot),
          )
          .sort((a, b) => Number(BigInt(a.slot) - BigInt(b.slot)))[0];
        const point =
          block?.point ?? references[0]?.creatingPoint ?? parentCheckpoint;
        if (point === undefined)
          throw new Error("Synthetic fixture has no requested child");
        const transactions =
          block === undefined
            ? references.map((row) => ({
                id: row.txHash,
                cbor: row.transactionCbor,
              }))
            : block.nativeBlock.transactionIds.map((id, index) => ({
                id,
                cbor: block.nativeBlock.transactionCbors[index]!,
              }));
        result = {
          direction: "forward",
          block: {
            id: point.blockHash,
            slot: Number(point.slot),
            height: Number(point.blockNo),
            ancestor: this.intersection.id,
            transactions,
          },
        };
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
      if (this.readyState >= 2) return;
      this.readyState = 2;
      queueMicrotask(() => {
        this.readyState = 3;
        this.dispatchEvent(
          Object.assign(new Event("close"), {
            code: 1000,
            reason: "",
            wasClean: true,
          }),
        );
      });
    }
  }
  vi.stubGlobal("WebSocket", BoundarySocket);
  const originalFetch = globalThis.fetch;
  vi.stubGlobal("fetch", async (url: string, init?: RequestInit) => {
    // Other local services (mutation leases) retain
    // their actual HTTP handlers when this fixture supplies chain transport.
    if (
      options.queryEndpoints !== undefined &&
      !Object.values(options.queryEndpoints).some(
        (endpoint) =>
          new URL(endpoint.replace(/^ws/u, "http")).origin ===
          new URL(url).origin,
      )
    ) {
      try {
        // Synthetic ledger time advances independently of the local HTTP
        // server's idle timers; do not reuse its idle test connections.
        const headers = new Headers(init?.headers);
        headers.set("connection", "close");
        return await originalFetch(url, { ...init, headers });
      } catch (cause) {
        const endpoint = new URL(url);
        throw new Error(
          `Fixture service request failed: ${init?.method ?? "GET"} ${endpoint.origin}${endpoint.pathname}`,
          { cause },
        );
      }
    }
    const path = new URL(url).pathname;
    if (init?.method === "POST") {
      const request = JSON.parse(String(init.body)) as {
        id: string;
        method: string;
      };
      if (
        request.method === "queryLedgerState/protocolParameters" &&
        options.protocolParameters !== undefined
      ) {
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: request.id,
            result: options.protocolParameters,
          }),
        );
      }
      if (request.method !== "queryNetwork/tip")
        throw new Error("Synthetic fixture has no requested HTTP RPC");
      const tip: SyntheticNativeTip =
        nativeTipMode === "controlled"
          ? (
              JSON.parse(readFileSync(controlPath, "utf8")) as {
                tip: SyntheticNativeTip;
              }
            ).tip
          : (JSON.parse(readFileSync(tipPath, "utf8")) as SyntheticNativeTip);
      return new Response(
        JSON.stringify({
          jsonrpc: "2.0",
          id: request.id,
          result: {
            id: tip.blockHash,
            height: Number(tip.blockNo),
            slot: Number(tip.slot),
          },
        }),
      );
    }
    const headers = {
      "X-Most-Recent-Checkpoint": "999999",
      ETag: "55".repeat(32),
    };
    if (path.startsWith("/matches/")) {
      const pattern = decodeURIComponent(path.slice("/matches/".length));
      const [rawIndex, txHash] = pattern.split("@");
      const unit = /^[0-9a-f]{56}\.(?:[0-9a-f]{2}){0,32}$/u.test(pattern)
        ? pattern.replace(".", "")
        : null;
      const address = /^addr(?:_test)?1[0-9a-z]+$/u.test(pattern)
        ? pattern
        : null;
      let rows: readonly CreatingOutput[];
      if (unit !== null) {
        rows = outputsByUnit.get(unit) ?? [];
      } else if (address !== null) {
        rows = outputsByAddress.get(address) ?? [];
      } else {
        const source = creatingByHash.get(txHash!);
        if (source === undefined)
          throw new Error(
            `Synthetic fixture has no requested creating frame: ${pattern}`,
          );
        if (rawIndex === "*") {
          rows = source.outputs;
        } else {
          const outputIndex = Number(rawIndex);
          const output = source.outputs[outputIndex];
          if (!Number.isSafeInteger(outputIndex) || output === undefined)
            throw new Error(
              `Synthetic fixture has no requested output: ${pattern}`,
            );
          rows = [output];
        }
      }
      const matches = rows.map((output) => {
        // Unit/address queries report native-chain consumption; exact outref
        // queries deliberately retain this fixture's existing unspent view.
        const consumptions =
          unit !== null || address !== null
            ? (consumptionsByOutRef.get(
                `${output.transaction_id}#${output.output_index.toString()}`,
              ) ?? [])
            : [];
        if (consumptions.length > 1)
          throw new Error(
            "Synthetic unit history contains a duplicate consumption",
          );
        return { ...output, spent_at: consumptions[0] ?? null };
      });
      return new Response(JSON.stringify(matches), { headers });
    }
    const slot = Number(path.split("/").at(-1));
    const points = [
      anchor,
      ...blocks.flatMap((block) => [block.point, block.parentPoint]),
      ...creating.flatMap((row) => [row.creatingPoint, row.predecessorPoint]),
    ];
    const point = points
      .sort((a, b) => Number(b.slot) - Number(a.slot))
      .find((point) => Number(point.slot) <= slot);
    if (point === undefined)
      throw new Error("Synthetic fixture has no requested checkpoint");
    return new Response(
      JSON.stringify({
        slot_no: Number(point.slot),
        header_hash: point.blockHash,
      }),
      { headers },
    );
  });
  const makeBlock = async ({
    transactions,
    creatingBodies = [],
    parent,
    slot,
  }: Readonly<{
    transactions: readonly string[];
    creatingBodies?: readonly string[];
    parent?: SyntheticUserEventBlock;
    slot?: number;
  }>) =>
    changeControl(async () => {
      // Resolve the default parent under the same lock as insertion. Concurrent
      // callers must extend one chain rather than race into sibling blocks.
      const actualParent = parent ?? blocks.at(-1)!;
      const interval =
        slot === undefined
          ? BigInt(blockSlotInterval)
          : BigInt(slot) - BigInt(actualParent.point.slot);
      if (interval < 1n)
        throw new Error(
          "Synthetic block inclusion must follow its parent slot",
        );
      const block = buildBlock(transactions, actualParent.point, interval);
      blocks.push(block);
      for (const body of creatingBodies) {
        const transaction = CML.Transaction.new(
          CML.TransactionBody.from_cbor_hex(body),
          CML.TransactionWitnessSet.new(),
          true,
        );
        registerCreating(transaction.to_cbor_hex());
      }
      transactions.forEach((cbor, index) =>
        registerCreating(cbor, block, index),
      );
      await persistBlocks();
      return block;
    });
  const snapshotTip = (tip: SyntheticNativeTip): SyntheticNativeTip => {
    if (
      !/^[0-9a-f]{64}$/u.test(tip.blockHash) ||
      [tip.blockNo, tip.slot].some(
        (value) =>
          !/^(0|[1-9][0-9]*)$/u.test(value) ||
          BigInt(value) > 0xffffffffffffffffn,
      )
    )
      throw new Error("Synthetic native tip has invalid coordinates");
    return Object.freeze({
      blockHash: tip.blockHash,
      blockNo: tip.blockNo,
      slot: tip.slot,
    });
  };
  const requireControlledTip = () => {
    if (nativeTipMode !== "controlled")
      throw new Error("Synthetic native tip controls require controlled mode");
  };
  const setNativeTip = (tip: SyntheticNativeTip) => {
    requireControlledTip();
    const snapshot = snapshotTip(tip);
    return changeControl(async () => {
      control.tip = snapshot;
      await persistControl();
    });
  };
  const appendNativeBlock = ({
    transactions,
    slot,
  }: Readonly<{ transactions: readonly string[]; slot: number }>) => {
    requireControlledTip();
    if (!Number.isSafeInteger(slot) || slot < 0)
      throw new Error("Native inclusion slot is invalid");
    return changeControl(async () => {
      const parent = control.tip;
      const interval = BigInt(slot) - BigInt(parent.slot);
      if (interval < 1n)
        throw new Error("Native inclusion must follow the controlled tip slot");
      for (const cbor of transactions) {
        const body = CML.Transaction.from_cbor_hex(cbor).body();
        const start = body.validity_interval_start();
        const end = body.ttl();
        if (
          (start !== undefined && BigInt(slot) < start) ||
          (end !== undefined && BigInt(slot) >= end)
        )
          throw new Error(
            `Confirmed transaction ${CML.hash_transaction(body).to_hex()} is outside its validity interval at slot ${slot}`,
          );
      }
      const block = buildBlock(
        transactions,
        pointAt(parent.blockHash, BigInt(parent.blockNo), BigInt(parent.slot)),
        interval,
      );
      blocks.push(block);
      transactions.forEach((cbor, index) =>
        registerCreating(cbor, block, index),
      );
      await persistBlocks();
      control.tip = snapshotTip(block.point);
      await persistControl();
      return block;
    });
  };
  const growNativeTip = (count = 1, minimumFirstSlot?: number) => {
    requireControlledTip();
    if (!Number.isSafeInteger(count) || count < 1 || count > 4096)
      throw new Error("Synthetic native tip growth is outside fixture bounds");
    if (
      minimumFirstSlot !== undefined &&
      (!Number.isSafeInteger(minimumFirstSlot) || minimumFirstSlot < 0)
    )
      throw new Error("Synthetic minimum growth slot is invalid");
    return changeControl(async () => {
      const start = control.tip;
      snapshotTip({
        ...start,
        blockNo: (BigInt(start.blockNo) + BigInt(count)).toString(),
        slot: (
          BigInt(start.slot) + BigInt(count * blockSlotInterval)
        ).toString(),
      });
      let parentPoint = pointAt(
        start.blockHash,
        BigInt(start.blockNo),
        BigInt(start.slot),
      );
      for (let index = 0; index < count; index++) {
        const minimumInterval =
          index === 0 && minimumFirstSlot !== undefined
            ? BigInt(minimumFirstSlot) - BigInt(parentPoint.slot)
            : 0n;
        const next = buildBlock(
          [],
          parentPoint,
          minimumInterval > BigInt(blockSlotInterval)
            ? minimumInterval
            : BigInt(blockSlotInterval),
        );
        blocks.push(next);
        parentPoint = next.point;
      }
      const tip = snapshotTip(parentPoint);
      await persistBlocks();
      control.tip = tip;
      await persistControl();
      return tip;
    });
  };
  const appendStreamCommand = (command: StreamCommand) =>
    changeControl(async () => {
      if (control.commands.length >= 128)
        throw new Error("Synthetic native stream command limit exceeded");
      control.commands.push(command);
      await persistControl();
    });
  const selectCanonicalBranch = (tip: SyntheticNativeTip) =>
    changeControl(async () => {
      const retained = new Set<string>();
      let cursor = blocks.find(
        (block) => block.point.blockHash === tip.blockHash,
      );
      if (
        cursor === undefined ||
        cursor.point.blockNo !== tip.blockNo ||
        cursor.point.slot !== tip.slot
      )
        throw new Error("Canonical fixture branch tip is not registered");
      while (cursor !== undefined) {
        if (retained.has(cursor.point.blockHash))
          throw new Error("Canonical fixture branch is cyclic");
        retained.add(cursor.point.blockHash);
        cursor = blocks.find(
          (block) => block.point.blockHash === cursor!.parentPoint.blockHash,
        );
      }
      const removed = new Set(
        blocks
          .filter((block) => !retained.has(block.point.blockHash))
          .map((block) => block.point.blockHash),
      );
      blocks.splice(
        0,
        blocks.length,
        ...blocks.filter((block) => retained.has(block.point.blockHash)),
      );
      for (const row of creating)
        if (removed.has(row.creatingPoint.blockHash))
          creatingByHash.delete(row.txHash);
      creating.splice(
        0,
        creating.length,
        ...creating.filter((row) => !removed.has(row.creatingPoint.blockHash)),
      );
      for (const index of [outputsByAddress, outputsByUnit])
        for (const [key, rows] of index)
          index.set(
            key,
            rows.filter((row) => !removed.has(row.created_at.header_hash)),
          );
      for (const [key, rows] of consumptionsByOutRef)
        consumptionsByOutRef.set(
          key,
          rows.filter((row) => !removed.has(row.header_hash)),
        );
      control.canonicalBranchSelected = true;
      if (nativeTipMode === "controlled") control.tip = snapshotTip(tip);
      await persistBlocks();
      await persistControl();
    });
  const rollbackNativeStream = (point: SyntheticNativeTip | "origin") =>
    appendStreamCommand({
      kind: "rollback",
      point: point === "origin" ? point : snapshotTip(point),
    });
  const holdExactQueries = (held: boolean) =>
    changeControl(async () => {
      control.exactQueriesHeld = held;
      await persistControl();
    });
  const exitNativeStream = (exitCode = 1) => {
    if (!Number.isSafeInteger(exitCode) || exitCode < 1 || exitCode > 255)
      throw new Error(
        "Synthetic native stream exit code is outside fixture bounds",
      );
    return appendStreamCommand({ kind: "exit", exitCode });
  };
  const readNativeQueries = async (): Promise<
    readonly SyntheticNativeQuery[]
  > => {
    const text = await readFile(queryLogPath, "utf8");
    return Object.freeze(
      text
        .slice(0, text.lastIndexOf("\n") + 1)
        .split("\n")
        .filter(Boolean)
        .map((line) => {
          const query = JSON.parse(line) as SyntheticNativeQuery;
          return Object.freeze({
            target: snapshotTip(query.target),
            tip: snapshotTip(query.tip),
          });
        }),
    );
  };
  let closing: Promise<void> | undefined;
  const close = () =>
    (closing ??= (async () => {
      await controlWrites;
      control.closed = true;
      const stopControl = await Promise.allSettled([persistControl()]);
      const outcomes = await Promise.allSettled(
        closed
          .splice(0)
          .reverse()
          .map((stop) => stop()),
      );
      const transports = await Promise.allSettled([
        closeSharedL1NodeTransports(),
      ]);
      const cleanup = await Promise.allSettled([
        rm(dir, { recursive: true, force: true }),
        Promise.resolve().then(() => vi.unstubAllGlobals()),
      ]);
      const errors = [
        ...stopControl,
        ...outcomes,
        ...transports,
        ...cleanup,
      ].flatMap((result) =>
        result.status === "rejected" ? [result.reason] : [],
      );
      if (errors.length > 0)
        throw new AggregateError(
          errors,
          "Synthetic origin fixture cleanup failed",
        );
    })());
  return {
    deployment,
    deploymentIdentity,
    scriptBinding,
    watcherConfig,
    l1NodeTransportBinaryPath: binaryPath,
    activationBlock,
    emptySuccessorBlock,
    activationTransactionCbor: initialization.transactionCbor,
    initializationBodyCbor: initializationTransaction.body().to_cbor_hex(),
    makeBlock,
    setNativeTip,
    appendNativeBlock,
    growNativeTip,
    rollbackNativeStream,
    holdExactQueries,
    selectCanonicalBranch,
    exitNativeStream,
    readNativeQueries,
    close,
  };
};
