import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  Lucid as makeLucid,
} from "@lucid-evolution/lucid";
import { Effect, Logger } from "effect";
import { Level } from "level";

import * as TxAdmissionsDB from "../../src/database/txAdmissions.js";
import {
  Columns as LedgerColumns,
  type MinimalEntry,
} from "../../src/database/utils/ledger.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../../src/database/utils/tx.js";
import { MidgardMpf, processMpfs } from "../../src/mpf/index.js";
import type { Lucid } from "../../src/services/index.js";
import {
  encodeNativeMpfEventLog,
  ProductionNativeMpfOwnerService,
} from "../../src/services/mpf-native-owner/index.js";
import { assertExactCommitRescueDaFrame } from "../../src/workers/commit-block-header.assert-rescue-frame.js";
import {
  buildNativeTx,
  expectBothPhasesAcceptOne,
  runBothPhases,
} from "../native-transaction-integration.build-native-tx.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinarySha256,
} from "./native-owner-binary.js";

export const delayProgram = (nodes: number) => {
  const material: MidgardCekProgramMaterialEntry[] = [];
  const append = (node: Parameters<typeof encodeMidgardCekTermNode>[0]) => {
    const entry = {
      kind: "term" as const,
      root: hashMidgardCekTermNode(node),
      preimage: encodeMidgardCekTermNode(node),
    };
    material.push(entry);
    return entry.root;
  };
  let root = append({ kind: "error" });
  for (let i = 1; i < nodes; i += 1)
    root = append({ kind: "delay", body: root });
  return {
    envelope: {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: root,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (sum, item) => sum + BigInt(item.preimage.length),
        0n,
      ),
    },
    material,
  };
};

export const runNativeFrameDatabase = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

/** Real canonical signatures, Phase A/B, SQL sidecars and native MPF builds.
 * The parent is a model ConfirmedState; this does not claim an L1 receipt. */
export const openNativeFrameFixture = async (
  options: { readonly timeMs?: number } = {},
) => {
  await runNativeFrameDatabase(resetApplicationTables);
  const privateKey = CML.PrivateKey.generate_ed25519();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  ).to_address();
  const addressBytes = Buffer.from(address.to_raw_bytes());
  const api = await makeLucid(
    new Emulator([
      {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: address.to_bech32(),
        assets: { lovelace: 100_000_000_000n },
      },
    ]),
    "Preprod",
  );
  api.selectWallet.fromPrivateKey(privateKey.to_bech32());
  const lucid = { api, switchToOperatorsMainWallet: Effect.void } as Lucid;
  const slotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
  const time = options.timeMs ?? slotConfig.zeroTime + 10_000;
  const output = (lovelace: bigint, datumBytes = 0, scriptBytes?: Buffer) =>
    encodeMidgardTxOutput({
      address: addressBytes,
      value: { lovelace, assets: new Map() },
      ...(datumBytes === 0
        ? {}
        : {
            datum: {
              kind: "inline" as const,
              cbor: Buffer.from(
                Data.to(Buffer.alloc(datumBytes, 7).toString("hex")),
                "hex",
              ),
            },
          }),
      ...(scriptBytes === undefined
        ? {}
        : { script_ref: { language: "PlutusV3" as const, scriptBytes } }),
    });
  const inputRef = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 91),
    outputIndex: 0,
  });
  const initial: readonly MinimalEntry[] = [
    {
      [LedgerColumns.OUTREF]: inputRef,
      [LedgerColumns.OUTPUT]: output(50_000_000_000n),
    },
  ];
  const directory = await mkdtemp(join(tmpdir(), "midgard-c2b-native-frame-"));
  const levelPath = join(directory, "ledger");
  const seed = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
  await seed.open();
  await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
  await seed.close();
  const native = await ProductionNativeMpfOwnerService.create({
    levelPath,
    sidecarPath: join(directory, "sidecar"),
    binaryPath: nativeOwnerBinaryPath,
    binarySha256: nativeOwnerBinarySha256(),
  });
  const initialHandle = await native.fork(SDK.EMPTY_MERKLE_TREE_ROOT);
  const initialized = await native.applyEvents(
    initialHandle,
    encodeNativeMpfEventLog(SDK.EMPTY_MERKLE_TREE_ROOT, [
      initial.map((row) => ({
        type: "insert" as const,
        key: row[LedgerColumns.OUTREF],
        value: row[LedgerColumns.OUTPUT],
      })),
    ]),
  );
  await native.promote(initialHandle);
  const parent = (root: string): SDK.StateQueueUTxO => ({
    assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    utxo: {
      txHash: "11".repeat(32),
      outputIndex: 0,
      address: address.to_bech32(),
      assets: { lovelace: 3_000_000n },
    },
    datum: {
      key: "Empty",
      next: "Empty",
      data: SDK.castConfirmedStateToData({
        headerHash: "22".repeat(28),
        prevHeaderHash: "00".repeat(28),
        utxoRoot: root,
        startTime: BigInt(time - 2_000),
        endTime: BigInt(time - 1_000),
        protocolVersion: 1n,
      }) as SDK.LinkedListNodeView["data"],
    },
  });
  const signed = (
    spend: Buffer,
    references: readonly Buffer[],
    outputs: readonly Buffer[],
  ) =>
    buildNativeTx({
      witnessMode: "valid",
      witnessSignerPrivateKey: privateKey,
      spendInputOutRefs: [spend],
      referenceInputOutRefs: references,
      outputCbors: outputs,
    });
  const entry = (tx: ReturnType<typeof signed>): EntryWithTimeStamp => ({
    [TxColumns.TX_ID]: tx.txId,
    [TxColumns.TX]: tx.txCbor,
    [TxColumns.TIMESTAMPTZ]: new Date(time - 500),
  });
  const validate = async (
    tx: ReturnType<typeof signed>,
    base: readonly MinimalEntry[],
    material: readonly MidgardCekProgramMaterialEntry[],
  ) => {
    const result = await runBothPhases(
      tx.txId,
      tx.txCbor,
      new Map(
        base.map((row) => [
          row[LedgerColumns.OUTREF].toString("hex"),
          row[LedgerColumns.OUTPUT],
        ]),
      ),
      undefined,
      material,
    );
    expectBothPhasesAcceptOne(result);
    return result;
  };
  const build = async (
    tx: ReturnType<typeof signed>,
    base: readonly MinimalEntry[],
    root: string,
    material: readonly MidgardCekProgramMaterialEntry[],
  ) => {
    await runNativeFrameDatabase(
      TxAdmissionsDB.tryInsert({
        txId: tx.txId,
        txCanonicalCbor: tx.txCbor,
        programMaterialSidecarCbor:
          encodeMidgardCekProgramMaterialSidecar(material),
        submitSource: "native",
      }),
    );
    const handle = await native.fork(root);
    const client = {
      applyEvents: (...args: Parameters<typeof native.applyEvents>) =>
        native.applyEvents(...args),
    };
    let builds = 0;
    const countedClient = {
      ...client,
      fork: native.fork.bind(native),
      discard: native.discard.bind(native),
      applyEvents: (...args: Parameters<typeof native.applyEvents>) => {
        builds += 1;
        return client.applyEvents(...args);
      },
    };
    const transactions = await runNativeFrameDatabase(
      MidgardMpf.createScratch("c2b-native-frame-transactions"),
    );
    try {
      const processed = await runNativeFrameDatabase(
        processMpfs(transactions, [entry(tx)], {
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          currentBlockStartTime: new Date(time - 1_000),
          fixedBlockEndTime: new Date(time),
          fixedBlockTime: new Date(time),
          initialLedgerEntries: base,
          selectedBaseUtxoRoot: root,
          payloadRootCheck: "off",
          nativeMpf: {
            client: countedClient,
            handle,
            ownerBinarySha256: nativeOwnerBinarySha256(),
          } as never,
          forcedValidation: {
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            bucketConcurrency: 1,
            l1SlotConfig: slotConfig,
            slotForUnixTime: (ms: number) =>
              BigInt(
                Math.floor((ms - slotConfig.zeroTime) / slotConfig.slotLength) +
                  slotConfig.zeroSlot,
              ),
          },
        }),
      );
      return { processed, handle, builds };
    } finally {
      await runNativeFrameDatabase(transactions.close());
    }
  };
  const exact = (
    processed: Awaited<ReturnType<typeof build>>["processed"],
    root: string,
  ) =>
    runNativeFrameDatabase(
      assertExactCommitRescueDaFrame({
        processed,
        parent: parent(root),
        lucid,
        profile: MIDGARD_CONSENSUS_PROFILE,
        endTimeMs: time,
        blockTimeMs: time,
        slotConfig,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
      }),
    );
  return {
    privateKey,
    addressBytes,
    output,
    initial,
    inputRef,
    root: initialized.candidateRoot,
    native,
    signed,
    entry,
    validate,
    build,
    exact,
    parent,
    lucid,
    slotConfig,
    time,
    close: async () => {
      await native.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

export const programScript = (program: ReturnType<typeof delayProgram>) =>
  encodeMidgardCekProgramEnvelope(program.envelope);
