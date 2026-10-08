import { join } from "node:path";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  decodeBlock,
  LOOP_PRUNE_BUDGET,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import { cbor } from "@al-ft/midgard-l1-follower/testing";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  loadCommitteeConfig,
  type LoadedCommitteeConfig,
} from "../../src/config.js";
import {
  committeeAvailabilityReads,
  type FollowerPoint,
} from "../../src/l1/follower/availability-reads.js";
import { ownWallets } from "../../src/l1/follower/committee-follower-config.js";
import {
  committeeFollowerStoreOptions,
  type CommitteeL1Readiness,
  depthParameters,
} from "../../src/l1/follower/l1-follower.js";
import {
  committeeL1Retention,
  pruneAfterPins,
} from "../../src/l1/follower/retention-pins.js";
import { tempDir } from "../helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "./committee-config-files.js";

const CHAIN_INDEX = /kupo|ogmios|kupmios/iu;

/** Every key or string under `value` that names a chain index. */
const chainIndexMentions = (value: unknown, path: string): string[] => {
  if (typeof value === "string")
    return CHAIN_INDEX.test(value) ? [`${path}=${value}`] : [];
  if (Array.isArray(value))
    return value.flatMap((item, index) =>
      chainIndexMentions(item, `${path}[${index.toString()}]`),
    );
  if (typeof value === "object" && value !== null)
    return Object.entries(value).flatMap(([key, child]) => [
      ...(CHAIN_INDEX.test(key) ? [`${path}.${key}`] : []),
      ...chainIndexMentions(child, `${path}.${key}`),
    ]);
  return [];
};

/**
 * A committee config loaded from an environment and manifest in which no key
 * or value names Kupo or Ogmios, with the emulator account's mnemonic as the
 * availability submitter. Throws if a chain-index key appears anywhere in
 * the environment or the loaded config.
 */
export const noChainIndexCommitteeConfig = async (
  seedPhrase: string,
): Promise<LoadedCommitteeConfig> => {
  const dir = await tempDir();
  const files = await writeConfigFiles(dir, libp2pManifest("01".repeat(32)));
  const env = {
    ...libp2pConfigEnv(files.manifestPath, files.deploymentInfoPath),
    DA_AVAILABILITY_SUBMITTER_KEY_SOURCE: `mnemonic:${seedPhrase}`,
    DA_AVAILABILITY_JOURNAL_PATH: join(dir, "availability-journal.sqlite"),
  };
  const config = await loadCommitteeConfig(env);
  const mentions = [
    ...chainIndexMentions(env, "env"),
    ...chainIndexMentions(config, "config"),
  ];
  if (mentions.length > 0)
    throw new Error(`a chain index is configured: ${mentions.join(", ")}`);
  return config;
};

/**
 * A Lucid emulator wallet that signs the committee's transactions. Its coins
 * stand in for protocol UTxOs: the committee judges a transaction by its
 * signed bytes and the spends of its inputs, not by who owns them.
 */
export const emulatorWallet = async () => {
  const account = generateEmulatorAccount({ lovelace: 500_000_000n });
  const emulator = new Emulator([account]);
  emulator.awaitBlock(5);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const address = await lucid.wallet().address();
  return {
    account,
    emulator,
    lucid,
    address,
    /** Splits `count` 20 ADA coins off the wallet: the signed split and its coins. */
    split: async (count: number) => {
      const builder = lucid.newTx();
      for (let index = 0; index < count; index += 1)
        builder.pay.ToAddress(address, { lovelace: 20_000_000n });
      const signed = await (await builder.complete()).sign
        .withWallet()
        .complete();
      await signed.submit();
      emulator.awaitBlock();
      const coins = (await lucid.wallet().getUtxos()).filter(
        (utxo) =>
          utxo.txHash === signed.toHash() &&
          utxo.assets.lovelace === 20_000_000n,
      );
      return { cbor: signed.toCBOR(), txHash: signed.toHash(), coins };
    },
    /**
     * The signed bytes of a transaction spending `inputs` and paying
     * `lovelace` back, with `collateral` on its body and a validity window
     * around the emulator's clock when given.
     */
    spend: async (
      input: Readonly<{
        inputs: readonly UTxO[];
        lovelace: bigint;
        collateral?: UTxO;
      }>,
    ): Promise<string> => {
      const builder = lucid
        .newTx()
        .collectFrom([...input.inputs])
        .pay.ToAddress(address, { lovelace: input.lovelace });
      if (input.collateral !== undefined)
        builder
          .validFrom(emulator.now() - 60_000)
          .validTo(emulator.now() + 60_000);
      const built = (
        await builder.complete({ coinSelection: false })
      ).toTransaction();
      const body = built.body();
      if (input.collateral !== undefined) {
        const collateral = CML.TransactionInputList.new();
        collateral.add(
          CML.TransactionInput.new(
            CML.TransactionHash.from_hex(input.collateral.txHash),
            BigInt(input.collateral.outputIndex),
          ),
        );
        body.set_collateral_inputs(collateral);
      }
      return (
        await lucid
          .fromTx(
            CML.Transaction.new(
              body,
              built.witness_set(),
              true,
              built.auxiliary_data(),
            ).to_cbor_hex(),
          )
          .sign.withWallet()
          .complete()
      ).toCBOR();
    },
  };
};

/** The id of a signed transaction. */
export const transactionId = (signedCbor: string): string =>
  CML.hash_transaction(
    CML.Transaction.from_cbor_hex(signedCbor).body(),
  ).to_hex();

/** A signed transaction to land, and whether it passes phase 2. */
export type LandedTx = Readonly<{ cbor: string; valid?: boolean }>;

const ORIGIN: FollowerPoint = {
  slot: 0,
  blockHash: "0e".repeat(32),
  blockNo: 0,
};

/**
 * The emulator follower's default k: small, so a test runs more than k + 2
 * blocks with the follower pruning, as production does after every block at
 * its tip. It is above the manifest's confirmation depth.
 */
export const EMULATOR_SECURITY_PARAMETER = 16;

/**
 * The committee's follower store, opened with the production store options
 * for `config` (its projection, tracked set and own wallets) at k =
 * `securityParameter`, fed with blocks that carry the exact bytes of
 * transactions the Lucid emulator signed. After every block it prunes
 * exactly as the follow loop does at its tip, through the committee's
 * retention pins (`retention`; bind the `records` holder to the committee
 * store's pin targets). The reads and the Lucid observer the availability,
 * promise and retirement paths use are the production ones over it; the
 * node transport answers that no transaction is in the mempool and no
 * untracked output exists.
 */
export const emulatorFollower = async (
  config: LoadedCommitteeConfig,
  securityParameter = EMULATOR_SECURITY_PARAMETER,
) => {
  const store = openSqliteFactStore({
    ...committeeFollowerStoreOptions(
      config,
      { ...depthParameters(config), securityParameter },
      await ownWallets(config),
      "sqlite",
    ),
    path: ":memory:",
  });
  const retention = committeeL1Retention(store);
  const pruning = pruneAfterPins(store, retention);
  /** Each prune that failed, as the follow loop logs it: the step is skipped. */
  const pruneErrors: string[] = [];
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`follower store did not start: ${started.kind}`);
  const initialized = await store.initialize({
    point: { slot: ORIGIN.slot, hash: Buffer.from(ORIGIN.blockHash, "hex") },
    height: ORIGIN.blockNo,
  });
  if (initialized.kind !== "initialized")
    throw new Error(`follower store did not initialize: ${initialized.kind}`);
  const chain: FollowerPoint[] = [ORIGIN];
  let branch = 0;
  let held: readonly CommitteeL1Readiness[] = [];
  const tip = (): FollowerPoint => chain[chain.length - 1]!;

  /** Applies one block holding `txs` at `slot` (default: the next slot). */
  const forward = async (
    txs: readonly LandedTx[] = [],
    slot = tip().slot + 1,
  ): Promise<FollowerPoint> => {
    const parent = tip();
    const parsed = txs.map((tx) => CML.Transaction.from_cbor_hex(tx.cbor));
    const raw = cbor.array(
      cbor.array(
        cbor.array(
          cbor.uint(parent.blockNo + 1),
          cbor.uint(slot),
          cbor.bytes(Buffer.from(parent.blockHash, "hex")),
          cbor.uint(branch),
        ),
        cbor.bytes(Buffer.alloc(8)),
      ),
      cbor.array(...parsed.map((tx) => Buffer.from(tx.body().to_cbor_bytes()))),
      cbor.array(
        ...parsed.map((tx) => Buffer.from(tx.witness_set().to_cbor_bytes())),
      ),
      cbor.map(
        ...parsed.flatMap((tx, index): [Buffer, Buffer][] => {
          const aux = tx.auxiliary_data();
          return aux === undefined
            ? []
            : [[cbor.uint(index), Buffer.from(aux.to_cbor_bytes())]];
        }),
      ),
      cbor.array(
        ...txs.flatMap((tx, index) =>
          tx.valid === false ? [cbor.uint(index)] : [],
        ),
      ),
    );
    const block = decodeBlock(raw);
    block.txs.forEach((tx, index) => {
      const expected = CML.hash_transaction(parsed[index]!.body()).to_hex();
      if (tx.hash.toString("hex") !== expected)
        throw new Error(`block carries ${expected} with other body bytes`);
    });
    const applied = await store.applyBlock(block);
    if (applied.kind !== "applied")
      throw new Error(`block did not apply: ${applied.kind}`);
    // The follow loop's prune at its tip (`maybePrune`): a failure is
    // logged and the loop goes on.
    const pruned = await pruning
      .prune(LOOP_PRUNE_BUDGET)
      .catch((error: unknown) => ({
        kind: "error" as const,
        error: error instanceof Error ? error : new Error(String(error)),
      }));
    if ("kind" in pruned && pruned.kind === "error")
      pruneErrors.push(pruned.error.message);
    const point = {
      slot,
      blockHash: block.point.hash.toString("hex"),
      blockNo: block.height,
    };
    chain.push(point);
    return point;
  };
  const provider = new L1FollowerProvider({
    store,
    transport: {
      hasTx: async () => false,
      query: async () => cbor.map(),
    } as unknown as L1NodeTransport,
  });
  return {
    config,
    store,
    /** The follower's k. */
    securityParameter,
    retention,
    pruneErrors,
    /** The slot the follower pruned through, or null before its first prune. */
    prunedThroughSlot: async (): Promise<number | null> => {
      const rows = await store.transaction("read", (tx) =>
        tx.query("SELECT pruned_through_slot FROM l1_follower_cursor"),
      );
      const slot = rows[0]?.pruned_through_slot;
      return slot === null || slot === undefined ? null : Number(slot);
    },
    tip,
    forward,
    /** Applies `count` empty blocks. */
    empty: async (count: number): Promise<FollowerPoint> => {
      for (let index = 0; index < count; index += 1) await forward();
      return tip();
    },
    /** Rolls the follower back to `point`; later blocks form a new branch. */
    rollBackTo: async (point: FollowerPoint): Promise<void> => {
      const rewound = await store.rewind({
        slot: point.slot,
        hash: Buffer.from(point.blockHash, "hex"),
      });
      if (rewound.kind !== "rewound")
        throw new Error(`follower did not roll back: ${rewound.kind}`);
      while (tip().blockHash !== point.blockHash) chain.pop();
      branch += 1;
    },
    /** Holds every read with the follower's readiness reasons. */
    hold: (reasons: readonly CommitteeL1Readiness[]) => {
      held = reasons;
    },
    reads: committeeAvailabilityReads({
      store,
      readiness: () => held,
      retention,
    }),
    /** The observer's Lucid reads, through the follower's provider. */
    lucid: {
      transactionStatus: (txHash: string) =>
        provider.getTransactionStatus(txHash),
      utxosByOutRef: (refs: Parameters<LucidEvolution["utxosByOutRef"]>[0]) =>
        provider.getUtxosByOutRef(refs),
    } as unknown as LucidEvolution,
    close: () => store.close(),
  };
};

export type EmulatorFollower = Awaited<ReturnType<typeof emulatorFollower>>;
