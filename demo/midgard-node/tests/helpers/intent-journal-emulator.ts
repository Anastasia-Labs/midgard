/**
 * The node's intent journal (§8.2) and S6 (§8.3) over a Lucid emulator:
 * a Postgres follower store carrying the intent journal, the emulator's
 * landed transactions applied to it as blocks, and a node transport whose
 * mempool, submission and ledger-state answers are the emulator's.
 *
 * - Every transaction the emulator accepts is captured as its exact bytes;
 *   `follow` applies the ones confirmed since the last call, one block per
 *   emulator block height, with synthetic block hashes.
 * - A fork is simulated on the follower side: `applyForkBlock` applies a
 *   block the emulator never sees, and `rewindTo` rolls the follower back
 *   (what the emulator confirmed is followed again).
 * - The journal records through the production `recordSignedIntent`, on a
 *   node SQL client over the same database, as `IntentJournalLive` does.
 * - The wallet seed is the production seeder's, answered from the
 *   emulator's UTxO set at the asked addresses.
 *
 * Differences from a followed chain, none of which these tests rely on:
 * synthetic block hashes, a block only where a transaction landed, and no
 * redeemers in the stored summaries (wallet transactions carry none).
 */
import { createHash } from "node:crypto";

import {
  type BlockSummary,
  compareOutRefs,
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  openPostgresFactStore,
  type OutputSummary,
  projectionStoreOptions,
  type TxSummary,
} from "@al-ft/midgard-l1-follower";
import { encodeUtxoAnswer } from "@al-ft/midgard-l1-follower/testing";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import {
  CML,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect, Layer, ManagedRuntime, Redacted } from "effect";

import { migrationByVersion } from "../../src/database/migrations/index.js";
import * as MigrationRunner from "../../src/database/migrations/runner.js";
import {
  IntentJournal,
  intentJournalOver,
  type IntentJournalService,
} from "../../src/services/intent-journal.js";
import { protocolPaymentCredentials } from "../../src/services/intent-journal.tracked-set.js";
import { nodeFamilyPredicate } from "../../src/services/l1-follower.intent-predicates.js";
import { createNodeIntentStage } from "../../src/services/l1-follower.intents.js";
import { selectNodeWallet } from "../../src/transactions/utils.wallet-view.js";
import { ledgerOutput } from "./intent-journal-replay.chain.js";
import type { testDatabases } from "./l1-events-store.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";

export const EMULATOR_K = 6;

/** `0010_intent_refusal_holds`. */
const INTENT_REFUSAL_HOLDS_MIGRATION = 10;

const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

const blockHash = (height: number): Buffer =>
  createHash("sha256").update(`intent-emulator-block-${height}`).digest();

/** A transaction's summary as the follower stores it, from its exact bytes. */
const txSummary = (cbor: Buffer, index: number): TxSummary => {
  const decoded = decodeTransaction(cbor);
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  return {
    hash: decoded.hash,
    index,
    isValid: true,
    bodyCbor: decoded.bodyCbor,
    witnessCbor: Buffer.from(tx.witness_set().to_cbor_bytes()),
    auxCbor: null,
    inputs: [...decoded.inputs].sort(compareOutRefs),
    referenceInputs: [...decoded.referenceInputs].sort(compareOutRefs),
    collaterals: [...decoded.collaterals].sort(compareOutRefs),
    outputs: decoded.outputs,
    collateralReturn: decoded.collateralReturn,
    mint: decoded.mint,
    withdrawals: decoded.withdrawals,
    redeemers: [],
    invalidBefore: decoded.invalidBefore,
    invalidAfter: decoded.invalidAfter,
  };
};

export type IntentEmulator = Awaited<ReturnType<typeof attachIntentFollower>>;

/**
 * An emulator with the node's own wallet (`own`) and a wallet that only
 * receives (`payee`), and the node's follower store at the emulator's
 * origin, tracking the own wallet.
 *
 * With `nodeSchema`, the database is migrated to the node's full schema
 * before the follower store opens there, as the node does, so a test can
 * run the node's own durable writes (a pending block finalization, the
 * history authority) in the database the journal records in.
 */
export const openIntentEmulator = async (
  databases: ReturnType<typeof testDatabases>,
  options: Readonly<{ nodeSchema?: boolean }> = {},
) => {
  const own: EmulatorAccount = generateEmulatorAccount({
    lovelace: 50_000_000n,
  });
  const payee = generateEmulatorAccount({ lovelace: 5_000_000n });
  return attachIntentFollower(databases, {
    emulator: new Emulator([own, payee]),
    own,
    payee,
    nodeSchema: options.nodeSchema,
  });
};

/**
 * The node's follower store and intent journal attached to `emulator` as it
 * stands (a fixture's deployment, say), tracking `own`'s wallet: the
 * follower's origin is the emulator's current slot, what the emulator
 * confirmed before it is history the follower never applies, and the first
 * `stage.run()` seeds the own wallet from the emulator's ledger. Its
 * outputs are seeded with their value and datum but no reference script,
 * so the own wallet must hold none; `payee` only receives.
 */
export const attachIntentFollower = async (
  databases: ReturnType<typeof testDatabases>,
  options: Readonly<{
    emulator: Emulator;
    own: Readonly<{ seedPhrase: string; address: string }>;
    payee: Readonly<{ seedPhrase: string; address: string }>;
    nodeSchema?: boolean;
    /**
     * The deployed protocol, tracked as the node tracks it (§8.2): its
     * validators' payment credentials and the hub oracle policy, with the
     * reference-script addresses seeded beside the own wallet. Its outputs
     * already on the emulator's ledger are seeded too, standing in for the
     * history a node following from before protocol init would hold.
     */
    protocol?: Readonly<{
      contracts: SDK.MidgardValidators;
      referenceScriptAddresses: readonly string[];
    }>;
  }>,
) => {
  const { emulator, own, payee, protocol } = options;
  const protocolCredentials = new Set(
    protocol === undefined
      ? []
      : protocolPaymentCredentials(protocol.contracts),
  );
  const seededAddresses = [
    ...new Map(
      [
        own.address,
        ...(protocol?.referenceScriptAddresses ?? []),
        ...Object.values(emulator.ledger)
          .filter(({ spent }) => !spent)
          .map(({ utxo }) => utxo.address)
          .filter((address) =>
            protocolCredentials.has(
              getAddressDetails(address).paymentCredential?.hash ?? "",
            ),
          ),
      ].map((address) => [address, addressBytes(address)]),
    ).values(),
  ];
  const accepted = new Map<string, Buffer>();
  const submitTx = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (tx) => {
    const hash = await submitTx(tx);
    accepted.set(hash, Buffer.from(tx, "hex"));
    return hash;
  };
  /**
   * The node's wallet. Its slot mapping starts at the emulator's slot 0, as
   * the fixture's own instances do (an instance made later would start at the
   * then-current slot, and a validity bound before it is out of range for
   * local script evaluation).
   */
  const wallet = async (): Promise<LucidEvolution> => {
    const lucid = await Lucid(emulator, "Custom", {
      slotConfig: {
        zeroTime: emulator.now() - emulator.slot * 1000,
        zeroSlot: 0,
        slotLength: 1000,
      },
    });
    selectNodeWallet(lucid, own.seedPhrase);
    return lucid;
  };
  const ownAddress = addressBytes(own.address);
  const connectionString = await databases.create();
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(connectionString) }),
  );
  const sql = await runtime.runPromise(SqlClient.SqlClient);
  if (options.nodeSchema === true)
    await runtime.runPromise(
      MigrationRunner.migrate({ appVersion: "test", actor: "intent-emulator" }),
    );
  const store: FactStore = openPostgresFactStore({
    ...projectionStoreOptions(
      [intentJournalProjection],
      {
        securityParameter: EMULATOR_K,
        trackedSet: {
          addresses: new Set([ownAddress.toString("hex")]),
          paymentCredentials: protocolCredentials,
          policies: new Set(
            protocol === undefined
              ? []
              : [protocol.contracts.hubOracle.policyId],
          ),
        },
      },
      "postgres",
    ),
    connection: { connectionString },
  });
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`store start: ${JSON.stringify(started)}`);
  const origin = { slot: emulator.slot, hash: blockHash(0) };
  const init = await store.initialize({ point: origin, height: 0 });
  if (init.kind !== "initialized") throw new Error(`initialize: ${init.kind}`);

  let tip = { hash: origin.hash, height: 0 };
  /** The point of every block applied so far, by height (0: the origin). */
  const points = new Map([[0, origin]]);
  /**
   * Emulator-confirmed hashes applied, and the height each was applied at;
   * those confirmed before the origin count as applied at it.
   */
  const applied = new Map<string, number>(
    Object.entries(emulator.transactionHistory)
      .filter(([, status]) => status.status === "confirmed")
      .map(([hash]) => [hash, 0]),
  );
  const applyNext = async (slot: number, txs: readonly Buffer[]) => {
    const blockHeight = tip.height + 1;
    const block: BlockSummary = {
      point: { slot, hash: blockHash(blockHeight) },
      height: blockHeight,
      parentHash: tip.hash,
      txs: txs.map((cbor, index) => txSummary(cbor, index)),
    };
    const result = await store.applyBlock(block);
    if (result.kind !== "applied") throw new Error(`apply: ${result.kind}`);
    tip = { hash: block.point.hash, height: blockHeight };
    points.set(blockHeight, block.point);
  };
  /** Applies the transactions the emulator confirmed since the last call. */
  const follow = async (): Promise<void> => {
    const byHeight = new Map<number, { slot: number; hashes: string[] }>();
    for (const [hash, status] of Object.entries(emulator.transactionHistory))
      if (status.status === "confirmed" && !applied.has(hash)) {
        const block = byHeight.get(status.blockHeight) ?? {
          slot: status.slot,
          hashes: [],
        };
        block.hashes.push(hash);
        byHeight.set(status.blockHeight, block);
      }
    for (const height of [...byHeight.keys()].sort((a, b) => a - b)) {
      const { slot, hashes } = byHeight.get(height)!;
      // In the order the emulator accepted them, so a transaction spending
      // another's output in the same block comes after it.
      const order = [...accepted.keys()];
      hashes.sort((a, b) => order.indexOf(a) - order.indexOf(b));
      await applyNext(
        slot,
        hashes.map((hash) => {
          const cbor = accepted.get(hash);
          if (cbor === undefined)
            throw new Error(`the emulator confirmed ${hash} unseen`);
          return cbor;
        }),
      );
      for (const hash of hashes) applied.set(hash, tip.height);
    }
  };
  /**
   * A fork's block the emulator never sees: `txs` applied to the follower
   * only, one slot after the emulator's, as the next block.
   */
  const applyForkBlock = (txs: readonly Buffer[]) =>
    applyNext(emulator.slot + 1, txs);
  /** Rolls the follower back to the block at `height` (0: the origin). */
  const rewindTo = async (height: number): Promise<void> => {
    const target = points.get(height);
    if (target === undefined) throw new Error(`no block at height ${height}`);
    const result = await store.rewind(target);
    if (result.kind !== "rewound")
      throw new Error(`rewind: ${JSON.stringify(result)}`);
    for (const above of [...points.keys()].filter((h) => h > height))
      points.delete(above);
    // The emulator still holds what was rolled back: `follow` applies it again.
    for (const [hash, at] of applied) if (at > height) applied.delete(hash);
    tip = { hash: target.hash, height };
  };

  const sent: Buffer[] = [];
  const transport = {
    hasTx: (txId: string) =>
      Promise.resolve(emulator.transactionHistory[txId]?.status === "pending"),
    submit: async (bytes: Uint8Array) => {
      sent.push(Buffer.from(bytes));
      try {
        await emulator.submitTx(Buffer.from(bytes).toString("hex"));
        return { accepted: true } as const;
      } catch (error) {
        return {
          accepted: false,
          rejection: Buffer.from(String(error)),
        } as const;
      }
    },
    withLedgerState: <T>(
      _at: unknown,
      use: (session: {
        query: (query: { addresses: readonly Buffer[] }) => Promise<Uint8Array>;
      }) => Promise<T>,
    ): Promise<T> =>
      use({
        query: async ({ addresses }) => {
          const wanted = new Set(addresses.map((a) => a.toString("hex")));
          const utxos = Object.values(emulator.ledger)
            .filter(({ spent }) => !spent)
            .map(({ utxo }) => utxo)
            .filter((utxo) =>
              wanted.has(addressBytes(utxo.address).toString("hex")),
            );
          return encodeUtxoAnswer(utxos.map(ledgerOutput));
        },
      }),
  };

  // The node table the journal keeps its refusal holds in, as the node
  // database (where the follower's tables live) has it.
  if (options.nodeSchema !== true)
    await runtime.runPromise(
      sql.unsafe(migrationByVersion.get(INTENT_REFUSAL_HOLDS_MIGRATION)!.sql),
    );
  const isOwnOutput = (output: OutputSummary) =>
    output.address.equals(ownAddress);
  /** The production journal over the node database (`IntentJournalLive`'s). */
  const journal: IntentJournalService = intentJournalOver(sql, isOwnOutput);
  const journalLayer = Layer.succeed(IntentJournal, journal);
  const record = (
    ...args: Parameters<IntentJournalService["record"]>
  ): Promise<unknown> => Effect.runPromise(journal.record(...args));

  const stage = createNodeIntentStage({
    store,
    transport: transport as never,
    securityParameter: EMULATOR_K,
    seededAddresses,
    wanted: nodeFamilyPredicate({
      store,
      stateQueue: SIM_QUEUE_CONFIG,
      operatorSet: null,
      slotToPosixMs: (slot) => slot * 1000,
      horizonLagBlocks: 0,
    }),
    log: () => undefined,
  });

  const close = async () => {
    stage.close();
    await runtime.dispose();
    await store.close();
  };
  return {
    connectionString,
    /** A node SQL client over the same database, for the test's own writes. */
    sql,
    runtime,
    emulator,
    own,
    payee,
    wallet,
    store,
    follow,
    applyForkBlock,
    rewindTo,
    sent,
    accepted,
    record,
    journal,
    journalLayer,
    stage,
    close,
  };
};

/** A signed payment from the own wallet; `wallet` picks the instance (and so its UTxO view). */
export const signedPayment = async (
  lucid: LucidEvolution,
  to: string,
  lovelace: bigint,
) => {
  const signed = await (
    await lucid.newTx().pay.ToAddress(to, { lovelace }).complete()
  ).sign
    .withWallet()
    .complete();
  return { cbor: signed.toCBOR(), hash: signed.toHash() };
};
