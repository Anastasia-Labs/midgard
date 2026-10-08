/**
 * Real transaction shapes for the node intent journal (I1-fix F5). A
 * family's real emulator flow runs as it always does; this replays what it
 * journaled onto a production follower that followed the same chain.
 *
 * - Every transaction an emulator accepts is kept as its exact bytes, and
 *   the emulator's genesis outputs as they stood before its first
 *   submission (`helpers/emulator-chain-capture.ts`).
 * - Every journaled intent a flow hands to the journal of a process with no
 *   follower is kept (`withoutFollowerJournal`, `helpers/intent-journal.ts`).
 * - `replayJournaledOnFollower` opens a follower store with the node's
 *   projections and its production tracked set (own wallets and
 *   reference-script addresses seeded at the origin, protocol payment
 *   credentials, the hub oracle policy), applies the emulator's blocks as
 *   the follower's block decoder reads them, and records each landed
 *   intent through the production journal at the block before the one it
 *   landed in: the latest tip the flow could have recorded it at, with
 *   every input confirmed or created by an intent recorded before it. S6
 *   runs there, so the family's §8.4 predicate is evaluated on the intent's
 *   real content reference; then the landing block applies.
 *
 * The flows do not run on this follower in-flow: their fixtures stand in
 * for the follower by writing the node database's follower tables
 * directly, and a follower store over the same tables would contend with
 * them. Differences from a followed chain: synthetic block headers, a
 * block with no transactions at an emulator height that confirmed none (at
 * the next slot), and a slot moved up by one where the emulator gives two
 * blocks one slot.
 */
import {
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  openPostgresFactStore,
  type OutputSummary,
  type OutRef,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import { eventProjectionConfigFromContracts } from "@al-ft/midgard-l1-follower/events";
import { eventProjection } from "@al-ft/midgard-l1-follower/events";
import { encodeUtxoAnswer } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import type { Emulator } from "@lucid-evolution/lucid";
import { Effect, Either, ManagedRuntime, Redacted } from "effect";

import * as MigrationRunner from "../../src/database/migrations/runner.js";
import { forcedOrderConfigFromContracts } from "../../src/forced-orders/config.js";
import { forcedOrderProjection } from "../../src/forced-orders/index.js";
import { operatorSetConfig } from "../../src/l1-operator-set/config.js";
import { operatorSetProjection } from "../../src/l1-operator-set/index.js";
import {
  stateQueueProjection,
  stateQueueProjectionConfig,
} from "../../src/l1-state-queue/index.js";
import type { NodeConfigDep } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import {
  intentJournalOver,
  IntentJournalRefused,
  type NodeIntentFamily,
} from "../../src/services/intent-journal.js";
import {
  nodeOwnWallets,
  nodeSeededAddresses,
  protocolPaymentCredentials,
} from "../../src/services/intent-journal.tracked-set.js";
import { nodeFamilyPredicate } from "../../src/services/l1-follower.intent-predicates.js";
import {
  createNodeIntentStage,
  nodeIntentTrackedSet,
} from "../../src/services/l1-follower.intents.js";
import { capturedChainOf, GENESIS_HASH } from "./emulator-chain-capture.js";
import {
  drainJournaledWithoutFollower,
  type RecordedIntent,
} from "./intent-journal.js";
import {
  addressBytes,
  blockOf,
  ledgerOutput,
} from "./intent-journal-replay.chain.js";
import { recordCommitThroughGate } from "./intent-journal-replay.commit-gate.js";
import { testDatabases } from "./l1-events-store.js";

/**
 * The flow's node tables the commit predicate and the commit's pre-broadcast
 * gate read, parents first, each with the event table its members' follower
 * admission identity comes from.
 */
const NODE_TABLES_THE_PREDICATES_READ = [
  ["deposits_utxos", null],
  ["withdrawal_utxos", null],
  ["pending_block_finalizations", null],
  ["pending_block_finalization_deposits", "deposits_utxos"],
  ["pending_block_finalization_withdrawals", "withdrawal_utxos"],
  ["forced_transaction_utxos", null],
  ["pending_block_finalization_forced_transactions", null],
] as const;

export type ReplayedIntent = Readonly<{
  family: NodeIntentFamily;
  workflowKey: string;
  txHash: string;
  contentRef: string | null;
  /**
   * The production journal's outcome: `recorded`, the refusal's reason, or
   * the gate's error.
   */
  outcome: string;
  /**
   * A commit only: whether its production pre-broadcast gate
   * (`submitWithDurableIntent`) wrote the signed bytes to its pending block
   * in the transaction that journaled them.
   */
  gated?: boolean;
  /** A refusal's message, and where each spent outref came from. */
  refusal?: string;
  /**
   * Whether it spends an output of a transaction that landed in the same
   * block: a chained intent, whose parent was still in flight at the tip it
   * was recorded at, so S6 waits on its inputs there.
   */
  chained: boolean;
  /** The §8.4 predicate at the block before landing, or its error. */
  verdict: boolean | string | undefined;
  /** The S6 action at that tip, then once the landing block applied. */
  before: string | undefined;
  after: string | undefined;
}>;

/** The node configuration a replay reads: its wallets, reference-script addresses and horizon lag. */
export type ReplayConfig = Parameters<typeof nodeSeededAddresses>[0] &
  Pick<NodeConfigDep, "HISTORY_COMMIT_HORIZON_LAG_BLOCKS">;

/**
 * Replays every journaled intent of this node (one spending its wallets)
 * whose transaction the emulator confirmed onto a production follower of
 * `emulator`'s chain. Intents never confirmed (a retried build, a refused
 * send) and other operators' are left out.
 */
export const replayJournaledOnFollower = async (input: {
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly config: ReplayConfig;
  readonly slotToPosixMs: (slot: number) => number;
  /** The operator key hash the operator-set predicates read as "ours". */
  readonly operatorKeyHash: string;
  readonly securityParameter?: number;
  /**
   * The journaled intents to replay; by default those handed to the
   * no-follower journal since the last drain. Several operators' nodes
   * replay one drain each from their own wallets' view.
   */
  readonly journaled?: readonly RecordedIntent[];
}): Promise<readonly ReplayedIntent[]> => {
  const { emulator, contracts, config } = input;
  const k = input.securityParameter ?? 2160;
  const chain = capturedChainOf(emulator);
  const journaled = [
    ...new Map(
      (input.journaled ?? drainJournaledWithoutFollower()).map((entry) => [
        entry.txHash,
        entry,
      ]),
    ).values(),
  ];
  const landedAt = (hash: string) => {
    const status = emulator.transactionHistory[hash];
    return status?.status === "confirmed" ? status.blockHeight : undefined;
  };
  const own = new Set(nodeOwnWallets(config).map((a) => a.toString("hex")));
  /** The address bytes (hex) of a captured or genesis output. */
  const addressOf = (outRef: OutRef): string | undefined => {
    const hash = outRef.txHash.toString("hex");
    const creator = chain.tx(hash);
    if (creator !== undefined)
      return decodeTransaction(creator).outputs[outRef.index]?.address.toString(
        "hex",
      );
    const utxo = chain.genesis?.find(
      (u) => u.txHash === hash && u.outputIndex === outRef.index,
    );
    return utxo === undefined
      ? undefined
      : addressBytes(utxo.address).toString("hex");
  };
  /** Another operator's node journaled it: it spends none of this node's wallets. */
  const ownIntent = (entry: RecordedIntent): boolean => {
    const decoded = decodeTransaction(Buffer.from(entry.signedTxCbor, "hex"));
    return [...decoded.inputs, ...decoded.collaterals].some((outRef) =>
      own.has(addressOf(outRef) ?? ""),
    );
  };
  const landedIntents = journaled.filter(
    (entry) => landedAt(entry.txHash) !== undefined && ownIntent(entry),
  );
  const blocks = new Map<number, { slot: number; hashes: string[] }>();
  for (const [hash, status] of Object.entries(emulator.transactionHistory))
    if (status.status === "confirmed") {
      if (!chain.has(hash) || chain.genesis === null)
        throw new Error(
          `The emulator confirmed ${hash} with no captured bytes: load helpers/emulator-chain-capture.ts before its first submission`,
        );
      const block = blocks.get(status.blockHeight) ?? {
        slot: status.slot,
        hashes: [],
      };
      block.hashes.push(hash);
      blocks.set(status.blockHeight, block);
    }
  const lastHeight = Math.max(0, ...blocks.keys());

  const databases = testDatabases();
  const connectionString = await databases.create();
  const network = config.NETWORK === "Mainnet" ? 1 : 0;
  const seeded = nodeSeededAddresses(config);
  const store: FactStore = openPostgresFactStore({
    ...projectionStoreOptions(
      [
        eventProjection(
          eventProjectionConfigFromContracts(
            SDK.requireEventHistoryContracts(contracts),
            network,
          ),
        ),
        stateQueueProjection(stateQueueProjectionConfig(contracts.stateQueue)),
        operatorSetProjection(operatorSetConfig(contracts)),
        forcedOrderProjection(forcedOrderConfigFromContracts(contracts)),
        intentJournalProjection,
      ],
      {
        securityParameter: k,
        trackedSet: nodeIntentTrackedSet({
          protocolPaymentCredentials: protocolPaymentCredentials(contracts),
          hubOraclePolicyId: contracts.hubOracle.policyId.toLowerCase(),
        }),
      },
      "postgres",
    ),
    wallets: seeded,
    connection: { connectionString },
  });
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(connectionString) }),
  );
  try {
    // The node's schema, as the node migrates its database before its
    // follower opens there, and the node's own block journal and forced
    // transactions, which the commit predicate reads: the flow's rows as
    // they stand at its end. The fixtures prepare a block without the
    // history producer's permit, so its event members carry no follower
    // admission identity; each gets its event row's, which is what the
    // production prepare binds.
    await runtime.runPromise(
      MigrationRunner.migrate({ appVersion: "test", actor: "intent-replay" }),
    );
    for (const [table, events] of NODE_TABLES_THE_PREDICATES_READ) {
      const [read] = await Effect.runPromise(
        Effect.flatMap(SqlClient.SqlClient, (flow) =>
          events === null
            ? flow<{
                readonly rows: string;
              }>`SELECT COALESCE(json_agg(t), '[]')::text AS rows FROM ${flow(table)} t`
            : flow<{ readonly rows: string }>`SELECT COALESCE(json_agg(
                to_jsonb(m) || jsonb_build_object(
                  'l1_event_key', COALESCE(m.l1_event_key, e.l1_event_key),
                  'l1_origin_outref', COALESCE(m.l1_origin_outref, e.l1_origin_outref))),
                '[]')::text AS rows
              FROM ${flow(table)} m
              LEFT JOIN ${flow(events)} e ON e.event_id = m.member_id`,
        ).pipe(Effect.provide(Database.layer)),
      );
      await runtime.runPromise(
        Effect.flatMap(
          SqlClient.SqlClient,
          (replay) => replay`INSERT INTO ${replay(table)}
            SELECT * FROM json_populate_recordset(NULL::${replay(table)},
              ${read!.rows}::text::json)`,
        ),
      );
    }
    const started = await store.start();
    if (started.kind !== "ready")
      throw new Error(`store start: ${JSON.stringify(started)}`);
    const origin = { slot: 0, hash: Buffer.alloc(32) };
    const init = await store.initialize({ point: origin, height: 0 });
    if (init.kind !== "initialized")
      throw new Error(`initialize: ${init.kind}`);
    const sql = await runtime.runPromise(SqlClient.SqlClient);
    const journal = intentJournalOver(sql, (output: OutputSummary) =>
      own.has(output.address.toString("hex")),
    );
    const genesis = chain.genesis ?? [];
    const verdicts = new Map<string, boolean | string>();
    const predicate = nodeFamilyPredicate({
      store,
      stateQueue: stateQueueProjectionConfig(contracts.stateQueue),
      operatorSet: {
        config: operatorSetConfig(contracts),
        ownKey: input.operatorKeyHash,
      },
      slotToPosixMs: input.slotToPosixMs,
      horizonLagBlocks: config.HISTORY_COMMIT_HORIZON_LAG_BLOCKS,
    });
    const stage = createNodeIntentStage({
      store,
      transport: {
        hasTx: () => Promise.resolve(false),
        // The flow already sent it; S6's resubmission goes nowhere.
        submit: () => Promise.resolve({ accepted: true } as const),
        withLedgerState: (<T>(
          _at: unknown,
          use: (session: {
            query: (query: {
              addresses: readonly Buffer[];
            }) => Promise<Uint8Array>;
          }) => Promise<T>,
        ): Promise<T> =>
          use({
            query: async ({ addresses }) => {
              const wanted = new Set(addresses.map((a) => a.toString("hex")));
              return encodeUtxoAnswer(
                genesis
                  .filter((utxo) =>
                    wanted.has(addressBytes(utxo.address).toString("hex")),
                  )
                  .map(ledgerOutput),
              );
            },
          })) as never,
      },
      securityParameter: k,
      seededAddresses: seeded,
      wanted: async (state) => {
        const key = state.intent.txHash.toString("hex");
        try {
          const verdict = await predicate(state);
          verdicts.set(key, verdict);
          return verdict;
        } catch (error) {
          verdicts.set(
            key,
            error instanceof Error ? error.message : String(error),
          );
          throw error;
        }
      },
      log: () => undefined,
    });

    let tip = { hash: origin.hash, height: 0, slot: origin.slot };
    /**
     * Applies the emulator's blocks below `height` (all of them with none),
     * one per emulator height, so the follower's heights (and the horizon
     * lag a commit predicate counts in them) are the emulator's.
     */
    const followTo = async (height?: number) => {
      while (
        tip.height <
        Math.min(height ?? lastHeight + 1, lastHeight + 1) - 1
      ) {
        const at = blocks.get(tip.height + 1);
        const block = blockOf(
          tip.height + 1,
          Math.max(at?.slot ?? 0, tip.slot + 1),
          tip.hash,
          (at?.hashes ?? []).sort().map((hash) => chain.tx(hash)!),
        );
        const applied = await store.applyBlock(block);
        if (applied.kind !== "applied")
          throw new Error(`apply: ${JSON.stringify(applied)}`);
        tip = {
          hash: block.point.hash,
          height: block.height,
          slot: block.point.slot,
        };
      }
    };
    /** Each spent outref's creator and address, for a refusal's diagnosis. */
    const originsOf = (cborHex: string): string => {
      const decoded = decodeTransaction(Buffer.from(cborHex, "hex"));
      return [
        ...decoded.inputs.map((o) => ["input", o] as const),
        ...decoded.referenceInputs.map((o) => ["reference", o] as const),
        ...decoded.collaterals.map((o) => ["collateral", o] as const),
      ]
        .map(([role, outRef]) => {
          const hash = outRef.txHash.toString("hex");
          const from =
            hash === GENESIS_HASH
              ? "genesis"
              : `the tx at height ${String(landedAt(hash))}`;
          return `${role} ${hash}#${outRef.index.toString()} from ${from} at ${addressOf(outRef) ?? "?"}`;
        })
        .join(", ");
    };
    const actionOf = (hash: string) =>
      stage
        .lastReport()
        ?.intents.find((entry) => entry.intent.txHash.toString("hex") === hash)
        ?.action;

    // The seed at the origin: the genesis outputs at the seeded addresses.
    const seededHolds = await stage.run();
    if (seededHolds.length > 0)
      throw new Error(`origin seed: ${JSON.stringify(seededHolds)}`);
    const ordered = landedIntents
      .map((entry, order) => ({ entry, order }))
      .sort(
        (a, b) =>
          landedAt(a.entry.txHash)! - landedAt(b.entry.txHash)! ||
          a.order - b.order,
      )
      .map(({ entry }) => entry);
    const replayed: Omit<ReplayedIntent, "after">[] = [];
    for (const entry of ordered) {
      const { intent } = entry;
      if (intent.kind !== "journaled") continue;
      await followTo(landedAt(entry.txHash));
      const header = intent.family === "commit" ? intent.contentRef : undefined;
      const { outcome, gated } =
        header === undefined
          ? {
              outcome: await Effect.runPromise(
                Effect.either(
                  journal.record(intent, entry.signedTxCbor, entry.txHash),
                ),
              ),
              gated: undefined,
            }
          : await recordCommitThroughGate({
              runtime,
              sql,
              journal,
              entry,
              header,
            });
      await stage.run();
      const failure = Either.isLeft(outcome)
        ? outcome.left instanceof Error
          ? outcome.left
          : new Error(String(outcome.left))
        : undefined;
      replayed.push({
        family: intent.family,
        workflowKey: intent.workflowKey,
        txHash: entry.txHash,
        contentRef: intent.contentRef?.toString("hex") ?? null,
        outcome: Either.isRight(outcome)
          ? outcome.right.kind
          : failure instanceof IntentJournalRefused
            ? failure.reason
            : `gate: ${String(failure?.message)}`,
        ...(gated === undefined ? {} : { gated }),
        ...(failure !== undefined
          ? {
              refusal: `${failure.message}; ${originsOf(entry.signedTxCbor)}`,
            }
          : {}),
        chained: decodeTransaction(
          Buffer.from(entry.signedTxCbor, "hex"),
        ).inputs.some(
          (parent) =>
            landedAt(parent.txHash.toString("hex")) === landedAt(entry.txHash),
        ),
        verdict: verdicts.get(entry.txHash),
        before: actionOf(entry.txHash),
      });
    }
    await followTo();
    await stage.run();
    const result = replayed.map((entry) => ({
      ...entry,
      after: actionOf(entry.txHash),
    }));
    stage.close();
    return result;
  } finally {
    await runtime.dispose();
    await store.close();
    await databases.dropAll();
  }
};
