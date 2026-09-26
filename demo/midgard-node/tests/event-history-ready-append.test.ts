import { createHash, randomUUID } from "node:crypto";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect, Option, Tracer } from "effect";
import { beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Deposits from "../src/database/deposits.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { pendingHistoryLedgerDisposition } from "../src/database/eventHistoryLedgerRepair.js";
import {
  MATERIALIZATION_CHUNK,
  materializeCanonicalHistory,
} from "../src/database/eventHistoryMaterialization.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import * as Withdrawals from "../src/database/withdrawals.js";
import { reconcileDepositProjection } from "../src/fibers/project-deposits-to-mempool-ledger.js";
import { historyIncarnationEntry } from "../src/l1-event-history-entries.js";
import {
  type HistoryIncarnation,
  historyIncarnationDigest,
  historyIncarnationId,
} from "../src/l1-event-history-provenance.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type {
  HistoryTransition,
  HistoryTransitionEvent,
} from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import type { HistoryOwnerChange } from "../src/services/event-history-owner.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { provideDatabaseLayers } from "./utils.js";

// The source owner's forward append at the head of its Ready generation, with
// the production reconciliation, over real PostgreSQL. Retired history is
// built through the production capture decoder; block and source admission
// are explicit model inputs, as in the journal tests.
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      program.pipe(
        Effect.tapError((error) => Effect.logError(formatDatabaseError(error))),
      ),
    ),
  );

/** Every SQL statement and every transaction or savepoint `program` opens. */
const counted = <A, E, R>(program: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const count = { statements: 0, transactions: 0 };
    const base = yield* Effect.tracer;
    const tracer = Tracer.make({
      span(name, parent, context, links, startTime, kind, options) {
        if (name === "sql.execute") count.statements += 1;
        if (name === "sql.transaction") count.transactions += 1;
        return base.span(
          name,
          parent,
          context,
          links,
          startTime,
          kind,
          options,
        );
      },
      context: (f, fiber) => base.context(f, fiber),
    });
    const value = yield* program.pipe(Effect.withTracer(tracer));
    return { value, ...count };
  });

const HEAD_SLOT = 100_000_000;
const HEAD_HEIGHT = 5_000_000;
const HEAD_ID = sha("ready-append-seed-head");
const DECODE_CHUNK = 400;
const INSERT_CHUNK = 250;
const owner = "aa".repeat(28);
const address = {
  paymentCredential: { PublicKeyCredential: [owner] as [string] },
  stakeCredential: null,
};
let binding: EventHistorySourceBinding;
let pair: SDK.EventHistoryContractPair;
let initialOutputs: LedgerSnapshotOutput[];
let addresses: string[];
const rootDatum = (next: string | null = null) =>
  Data.to(
    { position: "Root", next, protected_until: 0n, payload: "RootContent" },
    SDK.EventHistoryNode,
  );
const rootOf = (
  outputs: readonly LedgerSnapshotOutput[],
  kind: "deposit" | "withdrawal",
) =>
  outputs.find(
    (output) =>
      output.address === binding.deployments[kind].address &&
      output.assets[binding.deployments[kind].policyId] === 1n,
  )!;

type Item = {
  kind: "deposit" | "withdrawal";
  key: string;
  facts: SDK.EventHistoryFacts;
};
const makeItem = async (
  kind: "deposit" | "withdrawal",
  label: string,
): Promise<Item> => {
  const id = { transactionId: sha(`id-${label}`), outputIndex: 0n };
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: { l2_address: address, l2_network_id: 0n, l2_datum: "ab" },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: owner,
                  l2_value: new Map([["", new Map([["", 5_000_000n]])]]),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: "NoDatum",
          },
        };
  const plan = SDK.prepareEventHistoryPayload(
    payload,
    address.paymentCredential,
    pair[kind].recipe,
  );
  return {
    kind,
    key: await Effect.runPromise(SDK.eventHistoryKey(id)),
    facts: {
      event_id: id,
      inclusion_time: 1_000_000n,
      location: plan.location,
      structural_lovelace: 2_000_000n,
      structural_refund_key: owner,
    },
  };
};
const orderOutput = (
  item: Item,
  next: string | null,
  txHash: string,
  outputIndex: number,
): LedgerSnapshotOutput => {
  const deployment = binding.deployments[item.kind];
  return {
    txHash,
    outputIndex,
    address: deployment.address,
    assets: { lovelace: 7_000_000n, [deployment.policyId + item.key]: 1n },
    datum: Data.to(
      {
        position: { Key: [item.key] },
        next,
        protected_until: 0n,
        payload: { Order: { facts: item.facts } },
      },
      SDK.EventHistoryNode,
    ),
    hasReferenceScript: false,
  };
};
const transitionEvent = (
  event: SDK.DepositUTxO | SDK.WithdrawalUTxO,
): HistoryTransitionEvent => ({
  key: event.assetName,
  idCbor: event.idCbor.toString("hex"),
  inclusionTime: event.facts.inclusion_time,
  factsCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(event.utxo.datum!, [3, 0]),
  ),
  payloadCbor: event.history.payloadCbor,
  originalAssetsCbor: Data.to(
    SDK.assetsToValue(event.originalAssets),
    SDK.Value,
  ),
  outRef: { txHash: event.utxo.txHash, outputIndex: event.utxo.outputIndex },
});
const decodeOrders = async (
  base: readonly LedgerSnapshotOutput[],
  kind: "deposit" | "withdrawal",
  root: LedgerSnapshotOutput,
  orders: readonly LedgerSnapshotOutput[],
) => {
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        point: { id: HEAD_ID, slot: HEAD_SLOT },
        addresses,
        outputs: [
          ...base.filter((output) => output !== rootOf(base, kind)),
          root,
          ...orders,
        ],
      },
      binding,
    ),
  );
  return (
    kind === "deposit" ? capture.history.deposits : capture.history.withdrawals
  ).map(transitionEvent);
};

const placementAt = (height: number, transactionHash: string) => ({
  blockHash: sha(`block-at-${height}`),
  slot: height * 20,
  height,
  transactionHash,
  transactionIndex: 0,
});
/** `n` admissions retired before the seed point, half of each kind, with
 * immutable bytes from the production list decoder. */
const retiredHistory = async (n: number) => {
  const incarnations: HistoryIncarnation[] = [];
  const withdrawals = Math.floor(n / 2);
  for (const [kind, count] of [
    ["deposit", n - withdrawals],
    ["withdrawal", withdrawals],
  ] as const)
    for (let start = 0; start < count; start += DECODE_CHUNK) {
      const items: Item[] = [];
      for (let i = start; i < Math.min(count, start + DECODE_CHUNK); i++)
        items.push(await makeItem(kind, `retired-${kind}-${i}`));
      items.sort((a, b) => a.key.localeCompare(b.key));
      const orders = items.map((item, i) =>
        orderOutput(
          item,
          items[i + 1]?.key ?? null,
          sha(`order-${item.key}`),
          0,
        ),
      );
      const root = {
        ...rootOf(initialOutputs, kind),
        txHash: sha(`list-root-${kind}-${start}`),
        datum: rootDatum(items[0]!.key),
      };
      for (const event of await decodeOrders(
        initialOutputs,
        kind,
        root,
        orders,
      )) {
        const index = incarnations.length;
        incarnations.push({
          id: historyIncarnationId(binding.digest, kind, event),
          bindingDigest: binding.digest,
          kind,
          event,
          placement: {
            admission: placementAt(1_000_000 + 2 * index, event.outRef.txHash),
            current: null,
            retirement: {
              at: placementAt(1_000_001 + 2 * index, sha(`retire-${index}`)),
              outRef: event.outRef,
              reason: kind === "deposit" ? "absorbed" : "payout_initialized",
              observerRedeemerIndex: 0,
              witnessCbor: "d87980",
            },
          },
        });
      }
    }
  return incarnations;
};

/** The production owner's reconciliation (event-history-runtime). */
const reconcile = (
  change: HistoryOwnerChange,
  materialize = materializeCanonicalHistory,
) =>
  Effect.gen(function* () {
    const pending = yield* pendingHistoryLedgerDisposition(change);
    if (pending !== undefined) return pending;
    yield* materialize(change, "Preprod");
    const { reconciled } = yield* reconcileDepositProjection(
      new Date(change.after.head.slot * 1000),
    );
    const owned = yield* Authority.currentOwnedTransaction;
    if (
      reconciled.spendableUpserts.length > 0 &&
      Option.isSome(owned) &&
      owned.value.state === "ready"
    )
      return {
        status: "pending" as const,
        reason: "Deposit projection restored spendable ledger rows",
      };
    return undefined;
  });

/** Seed the journal at the model origin with `history`, and the event rows a
 * long-running node keeps for them: the exact bytes materialization derives,
 * with their association, since consumed or finalized. Then the startup
 * resume, and Ready. */
const startReady = async (history: readonly HistoryIncarnation[]) => {
  const token = await run(
    Authority.acquire({
      deploymentIdentity: binding.manifestId,
      ownerToken: randomUUID(),
      leaseDurationMs: 600_000,
    }),
  );
  await run(
    Authority.withRecovery(
      token,
      Effect.gen(function* () {
        const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { id: HEAD_ID, slot: HEAD_SLOT },
            addresses,
            outputs: initialOutputs,
          },
          binding,
        );
        const receipt = "Explicit model origin; not ledger admission";
        yield* Journal.seed({
          binding,
          capture,
          height: HEAD_HEIGHT,
          originReceipt: receipt,
          originReceiptDigest: sha(receipt),
          incarnations: history,
        });
        const sql = yield* SqlClient.SqlClient;
        const header = Buffer.alloc(28, 7);
        const rows: Record<string, Record<string, unknown>[]> = {
          [Deposits.tableName]: [],
          [Withdrawals.tableName]: [],
        };
        for (const incarnation of history) {
          const materialized = yield* historyIncarnationEntry(
            incarnation,
            "Preprod",
          );
          const association = {
            history_binding_digest: Buffer.from(binding.digest, "hex"),
            history_incarnation_id: Buffer.from(incarnation.id, "hex"),
          };
          if (materialized.kind === "deposit")
            rows[Deposits.tableName]!.push({
              ...materialized.entry,
              [Deposits.Columns.STATUS]: "consumed",
              [Deposits.Columns.PROJECTED_HEADER_HASH]: header,
              ...association,
            });
          else
            rows[Withdrawals.tableName]!.push({
              ...materialized.entry,
              [Withdrawals.Columns.STATUS]: "finalized",
              [Withdrawals.Columns.VALIDITY]: "WithdrawalIsValid",
              [Withdrawals.Columns.SETTLEMENT_EVENT_INFO]:
                materialized.entry[Withdrawals.Columns.RAW_EVENT_INFO],
              [Withdrawals.Columns.PROJECTED_HEADER_HASH]: header,
              [Withdrawals.Columns.VALIDITY_DETAIL]:
                sql`CAST(${JSON.stringify(materialized.entry[Withdrawals.Columns.VALIDITY_DETAIL])} AS TEXT)::JSONB`,
              ...association,
            });
        }
        for (const [table, entries] of Object.entries(rows))
          for (let at = 0; at < entries.length; at += INSERT_CHUNK) {
            const batch = entries.slice(at, at + INSERT_CHUNK);
            const columns = Object.keys(batch[0]!);
            yield* sql`INSERT INTO ${sql(table)}
              (${sql.csv(columns.map((column) => sql`${sql(column)}`))})
              VALUES ${sql.csv(batch.map((entry) => sql`(${sql.csv(columns.map((column) => sql`${entry[column] as never}`))})`))}`;
          }
      }),
    ),
  );
  return restart(token);
};
const cost = ({
  statements,
  transactions,
}: {
  statements: number;
  transactions: number;
}) => ({ statements, transactions });
/** A node restart: a new owner generation, the startup resume (a full chain
 * load, then the recovery reconciliation that walks and checks every
 * incarnation and event row), then Ready. */
const restart = async (previous: Authority.Token) => {
  const token = await run(
    Authority.acquire({
      deploymentIdentity: binding.manifestId,
      ownerToken: previous.ownerToken,
      leaseDurationMs: 600_000,
    }),
  );
  const { checkpoint, resume } = await run(
    Effect.gen(function* () {
      const loaded = yield* Journal.load(binding);
      if (loaded === null) return yield* Effect.die("Missing checkpoint");
      const resume = yield* counted(
        Authority.withRecovery(
          token,
          reconcile({ kind: "resume", before: loaded, after: loaded }),
        ),
      );
      if (resume.value !== undefined)
        return yield* Effect.die(resume.value.reason);
      return { checkpoint: loaded, resume };
    }),
  );
  await run(
    Authority.publishReady(token, {
      point: { id: checkpoint.head.id, slot: checkpoint.head.slot },
      snapshotDigest: checkpoint.capture.snapshotDigest,
    }),
  );
  return { token, checkpoint, resume: cost(resume) };
};

const prepare = async (
  checkpoint: Journal.Checkpoint,
  label: string,
  transitions: readonly HistoryTransition[] = [],
  outputs = checkpoint.capture.history.ledger.outputs,
) => {
  const block: BoundHistoryChainBlock = {
    parent: checkpoint.head.id,
    point: {
      id: sha(`block-${label}`),
      slot: checkpoint.head.slot + 20,
      height: checkpoint.head.height + 1,
    },
    transactions: transitions.map((transition) => ({
      txHash: transition.transactionHash,
      spends: "inputs",
      inputs: transition.consumed,
      references: [],
      collaterals: [],
      outputs: transition.produced,
      mint: {},
      withdrawals: [],
      redeemers: [],
    })),
  };
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        ...checkpoint.capture.history.ledger,
        point: { id: block.point.id, slot: block.point.slot },
        outputs,
      },
      binding,
    ),
  );
  return Journal.prepareAppend(checkpoint, block, {
    capture,
    transitions: transitions.map((transition, transactionIndex) => ({
      transactionIndex,
      transition,
    })),
  });
};
/** A block inserting one new `kind` order at the head of its (empty) live
 * list. */
const admit = async (
  checkpoint: Journal.Checkpoint,
  label: string,
  kind: "deposit" | "withdrawal" = "deposit",
) => {
  const item = await makeItem(kind, `live-${label}`);
  const outputs = checkpoint.capture.history.ledger.outputs;
  const root = rootOf(outputs, kind);
  const txHash = sha(`admit-${label}`);
  const rootAfter = {
    ...root,
    txHash,
    outputIndex: 0,
    datum: rootDatum(item.key),
  };
  const order = orderOutput(item, null, txHash, 1);
  const [admission] = await decodeOrders(outputs, kind, rootAfter, [order]);
  const after = [
    ...outputs.filter((output) => output !== root),
    rootAfter,
    order,
  ];
  return {
    order,
    prepared: await prepare(
      checkpoint,
      label,
      [
        {
          kind,
          operation: "InsertOrder",
          transactionHash: txHash,
          consumed: [root],
          produced: [rootAfter, order],
          continuations: [],
          admission: admission!,
        },
      ],
      after,
    ),
  };
};
/** A block moving the live `order` to a new output: a continuation. */
const move = (
  checkpoint: Journal.Checkpoint,
  label: string,
  kind: "deposit" | "withdrawal",
  order: LedgerSnapshotOutput,
) => {
  const at = (output: { txHash: string; outputIndex: number }) =>
    output.txHash === order.txHash && output.outputIndex === order.outputIndex;
  const live = checkpoint.capture.history.ledger.outputs.find(at)!;
  // The continuation is its transaction's only output.
  const moved = { ...live, txHash: sha(`moved-${label}`), outputIndex: 0 };
  const event = checkpoint.incarnations.find(
    (value) => value.placement?.current && at(value.placement.current.outRef),
  )!.event;
  return prepare(
    checkpoint,
    label,
    [
      {
        kind,
        operation: "InsertFiller",
        transactionHash: moved.txHash,
        consumed: [live],
        produced: [moved],
        continuations: [{ key: event.key, before: live, after: moved }],
      },
    ],
    checkpoint.capture.history.ledger.outputs.map((output) =>
      output === live ? moved : output,
    ),
  ).then((prepared) => ({ prepared, moved }));
};

/** The owner's Ready append: journal, reconcile, advance the Ready point. */
const appendReady = (
  token: Authority.Token,
  before: Journal.Checkpoint,
  prepared: ReturnType<typeof Journal.prepareAppend>,
  {
    horizon = 2160,
    materialize = materializeCanonicalHistory,
  }: {
    horizon?: number;
    materialize?: typeof materializeCanonicalHistory;
  } = {},
) =>
  Authority.withReadyAppend(
    token,
    Journal.append(
      binding,
      prepared,
      ({ after, changes }) =>
        reconcile(
          { kind: "forward", before, after, changes },
          materialize,
        ).pipe(
          Effect.flatMap((pending) =>
            pending === undefined
              ? Effect.succeed(after)
              : Effect.fail(new Error(pending.reason)),
          ),
        ),
      {
        tipHeight: prepared.block.point.height,
        horizon,
        holdSlot: undefined,
      },
    ).pipe(
      Effect.tap((appended) =>
        appended.applied
          ? Authority.advanceReadyPoint({
              point: appended.result.head,
              snapshotDigest: appended.result.capture.snapshotDigest,
            })
          : Effect.void,
      ),
    ),
  );
const applied = async (
  token: Authority.Token,
  before: Journal.Checkpoint,
  prepared: ReturnType<typeof Journal.prepareAppend>,
  options?: Parameters<typeof appendReady>[3],
) => {
  const appended = await run(appendReady(token, before, prepared, options));
  if (!appended.applied) throw new Error("Append did not apply");
  return appended.result;
};
const sqlRun = (
  program: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) =>
  run(
    Effect.gen(function* () {
      return yield* program(yield* SqlClient.SqlClient);
    }),
  );

/** Event-table rows the current transaction has read so far, from the
 * backend's unflushed counters: heap tuples returned by sequential scans plus
 * index entries returned by any index scan (plain, bitmap or index-only). */
const eventRowsRead = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{ rows: string }>`
    WITH tables AS (
      SELECT unnest(ARRAY['deposits_utxos', 'withdrawal_utxos']::regclass[])::oid AS oid
    )
    SELECT coalesce(sum(pg_stat_get_xact_tuples_returned(oid)), 0)::text AS rows FROM (
      SELECT oid FROM tables
      UNION ALL SELECT indexrelid FROM pg_index WHERE indrelid IN (SELECT oid FROM tables)
    ) relations`;
  return Number(row!.rows);
}).pipe(Effect.orDie);
/** The production materialization, counting the event rows it reads. At
 * these table sizes the planner may rightly prefer a sequential scan of a few
 * pages, which reads every row whatever the predicate; with it disabled the
 * count is the rows the predicates select (an unindexable or unscoped
 * predicate still reads the whole table). */
const materializeCounting =
  (read: { rows?: number }): typeof materializeCanonicalHistory =>
  (change, network) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`SET LOCAL enable_seqscan = off`.pipe(Effect.orDie);
      const before = yield* eventRowsRead;
      const result = yield* materializeCanonicalHistory(change, network);
      read.rows = (yield* eventRowsRead) - before;
      yield* sql`SET LOCAL enable_seqscan TO DEFAULT`.pipe(Effect.orDie);
      return result;
    });

beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  pair = SDK.requireEventHistoryContracts(contracts);
  binding = {
    digest: hash(902),
    manifestId: hash(903),
    network: "Preprod",
    endpointIdentitySha256: hash(904),
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
    genesisSha256: hash(905),
    hubAddress: contracts.hubOracle.spendingScriptAddress,
    hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    hubDatumCbor: Data.to(
      await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
      SDK.HubOracleDatum,
    ),
    deployments: {
      deposit: SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      withdrawal: SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
    },
  };
  initialOutputs = Object.values(binding.deployments).map(
    (entry, outputIndex) => ({
      txHash: hash(910),
      outputIndex,
      address: entry.address,
      assets: { lovelace: 3_000_000n, [entry.policyId]: 1n },
      datum: rootDatum(),
      hasReferenceScript: false,
    }),
  );
  initialOutputs.push({
    txHash: hash(911),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  });
  addresses = [
    binding.hubAddress,
    ...Object.values(binding.deployments).flatMap((entry) => [
      entry.address,
      entry.retentionAddress,
    ]),
  ];
});
const clear = () =>
  sqlRun(
    (sql) =>
      sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts, event_history_recovery_plans`,
  );
beforeEach(clear);

describe("Ready head append", () => {
  it("costs the same statements, savepoints and event rows read whatever the retained history", async () => {
    const measure = async (n: number) => {
      await clear();
      const { token, checkpoint, resume } = await startReady(
        await retiredHistory(n),
      );
      expect(checkpoint.incarnations).toHaveLength(n);
      const { prepared } = await admit(checkpoint, `measure-${n}`);
      const read: { rows?: number } = {};
      const admission = await run(
        counted(
          appendReady(token, checkpoint, prepared, {
            materialize: materializeCounting(read),
          }),
        ),
      );
      if (!admission.value.applied) throw new Error("Append did not apply");
      const admitted = admission.value.result;
      const empty = await run(
        counted(
          appendReady(
            token,
            admitted,
            await prepare(admitted, `measure-empty-${n}`),
          ),
        ),
      );
      if (!empty.value.applied) throw new Error("Append did not apply");
      expect(empty.value.result.incarnations).toHaveLength(n + 1);
      // The Ready appends materialized the admission, and the startup resume
      // still verifies every row.
      const [row] = (await sqlRun(
        (sql) =>
          sql`SELECT history_incarnation_id FROM deposits_utxos WHERE event_id = ${Buffer.from(prepared.changes[0]!.after.event.idCbor, "hex")}`,
      )) as { history_incarnation_id: Buffer }[];
      expect(row!.history_incarnation_id.toString("hex")).toBe(
        prepared.changes[0]!.after.id,
      );
      expect((await restart(token)).checkpoint).toEqual(empty.value.result);
      return {
        admission: cost(admission),
        empty: cost(empty),
        resume,
        eventRowsRead: read.rows!,
      };
    };
    const small = await measure(200);
    const large = await measure(2000);
    console.info(
      `Ready append and resume at 200 vs 2000 retained incarnations: ${JSON.stringify({ small, large })}`,
    );
    expect({ admission: large.admission, empty: large.empty }).toEqual({
      admission: small.admission,
      empty: small.empty,
    });
    // The admission's materialization, eligibility checks included, reads
    // only the rows of the incarnation it staged.
    expect(small.eventRowsRead).toBeGreaterThan(0);
    expect(large.eventRowsRead).toBe(small.eventRowsRead);
    // The startup resume still walks every incarnation, a chunk per
    // statement and without a savepoint per row.
    expect(large.resume.transactions).toBe(small.resume.transactions);
    expect(
      large.resume.statements - small.resume.statements,
    ).toBeLessThanOrEqual(
      3 *
        (Math.ceil(2000 / MATERIALIZATION_CHUNK) -
          Math.ceil(200 / MATERIALIZATION_CHUNK)),
    );
  }, 600_000);

  it.each(["deposit", "withdrawal"] as const)(
    "writes a %s continuation's new location and returns the checkpoint a load reads",
    async (kind) => {
      const { token, checkpoint } = await startReady(await retiredHistory(4));
      const { prepared, order } = await admit(checkpoint, "admit", kind);
      const admitted = await applied(token, checkpoint, prepared);
      expect(admitted).toEqual(await run(Journal.load(binding)));
      const eventId = Buffer.from(
        prepared.changes[0]!.after.event.idCbor,
        "hex",
      );
      const location = () =>
        sqlRun((sql) =>
          kind === "deposit"
            ? sql`SELECT encode(deposit_l1_tx_hash, 'hex') AS tx_hash, NULL AS output_index FROM deposits_utxos WHERE event_id = ${eventId}`
            : sql`SELECT encode(withdrawal_l1_tx_hash, 'hex') AS tx_hash, withdrawal_l1_output_index AS output_index FROM withdrawal_utxos WHERE event_id = ${eventId}`,
        );
      expect(await location()).toEqual([
        {
          tx_hash: order.txHash,
          output_index: kind === "deposit" ? null : order.outputIndex,
        },
      ]);
      const continuation = await move(admitted, "move", kind, order);
      const continued = await applied(token, admitted, continuation.prepared);
      expect(await location()).toEqual([
        {
          tx_hash: continuation.moved.txHash,
          output_index:
            kind === "deposit" ? null : continuation.moved.outputIndex,
        },
      ]);
      expect(continued).toEqual(await run(Journal.load(binding)));
      expect((await restart(token)).checkpoint).toEqual(continued);
    },
  );

  it("refuses a staged incarnation whose stored image is not the one it was prepared from", async () => {
    const { token, checkpoint } = await startReady(await retiredHistory(4));
    const { prepared, order } = await admit(checkpoint, "admit");
    const admitted = await applied(token, checkpoint, prepared);
    const continuation = await move(admitted, "move", "deposit", order);
    const id = Buffer.from(continuation.prepared.changes[0]!.after.id, "hex");
    expect(continuation.prepared.changes[0]!.before!.id).toBe(
      continuation.prepared.changes[0]!.after.id,
    );
    // The cursor is untouched: only the staged row read-back can see these.
    await sqlRun(
      (sql) =>
        sql`UPDATE event_history_incarnations SET incarnation_digest = ${Buffer.alloc(32, 1)} WHERE incarnation_id = ${id}`,
    );
    await expect(
      run(appendReady(token, admitted, continuation.prepared)),
    ).rejects.toThrow(/Prepared block images changed/);
    await sqlRun(
      (sql) =>
        sql`UPDATE event_history_incarnations SET incarnation_digest = ${Buffer.from(historyIncarnationDigest(continuation.prepared.changes[0]!.before!), "hex")} WHERE incarnation_id = ${id}`,
    );
    // A new admission's row must be absent.
    const other = await admit(admitted, "second", "withdrawal");
    const created = Buffer.from(other.prepared.changes[0]!.after.id, "hex");
    expect(other.prepared.changes[0]!.before).toBeNull();
    await sqlRun(
      (sql) =>
        sql`INSERT INTO event_history_incarnations (binding_digest, incarnation_id, kind, event_id, event_key, origin_canonical, incarnation_record, incarnation_digest)
          SELECT binding_digest, ${created}, kind, event_id, event_key, false, incarnation_record, incarnation_digest
          FROM event_history_incarnations WHERE incarnation_id = ${id}`,
    );
    await expect(
      run(appendReady(token, admitted, other.prepared)),
    ).rejects.toThrow(/Prepared block images changed/);
    await sqlRun(
      (sql) =>
        sql`DELETE FROM event_history_incarnations WHERE incarnation_id = ${created}`,
    );
    // Restored, both blocks apply as prepared.
    const continued = await applied(token, admitted, continuation.prepared);
    expect(continued).toEqual(await run(Journal.load(binding)));
  });

  it("refuses a staged live output whose stored image is not the one it was prepared from", async () => {
    const { token, checkpoint } = await startReady(await retiredHistory(4));
    const { prepared, order } = await admit(checkpoint, "admit");
    const admitted = await applied(token, checkpoint, prepared);
    const continuation = await move(admitted, "move", "deposit", order);
    const spent = Buffer.from(order.txHash, "hex");
    const created = Buffer.from(continuation.moved.txHash, "hex");
    // The cursor and every staged incarnation are untouched: only the staged
    // output read-back can see these.
    const [stored] = (await sqlRun(
      (sql) =>
        sql`SELECT output_digest FROM event_history_live_outputs WHERE tx_hash = ${spent} AND output_index = ${order.outputIndex}`,
    )) as { output_digest: Buffer }[];
    // The live output the block spends must hold the prepared image.
    await sqlRun(
      (sql) =>
        sql`UPDATE event_history_live_outputs SET output_digest = ${Buffer.alloc(32, 1)} WHERE tx_hash = ${spent} AND output_index = ${order.outputIndex}`,
    );
    await expect(
      run(appendReady(token, admitted, continuation.prepared)),
    ).rejects.toThrow(/Prepared block images changed/);
    await sqlRun(
      (sql) =>
        sql`UPDATE event_history_live_outputs SET output_digest = ${stored!.output_digest} WHERE tx_hash = ${spent} AND output_index = ${order.outputIndex}`,
    );
    // The output the block creates must be absent.
    await sqlRun(
      (sql) =>
        sql`INSERT INTO event_history_live_outputs (binding_digest, tx_hash, output_index, output_record, output_digest)
          SELECT binding_digest, ${created}, ${continuation.moved.outputIndex}, output_record, output_digest
          FROM event_history_live_outputs WHERE tx_hash = ${spent} AND output_index = ${order.outputIndex}`,
    );
    await expect(
      run(appendReady(token, admitted, continuation.prepared)),
    ).rejects.toThrow(/Prepared block images changed/);
    await sqlRun(
      (sql) =>
        sql`DELETE FROM event_history_live_outputs WHERE tx_hash = ${created} AND output_index = ${continuation.moved.outputIndex}`,
    );
    // Restored, the block applies as prepared.
    const continued = await applied(token, admitted, continuation.prepared);
    expect(continued).toEqual(await run(Journal.load(binding)));
    expect((await restart(token)).checkpoint).toEqual(continued);
  });

  it("advances the retention anchor across Ready appends and returns the checkpoint a load reads", async () => {
    const { token, checkpoint } = await startReady(await retiredHistory(4));
    // Every checkpoint's head, and its snapshot digest, by height.
    const heads = new Map([
      [
        checkpoint.head.height,
        {
          anchor: checkpoint.head,
          anchorSnapshotDigest: checkpoint.capture.snapshotDigest,
        },
      ],
    ]);
    let current = checkpoint;
    const append = async (
      prepared: ReturnType<typeof Journal.prepareAppend>,
    ) => {
      current = await applied(token, current, prepared, { horizon: 1 });
      heads.set(current.head.height, {
        anchor: current.head,
        anchorSnapshotDigest: current.capture.snapshotDigest,
      });
      // The anchor is the deepest block the horizon lets the append release,
      // the retained block at that height.
      expect(current.anchor.height).toBe(current.head.height - 1);
      expect({
        anchor: current.anchor,
        anchorSnapshotDigest: current.anchorSnapshotDigest,
      }).toEqual(heads.get(current.anchor.height));
      expect(current).toEqual(await run(Journal.load(binding)));
    };
    for (const label of ["empty-1", "empty-2", "empty-3"])
      await append(await prepare(current, label));
    const admission = await admit(current, "admit", "withdrawal");
    await append(admission.prepared);
    await append(
      (await move(current, "move", "withdrawal", admission.order)).prepared,
    );
    await append(await prepare(current, "empty-4"));
    expect(current.anchor.height).toBe(checkpoint.anchor.height + 5);
    expect(current.anchorSnapshotDigest).not.toBe(
      checkpoint.anchorSnapshotDigest,
    );
    expect((await restart(token)).checkpoint).toEqual(current);
  });

  it("refuses a preparation whose cursor revision or head moved, and one it did not stage", async () => {
    const { token, checkpoint } = await startReady(await retiredHistory(4));
    const first = await prepare(checkpoint, "first");
    const sibling = await prepare(checkpoint, "sibling");
    const head = await applied(token, checkpoint, first);
    expect(await run(appendReady(token, checkpoint, first))).toEqual({
      applied: false,
      revision: head.revision,
    });
    await expect(run(appendReady(token, checkpoint, sibling))).rejects.toThrow(
      /revision or head changed/,
    );
    const next = await prepare(head, "next");
    await sqlRun(
      (sql) => sql`UPDATE event_history_cursor SET revision = revision + 1`,
    );
    await expect(run(appendReady(token, head, next))).rejects.toThrow(
      /revision or head changed/,
    );
    await sqlRun(
      (sql) => sql`UPDATE event_history_cursor SET revision = revision - 1`,
    );
    await expect(run(appendReady(token, head, { ...next }))).rejects.toThrow(
      /staged from a loaded checkpoint/,
    );
    await expect(
      run(
        appendReady(
          token,
          head,
          Journal.prepareAppend({ ...head }, next.block, next.projection),
        ),
      ),
    ).rejects.toThrow(/staged from a loaded checkpoint/);
    expect(await applied(token, head, next)).toEqual(
      await run(Journal.load(binding)),
    );
  });

  it("leaves unstaged rows to the startup resume, which still refuses corrupted bytes and associations", async () => {
    const history = await retiredHistory(6);
    const deposit = Buffer.from(
      history.find((value) => value.kind === "deposit")!.event.idCbor,
      "hex",
    );
    const withdrawal = history.find((value) => value.kind === "withdrawal")!;
    const stray = Buffer.from(sha("stray"), "hex");
    const started = await startReady(history);
    let token = started.token;
    // Each corruption is outside the rows a Ready append's block stages, so
    // the append does not read it; the startup resume walks every row and
    // refuses it, and accepts the journal again once it is repaired.
    const detected = async (
      current: Journal.Checkpoint,
      block: ReturnType<typeof Journal.prepareAppend>,
      refused: RegExp,
      repair: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
    ) => {
      const next = await applied(token, current, block);
      await expect(restart(token)).rejects.toThrow(refused);
      await sqlRun(repair);
      const restarted = await restart(token);
      expect(restarted.checkpoint).toEqual(next);
      token = restarted.token;
      return next;
    };

    await sqlRun(
      (sql) =>
        sql`UPDATE deposits_utxos SET event_info = event_info || ${Buffer.from("00", "hex")} WHERE event_id = ${deposit}`,
    );
    const first = await admit(started.checkpoint, "first", "deposit");
    let current = await detected(
      started.checkpoint,
      first.prepared,
      /same event_id has conflicting persisted payload/,
      (sql) =>
        sql`UPDATE deposits_utxos SET event_info = substring(event_info FROM 1 FOR octet_length(event_info) - 1) WHERE event_id = ${deposit}`,
    );

    await sqlRun(
      (sql) =>
        sql`UPDATE withdrawal_utxos SET history_binding_digest = NULL, history_incarnation_id = NULL WHERE event_id = ${Buffer.from(withdrawal.event.idCbor, "hex")}`,
    );
    current = await detected(
      current,
      (await admit(current, "second", "withdrawal")).prepared,
      /without its exact history incarnation/,
      (sql) =>
        sql`UPDATE withdrawal_utxos SET history_binding_digest = ${Buffer.from(binding.digest, "hex")}, history_incarnation_id = ${Buffer.from(withdrawal.id, "hex")} WHERE event_id = ${Buffer.from(withdrawal.event.idCbor, "hex")}`,
    );

    // A local row no incarnation admits, under an unused public ID.
    await sqlRun((sql) =>
      sql.withTransaction(
        Effect.all([
          sql`CREATE TEMPORARY TABLE stray_deposit ON COMMIT DROP AS SELECT * FROM deposits_utxos WHERE event_id = ${deposit}`,
          sql`UPDATE stray_deposit SET event_id = ${stray}, history_binding_digest = NULL, history_incarnation_id = NULL`,
          sql`INSERT INTO deposits_utxos SELECT * FROM stray_deposit`,
        ]),
      ),
    );
    await detected(
      current,
      (await move(current, "third", "deposit", first.order)).prepared,
      /Local event eligibility is not backed by this canonical history/,
      (sql) => sql`DELETE FROM deposits_utxos WHERE event_id = ${stray}`,
    );
  });

  it("the startup resume checks the incarnations on both sides of a walk chunk boundary", async () => {
    const started = await startReady(
      await retiredHistory(MATERIALIZATION_CHUNK + 2),
    );
    let token = started.token;
    for (const position of [
      MATERIALIZATION_CHUNK - 1,
      MATERIALIZATION_CHUNK,
      MATERIALIZATION_CHUNK + 1,
    ]) {
      const incarnation = started.checkpoint.incarnations[position]!;
      const eventId = Buffer.from(incarnation.event.idCbor, "hex");
      const [table, column] =
        incarnation.kind === "deposit"
          ? [Deposits.tableName, "event_info"]
          : [Withdrawals.tableName, "raw_event_info"];
      await sqlRun(
        (sql) =>
          sql`UPDATE ${sql(table)} SET ${sql(column)} = ${sql(column)} || ${Buffer.from("00", "hex")} WHERE event_id = ${eventId}`,
      );
      await expect(restart(token)).rejects.toThrow(
        /same event_id has conflicting persisted payload/,
      );
      await sqlRun(
        (sql) =>
          sql`UPDATE ${sql(table)} SET ${sql(column)} = substring(${sql(column)} FROM 1 FOR octet_length(${sql(column)}) - 1) WHERE event_id = ${eventId}`,
      );
      const restarted = await restart(token);
      expect(restarted.checkpoint).toEqual(started.checkpoint);
      token = restarted.token;
    }
  }, 300_000);
});
