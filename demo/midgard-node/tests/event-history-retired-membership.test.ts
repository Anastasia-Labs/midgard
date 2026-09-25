import { createHash, randomUUID } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { pendingHistoryLedgerDisposition } from "../src/database/eventHistoryLedgerRepair.js";
import { materializeCanonicalHistory } from "../src/database/eventHistoryMaterialization.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { HistoryTransition } from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import { HistoryProducer } from "../src/services/event-history-producer.js";
import {
  signedHeaderRecoveryCandidates,
  signedHeaderRecoveryHoldSlot,
} from "../src/services/history-signed-header-recovery.js";
import {
  evaluateSignedIntentCoverage,
  type SignedIntentCoverageBlock,
} from "../src/services/signed-intent-canonical-coverage.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { retainEverything } from "./helpers/history-journal-retention.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { applyMidgardNodeTestEnv, testDatabaseName } from "./test-env.js";

// PostgreSQL schema regression only. Strict decoded history snapshots and branch
// ancestry are modeled. Direct DELETE below runs under actual recovery ownership
// but deliberately does not claim to authorize signed-header release/native undo.
// The signed journal body is real CML construction, not an accepted L1 commitment.
applyMidgardNodeTestEnv();
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const modelOriginReceipt =
  "Explicit model source replay evidence; not ledger admission";
const owner = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7))
  .to_public()
  .hash()
  .to_hex();
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    program.pipe(
      Effect.mapError((error) => new Error(formatDatabaseError(error))),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
let binding: EventHistorySourceBinding;
let initial: Journal.Checkpoint["capture"];
let pair: SDK.EventHistoryContractPair;
const rootDatum = (next: string | null = null) =>
  Data.to(
    { position: "Root", next, protected_until: 0n, payload: "RootContent" },
    SDK.EventHistoryNode,
  );
const acquire = () =>
  Authority.acquire({
    deploymentIdentity: binding.manifestId,
    ownerToken: randomUUID(),
    leaseDurationMs: 60_000,
  });
const read = async () => {
  const result = await run(Journal.load(binding));
  if (result === null) throw new Error("Missing test checkpoint");
  return result;
};
const start = async () => {
  const token = await run(acquire());
  await run(
    Authority.withRecovery(
      token,
      Journal.seed({
        binding,
        capture: initial,
        height: 1,
        originReceipt: modelOriginReceipt,
        originReceiptDigest: sha(modelOriginReceipt),
        incarnations: [],
      }),
    ),
  );
  return { token, checkpoint: await read() };
};
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
  const outputs: LedgerSnapshotOutput[] = Object.values(
    binding.deployments,
  ).map((entry, outputIndex) => ({
    txHash: hash(910),
    outputIndex,
    address: entry.address,
    assets: { lovelace: 3_000_000n, [entry.policyId]: 1n },
    datum: rootDatum(),
    hasReferenceScript: false,
  }));
  outputs.push({
    txHash: hash(911),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  });
  initial = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        point: { id: hash(1), slot: 100 },
        addresses: [
          binding.hubAddress,
          ...Object.values(binding.deployments).flatMap((entry) => [
            entry.address,
            entry.retentionAddress,
          ]),
        ],
        outputs,
      },
      binding,
    ),
  );
});
const prepare = async (
  checkpoint: Journal.Checkpoint,
  n: number,
  transitions: readonly HistoryTransition[] = [],
  outputs = checkpoint.capture.history.ledger.outputs,
) => {
  const block: BoundHistoryChainBlock = {
    parent: checkpoint.head.id,
    point: {
      id: hash(n),
      slot: checkpoint.head.slot + 1,
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
const admit = async (
  checkpoint: Journal.Checkpoint,
  n: number,
  kind: "deposit" | "withdrawal" = "deposit",
  eventNumber = 999,
) => {
  const destination = {
    paymentCredential: { PublicKeyCredential: [owner] as [string] },
    stakeCredential: null,
  };
  const id = { transactionId: hash(eventNumber), outputIndex: 0n };
  const eventKey = await Effect.runPromise(SDK.eventHistoryKey(id));
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: destination,
                l2_network_id: 0n,
                l2_datum: "ab",
              },
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
                  l1_address: destination,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: destination,
            refund_datum: "NoDatum",
          },
        };
  const plan = SDK.prepareEventHistoryPayload(
    payload,
    destination.paymentCredential,
    pair[kind].recipe,
  );
  const facts: SDK.EventHistoryFacts = {
    event_id: id,
    inclusion_time: 1_000_000n,
    location: plan.location,
    structural_lovelace: 2_000_000n,
    structural_refund_key: owner,
  };
  const nodes = checkpoint.capture.history.ledger.outputs
    .filter((output) => output.address === binding.deployments[kind].address)
    .map((output) => ({
      output,
      node: Data.from(output.datum!, SDK.EventHistoryNode),
    }));
  const predecessor = nodes.find(
    ({ node }) =>
      (node.position === "Root" || node.position.Key[0] < eventKey) &&
      (node.next === null || node.next > eventKey),
  );
  if (predecessor === undefined)
    throw new Error("Missing modeled insertion predecessor");
  const continued = {
    ...predecessor.output,
    txHash: hash(n + 1000),
    outputIndex: 0,
    datum: Data.to(
      { ...predecessor.node, next: eventKey },
      SDK.EventHistoryNode,
    ),
  };
  const order: LedgerSnapshotOutput = {
    ...predecessor.output,
    txHash: hash(n + 1000),
    outputIndex: 1,
    datum: Data.to(
      {
        position: { Key: [eventKey] },
        next: predecessor.node.next,
        protected_until: 0n,
        payload: { Order: { facts } },
      },
      SDK.EventHistoryNode,
    ),
    assets: {
      lovelace: 7_000_000n,
      [binding.deployments[kind].policyId + eventKey]: 1n,
    },
  };
  const outputs = [
    ...checkpoint.capture.history.ledger.outputs.filter(
      (output) => output !== predecessor.output,
    ),
    continued,
    order,
  ];
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      { ...checkpoint.capture.history.ledger, outputs },
      binding,
    ),
  );
  const event = (
    kind === "deposit" ? capture.history.deposits : capture.history.withdrawals
  ).find((event) => event.event.id.transactionId === id.transactionId);
  if (event === undefined) throw new Error("Missing modeled admission");
  const transition: HistoryTransition = {
    kind,
    operation: "InsertOrder",
    transactionHash: order.txHash,
    consumed: [predecessor.output],
    produced: [continued, order],
    continuations:
      predecessor.node.position === "Root"
        ? []
        : [
            {
              key: predecessor.node.position.Key[0],
              before: predecessor.output,
              after: continued,
            },
          ],
    admission: {
      key: eventKey,
      idCbor: Data.to(id, SDK.OutputReference),
      inclusionTime: facts.inclusion_time,
      factsCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(order.datum!, [3, 0]),
      ),
      payloadCbor: event.history.payloadCbor,
      originalAssetsCbor: Data.to(
        SDK.assetsToValue(event.originalAssets),
        SDK.Value,
      ),
      outRef: { txHash: order.txHash, outputIndex: order.outputIndex },
    },
  };
  return prepare(checkpoint, n, [transition], outputs);
};

beforeEach(async () => {
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const [database] = yield* sql<{
        name: string;
      }>`SELECT current_database() AS name`;
      expect(database?.name).toBe(testDatabaseName());
      yield* sql`TRUNCATE event_history_recovery_plans, event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalizations, pending_block_finalization_deposits, pending_block_finalization_withdrawals, pending_block_finalization_txs, pending_block_finalization_forced_transactions, pending_block_finalization_transition_trace, pending_block_finalization_event_to_step, pending_block_finalization_validation_traces, pending_block_finalization_validation_trace_witnesses, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts, tx_admission_payloads, tx_admissions, mempool, mempool_tx_deltas, address_history, processed_mempool, blocks, immutable`;
    }),
  );
});
const prepareSignedHeader = async (
  token: Authority.Token,
  checkpoint: Journal.Checkpoint,
  validity?: Readonly<{ start: bigint; ttl: bigint }>,
) => {
  await run(
    Authority.publishReady(token, {
      point: checkpoint.head,
      snapshotDigest: checkpoint.capture.snapshotDigest,
    }),
  );
  const permit = {
    token,
    coverage: {
      bindingDigest: binding.digest,
      checkpointRevision: checkpoint.revision,
      point: checkpoint.head,
      snapshotDigest: checkpoint.capture.snapshotDigest,
      includedThroughMs: checkpoint.head.slot,
    },
  };
  const ready = <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) =>
    run(
      Authority.withReady(
        token,
        work.pipe(Effect.provideService(HistoryProducer, permit)),
      ),
    );
  const root = SDK.EMPTY_MERKLE_TREE_ROOT;
  const roots = {
    utxosRoot: root,
    forcedTransactionsRoot: root,
    transactionsRoot: root,
    depositsRoot: root,
    withdrawalsRoot: root,
  };
  const expectedRoots = {
    ...roots,
    transitionTraceRoot: root,
    eventToStepRoot: root,
    validationTracesRoot: root,
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const time = new Date(1_000_000);
  const header: SDK.Header = {
    ...expectedRoots,
    ...counts,
    prevUtxosRoot: root,
    startTime: BigInt(time.getTime()),
    endTime: BigInt(time.getTime() + 60_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "41".repeat(28),
    operatorVkey: owner,
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  const signedCbor = Buffer.from(makeCardanoSignedMapOutputTxBytes(validity));
  const txHash = Buffer.from(
    CML.hash_transaction(
      CML.Transaction.from_cbor_bytes(signedCbor).body(),
    ).to_hex(),
    "hex",
  );
  // A genuine signed body retained in a modeled pending journal: no claim that
  // this L1 transaction was submitted or that this header has chain authority.
  await ready(
    Pending.preparePendingSubmission({
      headerHash,
      headerCbor: Buffer.from(Data.to(header, SDK.Header), "hex"),
      preparedTxHash: txHash,
      metadata: {
        deploymentMarker: makeDeploymentMarker(binding.manifestId),
        consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
        stateQueueLeaseToken: "modeled-pending-owner",
        baseSnapshotId: "modeled-pending-base",
        baseTailOutRef: `${hash(980)}#0`,
        baseTailHeaderHash: Buffer.alloc(28, 0x41),
        baseTailDatumCbor: "d87980",
        baseRoots: roots,
        blockStartTime: time,
        expectedRoots,
        expectedCounts: counts,
      },
      blockEndTime: new Date(time.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
      forcedTransactionEventIds: [],
      forcedTransactionEntries: [],
      withdrawalEventIds: [],
      withdrawalEntries: [],
      mempoolTxIds: [],
      mempoolTxs: [],
      mempoolTxSourceTable: "none",
      transitionTraceMembers: [],
      eventToStepMembers: [],
      validationTraceMembers: [],
      validationTraceWitnessMembers: [],
      ledgerDelta: { spent: [], produced: [] },
    }),
  );
  // The preparation above has committed. The signed-intent writer must acquire
  // its own Ready transaction; inheriting ready's SQL transaction is refused.
  await run(
    Pending.recordSignedIntent(headerHash, txHash, signedCbor).pipe(
      Effect.provideService(HistoryProducer, permit),
    ),
  );
  return {
    headerHash,
    signedCbor,
    token: await run(
      Authority.beginRecovery(token, "Schema-only member retention exercise"),
    ),
  };
};

type Kind = "deposit" | "withdrawal";
const tables = (kind: Kind) => ({
  live: kind === "deposit" ? "deposits_utxos" : "withdrawal_utxos",
  members:
    kind === "deposit"
      ? "pending_block_finalization_deposits"
      : "pending_block_finalization_withdrawals",
});
const rows = (table: string, order: string) =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        row: Record<string, unknown>;
      }>`SELECT to_jsonb(t) AS row FROM ${sql(table)} t ORDER BY ${sql(order)}`;
    }),
  );
const reconcile = (before: Journal.Checkpoint) =>
  Journal.load(binding).pipe(
    Effect.flatMap((after) =>
      after === null
        ? Effect.die("Missing checkpoint")
        : materializeCanonicalHistory(
            { kind: "forward", before, after },
            "Preprod",
          ),
    ),
  );
const fixture = async (
  kind: Kind,
  validity?: Readonly<{ start: bigint; ttl: bigint }>,
) => {
  const started = await start();
  for (const [height, eventNumber] of [
    [2, 999],
    [3, 998],
  ] as const) {
    const before = await read();
    await run(
      Authority.withRecovery(
        started.token,
        Journal.append(
          binding,
          await admit(before, height, kind, eventNumber),
          reconcile(before),
          retainEverything,
        ),
      ),
    );
  }
  const checkpoint = await read();
  const first = checkpoint.incarnations.find(
    (i) => i.event.outRef.txHash === hash(1002),
  );
  const other = checkpoint.incarnations.find(
    (i) => i.event.outRef.txHash === hash(1003),
  );
  if (first === undefined || other === undefined)
    throw new Error("Missing distinct modeled admissions");
  expect(first.event.idCbor).not.toBe(other.event.idCbor);
  const retained = await prepareSignedHeader(
    started.token,
    checkpoint,
    validity,
  );
  const table = tables(kind);
  const payload = Buffer.from(
    plutusConstrFieldCbor(first.event.payloadCbor, [0, 1]),
    "hex",
  );
  const member = {
    header_hash: retained.headerHash,
    member_id: Buffer.from(first.event.idCbor, "hex"),
    ordinal: 0,
    payload_cbor: payload,
    payload_sha256: createHash("sha256").update(payload).digest(),
    source_table: table.live,
    source_id: Buffer.from(first.event.idCbor, "hex"),
    source_time_stamp_tz: new Date(Number(first.event.inclusionTime)),
    history_binding_digest: Buffer.from(binding.digest, "hex"),
    history_incarnation_id: Buffer.from(first.id, "hex"),
  };
  await run(
    Authority.withRecovery(
      retained.token,
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Retained member bytes are inserted directly to isolate FK behavior from
        // classification/commit construction. No modeled validity becomes authority.
        const entry =
          kind === "deposit"
            ? member
            : {
                ...member,
                validity: "WithdrawalIsValid",
                validity_detail: sql`'{}'::jsonb`,
                classification_revision: 0,
                classification_sha256: createHash("sha256")
                  .update(payload)
                  .digest(),
              };
        yield* sql`INSERT INTO ${sql(table.members)} (${sql.csv(Object.keys(entry).map((column) => sql`${sql(column)}`))}) VALUES (${sql.csv(Object.values(entry).map((value) => sql`${value}`))})`;
      }),
    ),
  );
  return { ...retained, checkpoint, first, other, table, member };
};

// All refused mutations are caught outside withRecovery, so PostgreSQL rolls
// back the whole attempted write; no exception is swallowed inside a transaction.
const expectForeignKey = async (
  program: Effect.Effect<unknown, unknown, SqlClient.SqlClient>,
  constraint?: string,
) => {
  const result = await run(program.pipe(Effect.either));
  expect(result._tag).toBe("Left");
  if (result._tag !== "Left") throw new Error("Expected foreign-key refusal");
  expect(formatDatabaseError(result.left)).toContain("23503");
  if (constraint !== undefined)
    expect(formatDatabaseError(result.left)).toContain(constraint);
};

describe.each(["deposit", "withdrawal"] as const)(
  "retained %s membership",
  (kind) => {
    it("preserves exact old signed header/member through owned live-row removal and same-ID replacement", async () => {
      const f = await fixture(kind);
      const archived = await rows(f.table.members, "ordinal");
      const headers = await rows("pending_block_finalizations", "header_hash");
      expect(headers).toHaveLength(1);
      expect(headers[0]!.row.signed_tx_cbor).toBe(
        `\\x${f.signedCbor.toString("hex")}`,
      );
      // Undo modeled list admissions only. Bounded schema exercise deliberately
      // omits production disposition: no native release or financial repair claim.
      for (let n = 0; n < 2; n++)
        await run(
          Authority.withRecovery(
            f.token,
            Journal.undoHead(binding, await read(), Effect.void),
          ),
        );
      const orphaned = await read();
      expect(
        orphaned.incarnations.find((i) => i.id === f.first.id)?.placement,
      ).toBeNull();
      await run(
        Authority.withRecovery(
          f.token,
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const ids = [
              Buffer.from(f.first.event.idCbor, "hex"),
              Buffer.from(f.other.event.idCbor, "hex"),
            ];
            if (kind === "deposit")
              yield* sql`DELETE FROM mempool_ledger WHERE source_event_id IN ${sql.in(ids)}`;
            const removed = yield* sql<{
              event_id: Buffer;
            }>`DELETE FROM ${sql(f.table.live)} WHERE event_id IN ${sql.in(ids)} RETURNING event_id`;
            expect(
              removed.map((r) => r.event_id.toString("hex")).sort(),
            ).toEqual(ids.map((id) => id.toString("hex")).sort());
          }),
        ),
      );
      expect(await rows(f.table.members, "ordinal")).toEqual(archived);
      expect(await rows("pending_block_finalizations", "header_hash")).toEqual(
        headers,
      );
      expect(await rows(f.table.live, "event_id")).toEqual([]);
      // Actual Journal.append derives a new incarnation from a different creator.
      const before = await read();
      await run(
        Authority.withRecovery(
          f.token,
          Journal.append(
            binding,
            await admit(before, 4, kind, 999),
            reconcile(before),
            retainEverything,
          ),
        ),
      );
      const current = await read();
      const fresh = current.incarnations.find(
        (i) => i.event.idCbor === f.first.event.idCbor && i.placement !== null,
      );
      expect(fresh).toBeDefined();
      if (fresh === undefined) throw new Error("Missing fresh incarnation");
      expect(fresh.id).not.toBe(f.first.id);
      expect(fresh.event.outRef.txHash).not.toBe(f.first.event.outRef.txHash);
      const live = await rows(f.table.live, "event_id");
      expect(live).toHaveLength(1);
      expect(live[0]!.row.history_incarnation_id).toBe(`\\x${fresh.id}`);
      expect(await rows(f.table.members, "ordinal")).toEqual(archived);
      expect(await rows("pending_block_finalizations", "header_hash")).toEqual(
        headers,
      );
      // The retained original cannot disappear while archived membership points
      // to it, even though no live row refers to this old incarnation anymore.
      await expectForeignKey(
        Authority.withRecovery(
          f.token,
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM event_history_incarnations WHERE binding_digest=${Buffer.from(binding.digest, "hex")} AND incarnation_id=${Buffer.from(f.first.id, "hex")}`;
          }),
        ),
      );
      expect(await rows(f.table.members, "ordinal")).toEqual(archived);
    });

    it.each([
      "member_id",
      "history_incarnation_id",
      "history_binding_digest",
    ] as const)(
      "refuses mismatched %s while preserving all archived bytes",
      async (field) => {
        const f = await fixture(kind);
        const before = await rows(f.table.members, "ordinal");
        const value =
          field === "member_id"
            ? Buffer.from(f.other.event.idCbor, "hex")
            : field === "history_incarnation_id"
              ? Buffer.from(f.other.id, "hex")
              : Buffer.from(hash(2222), "hex");
        await expectForeignKey(
          Authority.withRecovery(
            f.token,
            Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              yield* sql`UPDATE ${sql(f.table.members)} SET ${sql(field)}=${value} WHERE header_hash=${f.headerHash}`;
            }),
          ),
          field === "history_binding_digest"
            ? undefined
            : `${f.table.members}_member_id_fkey`,
        );
        expect(await rows(f.table.members, "ordinal")).toEqual(before);
        const live = await rows(f.table.live, "event_id");
        expect(live).toHaveLength(2);
      },
    );

    it("retains the header foreign key", async () => {
      const f = await fixture(kind);
      const before = await rows(f.table.members, "ordinal");
      await expectForeignKey(
        Authority.withRecovery(
          f.token,
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE ${sql(f.table.members)} SET header_hash=${Buffer.alloc(28, 0xff)}`;
          }),
        ),
        `${f.table.members}_header_hash_fkey`,
      );
      expect(await rows(f.table.members, "ordinal")).toEqual(before);
    });
  },
);

// Retention hold: the single-sourced recovery candidate query pins the journal
// anchor so the retained canonical range still starts at or before the signed
// validity start. The hold is bounded: once the header's TTL is more than the
// finality depth behind the tip, that retained range classifies it
// covered_absent, and the abandonment recovery then writes releases the hold.
it("holds the retention anchor behind a stale unresolved signed header until recovery abandons it", async () => {
  const validity = { start: 103n, ttl: 104n };
  const f = await fixture("deposit", validity);
  for (let n = 0; n < 2; n++)
    await run(
      Authority.withRecovery(
        f.token,
        Journal.undoHead(binding, await read(), Effect.void),
      ),
    );
  expect(
    (await run(signedHeaderRecoveryCandidates(binding.digest))).map((row) =>
      row.header_hash.toString("hex"),
    ),
  ).toEqual([f.headerHash.toString("hex")]);
  expect(await run(signedHeaderRecoveryHoldSlot(binding.digest))).toBe(103);
  // Horizon 1 against a far tip: without the hold every step would advance
  // the anchor to one block behind head.
  let hold: Journal.RetentionHold | undefined;
  const forward = async (n: number) => {
    const prepared = await prepare(await read(), n);
    const appended = await run(
      Authority.withRecovery(
        f.token,
        signedHeaderRecoveryHoldSlot(binding.digest).pipe(
          Effect.flatMap((holdSlot) =>
            Journal.append(binding, prepared, Effect.void, {
              tipHeight: 10_000,
              horizon: 1,
              holdSlot,
            }),
          ),
        ),
      ),
    );
    hold = appended.applied ? appended.hold : undefined;
    return read();
  };
  let current = await read();
  for (let n = 20; n < 26; n++) current = await forward(n);
  // Reported, not silent: without the hold the anchor would be at height 6.
  expect(hold).toEqual({
    holdSlot: 103,
    anchorHeight: 3,
    unheldAnchorHeight: 6,
  });
  // Heights 2..7 at slots 101..106. The first retained block (height 4, slot
  // 103) is the last one not later than the signed validity start.
  expect(current.head).toEqual({ id: hash(25), slot: 106, height: 7 });
  expect(current.anchor).toEqual({ id: hash(21), slot: 102, height: 3 });
  const applications = await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        ledger_receipt: string;
        canonical: boolean;
      }>`SELECT ledger_receipt, canonical FROM event_history_block_applications
        WHERE binding_digest = ${Buffer.from(binding.digest, "hex")} ORDER BY block_height`;
    }),
  );
  // The undone admission blocks were orphans rooted behind the anchor.
  expect(applications.every((row) => row.canonical)).toBe(true);
  const blocks = applications.map((row) => {
    const { block } = JSON.parse(row.ledger_receipt) as {
      block: SignedIntentCoverageBlock;
    };
    return {
      point: block.point,
      parent: block.parent,
      transactions: block.transactions,
    };
  });
  expect(blocks.map((block) => block.point.height)).toEqual([4, 5, 6, 7]);
  const txHash = CML.hash_transaction(
    CML.Transaction.from_cbor_bytes(f.signedCbor).body(),
  ).to_hex();
  const classify = (range: typeof blocks) =>
    evaluateSignedIntentCoverage({
      signedTxCbor: f.signedCbor.toString("hex"),
      expectedTxHash: txHash,
      bindingDigest: binding.digest,
      manifestId: binding.manifestId,
      start: range[0]!.point,
      head: range.at(-1)!.point,
      blocks: range,
      requiredFinalityDepth: 2,
    });
  expect(classify(blocks).kind).toBe("covered_absent");
  // One more block of pruning would have made it unclassifiable.
  expect(() => classify(blocks.slice(1))).toThrow(
    /omits the earliest-inclusion boundary/,
  );
  // Recovery's covered_absent disposition abandons the header; the next
  // forward step then advances the anchor as far as the horizon allows.
  await run(
    Authority.withRecovery(
      f.token,
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations SET status = 'abandoned'
          WHERE header_hash = ${f.headerHash}`;
      }),
    ),
  );
  expect(await run(signedHeaderRecoveryHoldSlot(binding.digest))).toBe(
    undefined,
  );
  current = await forward(26);
  expect(hold).toBeUndefined();
  expect(current.anchor).toEqual({ id: hash(25), slot: 106, height: 7 });
  expect(current.head.height).toBe(8);
});

// A candidate recovery cannot classify from coverage (no signed validity start)
// bounds no retention, but it is not ignored: its orphaned member keeps the
// history disposition pending, and the owner never publishes Ready while a
// disposition is pending (history-source-owner-pending-recovery.test.ts).
it("keeps an unclassifiable pinned candidate pending without holding retention", async () => {
  const f = await fixture("deposit");
  for (let n = 0; n < 2; n++)
    await run(
      Authority.withRecovery(
        f.token,
        Journal.undoHead(binding, await read(), Effect.void),
      ),
    );
  const after = await read();
  expect(
    (await run(signedHeaderRecoveryCandidates(binding.digest))).map((row) =>
      row.header_hash.toString("hex"),
    ),
  ).toEqual([f.headerHash.toString("hex")]);
  expect(await run(signedHeaderRecoveryHoldSlot(binding.digest))).toBe(
    undefined,
  );
  expect(
    await run(
      Authority.withRecovery(
        f.token,
        pendingHistoryLedgerDisposition({
          kind: "resume",
          before: after,
          after,
        }),
      ),
    ),
  ).toMatchObject({ status: "pending" });
});

it("fails closed, naming the header, on an unreadable stored signed body", async () => {
  const f = await fixture("deposit", { start: 103n, ttl: 104n });
  for (let n = 0; n < 2; n++)
    await run(
      Authority.withRecovery(
        f.token,
        Journal.undoHead(binding, await read(), Effect.void),
      ),
    );
  expect(await run(signedHeaderRecoveryHoldSlot(binding.digest))).toBe(103);
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE pending_block_finalizations SET signed_tx_cbor = ${Buffer.from("84ff", "hex")}
        WHERE header_hash = ${f.headerHash}`;
    }),
  );
  await expect(
    run(signedHeaderRecoveryHoldSlot(binding.digest)),
  ).rejects.toThrow(
    `Recovery candidate ${f.headerHash.toString("hex")} has an unreadable signed body`,
  );
});
