import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import type { AdoptionLedgerRow } from "../services/foreign-native-adoption-projection.js";
import type { PersistedNativeMpfReplay } from "../services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../services/mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
  EVENT_LOG_HEADER_BYTES,
} from "../services/mpf-native-owner/service.normalize-owner-options.js";
import type { VerifiedForeignCommitBase } from "../workers/commit-block-header.verify-foreign-base.js";
import {
  currentOwnedTransaction,
  requireRecoveryTransaction,
  requireSourceTransaction,
} from "./eventHistoryAuthority.js";
import { requeueUnpublishedHistoryLedger } from "./eventHistoryLedgerRepair.js";
import { retainVerifiedForeignSegments } from "./foreignVerifiedSegments.js";
import * as MpfEngineState from "./mpfEngineState.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "foreign_native_adoptions";
const refuse = (message: string) =>
  Effect.fail(
    new DatabaseError({ table: tableName, message, cause: undefined }),
  );
type ReplayRecord = Omit<
  PersistedNativeMpfReplay,
  "eventLog" | "eventRoots"
> & {
  readonly eventLog: string;
  readonly eventRoots: string;
  readonly nativeNoop?: true;
};
export type Adoption = Readonly<{
  sequence: string;
  adoption_id: Buffer;
  binding_digest: Buffer;
  manifest_id: Buffer;
  source_hash: Buffer;
  source_slot: string;
  source_snapshot: Buffer;
  header_hash: Buffer;
  target_root: string;
  state: "requested" | "prepared" | "applied" | "rewinding";
  replay_record: ReplayRecord | null;
  canonical: boolean;
  removed: boolean;
}>;
const replayRecord = (replay: PersistedNativeMpfReplay): ReplayRecord => ({
  ...replay,
  eventLog: Buffer.from(replay.eventLog).toString("hex"),
  eventRoots: Buffer.from(replay.eventRoots).toString("hex"),
});
export type AdoptionEvents = Readonly<{
  deposits: readonly {
    id: string;
    header: string;
    status: "projected" | "consumed";
  }[];
  forced: readonly { id: string; header: string }[];
  withdrawals: readonly {
    id: string;
    header: string;
    validity: string;
    detail: unknown;
    settlement: string;
  }[];
}>;

const eventTables = [
  { name: "deposits_utxos", id: "event_id", key: "deposits" },
  { name: "forced_transaction_utxos", id: "tx_order_id", key: "forced" },
  { name: "withdrawal_utxos", id: "event_id", key: "withdrawals" },
] as const;
const retainEventProjection = (adoptionId: Buffer, events: AdoptionEvents) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE foreign_native_adoptions SET event_before = '{}'::jsonb,
    event_after = ${JSON.stringify(events)}::jsonb WHERE adoption_id = ${adoptionId}`;
    for (const table of eventTables) {
      const expected = events[table.key];
      const provenance =
        table.key === "forced"
          ? sql`TRUE`
          : sql`
        actual.history_binding_digest = (SELECT binding_digest FROM foreign_native_adoptions WHERE adoption_id = ${adoptionId})
        AND EXISTS (SELECT 1 FROM event_history_incarnations i WHERE i.binding_digest = actual.history_binding_digest
          AND i.incarnation_id = actual.history_incarnation_id AND i.origin_canonical AND i.event_id = actual.${sql(table.id)})`;
      const rows = yield* sql`SELECT 1 FROM ${sql(table.name)} actual,
      jsonb_to_recordset(${JSON.stringify(expected)}::jsonb) e(id text,header text)
      WHERE actual.${sql(table.id)} = decode(e.id,'hex')
        AND (actual.projected_header_hash IS NULL OR actual.projected_header_hash = decode(e.header,'hex'))
        AND actual.status <> 'finalized' AND ${provenance} FOR UPDATE OF actual`;
      if (rows.length !== expected.length)
        return yield* refuse(
          "Foreign event projection is missing or assigned to another header",
        );
      yield* sql`UPDATE foreign_native_adoptions SET event_before = event_before ||
      jsonb_build_object(${table.key},(SELECT COALESCE(jsonb_agg(to_jsonb(actual)),'[]'::jsonb)
        FROM ${sql(table.name)} actual WHERE actual.${sql(table.id)} IN
          (SELECT decode(e.id,'hex') FROM jsonb_to_recordset(${JSON.stringify(expected)}::jsonb) e(id text))))
      WHERE adoption_id = ${adoptionId}`;
    }
  });
const applyEventProjection = (adoptionId: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE deposits_utxos d SET status = e.status, projected_header_hash = decode(e.header,'hex')
    FROM foreign_native_adoptions p,
      LATERAL jsonb_to_recordset(p.event_after->'deposits') e(id text,header text,status text)
    WHERE p.adoption_id = ${adoptionId} AND d.event_id = decode(e.id,'hex')`;
    yield* sql`UPDATE forced_transaction_utxos d SET status = 'projected', projected_header_hash = decode(e.header,'hex'),updated_at = NOW()
    FROM foreign_native_adoptions p,
      LATERAL jsonb_to_recordset(p.event_after->'forced') e(id text,header text)
    WHERE p.adoption_id = ${adoptionId} AND d.tx_order_id = decode(e.id,'hex')`;
    yield* sql`UPDATE withdrawal_utxos d SET status = 'projected', projected_header_hash = decode(e.header,'hex'),
      validity = e.validity, validity_detail = e.detail, settlement_event_info = decode(e.settlement,'hex'),
      classification_revision = d.classification_revision + 1,updated_at = NOW()
    FROM foreign_native_adoptions p,
      LATERAL jsonb_to_recordset(p.event_after->'withdrawals') e(id text,header text,validity text,detail jsonb,settlement text)
    WHERE p.adoption_id = ${adoptionId} AND d.event_id = decode(e.id,'hex')`;
  });
const inverseEventProjection = (adoptionId: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const table of eventTables) {
      const sameOrigin =
        table.key === "forced"
          ? sql`d.raw_datum = old.raw_datum
      AND d.tx_order_l1_tx_hash = old.tx_order_l1_tx_hash AND d.tx_order_l1_output_index = old.tx_order_l1_output_index`
          : sql`d.history_binding_digest = old.history_binding_digest AND d.history_incarnation_id = old.history_incarnation_id`;
      yield* sql`UPDATE ${sql(table.name)} d SET status = old.status, projected_header_hash = old.projected_header_hash
      FROM foreign_native_adoptions p,
        LATERAL jsonb_populate_recordset(NULL::${sql(table.name)},p.event_before->${table.key}) old
      WHERE p.adoption_id = ${adoptionId} AND d.${sql(table.id)} = old.${sql(table.id)}
        AND ${sameOrigin}`;
    }
    yield* sql`UPDATE withdrawal_utxos d SET validity = old.validity, validity_detail = old.validity_detail,
    settlement_event_info = old.settlement_event_info, classification_revision = d.classification_revision + 1, updated_at = NOW()
    FROM foreign_native_adoptions p,
      LATERAL jsonb_populate_recordset(NULL::withdrawal_utxos,p.event_before->'withdrawals') old
    WHERE p.adoption_id = ${adoptionId} AND d.event_id = old.event_id
      AND d.history_binding_digest = old.history_binding_digest AND d.history_incarnation_id = old.history_incarnation_id`;
  });
export const retainedReplay = (plan: Adoption): PersistedNativeMpfReplay => {
  const record = plan.replay_record;
  if (
    record === null ||
    record.schema !== 1 ||
    !/^[0-9a-f]{64}$/.test(record.ownerBinarySha256) ||
    !/^[0-9a-f]{64}$/.test(record.baseRoot) ||
    !/^[0-9a-f]{64}$/.test(record.candidateRoot) ||
    record.candidateRoot !== plan.target_root ||
    !/^[0-9a-f]{64}$/.test(record.eventLogDigest) ||
    !/^(?:[0-9a-f]{2})*$/.test(record.eventLog) ||
    !/^(?:[0-9a-f]{64})*$/.test(record.eventRoots) ||
    !Number.isSafeInteger(record.eventCount) ||
    record.eventCount < 0 ||
    record.eventRoots.length !== record.eventCount * 64
  )
    throw new Error("Retained foreign adoption replay is malformed");
  const eventLog = Buffer.from(record.eventLog, "hex");
  if (
    eventLog.length < EVENT_LOG_HEADER_BYTES ||
    eventLog.subarray(0, 4).toString("ascii") !== "MEGO" ||
    eventLog.readUInt16LE(4) !== 1 ||
    eventLog.readUInt16LE(6) !== 0 ||
    eventLog.readUInt32LE(8) !== record.eventCount ||
    eventLog.subarray(28, 60).toString("hex") !== record.baseRoot ||
    digest(EVENT_LOG_DIGEST_DOMAIN, eventLog).toString("hex") !==
      record.eventLogDigest ||
    (record.nativeNoop === true &&
      (record.baseRoot !== record.candidateRoot ||
        record.eventCount !== 0 ||
        !eventLog.equals(encodeNativeMpfEventLog(record.baseRoot, []))))
  )
    throw new Error(
      "Retained foreign adoption replay does not match its immutable operation",
    );
  return {
    ...record,
    eventLog,
    eventRoots: Buffer.from(record.eventRoots, "hex"),
  };
};
export const isNativeNoop = (plan: Adoption) =>
  plan.replay_record?.nativeNoop === true;

/** Root equality cannot account for rejected forced events, empty withdrawals
 * or other canonical events without ledger mutations. Check actual assignments. */
export const hasAppliedForeignBase = (base: VerifiedForeignCommitBase) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const block of base.importedBlocks) {
      if (block.kind === "local") continue;
      if (block.memberships === undefined) return false;
      const members = {
        deposits: block.memberships.deposits.map(({ id }) => ({
          id: id.toString("hex"),
        })),
        forced: block.memberships.forcedTransactions.map((id) => ({
          id: id.toString("hex"),
        })),
        withdrawals: block.memberships.withdrawals.map(({ id }) => ({
          id: id.toString("hex"),
        })),
      };
      for (const table of eventTables) {
        const matched = yield* sql`SELECT 1 FROM ${sql(table.name)} d,
        jsonb_to_recordset(${JSON.stringify(members[table.key])}::jsonb) e(id text)
        WHERE d.${sql(table.id)} = decode(e.id,'hex') AND d.projected_header_hash = ${Buffer.from(block.headerHash, "hex")}
          AND d.status <> 'awaiting'`;
        if (matched.length !== members[table.key].length) return false;
      }
    }
    return true;
  });

/** Request identity carries no signing/native authority. Every recovery creates
 * its prepared operation from a newly verified current source checkpoint. */
export const request = (base: VerifiedForeignCommitBase) =>
  Effect.gen(function* () {
    const owned = yield* currentOwnedTransaction;
    if (
      Option.isNone(owned) ||
      owned.value.token.deploymentIdentity !==
        base.history.token.deploymentIdentity ||
      owned.value.token.ownerToken !== base.history.token.ownerToken ||
      owned.value.token.generation !== base.history.token.generation
    )
      return yield* refuse(
        "Foreign adoption request lacks current history ownership",
      );
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(base.history.coverage.bindingDigest, "hex");
    const pending =
      yield* sql`SELECT 1 FROM foreign_native_adoptions WHERE binding_digest = ${binding}
    AND state IN ('requested', 'prepared', 'rewinding') LIMIT 1`;
    if (pending.length !== 0) return;
    const coverage = base.history.coverage;
    const adoptionId = createHash("sha256")
      .update(
        JSON.stringify([
          "midgard-foreign-native-adoption-v1",
          coverage.bindingDigest,
          base.history.token.deploymentIdentity,
          coverage.point.id,
          coverage.snapshotDigest,
          base.headerHash,
          base.root,
        ]),
      )
      .digest();
    const inserted = yield* sql`INSERT INTO foreign_native_adoptions
    (adoption_id, binding_digest, manifest_id, source_hash, source_slot, source_height,
      source_snapshot, checkpoint_revision, header_hash, target_root, state, source_observation)
    SELECT ${adoptionId}, ${binding}, manifest_id, ${Buffer.from(coverage.point.id, "hex")},
      ${coverage.point.slot}, head_height, ${Buffer.from(coverage.snapshotDigest, "hex")},
      ${coverage.checkpointRevision}::bigint, ${Buffer.from(base.headerHash, "hex")}, ${base.root},
      'requested', ${JSON.stringify(base.observation)}::jsonb
    FROM event_history_cursor WHERE binding_digest = ${binding}
      AND manifest_id = ${Buffer.from(base.history.token.deploymentIdentity, "hex")}
      AND revision = ${coverage.checkpointRevision}::bigint
      AND head_hash = ${Buffer.from(coverage.point.id, "hex")}
      AND snapshot_digest = ${Buffer.from(coverage.snapshotDigest, "hex")}
    ON CONFLICT (adoption_id) DO UPDATE SET state = 'requested',updated_at = NOW()
      WHERE foreign_native_adoptions.state IN ('discarded','rewound') RETURNING sequence`;
    if (inserted.length !== 1)
      return yield* refuse(
        "Foreign adoption request source checkpoint changed",
      );
  });

export const unresolved = (bindingDigest: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Adoption>`SELECT p.*, p.sequence::text,
    p.source_slot::text,
    (EXISTS (SELECT 1 FROM event_history_cursor c WHERE c.binding_digest = p.binding_digest
      AND ((c.head_hash = p.source_hash AND c.snapshot_digest = p.source_snapshot)
        OR (c.anchor_hash = p.source_hash AND c.anchor_snapshot_digest = p.source_snapshot)))
    OR EXISTS (SELECT 1 FROM event_history_block_applications a WHERE a.binding_digest = p.binding_digest
      AND a.block_hash = p.source_hash AND a.after_snapshot_digest = p.source_snapshot AND a.canonical)) AS canonical
    , EXISTS (SELECT 1 FROM state_queue_terminal_observer_states s,
      LATERAL jsonb_array_elements(CASE jsonb_typeof(s.state_record)
        WHEN 'string' THEN (s.state_record #>> '{}')::jsonb ELSE s.state_record END -> 'admitted') t(transition),
      LATERAL jsonb_array_elements_text(t.transition->'removedHeaderHashes') h(value)
      WHERE s.deployment_identity_digest = p.manifest_id
        AND h.value = encode(p.header_hash,'hex')
        AND t.transition->>'transitionKind' IN ('timeout_correction','fraud_removal')) AS removed
    FROM foreign_native_adoptions p WHERE p.binding_digest = ${Buffer.from(bindingDigest, "hex")}
      AND p.state IN ('requested', 'prepared', 'applied', 'rewinding') ORDER BY p.sequence DESC`;
    return rows;
  });

export const pendingDisposition = (bindingDigest: string) =>
  unresolved(bindingDigest).pipe(
    Effect.map((plans) =>
      plans.some(
        (plan) => plan.state !== "applied" || !plan.canonical || plan.removed,
      )
        ? {
            status: "pending" as const,
            reason: "Foreign native adoption requires current source recovery",
          }
        : undefined,
    ),
  );

export const prepare = (input: {
  readonly request: Adoption;
  readonly base: VerifiedForeignCommitBase;
  readonly replay: PersistedNativeMpfReplay;
  readonly keys: readonly Buffer[];
  readonly rows: readonly AdoptionLedgerRow[];
  readonly leaseOwner: string;
  readonly events: AdoptionEvents;
  readonly nativeNoop?: true;
}) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (input.base.authority !== "recovery")
      return yield* refuse(
        "Foreign preparation requires newly verified recovery authority",
      );
    if (
      token.deploymentIdentity !==
        input.base.history.token.deploymentIdentity ||
      token.ownerToken !== input.base.history.token.ownerToken ||
      token.generation !== input.base.history.token.generation
    )
      return yield* refuse("Foreign adoption recovery generation changed");
    const sql = yield* SqlClient.SqlClient;
    yield* MpfEngineState.revalidateLedgerStoreLease(input.leaseOwner);
    yield* requeueUnpublishedHistoryLedger({
      bindingDigest: input.base.history.coverage.bindingDigest,
      checkpointRevision: input.base.history.coverage.checkpointRevision,
    });
    const coverage = input.base.history.coverage;
    const keys = JSON.stringify(input.keys.map((key) => key.toString("hex")));
    const updated = yield* sql`UPDATE foreign_native_adoptions p SET
    state = 'prepared', source_hash = ${Buffer.from(coverage.point.id, "hex")},
    source_slot = ${coverage.point.slot}, source_height = c.head_height,
    source_snapshot = ${Buffer.from(coverage.snapshotDigest, "hex")},
    checkpoint_revision = ${coverage.checkpointRevision}::bigint,
    header_hash = ${Buffer.from(input.base.headerHash, "hex")}, target_root = ${input.base.root},
    source_observation = ${JSON.stringify(input.base.observation)}::jsonb,
    replay_record = ${JSON.stringify({ ...replayRecord(input.replay), ...(input.nativeNoop ? { nativeNoop: true } : {}) })}::jsonb,
    touched_outrefs = ARRAY(SELECT decode(value, 'hex') FROM jsonb_array_elements_text(${keys}::jsonb)),
    ledger_before = (SELECT COALESCE(jsonb_agg(to_jsonb(l)), '[]'::jsonb) FROM mempool_ledger l
      WHERE l.outref IN (SELECT decode(value,'hex') FROM jsonb_array_elements_text(${keys}::jsonb))),
    ledger_after = ${JSON.stringify(input.rows)}::jsonb, updated_at = NOW()
    FROM event_history_cursor c WHERE p.adoption_id = ${input.request.adoption_id}
      AND p.state = 'requested' AND c.binding_digest = p.binding_digest
      AND c.binding_digest = ${Buffer.from(coverage.bindingDigest, "hex")}
      AND c.manifest_id = ${Buffer.from(input.base.history.token.deploymentIdentity, "hex")}
      AND c.revision = ${coverage.checkpointRevision}::bigint
      AND c.head_hash = ${Buffer.from(coverage.point.id, "hex")}
      AND c.snapshot_digest = ${Buffer.from(coverage.snapshotDigest, "hex")} RETURNING p.sequence`;
    if (updated.length !== 1)
      return yield* refuse(
        "Foreign adoption request changed before preparation",
      );
    yield* retainEventProjection(input.request.adoption_id, input.events);
    yield* retainVerifiedForeignSegments(input.base);
  });

export const apply = (plan: Adoption, leaseOwner: string) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (token.deploymentIdentity !== plan.manifest_id.toString("hex"))
      return yield* refuse("Foreign adoption deployment changed");
    yield* MpfEngineState.revalidateLedgerStoreLease(leaseOwner);
    const sql = yield* SqlClient.SqlClient;
    const current =
      yield* sql`SELECT 1 FROM foreign_native_adoptions WHERE adoption_id = ${plan.adoption_id}
    AND state = 'prepared' FOR UPDATE`;
    if (current.length !== 1)
      return yield* refuse("Foreign native adoption is no longer prepared");
    yield* sql`DELETE FROM mempool_ledger l USING foreign_native_adoptions p
    WHERE p.adoption_id = ${plan.adoption_id} AND l.outref = ANY(p.touched_outrefs)`;
    yield* sql`INSERT INTO mempool_ledger (tx_id,outref,output,address,source_event_id,time_stamp_tz)
    SELECT decode(n.tx_id,'hex'), decode(n.outref,'hex'), decode(n.output,'hex'), n.address,
      COALESCE(decode(n.source_event_id,'hex'),old.source_event_id), COALESCE(old.time_stamp_tz,NOW())
    FROM foreign_native_adoptions p,
      LATERAL jsonb_to_recordset(p.ledger_after) n(tx_id text,outref text,output text,address text,source_event_id text)
    LEFT JOIN LATERAL jsonb_populate_recordset(NULL::mempool_ledger,p.ledger_before) old
      ON old.outref = decode(n.outref,'hex') AND old.output = decode(n.output,'hex')
    WHERE p.adoption_id = ${plan.adoption_id}`;
    yield* applyEventProjection(plan.adoption_id);
    yield* MpfEngineState.stampLedgerMigration(plan.target_root);
    yield* sql`UPDATE foreign_native_adoptions SET state = 'applied', updated_at = NOW()
    WHERE adoption_id = ${plan.adoption_id} AND state = 'prepared'`;
  });

export const markRewinding = (plan: Adoption, leaseOwner: string) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (token.deploymentIdentity !== plan.manifest_id.toString("hex"))
      return yield* refuse("Foreign adoption rewind deployment changed");
    yield* MpfEngineState.revalidateLedgerStoreLease(leaseOwner);
    const sql = yield* SqlClient.SqlClient;
    const [cursor] = yield* sql<{
      revision: string;
    }>`SELECT revision::text FROM event_history_cursor
      WHERE binding_digest = ${plan.binding_digest}`;
    if (cursor === undefined)
      return yield* refuse(
        "Foreign adoption rewind lacks its current source cursor",
      );
    yield* requeueUnpublishedHistoryLedger({
      bindingDigest: plan.binding_digest.toString("hex"),
      checkpointRevision: cursor.revision,
    });
    yield* sql`UPDATE foreign_native_adoptions SET state = 'rewinding', updated_at = NOW()
    WHERE adoption_id = ${plan.adoption_id} AND state IN ('prepared','applied','rewinding')`;
  });

export const rewind = (plan: Adoption, leaseOwner: string) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (token.deploymentIdentity !== plan.manifest_id.toString("hex"))
      return yield* refuse("Foreign adoption rewind deployment changed");
    yield* MpfEngineState.revalidateLedgerStoreLease(leaseOwner);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM mempool_ledger l USING foreign_native_adoptions p
    WHERE p.adoption_id = ${plan.adoption_id} AND p.state = 'rewinding' AND l.outref = ANY(p.touched_outrefs)`;
    yield* sql`INSERT INTO mempool_ledger SELECT old.* FROM foreign_native_adoptions p,
    LATERAL jsonb_populate_recordset(NULL::mempool_ledger,p.ledger_before) old
    WHERE p.adoption_id = ${plan.adoption_id} AND p.state = 'rewinding'`;
    yield* inverseEventProjection(plan.adoption_id);
    yield* MpfEngineState.stampLedgerMigration(retainedReplay(plan).baseRoot);
    yield* sql`UPDATE foreign_native_adoptions SET state = 'rewound', updated_at = NOW()
    WHERE adoption_id = ${plan.adoption_id} AND state = 'rewinding'`;
  });

export const discardRequest = (plan: Adoption) =>
  Effect.gen(function* () {
    yield* requireRecoveryTransaction;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE foreign_native_adoptions SET state = 'discarded',updated_at = NOW()
    WHERE adoption_id = ${plan.adoption_id} AND state = 'requested'`;
  });

/** Seal only source-proven canonical plans beyond the automatic rollback
 * horizon, then allow that prefix to be pruned. Prepared plans always hold. */
export const foreignNativeAdoptionHoldSlot = (
  bindingDigest: string,
  horizon: number,
) =>
  Effect.gen(function* () {
    yield* requireSourceTransaction;
    const sql = yield* SqlClient.SqlClient;
    const eligible = (yield* unresolved(bindingDigest))
      .filter(
        (plan) => plan.state === "applied" && plan.canonical && !plan.removed,
      )
      .map((plan) => plan.adoption_id.toString("hex"));
    yield* sql`UPDATE foreign_native_adoptions p SET state = 'sealed',updated_at = NOW()
    FROM event_history_cursor c WHERE p.binding_digest = ${Buffer.from(bindingDigest, "hex")}
      AND c.binding_digest = p.binding_digest AND p.state = 'applied'
      AND p.source_height < c.head_height - ${horizon}
      AND p.adoption_id IN (SELECT decode(value,'hex') FROM jsonb_array_elements_text(${JSON.stringify(eligible)}::jsonb))
      AND (EXISTS (SELECT 1 FROM event_history_block_applications a WHERE a.binding_digest = p.binding_digest
        AND a.block_hash = p.source_hash AND a.after_snapshot_digest = p.source_snapshot AND a.canonical)
        OR (c.anchor_hash = p.source_hash AND c.anchor_snapshot_digest = p.source_snapshot))`;
    const [row] = yield* sql<{
      slot: string | null;
    }>`SELECT min(source_slot)::text AS slot FROM foreign_native_adoptions
    WHERE binding_digest = ${Buffer.from(bindingDigest, "hex")} AND state IN ('requested','prepared','applied','rewinding')`;
    yield* sql`UPDATE foreign_verified_segments s SET source_sealed=true
      FROM event_history_cursor c,event_history_block_applications a
      WHERE s.binding_digest=${Buffer.from(bindingDigest, "hex")} AND c.binding_digest=s.binding_digest
        AND s.confirmed AND NOT s.source_sealed AND a.binding_digest=s.binding_digest
        AND a.block_hash=s.source_hash AND a.after_snapshot_digest=s.source_snapshot AND a.canonical
        AND a.block_height < c.head_height - ${horizon}`;
    const [segments] = yield* sql<{
      slot: string | null;
    }>`SELECT min(source_slot)::text AS slot FROM foreign_verified_segments
      WHERE binding_digest=${Buffer.from(bindingDigest, "hex")} AND NOT source_sealed`;
    const slots = [row?.slot, segments?.slot]
      .filter((slot): slot is string => slot != null)
      .map(Number);
    return slots.length === 0 ? undefined : Math.min(...slots);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retain foreign native adoption source prefix",
    ),
  );
