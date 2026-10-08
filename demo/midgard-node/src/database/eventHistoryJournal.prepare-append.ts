import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { BoundHistoryCapture } from "../l1-event-history-projection.js";
import {
  type HistoryIncarnation,
  historyIncarnationDigest,
  type HistoryProvenanceChange,
  stageHistoryProvenance,
} from "../l1-event-history-provenance.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import type { HistoryTransition } from "../l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";
import {
  requireRecoveryTransaction,
  requireSourceTransaction,
} from "./eventHistoryAuthority.js";
import { loadLocked } from "./eventHistoryJournal.load-locked.js";
import {
  bytes,
  checked,
  type Checkpoint,
  type CursorRow,
  digest,
  fail,
  freeze,
  key,
  natural,
  preparations,
  samePoint,
  serialise,
  table,
  type Undo,
  validateLiveCoverage,
} from "./eventHistoryJournal.validate-live-coverage.js";
import {
  decodeJournalIncarnation,
  encodeJournalIncarnation,
  encodeJournalOutput,
} from "./eventHistoryJournalCodec.js";
import { requireOriginCoverage } from "./eventHistoryReplayReceipts.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const putOutput = (
  bindingDigest: string,
  output: LedgerSnapshotOutput,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const record = encodeJournalOutput(output);
    yield* sql`INSERT INTO event_history_live_outputs (binding_digest, tx_hash, output_index, output_record, output_digest)
    VALUES (${bytes(bindingDigest)}, ${bytes(output.txHash)}, ${output.outputIndex}, ${record}, ${bytes(digest(record))})
    ON CONFLICT (binding_digest, tx_hash, output_index) DO UPDATE SET output_record = EXCLUDED.output_record, output_digest = EXCLUDED.output_digest`;
  });

export const putIncarnation = (value: HistoryIncarnation) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO event_history_incarnations (binding_digest, incarnation_id, kind, event_id, event_key, origin_canonical, incarnation_record, incarnation_digest)
    VALUES (${bytes(value.bindingDigest)}, ${bytes(value.id)}, ${value.kind}, ${Buffer.from(value.event.idCbor, "hex")}, ${bytes(value.event.key)}, ${value.placement !== null}, ${encodeJournalIncarnation(value)}, ${bytes(historyIncarnationDigest(value))})
    ON CONFLICT (binding_digest, incarnation_id) DO UPDATE SET origin_canonical = EXCLUDED.origin_canonical, incarnation_record = EXCLUDED.incarnation_record, incarnation_digest = EXCLUDED.incarnation_digest`;
  });

// Seed and undo belong to recovery. An append only extends the journaled
// prefix producers hold, so the owner may also run it at the head of its Ready
// generation (withReadyAppend).
const owned = (
  binding: EventHistorySourceBinding,
  transaction: "recovery" | "source",
) =>
  Effect.gen(function* () {
    const token =
      transaction === "recovery"
        ? yield* requireRecoveryTransaction
        : yield* requireSourceTransaction;
    yield* checked(() => {
      if (token.deploymentIdentity !== binding.manifestId)
        fail("Recovery owner belongs to another manifest");
    });
  });

/** Seed only from an authenticated continuous initialization replay. An empty
 * current list is not evidence of missing historical origins. originReceipt
 * retains the exact replay evidence atomically with its digest and projection.
 * Receipt integrity does not establish its source authority on restart.
 * Compose inside Authority.withRecovery with dependent SQL materialization. */
export const seed = (input: {
  binding: EventHistorySourceBinding;
  capture: BoundHistoryCapture;
  height: number;
  originReceipt: string;
  originReceiptDigest: string;
  incarnations: readonly HistoryIncarnation[];
}) =>
  Effect.gen(function* () {
    yield* owned(input.binding, "recovery");
    const sql = yield* SqlClient.SqlClient;
    const prepared = yield* checked(() => {
      const originReceipt = input.originReceipt;
      const originReceiptDigest = input.originReceiptDigest;
      bytes(originReceiptDigest);
      if (
        originReceipt.length === 0 ||
        digest(originReceipt) !== originReceiptDigest
      )
        fail("Seed origin receipt digest disagrees");
      natural(input.height);
      if (input.capture.bindingDigest !== input.binding.digest)
        fail("Seed capture binding disagrees");
      const incarnations = Object.freeze(
        input.incarnations.map((value) =>
          decodeJournalIncarnation(encodeJournalIncarnation(value)),
        ),
      );
      const head = {
        ...input.capture.history.ledger.point,
        height: input.height,
      };
      return {
        originReceipt,
        originReceiptDigest,
        incarnations,
        head,
        ledger: freeze(structuredClone(input.capture.history.ledger)),
        snapshotDigest: input.capture.snapshotDigest,
      };
    });
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      prepared.ledger,
      input.binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({ table, message: "Invalid seed capture", cause }),
      ),
    );
    yield* checked(() => {
      if (capture.snapshotDigest !== prepared.snapshotDigest)
        fail("Seed snapshot digest disagrees");
      validateLiveCoverage(capture, prepared.incarnations, prepared.head);
    });
    yield* requireOriginCoverage({
      binding: input.binding,
      originReceipt: prepared.originReceipt,
      anchor: prepared.head,
      anchorSnapshotDigest: capture.snapshotDigest,
    });
    yield* sql`INSERT INTO event_history_cursor (binding_digest, manifest_id, origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot, anchor_height, anchor_snapshot_digest, head_hash, head_slot, head_height, head_application_revision, snapshot_digest, revision, addresses)
    VALUES (${bytes(input.binding.digest)}, ${bytes(input.binding.manifestId)}, ${prepared.originReceipt}, ${bytes(prepared.originReceiptDigest)}, ${bytes(prepared.head.id)}, ${prepared.head.slot}, ${prepared.head.height}, ${bytes(capture.snapshotDigest)}, ${bytes(prepared.head.id)}, ${prepared.head.slot}, ${prepared.head.height}, NULL, ${bytes(capture.snapshotDigest)}, 0, CAST(${JSON.stringify(capture.history.ledger.addresses)} AS TEXT)::jsonb)`;
    for (const output of capture.history.ledger.outputs)
      yield* putOutput(input.binding.digest, output);
    for (const value of prepared.incarnations) yield* putIncarnation(value);
  }).pipe(sqlErrorToDatabaseError(table, "Failed to seed history journal"));

type Projection = Readonly<{
  capture: BoundHistoryCapture;
  transitions: readonly Readonly<{
    transactionIndex: number;
    transition: HistoryTransition;
  }>[];
}>;

export type Prepared = Readonly<{
  expected: Checkpoint;
  block: BoundHistoryChainBlock;
  projection: Projection;
  receipt: string;
  undo: Undo;
  changes: readonly HistoryProvenanceChange[];
}>;

/** Network/reference resolution and whole-block projection precede this pure
 * staging step. A projection must come from projectEventHistoryBlock for this
 * exact checkpoint and admitted block; this storage layer is not a validator. */
export const prepareAppend = (
  expected: Checkpoint,
  block: BoundHistoryChainBlock,
  projection: Projection,
): Prepared => {
  if (
    block.parent !== expected.head.id ||
    block.point.height !== expected.head.height + 1 ||
    block.point.slot <= expected.head.slot ||
    block.point.id === expected.head.id ||
    projection.capture.bindingDigest !== expected.bindingDigest ||
    projection.capture.history.ledger.point.id !== block.point.id ||
    projection.capture.history.ledger.point.slot !== block.point.slot ||
    serialise([...expected.capture.history.ledger.addresses].sort()) !==
      serialise([...projection.capture.history.ledger.addresses].sort())
  )
    fail("Block projection does not extend its exact checkpoint");
  for (const item of projection.transitions) {
    if (
      block.transactions[item.transactionIndex]?.txHash !==
      item.transition.transactionHash
    )
      fail("Transition is not from its indexed block transaction");
  }
  const changes = stageHistoryProvenance({
    bindingDigest: expected.bindingDigest,
    block,
    transitions: projection.transitions,
    incarnations: expected.incarnations,
  });
  const after = new Map(
    expected.incarnations.map((value) => [value.id, value]),
  );
  for (const change of changes) after.set(change.after.id, change.after);
  validateLiveCoverage(projection.capture, [...after.values()], block.point);
  const beforeOutputs = new Map(
    expected.capture.history.ledger.outputs.map((value) => [
      key(value),
      encodeJournalOutput(value),
    ]),
  );
  const afterOutputs = new Map(
    projection.capture.history.ledger.outputs.map((value) => [
      key(value),
      encodeJournalOutput(value),
    ]),
  );
  const outputs = [
    ...new Set([...beforeOutputs.keys(), ...afterOutputs.keys()]),
  ]
    .sort()
    .flatMap((ref) => {
      const before = beforeOutputs.get(ref) ?? null;
      const next = afterOutputs.get(ref) ?? null;
      return before === next ? [] : [{ before, after: next }];
    });
  // The immutable receipt excludes application revision and incarnation
  // before-images: reapplying a reverted admission starts from a retained orphan.
  const receipt = serialise({
    domain: "midgard-history-ledger-application-v1",
    bindingDigest: expected.bindingDigest,
    block,
    beforeSnapshotDigest: expected.capture.snapshotDigest,
    afterSnapshotDigest: projection.capture.snapshotDigest,
    transitions: projection.transitions,
  });
  const prepared: Prepared = Object.freeze({
    expected,
    block: freeze(structuredClone(block)),
    projection: Object.freeze({
      capture: Object.freeze({
        ...projection.capture,
        history: Object.freeze({
          ...projection.capture.history,
          ledger: freeze(structuredClone(projection.capture.history.ledger)),
        }),
      }),
      transitions: freeze(structuredClone(projection.transitions)),
    }),
    receipt,
    changes,
    undo: freeze({
      outputs,
      incarnations: changes.map((change) => ({
        before:
          change.before === null
            ? null
            : encodeJournalIncarnation(change.before),
        after: encodeJournalIncarnation(change.after),
      })),
    }),
  });
  preparations.add(prepared);
  return prepared;
};

export const requireExpected = (actual: Checkpoint, expected: Checkpoint) => {
  if (
    actual.bindingDigest !== expected.bindingDigest ||
    actual.manifestId !== expected.manifestId ||
    actual.originReceipt !== expected.originReceipt ||
    actual.originReceiptDigest !== expected.originReceiptDigest ||
    actual.revision !== expected.revision ||
    actual.headApplicationRevision !== expected.headApplicationRevision ||
    !samePoint(actual.head, expected.head) ||
    actual.capture.snapshotDigest !== expected.capture.snapshotDigest ||
    !samePoint(actual.anchor, expected.anchor) ||
    actual.anchorSnapshotDigest !== expected.anchorSnapshotDigest
  )
    fail("History cursor revision or head changed");
};

export const lockCursor = (
  binding: EventHistorySourceBinding,
  transaction: "recovery" | "source",
) =>
  Effect.gen(function* () {
    yield* owned(binding, transaction);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<CursorRow>`SELECT * FROM event_history_cursor WHERE binding_digest = ${bytes(binding.digest)} FOR UPDATE`;
    if (rows[0] === undefined)
      return yield* checked(() => fail("History journal has no replay anchor"));
    return rows[0];
  });

export const lockCheckpoint = (
  binding: EventHistorySourceBinding,
  transaction: "recovery" | "source",
) =>
  lockCursor(binding, transaction).pipe(
    Effect.flatMap((row) => loadLocked(binding, row, "head")),
  );

/** Rows each retention step may delete per table. Forward progress adds one
 * block per step, so any backlog (a first move off an old seed, a restart far
 * behind the tip) drains at this rate without an unbounded transaction. */
export const RETENTION_BATCH = 128;

export type Retention = Readonly<{
  /** Height of the follower's current source tip (at least the new block's). */
  tipHeight: number;
  /** k: the manifest's deepest automatic rollback. Blocks deeper than this
   * behind the tip are never rolled back automatically. */
  horizon: number;
}>;
