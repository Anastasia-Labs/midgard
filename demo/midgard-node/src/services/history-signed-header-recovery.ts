import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { loadCanonicalHistoryCoverage } from "../database/eventHistoryCanonicalCoverage.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import { AuthorizedHistoryHeaderRetirement } from "../database/eventHistoryLedgerRepair.js";
import { materializeCanonicalHistory } from "../database/eventHistoryMaterialization.js";
import { prepareRetainedNativeHistoryRecoveryPlan } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import { DatabaseError } from "../database/utils/common.js";
import type { MinimalEntry } from "../database/utils/ledger.js";
import { reconcileDepositProjection } from "../fibers/project-deposits-to-mempool-ledger.js";
import { invalidateSpeculativeCommitCandidate } from "../fibers/speculative-commit-builder.js";
import {
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
} from "../mpf/index.js";
import { serializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import { validateRecoveryStateQueue } from "./history-recovery-state-queue.js";
import { ProductionNativeMpfOwnerService } from "./mpf-native-owner/service.js";
import { evaluateSignedIntentCoverage } from "./signed-intent-canonical-coverage.js";

const table = "event_history_recovery_plans";
const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");
const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });
const C = Pending.Columns;
const journalIdentity = (header: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ document: string }>`SELECT jsonb_build_object(
    'journal', to_jsonb(p),
    'deposits', (SELECT COALESCE(jsonb_agg(to_jsonb(d) ORDER BY ordinal), '[]'::jsonb)
      FROM pending_block_finalization_deposits d WHERE d.header_hash = p.header_hash))::text AS document
    FROM pending_block_finalizations p WHERE p.header_hash = ${header}`;
    if (rows.length !== 1)
      return yield* Effect.fail(failure("Recovery journal disappeared"));
    return sha(rows[0]!.document);
  });
const confirmedRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<MinimalEntry>`SELECT outref, output FROM confirmed_ledger ORDER BY outref`;
});
const ledgerIdentity = (rows: readonly MinimalEntry[]) =>
  sha(
    eventHistoryCanonicalJson(
      rows.map((row) => [
        row.outref.toString("hex"),
        row.output.toString("hex"),
      ]),
    ),
  );

/** The single source of the headers signed-header recovery still has to
 * classify: not abandoned, with a deposit member whose admission incarnation is
 * no longer origin-canonical. Journal retention holds its anchor behind these.
 */
export const signedHeaderRecoveryCandidates = (bindingDigest: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      header_hash: Buffer;
      signed_tx_cbor: Buffer | null;
    }>`SELECT DISTINCT p.header_hash, p.signed_tx_cbor
      FROM pending_block_finalizations p JOIN pending_block_finalization_deposits m ON m.header_hash = p.header_hash
      JOIN event_history_incarnations i ON i.binding_digest = m.history_binding_digest AND i.incarnation_id = m.history_incarnation_id
      WHERE i.binding_digest = ${Buffer.from(bindingDigest, "hex")} AND NOT i.origin_canonical
        AND p.status <> 'abandoned'`;
  });

/** The earliest signed validity-start slot among recovery candidates, which
 * the retained journal range must still cover: canonical coverage has to start
 * at or before it (see evaluateSignedIntentCoverage). Once recovery proves a
 * candidate covered_absent it is abandoned and leaves this set, releasing the
 * hold. A candidate that is not yet signed, or whose signed body has no
 * validity start, has no slot coverage could ever be evaluated from, so it
 * does not bound retention; it is not ignored either: its orphaned member keeps
 * the history disposition pending, so the owner stays not-ready until the
 * header is resolved. A stored signed body that does not decode is corruption
 * of this node's own journal and fails closed, naming the header. */
export const signedHeaderRecoveryHoldSlot = (bindingDigest: string) =>
  signedHeaderRecoveryCandidates(bindingDigest).pipe(
    Effect.mapError((cause) =>
      failure("Recovery candidates could not be read", cause),
    ),
    Effect.flatMap((headers) => {
      let earliest: number | undefined;
      for (const { header_hash, signed_tx_cbor } of headers) {
        if (signed_tx_cbor === null) continue;
        let start: bigint | undefined;
        try {
          const tx = CML.Transaction.from_cbor_hex(
            signed_tx_cbor.toString("hex"),
          );
          const body = tx.body();
          start = body.validity_interval_start();
          body.free();
          tx.free();
        } catch (cause) {
          return Effect.fail(
            failure(
              `Recovery candidate ${header_hash.toString("hex")} has an unreadable signed body`,
              cause,
            ),
          );
        }
        if (start === undefined) continue;
        const slot = Number(start);
        if (!Number.isSafeInteger(slot))
          return Effect.fail(
            failure(
              `Recovery candidate ${header_hash.toString("hex")} has a signed validity start that is not a safe slot`,
            ),
          );
        earliest = earliest === undefined ? slot : Math.min(earliest, slot);
      }
      return Effect.succeed(earliest);
    }),
  );

/** First published recovery slice: an orphan-funded deposit-only header whose
 * exact original confirmed base is freshly restored on L1. Includes observed
 * deposit-only headers before any local-finalization job/DA work. Other published
 * shapes stay pending until their complete confirmed-ledger inverse is available.
 * Canonical absence through signed TTL plus finality is mandatory even when the
 * old base output is unspent. A local archive or queue absence cannot authorize it.
 */
export const prepareSignedHeaderRecovery = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly contracts: SDK.MidgardValidators;
  readonly config: NodeConfigDep;
  readonly confirmationDepth: number;
  readonly slotToUnixTime: (slot: number) => number;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation } = input;
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    yield* preparation.assertCurrent;
    const candidate = yield* owned(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const headers = yield* signedHeaderRecoveryCandidates(
          input.binding.digest,
        );
        if (headers.length !== 1) return undefined;
        const maybe = yield* Pending.retrieveByHeaderHash(
          headers[0]!.header_hash,
          true,
        );
        if (Option.isNone(maybe)) return undefined;
        const record = maybe.value;
        if (
          (record[C.STATUS] !== Pending.Status.Finalized &&
            record[C.STATUS] !== Pending.Status.ObservedWaitingStability) ||
          record[C.DEPLOYMENT_MANIFEST_ID] !== checkpoint.manifestId ||
          record.txMembers.length ||
          record.withdrawalMembers.length ||
          record.forcedTransactionMembers.length ||
          !record.depositMembers.length ||
          record.nativeMpfReplay === undefined ||
          record[C.SIGNED_TX_CBOR] == null ||
          record[C.INTENDED_TX_HASH] == null
        )
          return undefined;
        const remaining =
          yield* sql`SELECT 1 FROM pending_block_finalization_deposits m
      LEFT JOIN event_history_incarnations i ON i.binding_digest = m.history_binding_digest AND i.incarnation_id = m.history_incarnation_id
      LEFT JOIN deposits_utxos d ON d.history_binding_digest = m.history_binding_digest AND d.history_incarnation_id = m.history_incarnation_id
      WHERE m.header_hash = ${record[C.HEADER_HASH]} AND
        (i.binding_digest IS DISTINCT FROM ${Buffer.from(input.binding.digest, "hex")} OR i.origin_canonical IS DISTINCT FROM false
          OR d.projected_header_hash IS DISTINCT FROM m.header_hash)`;
        const otherPending =
          yield* sql`SELECT 1 FROM pending_block_finalizations WHERE status NOT IN ('finalized','abandoned')
        AND header_hash <> ${record[C.HEADER_HASH]}
      UNION ALL SELECT 1 FROM processed_mempool`;
        if (remaining.length || otherPending.length) return undefined;
        if (record[C.STATUS] === Pending.Status.ObservedWaitingStability) {
          // Observed can survive a partially committed local finalization. Its
          // job/DA disposition is a separate recovery shape, never inferred from
          // this status or silently marked complete by deposit-only rollback.
          const partial = yield* sql`SELECT 1 FROM local_mutation_jobs
            WHERE job_id = ${`local_block_finalization:${record[C.HEADER_HASH].toString("hex")}`}
            UNION ALL SELECT 1 FROM da_payloads WHERE header_hash = ${record[C.HEADER_HASH]}`;
          if (partial.length !== 0) return undefined;
        }
        const engine =
          yield* sql`SELECT 1 FROM mpf_engine_state WHERE store_name = 'ledger'
          AND root_hex IN (${record[C.BASE_UTXOS_ROOT]}, ${record[C.EXPECTED_UTXOS_ROOT]})`;
        if (engine.length !== 1) return undefined;
        const coverage = yield* loadCanonicalHistoryCoverage(
          input.binding,
          checkpoint,
        );
        const evidence = yield* Effect.try({
          try: () =>
            evaluateSignedIntentCoverage({
              signedTxCbor: record[C.SIGNED_TX_CBOR]!.toString("hex"),
              expectedTxHash: record[C.INTENDED_TX_HASH]!.toString("hex"),
              bindingDigest: coverage.bindingDigest,
              manifestId: coverage.manifestId,
              start: coverage.start,
              head: coverage.head,
              blocks: coverage.blocks,
              requiredFinalityDepth: input.confirmationDepth,
            }),
          catch: (cause) =>
            failure("Signed submission coverage is not sufficient", cause),
        });
        if (evidence.kind !== "covered_absent") return undefined;
        return {
          record,
          evidence,
          journalDigest: yield* journalIdentity(record[C.HEADER_HASH]),
          rows: yield* confirmedRows,
        };
      }),
    );
    if (candidate === undefined) return;
    const { record } = candidate;
    const targetRoot = record[C.BASE_UTXOS_ROOT];
    if (
      record.nativeMpfReplay!.baseRoot.toString("hex") !== targetRoot ||
      record.nativeMpfReplay!.candidateRoot.toString("hex") !==
        record[C.EXPECTED_UTXOS_ROOT]
    )
      return yield* Effect.fail(
        failure("Recovery journal roots disagree with retained native replay"),
      );
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          binding: input.binding,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure("Exact-point recovery queue capture failed", cause),
    });
    yield* preparation.assertCurrent;
    const target = yield* validateRecoveryStateQueue({
      outputs: capture.ledger.outputs,
      contracts: input.contracts,
      expectedBase: {
        outRef: record[C.BASE_TAIL_OUT_REF],
        datumCbor: record[C.BASE_TAIL_DATUM_CBOR],
        utxosRoot: targetRoot,
      },
    }).pipe(
      Effect.mapError((cause) =>
        failure(
          "Canonical queue does not authorize the retained recovery base",
          cause,
        ),
      ),
    );
    const confirmed = yield* SDK.getConfirmedStateFromStateQueueDatum(
      target.queueUTxO.datum,
    );
    const restoredBoundary = Number(confirmed.data.endTime);
    if (!Number.isSafeInteger(restoredBoundary))
      return yield* Effect.fail(
        failure("Canonical confirmed boundary is not a safe timestamp"),
      );
    const restoredQueue = yield* serializeStateQueueUTxO(target.queueUTxO);
    const confirmedRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      candidate.rows,
    );
    if (confirmedRoot !== targetRoot)
      return yield* Effect.fail(
        failure(
          "Confirmed SQL baseline requires authenticated inverse reconstruction",
        ),
      );
    const baselineDigest = ledgerIdentity(candidate.rows);
    const aggregate = ledgerPayloadAggregateFromEntries(candidate.rows);
    const evidenceDigest = sha(
      eventHistoryCanonicalJson({
        signedCoverage: candidate.evidence.evidenceDigest,
        queue: capture.ledger.outputs,
        baselineDigest,
        point: checkpoint.head,
        snapshot: checkpoint.capture.snapshotDigest,
      }),
    );
    const recheck = Effect.gen(function* () {
      if (
        (yield* journalIdentity(record[C.HEADER_HASH])) !==
          candidate.journalDigest ||
        ledgerIdentity(yield* confirmedRows) !== baselineDigest
      )
        return yield* Effect.fail(
          failure("Recovery journal or confirmed SQL baseline changed"),
        );
    });
    const globals = yield* Globals;
    if (input.config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, input.config, "T1");
    let owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (owner === undefined) {
      // Open only retained native bytes; never genesis-bootstrap or replay an
      // orphaned journal on this restart path. Create validates the durable marker.
      owner = yield* Effect.uninterruptible(
        Effect.gen(function* () {
          const opened = yield* Effect.tryPromise({
            try: () =>
              ProductionNativeMpfOwnerService.create({
                levelPath: input.config.LEDGER_MPF_DB_PATH,
                binaryPath: input.config.MPF_NATIVE_OWNER_BINARY_PATH,
                binarySha256: input.config.MPF_NATIVE_OWNER_BINARY_SHA256,
                maxFrameBytes: input.config.MPF_NATIVE_OWNER_MAX_FRAME_BYTES,
                maxChunkBytes: input.config.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES,
                requestTimeoutMs:
                  input.config.MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS,
                restartLimit: input.config.MPF_NATIVE_OWNER_RESTART_LIMIT,
                sidecarPath: input.config.MPF_NATIVE_OWNER_SIDECAR_PATH,
              }),
            catch: (cause) =>
              failure("Retained native recovery owner could not open", cause),
          });
          yield* Ref.set(globals.NATIVE_MPF_OWNER, opened);
          return opened;
        }),
      );
    }
    yield* preparation.assertCurrent;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained native recovery diagnostics failed", cause),
    });
    const plan = yield* owned(
      recheck.pipe(
        Effect.zipRight(
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            {
              bindingDigest: input.binding.digest,
              manifestId: checkpoint.manifestId,
              headerHash: record[C.HEADER_HASH].toString("hex"),
              signedTransactionHash:
                record[C.INTENDED_TX_HASH]!.toString("hex"),
              signedTransactionCborSha256: sha(record[C.SIGNED_TX_CBOR]!),
              targetRoot,
              journalDigest: candidate.journalDigest,
            },
            evidenceDigest,
            {
              durableRoot: diagnostics.durableRoot,
              candidateRoot: record[C.EXPECTED_UTXOS_ROOT],
            },
          ),
        ),
      ),
    );
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          restoredBoundary,
        );
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, restoredQueue);
        yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
        yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
        yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
      }),
      repair: Effect.gen(function* () {
        yield* recheck;
        const sql = yield* SqlClient.SqlClient;
        // This disposition and every inverse write commit together. An inverse
        // failure restores the previous journal status and keeps the native plan.
        const retired = yield* sql`UPDATE pending_block_finalizations
          SET status = 'abandoned', updated_at = NOW()
          WHERE header_hash = ${record[C.HEADER_HASH]} AND status = ${record[C.STATUS]}
          RETURNING header_hash`;
        if (retired.length !== 1)
          return yield* Effect.fail(
            failure("Recovery journal disposition changed"),
          );
        yield* materializeCanonicalHistory(
          { kind: "resume", before: checkpoint, after: checkpoint },
          input.config.NETWORK,
        ).pipe(
          Effect.provideService(AuthorizedHistoryHeaderRetirement, {
            headerHash: record[C.HEADER_HASH],
          }),
        );
        // The full commit worker lifetime was drained before this preparation.
        // Retire only this immutable journal's lease, atomically with its proved
        // abandonment. A late release/renewal finalizer can no longer keep a
        // crashed producer's token active after Ready; unrelated tokens stay put.
        yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
        const engine =
          yield* sql`UPDATE mpf_engine_state SET root_hex = ${targetRoot},
        utxo_payload_entry_count = ${aggregate.entryCount}, utxo_payload_encoded_tuple_bytes = ${aggregate.encodedTupleBytes}, updated_at = NOW()
        WHERE store_name = 'ledger' AND root_hex IN (${record[C.EXPECTED_UTXOS_ROOT]}, ${targetRoot}) RETURNING store_name`;
        if (engine.length !== 1)
          return yield* Effect.fail(
            failure("Native SQL marker changed before dependent recovery"),
          );
        yield* reconcileDepositProjection(
          new Date(input.slotToUnixTime(checkpoint.head.slot)),
        );
      }),
    });
  });
