import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Either, Option } from "effect";

import { retrieveAppliedRecoveryAfterJournal } from "../database/eventHistoryRecoveryPlans.js";
import {
  ConfirmedLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import type { DatabaseError } from "../database/utils/common.js";
import type * as Ledger from "../database/utils/ledger.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
  MidgardMpf,
  setMpfScratchBuild,
} from "../mpf/index.js";
import { Database, NodeConfig } from "../services/index.js";
import { materializeLedgerDeltaSuffix } from "../transactions/state-queue/confirmed-ledger-snapshot.js";

/**
 * Which independently recomputed ledger point the persisted MPF root matched.
 *
 * - `confirmed`: the merged ledger in `confirmed_ledger`.
 * - `tip`: the confirmed ledger plus the ledger deltas of every finalized but
 *   not-yet-merged journal, i.e. the committed tip.
 */
export type MpfAuditLedgerPoint = "confirmed" | "tip";

export type MpfAuditResult = {
  readonly persistedRoot: string;
  /**
   * The recomputed root the persisted root was compared against: the matched
   * point on a clean audit, otherwise the committed tip (or the confirmed root
   * when the tip could not be recomputed).
   */
  readonly recomputedRoot: string;
  readonly confirmedRoot: string;
  readonly tipRoot?: string;
  /** Why the committed tip could not be recomputed; always a divergence. */
  readonly tipIntegrityFailure?: string;
  /**
   * Why the committed tip is not locally reconstructible (it is, or is built
   * on, a foreign block's ledger this node never held, or a merged journal's
   * post-state the confirmed ledger has moved past). With the persisted root
   * equal to `tipCommittedRoot` the audit is `tip_unverifiable`: neither clean
   * nor a divergence.
   */
  readonly tipUnverifiable?: string;
  /**
   * The root of the native committed point: the expected UTxO root of this
   * node's newest finalized journal, or the target root of a native recovery
   * (correction rewind or signed-header recovery) applied after it.
   */
  readonly tipCommittedRoot?: string;
  readonly unmergedJournalCount: number;
  readonly matchedPoint?: MpfAuditLedgerPoint;
  readonly entryCount: number;
  readonly diverged: boolean;
  readonly durationMs: number;
  readonly skippedReason?:
    | "legacy_engine"
    | "active_pending_submission"
    | "state_queue_busy"
    | "store_busy"
    | "tip_unverifiable";
  readonly acknowledged?: boolean;
};

type RecomputedLedgerPoint = {
  readonly root: string;
  readonly entries: readonly Ledger.Entry[];
};

export type CommittedTipRecomputation =
  | (RecomputedLedgerPoint & {
      readonly _tag: "Recomputed";
      readonly unmergedJournalCount: number;
    })
  | {
      readonly _tag: "Unverifiable";
      readonly reason: string;
      readonly committedRoot: string;
    }
  | { readonly _tag: "IntegrityFailure"; readonly reason: string };

const integrityFailure = (reason: string): CommittedTipRecomputation => ({
  _tag: "IntegrityFailure",
  reason,
});

const headerHex = (record: PendingBlockFinalizationsDB.Record) =>
  record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");

/**
 * The finalized journal whose post-state `record` was built on. A block built
 * on this node's own block names that journal as its base tail. A block built
 * on a foreign block has no journal for its tail; the foreign block's ledger is
 * then locally known only if its UTxO root is one this node itself reached,
 * since equal MPF roots commit to the same ledger.
 */
const resolveBaseJournal = (record: PendingBlockFinalizationsDB.Record) =>
  Effect.gen(function* () {
    const linked =
      yield* PendingBlockFinalizationsDB.retrieveFinalizedByHeaderHash(
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH],
      );
    if (Option.isSome(linked)) return linked;
    return yield* PendingBlockFinalizationsDB.retrieveNewestFinalizedWithExpectedRoot(
      {
        expectedUtxosRoot:
          record[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT],
        endedBy: record[PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME],
      },
    );
  });

/** The native committed point: its root and, when one exists, the finalized
 * journal of this node whose post-state it is. */
type CommittedPoint = {
  readonly root: string;
  readonly anchor: Option.Option<PendingBlockFinalizationsDB.Record>;
  readonly source: string;
};

/**
 * Where the native committed root is. Only this node's own commits advance it
 * (to the committed block's expected root), and an applied native recovery
 * resets it to the recovery's target root. The later of this node's newest
 * finalized journal and the newest applied recovery therefore fixes it. A
 * recovery target is anchored at the newest finalized journal that reached
 * that root, if any; a foreign root has no anchor.
 */
const resolveCommittedPoint = Effect.gen(function* () {
  const newest = yield* PendingBlockFinalizationsDB.retrieveNewestFinalized();
  const recovery = yield* retrieveAppliedRecoveryAfterJournal(
    Option.isSome(newest)
      ? newest.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH]
      : undefined,
  );
  if (Option.isSome(recovery)) {
    const { kind, recoveryId, targetRoot } = recovery.value;
    return Option.some<CommittedPoint>({
      root: targetRoot,
      anchor:
        yield* PendingBlockFinalizationsDB.retrieveNewestFinalizedWithExpectedRoot(
          { expectedUtxosRoot: targetRoot },
        ),
      source: `applied ${kind} recovery_id=${recoveryId}`,
    });
  }
  return Option.map(
    newest,
    (tip): CommittedPoint => ({
      root: tip[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
      anchor: Option.some(tip),
      source: `newest finalized journal header_hash=${headerHex(tip)}`,
    }),
  );
});

/**
 * Recompute the committed tip ledger independently of any MPF store: the
 * confirmed ledger plus the recorded ledger deltas of this node's finalized
 * blocks that have not been merged yet, each step checked against the
 * journal's own expected root.
 *
 * The committed point is found by `resolveCommittedPoint`. Its unmerged suffix
 * is walked back to the confirmed ledger through each block's base journal,
 * bridging a foreign tail by UTxO root (see `resolveBaseJournal`). The walk
 * stops at the confirmed boundary, the newest finalized journal whose post-state
 * is the confirmed ledger: a journal that ended at or before it is merged and
 * cannot bridge to the confirmed ledger, so no audit reads past it.
 *
 * A committed point no local journal reached, a base no local journal reached
 * and a merged base are unverifiable, not divergent: the node cannot
 * reconstruct a ledger it never held (or no longer holds). A loaded suffix that
 * does not fold to the tip (a delta that does not apply, a root that disagrees
 * with its journal, a cycle) is an integrity failure the caller must treat as a
 * divergence. Storage errors propagate unchanged, so a transient read failure
 * never records one.
 */
export const recomputeCommittedTip = ({
  confirmedEntries,
  confirmedRoot,
}: {
  readonly confirmedEntries: readonly Ledger.Entry[];
  readonly confirmedRoot: string;
}): Effect.Effect<CommittedTipRecomputation, DatabaseError, Database> =>
  Effect.gen(function* () {
    const confirmedPoint = {
      _tag: "Recomputed" as const,
      root: confirmedRoot,
      entries: confirmedEntries,
      unmergedJournalCount: 0,
    };
    const point = yield* resolveCommittedPoint;
    if (Option.isNone(point)) return confirmedPoint;
    const committedRoot = point.value.root;
    if (committedRoot === confirmedRoot) return confirmedPoint;
    const unverifiable = (reason: string): CommittedTipRecomputation => ({
      _tag: "Unverifiable",
      committedRoot,
      reason: `${point.value.source}: ${reason}`,
    });
    if (Option.isNone(point.value.anchor)) {
      return unverifiable(
        `committed root ${committedRoot} is neither the confirmed ledger root ${confirmedRoot} nor the post-state of a finalized block of this node`,
      );
    }
    const boundary =
      yield* PendingBlockFinalizationsDB.retrieveNewestFinalizedWithExpectedRoot(
        { expectedUtxosRoot: confirmedRoot },
      );

    // Load the unmerged suffix, newest first. Every read happens here, so the
    // fold below is pure and any failure it reports is an integrity failure.
    const suffix: PendingBlockFinalizationsDB.Record[] = [];
    const seen = new Set<string>();
    let current = point.value.anchor.value;
    for (;;) {
      const headerHashHex = headerHex(current);
      if (seen.has(headerHashHex)) {
        return integrityFailure(
          `finalized journal chain contains a cycle at header_hash=${headerHashHex}`,
        );
      }
      seen.add(headerHashHex);
      if (
        Option.isSome(boundary) &&
        current[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME].getTime() <=
          boundary.value[
            PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
          ].getTime()
      ) {
        return unverifiable(
          `header_hash=${headerHashHex} ended at or before the confirmed boundary header_hash=${headerHex(
            boundary.value,
          )}, so it is merged and its post-state ${
            current[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]
          } cannot be rebuilt from the confirmed ledger root ${confirmedRoot}`,
        );
      }
      suffix.push(current);
      const baseRoot =
        current[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT];
      if (baseRoot === confirmedRoot) break;
      const base = yield* resolveBaseJournal(current);
      if (Option.isNone(base)) {
        return unverifiable(
          `header_hash=${headerHashHex} was built on tail ${current[
            PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH
          ].toString(
            "hex",
          )} at base root ${baseRoot}, which is neither the confirmed ledger root ${confirmedRoot} nor the post-state of a finalized block of this node`,
        );
      }
      current = base.value;
    }

    const folded = yield* materializeLedgerDeltaSuffix({
      baseEntries: confirmedEntries,
      baseRoot: confirmedRoot,
      records: suffix.reverse(),
    }).pipe(Effect.either);
    if (Either.isLeft(folded)) {
      return integrityFailure(
        `${folded.left.message}: ${formatUnknownError(folded.left.cause)}`,
      );
    }
    return {
      _tag: "Recomputed",
      root: folded.right.root,
      entries: folded.right.entries,
      unmergedJournalCount: suffix.length,
    };
  });

const skipped = (
  startedAt: number,
  skippedReason: NonNullable<MpfAuditResult["skippedReason"]>,
): MpfAuditResult => ({
  persistedRoot: "",
  recomputedRoot: "",
  confirmedRoot: "",
  unmergedJournalCount: 0,
  entryCount: 0,
  diverged: false,
  durationMs: performance.now() - startedAt,
  skippedReason,
});

const readLevelDbLedgerRoot = (path: string) =>
  MidgardMpf.create("ledger-audit", path).pipe(
    Effect.flatMap((ledger) =>
      ledger
        .rootHex()
        .pipe(
          Effect.ensuring(
            ledger.close().pipe(Effect.catchAll(() => Effect.void)),
          ),
        ),
    ),
  );

/**
 * Audit the persisted ledger MPF root against an independent recomputation of
 * the same ledger point.
 *
 * Architecture G's native durable root advances when a commit is submitted, so
 * it describes the committed tip and must equal the recomputed tip. The
 * overlay/event-flat LevelDB store is at the tip after a commit and is resynced
 * to the confirmed ledger after a merge or at startup, so it must equal one of
 * those two recomputed points. Both roots are read under the state-queue and
 * ledger-store leases, after the no-active-submission check, so no commit or
 * merge can move either side between the two reads.
 *
 * When the committed tip is unverifiable (see `recomputeCommittedTip`), a
 * persisted root at the native committed point is reported as
 * `tip_unverifiable` (never clean, never acknowledged) and any other root not
 * matching an honest point still diverges.
 *
 * `readNativeDurableRoot` is the running node's native owner read. Without it
 * (the offline `mpf-audit` command) the durable `__root__` marker is read from
 * the LevelDB store directly, which under Architecture G is the native owner's
 * store and so still describes the committed tip.
 */
export const runMpfAudit = ({
  acknowledgeClean = false,
  readNativeDurableRoot,
}: {
  readonly acknowledgeClean?: boolean;
  readonly readNativeDurableRoot?: Effect.Effect<string, unknown>;
} = {}): Effect.Effect<MpfAuditResult, unknown, Database | NodeConfig> =>
  Effect.gen(function* () {
    const startedAt = performance.now();
    const config = yield* NodeConfig;
    if (config.MPF_ENGINE === "legacy") {
      return skipped(startedAt, "legacy_engine");
    }
    const nativeEngine = config.MPF_ENGINE === "architecture_g";
    const readPersistedRoot: Effect.Effect<string, unknown> =
      nativeEngine && readNativeDurableRoot !== undefined
        ? readNativeDurableRoot
        : readLevelDbLedgerRoot(config.LEDGER_MPF_DB_PATH);

    const stateQueueResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "mpf-payload-audit",
      (stateQueueLeaseToken) =>
        Effect.gen(function* () {
          if (yield* PendingBlockFinalizationsDB.hasActive) {
            return skipped(startedAt, "active_pending_submission");
          }

          const leaseOwner = `audit:${randomUUID()}`;
          const mpfLeaseResult =
            yield* MpfEngineStateDB.tryWithLedgerStoreLease(
              leaseOwner,
              (activeMpfLeaseOwner) =>
                Effect.gen(function* () {
                  yield* StateQueueMutationLeasesDB.revalidate(
                    stateQueueLeaseToken,
                  );
                  setMpfScratchBuild("fromlist");
                  const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
                  const confirmedRoot =
                    yield* computeLedgerMpfRootFromLedgerEntries(
                      confirmedEntries,
                    );
                  const tip = yield* recomputeCommittedTip({
                    confirmedEntries,
                    confirmedRoot,
                  });

                  // Revalidate both ownership domains immediately before
                  // reading the persisted root and recording the audit result.
                  yield* StateQueueMutationLeasesDB.revalidate(
                    stateQueueLeaseToken,
                  );
                  yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                    activeMpfLeaseOwner,
                  );
                  const persistedRoot = yield* readPersistedRoot;

                  const tipPoint = tip._tag === "Recomputed" ? tip : undefined;
                  const matchedPoint: MpfAuditLedgerPoint | undefined =
                    !nativeEngine && persistedRoot === confirmedRoot
                      ? "confirmed"
                      : tipPoint !== undefined &&
                          persistedRoot === tipPoint.root
                        ? "tip"
                        : undefined;
                  const tipDetail = {
                    ...(tipPoint === undefined
                      ? {}
                      : { tipRoot: tipPoint.root }),
                    ...(tip._tag === "IntegrityFailure"
                      ? { tipIntegrityFailure: tip.reason }
                      : {}),
                    ...(tip._tag === "Unverifiable"
                      ? {
                          tipUnverifiable: tip.reason,
                          tipCommittedRoot: tip.committedRoot,
                        }
                      : {}),
                  };
                  const points = `confirmed_root=${confirmedRoot} tip_root=${tipPoint?.root ?? "unavailable"} unmerged_journals=${(tipPoint?.unmergedJournalCount ?? 0).toString()}`;
                  if (
                    matchedPoint === undefined &&
                    tip._tag === "Unverifiable" &&
                    persistedRoot === tip.committedRoot
                  ) {
                    // The persisted root is at the native committed point, but
                    // that point's ledger cannot be rebuilt here.
                    // Record the attempt for the audit cadence only: neither the
                    // sticky divergence flag nor a clean root.
                    yield* MpfEngineStateDB.recordLedgerAuditAttempt;
                    if (acknowledgeClean) {
                      return yield* Effect.fail(
                        new Error(
                          `Refusing to acknowledge MPF audit because the committed tip is unverifiable: ${tip.reason}`,
                        ),
                      );
                    }
                    yield* Effect.logWarning(
                      `mpf_payload_audit_unverifiable=1 engine=${config.MPF_ENGINE} persisted_root=${persistedRoot} ${points} tip_unverifiable=${JSON.stringify(tip.reason)}`,
                    );
                    return {
                      persistedRoot,
                      recomputedRoot: confirmedRoot,
                      confirmedRoot,
                      ...tipDetail,
                      unmergedJournalCount: 0,
                      entryCount: confirmedEntries.length,
                      diverged: false,
                      durationMs: performance.now() - startedAt,
                      skippedReason: "tip_unverifiable",
                      acknowledged: false,
                    } satisfies MpfAuditResult;
                  }
                  const diverged = matchedPoint === undefined;
                  const recorded: RecomputedLedgerPoint =
                    matchedPoint === "confirmed" || tipPoint === undefined
                      ? { root: confirmedRoot, entries: confirmedEntries }
                      : tipPoint;
                  yield* MpfEngineStateDB.recordLedgerAudit({
                    rootHex: recorded.root,
                    diverged,
                    utxoPayloadAggregate: ledgerPayloadAggregateFromEntries(
                      recorded.entries,
                    ),
                  });
                  if (acknowledgeClean) {
                    if (diverged) {
                      return yield* Effect.fail(
                        new Error(
                          "Refusing to acknowledge MPF divergence because the current audit is not clean",
                        ),
                      );
                    }
                    yield* MpfEngineStateDB.acknowledgeCleanLedgerAudit(
                      recorded.root,
                    );
                  }
                  const result: MpfAuditResult = {
                    persistedRoot,
                    recomputedRoot: recorded.root,
                    confirmedRoot,
                    ...tipDetail,
                    unmergedJournalCount: tipPoint?.unmergedJournalCount ?? 0,
                    ...(matchedPoint === undefined ? {} : { matchedPoint }),
                    entryCount: recorded.entries.length,
                    diverged,
                    durationMs: performance.now() - startedAt,
                    acknowledged: acknowledgeClean,
                  };
                  yield* diverged
                    ? Effect.logError(
                        `mpf_payload_audit_divergence=1 engine=${config.MPF_ENGINE} persisted_root=${persistedRoot} ${points}${
                          tip._tag === "IntegrityFailure"
                            ? ` tip_integrity_failure=${JSON.stringify(tip.reason)}`
                            : tip._tag === "Unverifiable"
                              ? ` tip_unverifiable=${JSON.stringify(tip.reason)} tip_committed_root=${tip.committedRoot}`
                              : ""
                        }`,
                      )
                    : Effect.logInfo(
                        `mpf_payload_audit_divergence=0 engine=${config.MPF_ENGINE} root=${persistedRoot} matched_point=${matchedPoint} ${points} entry_count=${result.entryCount.toString()} duration_ms=${result.durationMs.toString()} acknowledged=${acknowledgeClean.toString()}`,
                      );
                  return result;
                }),
              { ttlMs: 60 * 60 * 1000 },
            );
          return mpfLeaseResult._tag === "Ran"
            ? mpfLeaseResult.value
            : skipped(startedAt, "store_busy");
        }),
      { ttlMs: 60 * 60 * 1000 },
    );
    if (stateQueueResult._tag === "Busy") {
      return skipped(startedAt, "state_queue_busy");
    }
    return stateQueueResult.value;
  });

export const mpfAuditProgram = runMpfAudit();
