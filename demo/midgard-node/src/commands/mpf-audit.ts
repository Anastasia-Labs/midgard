import { randomUUID } from "node:crypto";

import { Effect } from "effect";

import {
  ConfirmedLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { Database, NodeConfig } from "../services/index.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
  MidgardMpf,
  setMpfScratchBuild,
} from "../workers/utils/mpf.js";

export type MpfAuditResult = {
  readonly persistedRoot: string;
  readonly recomputedRoot: string;
  readonly entryCount: number;
  readonly diverged: boolean;
  readonly durationMs: number;
  readonly skippedReason?:
    | "legacy_engine"
    | "active_pending_submission"
    | "state_queue_busy"
    | "store_busy";
  readonly acknowledged?: boolean;
};

export const runMpfAudit = ({
  acknowledgeClean = false,
  persistedRootOverride,
}: {
  readonly acknowledgeClean?: boolean;
  readonly persistedRootOverride?: string;
} = {}): Effect.Effect<MpfAuditResult, unknown, Database | NodeConfig> =>
  Effect.gen(function* () {
    const startedAt = performance.now();
    const config = yield* NodeConfig;
    if (config.MPF_ENGINE === "legacy") {
      return {
        persistedRoot: "",
        recomputedRoot: "",
        entryCount: 0,
        diverged: false,
        durationMs: performance.now() - startedAt,
        skippedReason: "legacy_engine",
      };
    }
    const stateQueueResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "mpf-payload-audit",
      (stateQueueLeaseToken) =>
        Effect.gen(function* () {
          if (yield* PendingBlockFinalizationsDB.hasActive) {
            return {
              persistedRoot: "",
              recomputedRoot: "",
              entryCount: 0,
              diverged: false,
              durationMs: performance.now() - startedAt,
              skippedReason: "active_pending_submission" as const,
            };
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
                  const entries = yield* ConfirmedLedgerDB.retrieve;
                  const recomputedRoot =
                    yield* computeLedgerMpfRootFromLedgerEntries(entries);

                  // Revalidate both ownership domains immediately before reading
                  // the durable LevelDB marker and recording the audit result.
                  yield* StateQueueMutationLeasesDB.revalidate(
                    stateQueueLeaseToken,
                  );
                  yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                    activeMpfLeaseOwner,
                  );
                  const persistedRoot =
                    persistedRootOverride ??
                    (yield* MidgardMpf.create(
                      "ledger-audit",
                      config.LEDGER_MPF_DB_PATH,
                    ).pipe(
                      Effect.flatMap((ledger) =>
                        ledger
                          .rootHex()
                          .pipe(
                            Effect.ensuring(
                              ledger
                                .close()
                                .pipe(Effect.catchAll(() => Effect.void)),
                            ),
                          ),
                      ),
                    ));
                  const diverged = persistedRoot !== recomputedRoot;
                  yield* MpfEngineStateDB.recordLedgerAudit({
                    rootHex: recomputedRoot,
                    diverged,
                    utxoPayloadAggregate:
                      ledgerPayloadAggregateFromEntries(entries),
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
                      recomputedRoot,
                    );
                  }
                  const result = {
                    persistedRoot,
                    recomputedRoot,
                    entryCount: entries.length,
                    diverged,
                    durationMs: performance.now() - startedAt,
                    acknowledged: acknowledgeClean,
                  };
                  yield* diverged
                    ? Effect.logError(
                        `mpf_payload_audit_divergence=1 persisted_root=${persistedRoot} recomputed_root=${recomputedRoot} entry_count=${entries.length.toString()}`,
                      )
                    : Effect.logInfo(
                        `mpf_payload_audit_divergence=0 root=${persistedRoot} entry_count=${entries.length.toString()} duration_ms=${result.durationMs.toString()} acknowledged=${acknowledgeClean.toString()}`,
                      );
                  return result;
                }),
              { ttlMs: 60 * 60 * 1000 },
            );
          return mpfLeaseResult._tag === "Ran"
            ? mpfLeaseResult.value
            : {
                persistedRoot: "",
                recomputedRoot: "",
                entryCount: 0,
                diverged: false,
                durationMs: performance.now() - startedAt,
                skippedReason: "store_busy" as const,
              };
        }),
      { ttlMs: 60 * 60 * 1000 },
    );
    if (stateQueueResult._tag === "Busy") {
      return {
        persistedRoot: "",
        recomputedRoot: "",
        entryCount: 0,
        diverged: false,
        durationMs: performance.now() - startedAt,
        skippedReason: "state_queue_busy",
      };
    }
    return stateQueueResult.value;
  });

export const mpfAuditProgram = runMpfAudit();
