import { spawn } from "node:child_process";
import { default as fs } from "node:fs";
import { default as path } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it as vitestIt } from "vitest";

import {
  DepositsDB,
  LedgerUtils,
  MempoolDB,
  MempoolLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  TxUtils,
  WithdrawalsDB,
} from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import {
  COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
  commitStageInputPostState,
} from "../../src/mpf/index.js";
import { reincludeStateQueueCorrectedBlocks } from "../../src/services/state-queue-correction-recovery.js";
import { ProcessedTx } from "../../src/utils.js";
import { revalidateAndPersistSpeculativeCandidateSources } from "../../src/workers/commit-block-header.js";
import {
  resolveDepositsRoot,
  resolveWithdrawalsRoot,
} from "../../src/workers/commit-block-header/event-roots.js";
import { makeCardanoSignedMapOutputTxBytes } from ".././helpers/cardano-native-fixtures.js";
import { externalTimeoutTransition } from ".././helpers/state-queue-correction-transition.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from ".././midgard-output-helpers.js";
import { provideDatabaseLayers } from ".././utils.js";
import {
  address1,
  bundleChildProcessHelper,
  collectChildProcess,
  databaseChildProcessEnv,
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  emptyProgramMaterialSidecar,
  isolatedDb,
  makeDepositEntry,
  makeHistoryWithdrawalEntry,
} from "./fixtures.js";

const databaseTestDirectory = path.resolve(__dirname, "..");

export const registerFinalizationTests = () => {
  describe("PendingBlockFinalizationsDB", () => {
    const pendingSubmissionFixture = (
      headerHash: Buffer,
    ): PendingBlockFinalizationsDB.PrepareInput => {
      const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
      const emptyRoots = {
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      };
      const emptyExpectedRoots = {
        ...emptyRoots,
        transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      };
      const emptyExpectedCounts = {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 0n,
        depositCount: 0n,
        totalEventCount: 0n,
        transitionStepCount: 0n,
        validationTraceCount: 0n,
      };
      const header: SDK.Header = {
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        utxosRoot: emptyExpectedRoots.utxosRoot,
        withdrawalsRoot: emptyExpectedRoots.withdrawalsRoot,
        forcedTransactionsRoot: emptyExpectedRoots.forcedTransactionsRoot,
        transactionsRoot: emptyExpectedRoots.transactionsRoot,
        depositsRoot: emptyExpectedRoots.depositsRoot,
        transitionTraceRoot: emptyExpectedRoots.transitionTraceRoot,
        eventToStepRoot: emptyExpectedRoots.eventToStepRoot,
        validationTracesRoot: emptyExpectedRoots.validationTracesRoot,
        withdrawalCount: emptyExpectedCounts.withdrawalCount,
        forcedTransactionCount: emptyExpectedCounts.forcedTransactionCount,
        l2TransactionCount: emptyExpectedCounts.l2TransactionCount,
        depositCount: emptyExpectedCounts.depositCount,
        totalEventCount: emptyExpectedCounts.totalEventCount,
        transitionStepCount: emptyExpectedCounts.transitionStepCount,
        validationTraceCount: emptyExpectedCounts.validationTraceCount,
        startTime: 1n,
        endTime: 2n,
        blockSlot: 0n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: "11".repeat(28),
        operatorVkey: "22".repeat(28),
        protocolVersion: 1n,
      };
      return {
        headerHash,
        headerCbor: Buffer.from(
          LucidData.to(header as never, SDK.Header as never),
          "hex",
        ),
        metadata: {
          deploymentMarker: makeDeploymentMarker("de".repeat(32)),
          consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
          stateQueueLeaseToken: "lease-token",
          baseSnapshotId: "snapshot",
          baseTailOutRef: "base#0",
          baseTailHeaderHash: databaseFixtureBytes("base-tail-header", 28),
          baseTailDatumCbor: "d87980",
          baseRoots: emptyRoots,
          blockStartTime,
          expectedRoots: emptyExpectedRoots,
          expectedCounts: emptyExpectedCounts,
        },
        blockEndTime: new Date(blockStartTime.getTime() + 60_000),
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
        ledgerDelta: {
          spent: [],
          produced: [],
        },
      };
    };
    const speculativeCandidateEventSnapshot = {
      candidateEndTime: new Date("2100-01-01T00:00:00.000Z"),
      excludedUserEventIds: {
        depositEventIds: new Set<string>(),
        forcedTransactionEventIds: new Set<string>(),
        withdrawalEventIds: new Set<string>(),
      },
    } as const;

    it.effect(
      "journals correction classification and makes reinclusion idempotent",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const transition = externalTimeoutTransition({ terminal: true });
            const removed = transition.removedHeaderHashes.map(
              (headerHash) => ({
                headerHash,
                transitionDigest: transition.transitionDigest,
                kind: "removed" as const,
              }),
            );
            const header = Buffer.from(
              transition.removedHeaderHashes[0]!,
              "hex",
            );
            const initial = makeHistoryWithdrawalEntry();
            const assignment = {
              eventId: initial[WithdrawalsDB.Columns.ID],
              expectedClassificationRevision: 0,
              settlementEventInfo: Buffer.from("8101", "hex"),
              validity: WithdrawalsDB.Validity.WithdrawalIsValid,
              validityDetail: { z: 1, a: { z: 2, a: 3 } },
            };
            yield* WithdrawalsDB.insertEntries([initial]);
            yield* WithdrawalsDB.setSettlementInfoForEventIds([assignment]);
            yield* WithdrawalsDB.markAwaitingAsProjected([assignment]);
            const classified = Option.getOrThrow(
              yield* WithdrawalsDB.retrieveByEventId(assignment.eventId),
            );
            yield* PendingBlockFinalizationsDB.preparePendingSubmission({
              ...pendingSubmissionFixture(header),
              withdrawalEventIds: [assignment.eventId],
              withdrawalEntries: [classified],
            });
            const journal = Option.getOrThrow(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header),
            );
            expect(
              journal.withdrawalMembers[0]![
                PendingBlockFinalizationsDB.WithdrawalMemberColumns
                  .VALIDITY_DETAIL
              ],
            ).toEqual(assignment.validityDetail);
            yield* PendingBlockFinalizationsDB.markSubmitted(
              header,
              Buffer.alloc(32, 31),
            );
            yield* WithdrawalsDB.markProjectedByEventIds([assignment], header);
            yield* WithdrawalsDB.markFinalizedByEventIds(
              [assignment.eventId],
              header,
            );
            expect(
              (yield* reincludeStateQueueCorrectedBlocks(removed))[0]!
                .reopenedEvents,
            ).toBe(1);
            const replacement = {
              ...assignment,
              expectedClassificationRevision: 1,
              settlementEventInfo: Buffer.from("8102", "hex"),
              validity: WithdrawalsDB.Validity.SpentWithdrawalUtxo,
              validityDetail: { changed: true },
            };
            yield* WithdrawalsDB.setSettlementInfoForEventIds([replacement]);
            expect(
              (yield* reincludeStateQueueCorrectedBlocks(removed))[0]!
                .reopenedEvents,
            ).toBe(0);
            const row = Option.getOrThrow(
              yield* WithdrawalsDB.retrieveByEventId(assignment.eventId),
            );
            expect(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]).toEqual(
              replacement.settlementEventInfo,
            );
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE pending_block_finalization_withdrawals SET validity_detail = '{"tampered":true}'::jsonb WHERE header_hash = ${header}`;
            expect(
              (yield* Effect.either(
                PendingBlockFinalizationsDB.retrieveByHeaderHash(header),
              ))._tag,
            ).toBe("Left");
          }),
        ),
    );

    it.effect(
      "reopens after a correction a withdrawal an unlanded block selected, and refuses one assigned to another header or never selected",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const removed = databaseFixtureBytes("reopen-removed-header", 28);
            const other = databaseFixtureBytes("reopen-other-header", 28);
            const entry = (label: string): WithdrawalsDB.Entry => ({
              ...makeHistoryWithdrawalEntry(),
              [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(
                `reopen-${label}`,
              ),
              [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
                `reopen-${label}-l1`,
              ),
            });
            const selected = entry("selected");
            const elsewhere = entry("elsewhere");
            const unselected = entry("unselected");
            const classify = (row: WithdrawalsDB.Entry) => ({
              eventId: row[WithdrawalsDB.Columns.ID],
              expectedClassificationRevision: 0,
              settlementEventInfo: Buffer.from("8101", "hex"),
              validity: WithdrawalsDB.Validity.WithdrawalIsValid,
              validityDetail: {},
            });
            yield* WithdrawalsDB.insertEntries([
              selected,
              elsewhere,
              unselected,
            ]);
            const classified = [selected, elsewhere].map(classify);
            yield* WithdrawalsDB.setSettlementInfoForEventIds(classified);
            yield* WithdrawalsDB.markAwaitingAsProjected(classified);
            yield* WithdrawalsDB.markProjectedByEventIds(
              [classified[1]!],
              other,
            );
            const current = (row: WithdrawalsDB.Entry) =>
              WithdrawalsDB.retrieveByEventId(
                row[WithdrawalsDB.Columns.ID],
              ).pipe(Effect.map(Option.getOrThrow));
            const before = {
              elsewhere: yield* current(elsewhere),
              unselected: yield* current(unselected),
            };
            // A withdrawal another header holds (kills "accept any projected
            // row"), and one no block selected (kills "drop the Projected
            // clause" and "accept any null-header row"), are not the removed
            // block's to reopen.
            for (const refused of [elsewhere, unselected]) {
              const error = yield* Effect.flip(
                WithdrawalsDB.reopenAfterStateQueueCorrectionByEventIds(
                  [refused[WithdrawalsDB.Columns.ID]],
                  removed,
                ),
              );
              // The unowned-history fixture gate wraps the refusal as its cause.
              const refusal =
                error.cause instanceof DatabaseError ? error.cause : error;
              expect(refusal.message).toBe(
                "Cannot reopen withdrawal not assigned to the corrected header",
              );
            }
            expect(yield* current(elsewhere)).toEqual(before.elsewhere);
            expect(yield* current(unselected)).toEqual(before.unselected);
            // Selected and classified by the removed unlanded block, with no
            // header assigned: it reopens from the removed header.
            yield* WithdrawalsDB.reopenAfterStateQueueCorrectionByEventIds(
              [selected[WithdrawalsDB.Columns.ID]],
              removed,
            );
            const reopened = yield* current(selected);
            expect(reopened[WithdrawalsDB.Columns.STATUS]).toBe(
              WithdrawalsDB.Status.Awaiting,
            );
            expect(reopened[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]).toBe(
              null,
            );
            expect(reopened[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]).toBe(
              null,
            );
            expect(
              reopened[WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH],
            ).toEqual(removed);
            expect(
              reopened[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
            ).toBe(1);
          }),
        ),
    );

    it.effect(
      "retains durable signed intent across cleanup, conflicting writes and duplicate acknowledgement",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes("signed-intent-header", 28);
            const input = pendingSubmissionFixture(headerHash);
            const cbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
            const tx = CML.Transaction.from_cbor_bytes(cbor);
            const body = tx.body();
            const hash = CML.hash_transaction(body);
            const txHash = Buffer.from(hash.to_hex(), "hex");
            hash.free();
            body.free();
            tx.free();
            yield* PendingBlockFinalizationsDB.preparePendingSubmission({
              ...input,
              preparedTxHash: txHash,
            });
            const sql = yield* SqlClient.SqlClient;
            expect(
              (yield* Effect.either(
                sql.withTransaction(
                  PendingBlockFinalizationsDB.recordSignedIntent(
                    headerHash,
                    txHash,
                    cbor,
                  ),
                ),
              ))._tag,
            ).toBe("Left");
            yield* PendingBlockFinalizationsDB.recordSignedIntent(
              headerHash,
              txHash,
              cbor,
            );
            yield* PendingBlockFinalizationsDB.recordSignedIntent(
              headerHash,
              txHash,
              cbor,
            );
            expect(
              (yield* Effect.either(
                PendingBlockFinalizationsDB.recordSignedIntent(
                  headerHash,
                  Buffer.alloc(32, 3),
                  cbor,
                ),
              ))._tag,
            ).toBe("Left");
            yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
              headerHash,
            );
            expect(
              yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(
                headerHash,
              ),
            ).toBe(false);
            expect(
              (yield* Effect.either(
                PendingBlockFinalizationsDB.markAbandoned(headerHash),
              ))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(
                PendingBlockFinalizationsDB.preparePendingSubmission(input),
              ))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(
                PendingBlockFinalizationsDB.markSubmitted(
                  headerHash,
                  Buffer.alloc(32, 4),
                ),
              ))._tag,
            ).toBe("Left");
            // Canonical header observation need not prove the original transaction hash:
            // a later list append may already have recreated its current node.
            yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
              headerHash,
              1n,
            );
            let record = Option.getOrThrow(
              yield* PendingBlockFinalizationsDB.retrieveActive(),
            );
            expect(
              record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH],
            ).toBeNull();
            expect(
              record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH],
            ).toEqual(txHash);
            expect(
              record[PendingBlockFinalizationsDB.Columns.SIGNED_TX_CBOR],
            ).toEqual(cbor);
            yield* PendingBlockFinalizationsDB.markSubmitted(
              headerHash,
              txHash,
            );
            expect(
              Option.getOrThrow(
                yield* PendingBlockFinalizationsDB.retrieveActive(),
              )[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.ObservedWaitingStability);
            yield* PendingBlockFinalizationsDB.markFinalized(headerHash);
            yield* PendingBlockFinalizationsDB.markSubmitted(
              headerHash,
              txHash,
            );
            record = Option.getOrThrow(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headerHash,
              ),
            );
            expect(record[PendingBlockFinalizationsDB.Columns.STATUS]).toBe(
              PendingBlockFinalizationsDB.Status.Finalized,
            );
          }),
        ),
    );

    it.effect(
      "can discard and replace no-submission pending journals for retry recovery",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "retryable-pending-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);

            yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
            yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
              headerHash,
            );
            let active = yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(active._tag).toBe("None");

            yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
            yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
            active = yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(active._tag).toBe("None");

            yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
            active = yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(active._tag).toBe("Some");
            if (active._tag === "Some") {
              expect(
                active.value[PendingBlockFinalizationsDB.Columns.STATUS],
              ).toBe(PendingBlockFinalizationsDB.Status.PendingSubmission);
              expect(
                typeof active.value[
                  PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT
                ],
              ).toBe("bigint");
              expect(
                active.value[
                  PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT
                ],
              ).toBe(0n);
              expect(
                active.value[
                  PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                ],
              ).toBeNull();
            }
          }),
        ),
    );

    it.effect(
      "rejects startup recovery when an active journal omits committed validation traces",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "missing-validation-trace-header",
              28,
            );
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              pendingSubmissionFixture(headerHash),
            );
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE ${sql(PendingBlockFinalizationsDB.tableName)}
            SET ${sql(
              PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT,
            )} = 1,
            ${sql(
              PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT,
            )} = 1,
            ${sql(
              PendingBlockFinalizationsDB.Columns
                .EXPECTED_TRANSITION_STEP_COUNT,
            )} = 1,
            ${sql(
              PendingBlockFinalizationsDB.Columns
                .EXPECTED_VALIDATION_TRACES_ROOT,
            )} = ${"11".repeat(32)},
            ${sql(
              PendingBlockFinalizationsDB.Columns
                .EXPECTED_VALIDATION_TRACE_COUNT,
            )} = 1
            WHERE ${sql(
              PendingBlockFinalizationsDB.Columns.HEADER_HASH,
            )} = ${headerHash}`;

            const exit = yield* Effect.exit(
              PendingBlockFinalizationsDB.assertActiveJournalPayloadsComplete,
            );
            expect(exit._tag).toBe("Failure");
          }),
        ),
    );

    it.effect(
      "retrieves lease-token journal evidence across active and abandoned statuses",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const leaseToken = "all-status-lease-token";
            const headerHash = databaseFixtureBytes(
              "all-status-lease-token-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);
            yield* PendingBlockFinalizationsDB.preparePendingSubmission({
              ...input,
              metadata: {
                ...input.metadata,
                stateQueueLeaseToken: leaseToken,
              },
            });

            const pending =
              yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
                leaseToken,
              );
            expect(pending).toHaveLength(1);
            expect(
              pending[0]?.[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.PendingSubmission);

            yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
            expect(
              yield* PendingBlockFinalizationsDB.retrieveActiveByStateQueueLeaseToken(
                leaseToken,
              ),
            ).toEqual([]);

            const allStatuses =
              yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
                leaseToken,
              );
            expect(allStatuses).toHaveLength(1);
            expect(
              allStatuses[0]?.[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.Abandoned);
            expect(
              yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
                "missing-lease-token",
              ),
            ).toEqual([]);
          }),
        ),
    );

    it.effect(
      "round-trips Architecture G replay journals and rejects inconsistent root counts",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "architecture-g-replay-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);
            const eventLog = Buffer.alloc(92, 7);
            const replay = {
              schema: 1 as const,
              ownerBinarySha256: databaseFixtureBytes(
                "architecture-g-binary-sha",
                32,
              ),
              baseRoot: Buffer.from(input.metadata.baseRoots.utxosRoot, "hex"),
              candidateRoot: Buffer.from(
                input.metadata.expectedRoots.utxosRoot,
                "hex",
              ),
              eventLog,
              eventLogDigest: databaseFixtureBytes(
                "architecture-g-event-digest",
                32,
              ),
              eventRoots: Buffer.concat([
                databaseFixtureBytes("architecture-g-event-root-0", 32),
                databaseFixtureBytes("architecture-g-event-root-1", 32),
              ]),
              eventCount: 2,
            };
            yield* PendingBlockFinalizationsDB.preparePendingSubmission({
              ...input,
              nativeMpfReplay: replay,
            });
            const active = yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(active._tag).toBe("Some");
            if (Option.isSome(active)) {
              expect(active.value.nativeMpfReplay).toEqual(replay);
            }

            yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
              headerHash,
            );
            const invalid = yield* Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission({
                ...pendingSubmissionFixture(
                  databaseFixtureBytes("architecture-g-invalid-header", 28),
                ),
                nativeMpfReplay: {
                  ...replay,
                  eventCount: 3,
                },
              }),
            );
            expect(invalid._tag).toBe("Left");
          }),
        ),
    );

    it.effect(
      "only abandons pending-submission journals that still have no submitted tx hash",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const submittedHeaderHash = databaseFixtureBytes(
              "submitted-cas-pending-header",
              28,
            );
            const unsubmittedHeaderHash = databaseFixtureBytes(
              "unsubmitted-cas-pending-header",
              28,
            );
            const submittedTxHash = databaseTxHash("submitted-cas-tx");

            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              pendingSubmissionFixture(unsubmittedHeaderHash),
            );
            const unsubmittedAbandoned =
              yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(
                unsubmittedHeaderHash,
              );
            expect(unsubmittedAbandoned).toBe(true);

            const unsubmitted =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                unsubmittedHeaderHash,
              );
            expect(unsubmitted._tag).toBe("Some");
            if (unsubmitted._tag === "Some") {
              expect(
                unsubmitted.value[PendingBlockFinalizationsDB.Columns.STATUS],
              ).toBe(PendingBlockFinalizationsDB.Status.Abandoned);
              expect(
                unsubmitted.value[
                  PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                ],
              ).toBeNull();
            }

            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              pendingSubmissionFixture(submittedHeaderHash),
            );
            yield* PendingBlockFinalizationsDB.markSubmitted(
              submittedHeaderHash,
              submittedTxHash,
            );
            const submittedAbandoned =
              yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(
                submittedHeaderHash,
              );
            expect(submittedAbandoned).toBe(false);

            const submitted =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                submittedHeaderHash,
              );
            expect(submitted._tag).toBe("Some");
            if (submitted._tag === "Some") {
              expect(
                submitted.value[PendingBlockFinalizationsDB.Columns.STATUS],
              ).toBe(
                PendingBlockFinalizationsDB.Status
                  .SubmittedLocalFinalizationPending,
              );
              expect(
                submitted.value[
                  PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                ],
              ).toEqual(submittedTxHash);
            }
          }),
        ),
    );

    it.effect(
      "keeps only the submitted base journal across all memory-only speculative crash checkpoints",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "speculative-crash-base-header",
              28,
            );
            const submittedTxHash = databaseTxHash(
              "speculative-crash-base-submission",
            );
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              pendingSubmissionFixture(headerHash),
            );
            yield* PendingBlockFinalizationsDB.markSubmitted(
              headerHash,
              submittedTxHash,
            );

            const memoryOnlyCheckpoints: readonly string[] = [
              "mid_build",
              "candidate_ready",
            ];
            for (const checkpoint of memoryOnlyCheckpoints) {
              const active =
                yield* PendingBlockFinalizationsDB.retrieveActive();
              expect(active._tag, checkpoint).toBe("Some");
              if (Option.isSome(active)) {
                expect(
                  active.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
                  checkpoint,
                ).toEqual(headerHash);
                expect(
                  active.value[
                    PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                  ],
                  checkpoint,
                ).toEqual(submittedTxHash);
              }
            }

            // Confirmation wake is durable only as the existing N journal's
            // recovery transition. A crash before N+1 journal preparation still
            // leaves no speculative or second active row.
            yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
              headerHash,
              BigInt(Date.now()),
              submittedTxHash,
            );
            const afterWake =
              yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(Option.isSome(afterWake)).toBe(true);
            if (Option.isSome(afterWake)) {
              expect(
                afterWake.value[
                  PendingBlockFinalizationsDB.Columns.HEADER_HASH
                ],
              ).toEqual(headerHash);
              expect(
                afterWake.value[PendingBlockFinalizationsDB.Columns.STATUS],
              ).toBe(
                PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
              );
            }
            const sql = yield* SqlClient.SqlClient;
            const [{ activeCount }] = yield* sql<{
              readonly activeCount: number;
            }>`SELECT COUNT(*)::int AS "activeCount"
            FROM ${sql(PendingBlockFinalizationsDB.tableName)}
            WHERE status IN (
              ${PendingBlockFinalizationsDB.Status.PendingSubmission},
              ${PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending},
              ${PendingBlockFinalizationsDB.Status.ObservedWaitingStability}
            )`;
            expect(activeCount).toBe(1);
          }),
        ),
    );

    it.effect(
      "atomically couples speculative projection writes to pending-journal preparation",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const makeInput = (
              headerLabel: string,
              deposit: DepositsDB.Entry,
            ) => {
              const base = pendingSubmissionFixture(
                databaseFixtureBytes(headerLabel, 28),
              );
              return {
                ...base,
                depositEventIds: [deposit[DepositsDB.Columns.ID]],
                depositEntries: [deposit],
              };
            };

            // Crash before prepare: neither projection nor a candidate journal
            // exists because candidate-ready is entirely memory-only.
            const beforeDeposit = makeDepositEntry();
            yield* DepositsDB.insertEntries([beforeDeposit]);
            const beforeRows = yield* DepositsDB.retrieveAllEntries();
            const beforeAfter = beforeRows.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                beforeDeposit[DepositsDB.Columns.ID],
              ),
            );
            expect(beforeAfter?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Awaiting,
            );
            expect(
              Option.isNone(
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                  databaseFixtureBytes("atomic-before-header", 28),
                ),
              ),
            ).toBe(true);

            // Crash/failure during prepare: a deferred projection write and the
            // journal insert share one SQL transaction, so both roll back.
            const duringDeposit = makeDepositEntry();
            yield* DepositsDB.insertEntries([duringDeposit]);
            const duringInput = makeInput(
              "atomic-during-header",
              duringDeposit,
            );
            const duringResult = yield* Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission(
                duringInput,
                {
                  beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                    duringDeposit[DepositsDB.Columns.ID],
                  ]).pipe(
                    Effect.andThen(
                      Effect.fail(
                        new DatabaseError({
                          table: PendingBlockFinalizationsDB.tableName,
                          message: "Injected crash during journal preparation",
                          cause: "crash_during_prepare",
                        }),
                      ),
                    ),
                  ),
                },
              ),
            );
            expect(duringResult._tag).toBe("Left");
            const duringRows = yield* DepositsDB.retrieveAllEntries();
            const duringAfter = duringRows.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                duringDeposit[DepositsDB.Columns.ID],
              ),
            );
            expect(duringAfter?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Awaiting,
            );
            expect(
              Option.isNone(
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                  duringInput.headerHash,
                ),
              ),
            ).toBe(true);

            // Crash immediately after prepare: the projection and its complete
            // pending journal are both durable, never only one of the pair.
            const afterDeposit = makeDepositEntry();
            yield* DepositsDB.insertEntries([afterDeposit]);
            const afterInput = makeInput("atomic-after-header", afterDeposit);
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              afterInput,
              {
                beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                  afterDeposit[DepositsDB.Columns.ID],
                ]),
              },
            );
            const afterRows = yield* DepositsDB.retrieveAllEntries();
            const afterProjected = afterRows.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                afterDeposit[DepositsDB.Columns.ID],
              ),
            );
            expect(afterProjected?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Projected,
            );
            const afterJournal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                afterInput.headerHash,
              );
            expect(Option.isSome(afterJournal)).toBe(true);
            if (Option.isSome(afterJournal)) {
              expect(afterJournal.value.depositEventIds).toEqual([
                afterDeposit[DepositsDB.Columns.ID],
              ]);
            }
          }),
        ),
    );

    vitestIt(
      "survives real process kills before, during, and after atomic journal prepare",
      async () => {
        const deposits = [
          makeDepositEntry(),
          makeDepositEntry(),
          makeDepositEntry(),
        ];
        await Effect.runPromise(isolatedDb(DepositsDB.insertEntries(deposits)));
        const helper = bundleChildProcessHelper(
          "helpers/pending-journal-crash-process.ts",
        );
        const cwd = path.resolve(databaseTestDirectory, "..");
        const checkpoints = ["before", "during", "after"] as const;
        try {
          for (const [index, checkpoint] of checkpoints.entries()) {
            const deposit = deposits[index]!;
            const headerHash = databaseFixtureBytes(
              `speculative-process-crash-${checkpoint}`,
              28,
            );
            const child = spawn(
              process.execPath,
              [
                helper,
                checkpoint,
                deposit[DepositsDB.Columns.ID].toString("hex"),
                headerHash.toString("hex"),
              ],
              {
                cwd,
                env: databaseChildProcessEnv(),
                shell: false,
                stdio: ["ignore", "pipe", "pipe"],
              },
            );
            const result = await collectChildProcess(child);
            expect(result.signal, result.stderr).toBe("SIGKILL");

            const snapshot = await Effect.runPromise(
              provideDatabaseLayers(
                Effect.all({
                  rows: DepositsDB.retrieveAllEntries(),
                  journal:
                    PendingBlockFinalizationsDB.retrieveByHeaderHash(
                      headerHash,
                    ),
                }),
              ),
            );
            const persistedDeposit = snapshot.rows.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                deposit[DepositsDB.Columns.ID],
              ),
            );
            if (checkpoint === "after") {
              expect(persistedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
                DepositsDB.Status.Projected,
              );
              expect(Option.isSome(snapshot.journal)).toBe(true);
            } else {
              expect(persistedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
                DepositsDB.Status.Awaiting,
              );
              expect(Option.isNone(snapshot.journal)).toBe(true);
            }
          }
        } finally {
          fs.rmSync(helper, { force: true });
        }
      },
      30_000,
    );

    it.effect(
      "fails closed when a same-count speculative source changes before atomic prepare",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const candidateDeposit = makeDepositEntry();
            yield* DepositsDB.insertEntries([candidateDeposit]);
            const candidateRoot = yield* resolveDepositsRoot([
              candidateDeposit,
            ]);
            expect(Option.isSome(candidateRoot)).toBe(true);
            if (Option.isNone(candidateRoot)) return;

            const sql = yield* SqlClient.SqlClient;
            const changedInfo = databaseFixtureBytes(
              "speculative-source-changed-info",
              48,
            );
            yield* sql`UPDATE ${sql(DepositsDB.tableName)}
            SET ${sql(DepositsDB.Columns.INFO)} = ${changedInfo}
            WHERE ${sql(DepositsDB.Columns.ID)} = ${candidateDeposit[DepositsDB.Columns.ID]}`;

            const inputBase = pendingSubmissionFixture(
              databaseFixtureBytes("speculative-source-changed-header", 28),
            );
            const input = {
              ...inputBase,
              depositEventIds: [candidateDeposit[DepositsDB.Columns.ID]],
              depositEntries: [candidateDeposit],
            };
            const attempt = yield* Effect.either(
              StateQueueMutationLeasesDB.tryWithLease(
                "speculative-source-revalidation-test",
                (stateQueueLeaseToken) =>
                  MpfEngineStateDB.tryWithLedgerStoreLease(
                    "speculative-source-revalidation-test",
                    (activeMpfLeaseOwner) =>
                      PendingBlockFinalizationsDB.preparePendingSubmission(
                        input,
                        {
                          beforeJournalInsert:
                            revalidateAndPersistSpeculativeCandidateSources({
                              includedDepositEntries: [candidateDeposit],
                              includedForcedTransactionEntries: [],
                              includedWithdrawalEntries: [],
                              selectedMempoolTxs: [],
                              rejectedMempoolTxs: [],
                              mempoolTxSourceTable: "none",
                              rejectionEntries: [],
                              ledgerRevert: {
                                rejected: [],
                                resolveInputPostState: () => undefined,
                              },
                              expectedEventRoots: {
                                deposits: candidateRoot.value,
                                forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                                withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                              },
                              ...speculativeCandidateEventSnapshot,
                              stateQueueLeaseToken,
                              activeMpfLeaseOwner,
                            }),
                        },
                      ),
                  ),
              ),
            );
            expect(attempt._tag).toBe("Left");
            expect(
              Option.isNone(
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                  input.headerHash,
                ),
              ),
            ).toBe(true);
            const rows = yield* DepositsDB.retrieveAllEntries();
            const retained = rows.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                candidateDeposit[DepositsDB.Columns.ID],
              ),
            );
            expect(retained?.[DepositsDB.Columns.INFO]).toEqual(changedInfo);
            expect(retained?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Awaiting,
            );
          }),
        ),
    );

    it.effect(
      "rejects changed inline mempool payloads and duplicate selected/rejected ids",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const txId = databaseTxHash("speculative-accepted-payload");
            const txCbor = databaseFixtureBytes(
              "speculative-accepted-payload-cbor",
              96,
            );
            yield* TxAdmissionsDB.tryInsert({
              txId,
              txCanonicalCbor: txCbor,
              programMaterialSidecarCbor: emptyProgramMaterialSidecar,
              submitSource: "native",
            });
            yield* sql`INSERT INTO ${sql(MempoolDB.tableName)} (
              ${sql(TxUtils.Columns.TX_ID)},
              ${sql(TxUtils.Columns.TX)}
            ) VALUES (${txId}, ${txCbor})`;
            const page = yield* MempoolDB.retrievePage({ limit: 10 });
            const candidate = page.entries.find((entry) =>
              entry[TxUtils.Columns.TX_ID].equals(txId),
            );
            expect(candidate).toBeDefined();
            if (candidate === undefined) return;
            expect(candidate[TxUtils.Columns.TX]).toEqual(txCbor);

            // Changing the authoritative inline payload after candidate-ready
            // must fail exact source revalidation even though membership and
            // transaction count are unchanged.
            const changedTxCbor = databaseFixtureBytes(
              "speculative-changed-inline-payload-cbor",
              96,
            );
            yield* sql`UPDATE ${sql(MempoolDB.tableName)}
            SET ${sql(TxUtils.Columns.TX)} = ${changedTxCbor}
            WHERE ${sql(TxUtils.Columns.TX_ID)} = ${txId}`;
            const changedPayload = yield* Effect.either(
              StateQueueMutationLeasesDB.tryWithLease(
                "speculative-changed-inline-payload-test",
                (stateQueueLeaseToken) =>
                  MpfEngineStateDB.tryWithLedgerStoreLease(
                    "speculative-changed-inline-payload-test",
                    (activeMpfLeaseOwner) =>
                      revalidateAndPersistSpeculativeCandidateSources({
                        includedDepositEntries: [],
                        includedForcedTransactionEntries: [],
                        includedWithdrawalEntries: [],
                        selectedMempoolTxs: [candidate],
                        rejectedMempoolTxs: [],
                        mempoolTxSourceTable: MempoolDB.tableName,
                        rejectionEntries: [],
                        ledgerRevert: {
                          rejected: [],
                          resolveInputPostState: () => undefined,
                        },
                        expectedEventRoots: {
                          deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                          forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                          withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                        },
                        ...speculativeCandidateEventSnapshot,
                        stateQueueLeaseToken,
                        activeMpfLeaseOwner,
                      }),
                  ),
              ),
            );
            expect(changedPayload._tag).toBe("Left");

            const duplicateTxId = databaseTxHash("speculative-duplicate-union");
            yield* ProcessedMempoolDB.insertTx({
              [TxUtils.Columns.TX_ID]: duplicateTxId,
              [TxUtils.Columns.TX]: databaseFixtureBytes(
                "speculative-duplicate-union-cbor",
                96,
              ),
            });
            const processed = yield* ProcessedMempoolDB.retrieve;
            const duplicateCandidate = processed.find((entry) =>
              entry[TxUtils.Columns.TX_ID].equals(duplicateTxId),
            );
            expect(duplicateCandidate).toBeDefined();
            if (duplicateCandidate === undefined) return;
            const duplicateUnion = yield* Effect.either(
              StateQueueMutationLeasesDB.tryWithLease(
                "speculative-duplicate-union-test",
                (stateQueueLeaseToken) =>
                  MpfEngineStateDB.tryWithLedgerStoreLease(
                    "speculative-duplicate-union-test",
                    (activeMpfLeaseOwner) =>
                      revalidateAndPersistSpeculativeCandidateSources({
                        includedDepositEntries: [],
                        includedForcedTransactionEntries: [],
                        includedWithdrawalEntries: [],
                        selectedMempoolTxs: [duplicateCandidate],
                        rejectedMempoolTxs: [duplicateCandidate],
                        mempoolTxSourceTable: ProcessedMempoolDB.tableName,
                        rejectionEntries: [
                          {
                            [TxRejectionsDB.Columns.TX_ID]: duplicateTxId,
                            [TxRejectionsDB.Columns.REJECT_CODE]:
                              "E_TEST_DUPLICATE_UNION",
                            [TxRejectionsDB.Columns.REJECT_DETAIL]:
                              "duplicate selected/rejected id test",
                          },
                        ],
                        ledgerRevert: {
                          rejected: [],
                          resolveInputPostState: () => undefined,
                        },
                        expectedEventRoots: {
                          deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                          forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                          withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                        },
                        ...speculativeCandidateEventSnapshot,
                        stateQueueLeaseToken,
                        activeMpfLeaseOwner,
                      }),
                  ),
              ),
            );
            expect(duplicateUnion._tag).toBe("Left");
          }),
        ),
    );

    it.effect(
      "rolls back projections, ledger reconciliation and rejections when journal member insertion fails",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            yield* DepositsDB.insertEntries([deposit]);
            const depositRoot = yield* resolveDepositsRoot([deposit]);
            expect(Option.isSome(depositRoot)).toBe(true);
            if (Option.isNone(depositRoot)) return;

            const rejectedTxId = databaseTxHash(
              "speculative-atomic-rejected-tx",
            );
            yield* ProcessedMempoolDB.insertTx({
              [TxUtils.Columns.TX_ID]: rejectedTxId,
              [TxUtils.Columns.TX]: databaseFixtureBytes(
                "speculative-atomic-rejected-cbor",
                96,
              ),
            });
            const rejectedTx = (yield* ProcessedMempoolDB.retrieve).find(
              (entry) => entry[TxUtils.Columns.TX_ID].equals(rejectedTxId),
            );
            expect(rejectedTx).toBeDefined();
            if (rejectedTx === undefined) return;

            const base = pendingSubmissionFixture(
              databaseFixtureBytes("speculative-atomic-failing-header", 28),
            );
            // Duplicate journal members pass set-equality preflight but violate
            // the (header_hash, member_id) primary key after the deferred writes
            // have run, forcing a transaction rollback at the sharp boundary.
            const failingInput = {
              ...base,
              depositEventIds: [
                deposit[DepositsDB.Columns.ID],
                deposit[DepositsDB.Columns.ID],
              ],
              depositEntries: [deposit, deposit],
            };
            const result = yield* Effect.either(
              StateQueueMutationLeasesDB.tryWithLease(
                "speculative-atomic-rollback-test",
                (stateQueueLeaseToken) =>
                  MpfEngineStateDB.tryWithLedgerStoreLease(
                    "speculative-atomic-rollback-test",
                    (activeMpfLeaseOwner) =>
                      PendingBlockFinalizationsDB.preparePendingSubmission(
                        failingInput,
                        {
                          beforeJournalInsert:
                            revalidateAndPersistSpeculativeCandidateSources({
                              includedDepositEntries: [deposit],
                              includedForcedTransactionEntries: [],
                              includedWithdrawalEntries: [],
                              selectedMempoolTxs: [],
                              rejectedMempoolTxs: [rejectedTx],
                              mempoolTxSourceTable:
                                ProcessedMempoolDB.tableName,
                              rejectionEntries: [
                                {
                                  [TxRejectionsDB.Columns.TX_ID]: rejectedTxId,
                                  [TxRejectionsDB.Columns.REJECT_CODE]:
                                    "E_TEST_ATOMIC_ROLLBACK",
                                  [TxRejectionsDB.Columns.REJECT_DETAIL]:
                                    "injected rejected tx for rollback test",
                                },
                              ],
                              ledgerRevert: {
                                rejected: [],
                                resolveInputPostState: () => undefined,
                              },
                              expectedEventRoots: {
                                deposits: depositRoot.value,
                                forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                                withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                              },
                              ...speculativeCandidateEventSnapshot,
                              stateQueueLeaseToken,
                              activeMpfLeaseOwner,
                            }),
                        },
                      ),
                  ),
              ),
            );
            expect(result._tag).toBe("Left");
            expect(
              Option.isNone(
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                  failingInput.headerHash,
                ),
              ),
            ).toBe(true);
            const deposits = yield* DepositsDB.retrieveAllEntries();
            const retainedDeposit = deposits.find((entry) =>
              entry[DepositsDB.Columns.ID].equals(
                deposit[DepositsDB.Columns.ID],
              ),
            );
            expect(retainedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Awaiting,
            );
            expect(
              yield* MempoolLedgerDB.retrieveBySourceEventIds([
                deposit[DepositsDB.Columns.ID],
              ]),
            ).toHaveLength(0);
            expect(
              (yield* ProcessedMempoolDB.retrieve).some((entry) =>
                entry[TxUtils.Columns.TX_ID].equals(rejectedTxId),
              ),
            ).toBe(true);
            expect(
              yield* TxRejectionsDB.retrieveByTxId(rejectedTxId),
            ).toHaveLength(0);
          }),
        ),
    );

    it.effect(
      "reverts a speculative candidate's rejected ledger effects with its sources",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const address = CML.Address.from_bech32(address1);
            const entry = (byte: number): LedgerUtils.Entry => ({
              [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, byte),
              [LedgerUtils.Columns.OUTREF]: makeOutRefCbor(byte, 0),
              [LedgerUtils.Columns.OUTPUT]: Buffer.from(
                makeMidgardTxOutput(
                  address,
                  CML.Value.from_coin(1_000_000n + BigInt(byte)),
                ).to_cbor_bytes(),
              ),
              [LedgerUtils.Columns.ADDRESS]: address1,
            });
            const committedInput = entry(0x51);
            const rejectedOutput = entry(0x52);
            const rejected: ProcessedTx = {
              txId: databaseTxHash("speculative-ledger-revert-tx"),
              txCbor: databaseFixtureBytes(
                "speculative-ledger-revert-cbor",
                96,
              ),
              spent: [committedInput[LedgerUtils.Columns.OUTREF]],
              produced: [rejectedOutput],
            };
            yield* MempoolLedgerDB.insert([committedInput]);
            yield* ProcessedMempoolDB.insertTx({
              [TxUtils.Columns.TX_ID]: rejected.txId,
              [TxUtils.Columns.TX]: rejected.txCbor,
            });
            yield* MempoolDB.applyLedgerEffectsCore([rejected]);
            const rejectedTx = (yield* ProcessedMempoolDB.retrieve).find(
              (row) => row[TxUtils.Columns.TX_ID].equals(rejected.txId),
            );
            expect(rejectedTx).toBeDefined();
            if (rejectedTx === undefined) return;

            let reverted: boolean | undefined;
            const prepared = yield* StateQueueMutationLeasesDB.tryWithLease(
              "speculative-ledger-revert-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-ledger-revert-test",
                  (activeMpfLeaseOwner) =>
                    PendingBlockFinalizationsDB.preparePendingSubmission(
                      pendingSubmissionFixture(
                        databaseFixtureBytes(
                          "speculative-ledger-revert-header",
                          28,
                        ),
                      ),
                      {
                        beforeJournalInsert:
                          revalidateAndPersistSpeculativeCandidateSources({
                            includedDepositEntries: [],
                            includedForcedTransactionEntries: [],
                            includedWithdrawalEntries: [],
                            selectedMempoolTxs: [],
                            rejectedMempoolTxs: [rejectedTx],
                            mempoolTxSourceTable: ProcessedMempoolDB.tableName,
                            rejectionEntries: [
                              {
                                [TxRejectionsDB.Columns.TX_ID]: rejected.txId,
                                [TxRejectionsDB.Columns.REJECT_CODE]:
                                  COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
                                [TxRejectionsDB.Columns.REJECT_DETAIL]:
                                  "withdrawn input",
                              },
                            ],
                            ledgerRevert: {
                              rejected: [rejected],
                              resolveInputPostState: commitStageInputPostState({
                                baseLedgerOutputs: new Map([
                                  [
                                    committedInput[
                                      LedgerUtils.Columns.OUTREF
                                    ].toString("hex"),
                                    committedInput[LedgerUtils.Columns.OUTPUT],
                                  ],
                                ]),
                                insertedOutputs: new Map(),
                                spentOutRefHexes: new Set(),
                              }),
                            },
                            expectedEventRoots: {
                              deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                              forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                              withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                            },
                            ...speculativeCandidateEventSnapshot,
                            stateQueueLeaseToken,
                            activeMpfLeaseOwner,
                          }).pipe(
                            Effect.map((result) => {
                              reverted = result;
                            }),
                          ),
                      },
                    ),
                ),
            );

            expect(prepared._tag).toBe("Ran");
            expect(reverted).toBe(true);
            expect(
              (yield* MempoolLedgerDB.retrieve).map((row) => [
                row[MempoolLedgerDB.Columns.OUTREF],
                row[MempoolLedgerDB.Columns.OUTPUT],
              ]),
            ).toEqual([
              [
                committedInput[LedgerUtils.Columns.OUTREF],
                committedInput[LedgerUtils.Columns.OUTPUT],
              ],
            ]);
            expect(yield* ProcessedMempoolDB.retrieve).toEqual([]);
          }),
        ),
    );

    it.effect(
      "persists an in-memory withdrawal classification atomically with its journal",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const withdrawalId = databaseOutputReferenceId(
              "speculative-memory-withdrawal",
              0n,
            );
            const currentWithdrawal: WithdrawalsDB.Entry = {
              [WithdrawalsDB.Columns.ID]: withdrawalId,
              [WithdrawalsDB.Columns.RAW_EVENT_INFO]: databaseFixtureBytes(
                "speculative-memory-withdrawal-raw",
                96,
              ),
              [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: null,
              [WithdrawalsDB.Columns.INCLUSION_TIME]: new Date(
                "2026-04-13T18:00:00.000Z",
              ),
              [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
                "speculative-memory-withdrawal-l1",
              ),
              [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 0,
              [WithdrawalsDB.Columns.ASSET_NAME]: databaseFixtureBytes(
                "speculative-memory-withdrawal-asset",
                32,
              ),
              [WithdrawalsDB.Columns.L2_OUTREF]: databaseOutputReferenceId(
                "speculative-memory-withdrawal-l2",
                1n,
              ),
              [WithdrawalsDB.Columns.L2_OWNER]: databaseFixtureBytes(
                "speculative-memory-withdrawal-owner",
                28,
              ),
              [WithdrawalsDB.Columns.L2_VALUE]: databaseFixtureBytes(
                "speculative-memory-withdrawal-value",
                48,
              ),
              [WithdrawalsDB.Columns.L1_ADDRESS]: databaseFixtureBytes(
                "speculative-memory-withdrawal-address",
                32,
              ),
              [WithdrawalsDB.Columns.L1_DATUM]: databaseFixtureBytes(
                "speculative-memory-withdrawal-datum",
                16,
              ),
              [WithdrawalsDB.Columns.REFUND_ADDRESS]: databaseFixtureBytes(
                "speculative-memory-withdrawal-refund-address",
                32,
              ),
              [WithdrawalsDB.Columns.REFUND_DATUM]: databaseFixtureBytes(
                "speculative-memory-withdrawal-refund-datum",
                16,
              ),
              [WithdrawalsDB.Columns.VALIDITY]: null,
              [WithdrawalsDB.Columns.CLASSIFICATION_REVISION]: 0,
              [WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH]: null,
              [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
              [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
              [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
            };
            yield* WithdrawalsDB.insertEntries([currentWithdrawal]);
            const settlementEventInfo = databaseFixtureBytes(
              "speculative-memory-withdrawal-settlement",
              64,
            );
            const candidateWithdrawal: WithdrawalsDB.Entry = {
              ...currentWithdrawal,
              [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
                settlementEventInfo,
              [WithdrawalsDB.Columns.VALIDITY]:
                WithdrawalsDB.Validity.WithdrawalIsValid,
              [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {
                source: "speculative-memory",
              },
              [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Projected,
            };
            const withdrawalRoot = yield* resolveWithdrawalsRoot([
              candidateWithdrawal,
            ]);
            expect(Option.isSome(withdrawalRoot)).toBe(true);
            if (Option.isNone(withdrawalRoot)) return;

            const base = pendingSubmissionFixture(
              databaseFixtureBytes("speculative-memory-withdrawal-header", 28),
            );
            const input = {
              ...base,
              withdrawalEventIds: [withdrawalId],
              withdrawalEntries: [candidateWithdrawal],
            };
            const prepared = yield* StateQueueMutationLeasesDB.tryWithLease(
              "speculative-memory-withdrawal-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-memory-withdrawal-test",
                  (activeMpfLeaseOwner) =>
                    PendingBlockFinalizationsDB.preparePendingSubmission(
                      input,
                      {
                        beforeJournalInsert:
                          revalidateAndPersistSpeculativeCandidateSources({
                            includedDepositEntries: [],
                            includedForcedTransactionEntries: [],
                            includedWithdrawalEntries: [candidateWithdrawal],
                            selectedMempoolTxs: [],
                            rejectedMempoolTxs: [],
                            mempoolTxSourceTable: "none",
                            rejectionEntries: [],
                            ledgerRevert: {
                              rejected: [],
                              resolveInputPostState: () => undefined,
                            },
                            expectedEventRoots: {
                              deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                              forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                              withdrawals: withdrawalRoot.value,
                            },
                            ...speculativeCandidateEventSnapshot,
                            stateQueueLeaseToken,
                            activeMpfLeaseOwner,
                          }),
                      },
                    ),
                ),
            );
            expect(prepared._tag).toBe("Ran");
            if (prepared._tag === "Ran") {
              expect(prepared.value._tag).toBe("Ran");
            }
            const persisted =
              yield* WithdrawalsDB.retrieveByEventId(withdrawalId);
            expect(Option.isSome(persisted)).toBe(true);
            if (Option.isSome(persisted)) {
              expect(
                persisted.value[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO],
              ).toEqual(settlementEventInfo);
              expect(persisted.value[WithdrawalsDB.Columns.STATUS]).toBe(
                WithdrawalsDB.Status.Projected,
              );
            }
            expect(
              Option.isSome(
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                  input.headerHash,
                ),
              ),
            ).toBe(true);
          }),
        ),
    );
  });
};
