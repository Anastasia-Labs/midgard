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
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import { makeCardanoSignedMapOutputTxBytes } from ".././helpers/cardano-native-fixtures.js";
import { provideDatabaseLayers } from ".././utils.js";
import { insertDeposits } from "../helpers/event-rows.js";
import { databaseTestDirectory } from "./finalization.database-test-directory.js";
import {
  bundleChildProcessHelper,
  collectChildProcess,
  databaseChildProcessEnv,
  databaseFixtureBytes,
  databaseTxHash,
  isolatedDb,
  makeDepositEntry,
} from "./fixtures.js";

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
              PendingBlockFinalizationsDB.Status.LocallyApplied,
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
      "atomically couples projection writes to pending-journal preparation",
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

            // Crash before prepare: neither the projection nor a journal exists.
            const beforeDeposit = makeDepositEntry();
            yield* insertDeposits([beforeDeposit]);
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
            yield* insertDeposits([duringDeposit]);
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
            yield* insertDeposits([afterDeposit]);
            const afterInput = makeInput("atomic-after-header", afterDeposit);
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              afterInput,
              {
                beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                  afterDeposit[DepositsDB.Columns.ID],
                ]).pipe(Effect.as(undefined)),
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
        await Effect.runPromise(isolatedDb(insertDeposits(deposits)));
        const helper = bundleChildProcessHelper(
          "helpers/pending-journal-crash-process.ts",
        );
        const cwd = path.resolve(databaseTestDirectory, "..");
        const checkpoints = ["before", "during", "after"] as const;
        try {
          for (const [index, checkpoint] of checkpoints.entries()) {
            const deposit = deposits[index]!;
            const headerHash = databaseFixtureBytes(
              `process-crash-${checkpoint}`,
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
  });
};
