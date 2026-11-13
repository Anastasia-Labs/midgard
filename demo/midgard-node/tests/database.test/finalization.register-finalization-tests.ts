import { spawn } from "node:child_process";
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
import { Frontier } from "../../src/landed-blocks/store.js";
import { makeCardanoSignedMapOutputTxBytes } from ".././helpers/cardano-native-fixtures.js";
import { provideDatabaseLayers } from ".././utils.js";
import { insertDeposits } from "../helpers/event-rows.js";
import { simCommitAnchor } from "../helpers/landed-blocks-sim.own.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "../helpers/mutation-lease.js";
import { insertCommitIntent } from "../helpers/own-block-facts.js";
import { databaseTestDirectory } from "./finalization.database-test-directory.js";
import {
  bundleChildProcessHelper,
  collectChildProcess,
  databaseChildProcessEnv,
  databaseFixtureBytes,
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
        ...emptyExpectedRoots,
        ...emptyExpectedCounts,
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
        blockTime: new Date(blockStartTime.getTime() + 60_000),
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
        ledgerDelta: {
          spent: [],
          produced: [],
        },
      };
    };
    // A journal the commit worker writes stores its commit anchor and
    // builds on a base on the landed chain (here the frontier); without
    // either the journal is dead (`deadJournals`) and never live.
    const anchored = {
      beforeJournalInsert: Effect.orDie(
        Effect.zipRight(
          Frontier.upsert({
            headerHash: databaseFixtureBytes("base-tail-header", 28).toString(
              "hex",
            ),
            utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          }),
          simCommitAnchor,
        ),
      ),
    };
    const Journals = PendingBlockFinalizationsDB;

    it.effect(
      "records a signed intent once, only under the journal's lease, and never replaces the signed journal",
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
            yield* prepareUnderTestLease(
              { ...input, preparedTxHash: txHash },
              anchored,
            );
            const sql = yield* SqlClient.SqlClient;
            expect(
              (yield* Effect.either(
                sql.withTransaction(
                  Journals.recordSignedIntent(headerHash, txHash, cbor),
                ),
              ))._tag,
            ).toBe("Left");
            const signUnderLease = (
              signedTxHash: Buffer,
              journalInsert?: Effect.Effect<void, DatabaseError>,
            ) =>
              Journals.recordSignedIntent(
                headerHash,
                signedTxHash,
                cbor,
                journalInsert,
              ).pipe(
                withTestMutationLease(input.metadata.stateQueueLeaseToken),
              );
            yield* signUnderLease(
              txHash,
              insertCommitIntent(headerHash, txHash, cbor).pipe(
                Effect.orDie,
                Effect.provideService(SqlClient.SqlClient, sql),
              ),
            );
            yield* signUnderLease(txHash);
            expect(
              (yield* Effect.either(signUnderLease(Buffer.alloc(32, 3))))._tag,
            ).toBe("Left");
            // A signed journal's commit may be on L1: it is neither discarded
            // nor replaced by a prepare of its header.
            expect(yield* Journals.discardUnsignedJournal(headerHash)).toBe(
              false,
            );
            expect(yield* prepareUnderTestLease(input, anchored)).toEqual({
              kind: "held",
              heldHeaderHash: headerHash,
            });
            const live = Option.getOrThrow(yield* Journals.retrieveLive());
            expect(live[Journals.Columns.HEADER_HASH]).toEqual(headerHash);
            expect(live[Journals.Columns.INTENDED_TX_HASH]).toEqual(txHash);
            expect(yield* Journals.retrieveStatusByHeaderHash(headerHash)).toBe(
              Journals.OwnBlockStatus.Unlanded,
            );
          }),
        ),
    );

    it.effect(
      "never signs a journal prepared without a transaction body hash",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            // A signed journal always records its prepared hash, so a signed
            // journal with none is one 0037 carried over from a commit
            // submitted unjournaled.
            const headerHash = databaseFixtureBytes(
              "unprepared-signed-intent-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);
            yield* prepareUnderTestLease(input, anchored);
            const cbor = Buffer.from(makeCardanoSignedMapOutputTxBytes());
            const tx = CML.Transaction.from_cbor_bytes(cbor);
            const body = tx.body();
            const hash = CML.hash_transaction(body);
            const txHash = Buffer.from(hash.to_hex(), "hex");
            hash.free();
            body.free();
            tx.free();
            const refused = yield* Effect.either(
              Journals.recordSignedIntent(headerHash, txHash, cbor).pipe(
                withTestMutationLease(input.metadata.stateQueueLeaseToken),
              ),
            );
            expect(refused._tag).toBe("Left");
            if (refused._tag === "Left")
              expect(refused.left.message).toBe(
                "Signed intent conflicts with the pending journal",
              );
            const journal = Option.getOrThrow(
              yield* Journals.retrieveByHeaderHash(headerHash),
            );
            expect(journal[Journals.Columns.PREPARED_TX_HASH]).toBeNull();
            expect(journal[Journals.Columns.INTENDED_TX_HASH]).toBeNull();
          }),
        ),
    );

    it.effect(
      "discards an unsigned journal, and a prepare of its header supersedes it",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "retryable-pending-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);

            yield* prepareUnderTestLease(input, anchored);
            expect(yield* Journals.discardUnsignedJournal(headerHash)).toBe(
              true,
            );
            expect(
              Option.isNone(yield* Journals.retrieveByHeaderHash(headerHash)),
            ).toBe(true);
            expect(Option.isNone(yield* Journals.retrieveLive())).toBe(true);

            yield* prepareUnderTestLease(input, anchored);
            expect(yield* prepareUnderTestLease(input, anchored)).toEqual({
              kind: "prepared",
            });
            const live = Option.getOrThrow(yield* Journals.retrieveLive());
            expect(live[Journals.Columns.HEADER_HASH]).toEqual(headerHash);
            expect(
              live[Journals.Columns.EXPECTED_TOTAL_EVENT_COUNT],
            ).toStrictEqual(0n);
            expect(live[Journals.Columns.INTENDED_TX_HASH]).toBeNull();
          }),
        ),
    );

    it.effect("refuses a second prepare while another journal is live", () =>
      isolatedDb(
        Effect.gen(function* () {
          const first = pendingSubmissionFixture(
            databaseFixtureBytes("single-live-first-header", 28),
          );
          const second = pendingSubmissionFixture(
            databaseFixtureBytes("single-live-second-header", 28),
          );
          yield* prepareUnderTestLease(first, anchored);
          const refused = yield* Effect.either(
            prepareUnderTestLease(second, anchored),
          );
          expect(refused._tag).toBe("Left");
          if (refused._tag === "Left")
            expect(refused.left.message).toBe(Journals.LIVE_JOURNAL_REFUSAL);
          expect(
            Option.isNone(
              yield* Journals.retrieveByHeaderHash(second.headerHash),
            ),
          ).toBe(true);
          // Once the first is gone, the second prepares.
          yield* Journals.discardUnsignedJournal(first.headerHash);
          expect(yield* prepareUnderTestLease(second, anchored)).toEqual({
            kind: "prepared",
          });
        }),
      ),
    );

    it.effect(
      "rejects startup recovery when the live journal omits committed validation traces",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes(
              "missing-validation-trace-header",
              28,
            );
            yield* prepareUnderTestLease(
              pendingSubmissionFixture(headerHash),
              anchored,
            );
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE ${sql(Journals.tableName)}
            SET ${sql(Journals.Columns.EXPECTED_L2_TRANSACTION_COUNT)} = 1,
            ${sql(Journals.Columns.EXPECTED_TOTAL_EVENT_COUNT)} = 1,
            ${sql(Journals.Columns.EXPECTED_TRANSITION_STEP_COUNT)} = 1,
            ${sql(
              Journals.Columns.EXPECTED_VALIDATION_TRACES_ROOT,
            )} = ${"11".repeat(32)},
            ${sql(Journals.Columns.EXPECTED_VALIDATION_TRACE_COUNT)} = 1
            WHERE ${sql(Journals.Columns.HEADER_HASH)} = ${headerHash}`;

            const exit = yield* Effect.exit(
              Journals.assertLiveJournalPayloadsComplete,
            );
            expect(exit._tag).toBe("Failure");
          }),
        ),
    );

    it.effect(
      "retrieves a lease token's journals with their derived status, dead or not",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const leaseToken = "all-status-lease-token";
            const headerHash = databaseFixtureBytes(
              "all-status-lease-token-header",
              28,
            );
            const input = pendingSubmissionFixture(headerHash);
            yield* prepareUnderTestLease(
              {
                ...input,
                metadata: {
                  ...input.metadata,
                  stateQueueLeaseToken: leaseToken,
                },
              },
              anchored,
            );
            const byToken = (token: string) =>
              Journals.retrieveByStateQueueLeaseToken(token).pipe(
                Effect.map((journals) =>
                  journals.map(({ row, status, dead }) => ({
                    headerHash: row[Journals.Columns.HEADER_HASH],
                    status,
                    dead,
                  })),
                ),
              );
            expect(yield* byToken(leaseToken)).toEqual([
              {
                headerHash,
                status: Journals.OwnBlockStatus.Unlanded,
                dead: false,
              },
            ]);

            // Its commit anchor leaves the chain: the journal is dead, and
            // nothing is written to it.
            const anchor = yield* simCommitAnchor;
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM l1_blocks WHERE hash = ${anchor.hash}`;
            expect(yield* byToken(leaseToken)).toEqual([
              {
                headerHash,
                status: Journals.OwnBlockStatus.Unlanded,
                dead: true,
              },
            ]);
            expect(Option.isNone(yield* Journals.retrieveLive())).toBe(true);
            expect(yield* byToken("missing-lease-token")).toEqual([]);
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
            yield* prepareUnderTestLease(
              { ...input, nativeMpfReplay: replay },
              anchored,
            );
            const live = yield* Journals.retrieveLive();
            expect(live._tag).toBe("Some");
            if (Option.isSome(live)) {
              expect(live.value.nativeMpfReplay).toEqual(replay);
            }

            yield* Journals.discardUnsignedJournal(headerHash);
            const invalid = yield* Effect.either(
              prepareUnderTestLease({
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
              prepareUnderTestLease(duringInput, {
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
              }),
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
            yield* prepareUnderTestLease(afterInput, {
              beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                afterDeposit[DepositsDB.Columns.ID],
              ]).pipe(Effect.as(undefined)),
            });
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
                  PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
              }),
            ),
          );
          const persistedDeposit = snapshot.rows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(deposit[DepositsDB.Columns.ID]),
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
      },
      30_000,
    );
  });
};
