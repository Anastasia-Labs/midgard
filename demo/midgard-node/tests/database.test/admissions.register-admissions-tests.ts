import { default as path } from "node:path";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxFullHashFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { SqlClient } from "@effect/sql";
import { type PgClient } from "@effect/sql-pg/PgClient";
import { it } from "@effect/vitest";
import { CML, toHex } from "@lucid-evolution/lucid";
import {
  Deferred,
  Duration,
  Effect,
  Fiber,
  Option,
  Ref,
  Schedule,
  Stream,
  SubscriptionRef,
  TestClock,
} from "effect";
import { superviseHostProcess } from "midgard-node-tools/e2e/service-supervisor";
import { describe, expect } from "vitest";

import { normalizeSubmitTxCanonicalCborToNative } from "../../src/commands/listen-utils.js";
import {
  AddressHistoryDB,
  BlocksDB,
  DepositsDB,
  LedgerUtils,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  TxUtils,
} from "../../src/database/index.js";
import {
  admissionBacklogGaugeFiber,
  commitAdmissionBacklogSlot,
  noteLocalAdmit,
  readAdmissionBacklogGauge,
  refreshAdmissionBacklogGauge,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../../src/fibers/admission-backlog-gauge.js";
import {
  requestTxQueueProcessorWakeup,
  withAdmissionLeaseRecovery,
} from "../../src/fibers/tx-queue-processor.js";
import {
  COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
  COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
  commitStageInputPostState,
  persistCommitStageRejectedTransactions,
  resolveTxDeltaForCommit,
} from "../../src/mpf/index.js";
import { NodeConfig } from "../../src/services/config.js";
import { AdmissionSql, BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  makeMempoolLedgerCacheService,
  MempoolLedgerCache,
} from "../../src/services/mempool-ledger-cache.js";
import {
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import { breakDownTx, ProcessedTx } from "../../src/utils.js";
import { selectCommitTxCandidates } from "../../src/workers/utils/commit-block-planner.js";
import { finalizeCommittedBlockLocally } from "../../src/workers/utils/commit-submission.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from ".././midgard-output-helpers.js";
import { insertDeposits } from "../helpers/event-rows.js";
import { writeFollowerTip } from "../helpers/follower-view.js";
import { openFollowerWriteGateAt } from "../helpers/follower-write-gate.js";
import { attachTestL1Access } from "../helpers/l1-tip.js";
import { testingInlineCekVerdicts } from "../helpers/validation-pool-cek.js";
import { databaseTestDirectory } from "./admissions.database-test-directory.js";
import {
  address1,
  address2,
  bundleChildProcessHelper,
  databaseChildProcessEnv,
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  expectSubmitBody,
  isolatedDb,
  ledgerEntry1,
  ledgerEntry2,
  makeDepositEntry,
  makeNativeSubmitTx,
  makeProofSubmitTx,
  makeReferenceScriptProofSubmitTx,
  makeScriptProofSubmitTx,
  retrieveAllMempool,
  submitThroughRouter,
  type TxQueueWakeRequirements,
  wrapNativeSubmitTx,
} from "./fixtures.js";

export const registerAdmissionsTests = () => {
  describe("TxAdmissionsDB", () => {
    it.effect(
      "keeps V1 transaction bytes durable and rejects unsupported media types",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const proofTx = makeProofSubmitTx();
            const proofEnvelope = encodeMidgardProofSubmission({
              transactionCbor: proofTx.txCanonicalCbor,
            });
            const submitProof = (body: Buffer, contentType: string) =>
              submitThroughRouter(body, Effect.void, {
                contentType,
                consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              }).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, nodeConfig),
              );

            const rawToProof = yield* submitProof(
              proofTx.txCanonicalCbor,
              "application/cbor",
            );
            expect(rawToProof.status).toBe(415);

            expect(yield* TxAdmissionsDB.getByTxId(proofTx.txId)).toBeNull();

            const accepted = yield* submitProof(
              proofEnvelope,
              "application/vnd.midgard.v1+cbor",
            );
            expectSubmitBody(accepted, {
              status: 202,
              txIdHex: proofTx.txIdHex,
              duplicate: false,
            });
            const stored = yield* TxAdmissionsDB.getByTxId(proofTx.txId);
            expect(stored?.tx_canonical_cbor).toEqual(proofTx.txCanonicalCbor);
            expect(stored?.tx_full_hash_v1).toEqual(
              computeMidgardNativeTxFullHashFromCanonicalCbor(
                proofTx.txCanonicalCbor,
              ),
            );
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "finalizes local mempool rows and rolls back on a block conflict",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const successful = makeScriptProofSubmitTx(74);
            const conflicting = makeScriptProofSubmitTx(75);
            for (const attempt of [successful, conflicting]) {
              yield* MempoolDB.insertMultipleCore([
                {
                  txId: attempt.txId,
                  txCbor: attempt.txCanonicalCbor,
                  spent: [],
                  produced: [],
                },
              ]);
            }
            const page = yield* MempoolDB.retrievePage({ limit: 10 });
            const entryById = new Map(
              page.entries.map((entry) => [
                entry[TxUtils.Columns.TX_ID].toString("hex"),
                entry,
              ]),
            );
            const resetCount = yield* Ref.make(0);
            const transactionsMpf = {
              resetToEmpty: () => Ref.update(resetCount, (count) => count + 1),
            } as unknown as Parameters<typeof finalizeCommittedBlockLocally>[0];
            yield* finalizeCommittedBlockLocally(
              transactionsMpf,
              [entryById.get(successful.txIdHex)!],
              [successful.txId],
              "31".repeat(28),
              [],
              { useAmbientProcessedMempool: false },
            );
            expect(yield* Ref.get(resetCount)).toBe(1);
            expect(
              yield* MempoolDB.retrieveTxCborByHash(successful.txId).pipe(
                Effect.option,
              ),
            ).toEqual(Option.none());

            yield* BlocksDB.insert(Buffer.from("41".repeat(28), "hex"), [
              conflicting.txId,
            ]);
            const failed = yield* Effect.either(
              finalizeCommittedBlockLocally(
                transactionsMpf,
                [entryById.get(conflicting.txIdHex)!],
                [conflicting.txId],
                "42".repeat(28),
                [],
                { useAmbientProcessedMempool: false },
              ),
            );
            expect(failed._tag).toBe("Left");
            expect(
              yield* MempoolDB.retrieveTxCborByHash(conflicting.txId),
            ).toEqual(conflicting.txCanonicalCbor);
            expect(yield* Ref.get(resetCount)).toBe(1);
          }),
        ),
    );

    it.effect(
      "atomically removes commit-stage rejected transactions and preserves them when rejection persistence fails",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const successful = makeScriptProofSubmitTx(76);
            const conflicting = makeScriptProofSubmitTx(77);
            for (const attempt of [successful, conflicting]) {
              yield* MempoolDB.insertMultipleCore([
                {
                  txId: attempt.txId,
                  txCbor: attempt.txCanonicalCbor,
                  spent: [],
                  produced: [],
                },
              ]);
            }
            const rejectionFor = (txId: Buffer, detail: string) => ({
              [TxRejectionsDB.Columns.TX_ID]: txId,
              [TxRejectionsDB.Columns.REJECT_CODE]:
                RejectCodes.PlutusEvaluationUnavailable,
              [TxRejectionsDB.Columns.REJECT_DETAIL]: detail,
            });
            yield* persistCommitStageRejectedTransactions({
              rejectionEntries: [
                rejectionFor(successful.txId, "commit-stage rejection"),
              ],
              resolveInputPostState: () => undefined,
            });
            expect(
              yield* MempoolDB.retrieveTxCborByHash(successful.txId).pipe(
                Effect.option,
              ),
            ).toEqual(Option.none());
            expect(
              (yield* TxRejectionsDB.retrieveByTxId(successful.txId)).map(
                (row) => [
                  row[TxRejectionsDB.Columns.REJECT_CODE],
                  row[TxRejectionsDB.Columns.REJECT_DETAIL],
                ],
              ),
            ).toEqual([
              [
                RejectCodes.PlutusEvaluationUnavailable,
                "commit-stage rejection",
              ],
            ]);

            yield* TxRejectionsDB.insert(
              rejectionFor(conflicting.txId, "preexisting conflict"),
            );
            const failed = yield* Effect.either(
              persistCommitStageRejectedTransactions({
                rejectionEntries: [
                  rejectionFor(conflicting.txId, "duplicate conflict"),
                ],
                resolveInputPostState: () => undefined,
              }),
            );
            expect(failed._tag).toBe("Left");
            expect(
              yield* MempoolDB.retrieveTxCborByHash(conflicting.txId),
            ).toEqual(conflicting.txCanonicalCbor);
            expect(
              (yield* TxRejectionsDB.retrieveByTxId(conflicting.txId)).map(
                (row) => row[TxRejectionsDB.Columns.REJECT_DETAIL],
              ),
            ).toEqual(["preexisting conflict"]);
          }),
        ),
    );

    it.effect(
      "reverts a commit-stage rejection's admitted ledger effects and rejects its pending descendants",
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
            const outRef = (e: LedgerUtils.Entry) =>
              e[LedgerUtils.Columns.OUTREF];
            const pendingTx = (
              byte: number,
              spent: readonly LedgerUtils.Entry[],
              produced: readonly LedgerUtils.Entry[],
            ): ProcessedTx => ({
              txId: Buffer.alloc(32, byte),
              txCbor: databaseFixtureBytes(`commit-revert.tx-${byte}`, 64),
              spent: spent.map(outRef),
              produced: [...produced],
            });
            // A deposit's ledger outref is its event id; the id is the
            // serialiseData form of that OutputReference.
            const projectedDeposit = (byte: number) =>
              makeDepositEntry({
                [DepositsDB.Columns.ID]: Buffer.concat([
                  Buffer.from("d8799f5820", "hex"),
                  Buffer.alloc(32, byte),
                  Buffer.from("00ff", "hex"),
                ]),
                [DepositsDB.Columns.LEDGER_OUTPUT]:
                  entry(byte)[LedgerUtils.Columns.OUTPUT],
                [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
              });
            // The rejected transaction spends one deposit; its pending child
            // spends the other beside the rejected output.
            const deposit = projectedDeposit(0x13);
            const childDeposit = projectedDeposit(0x15);
            yield* insertDeposits([deposit, childDeposit]);
            const depositSource =
              yield* DepositsDB.toMempoolLedgerEntry(deposit);
            const childDepositSource =
              yield* DepositsDB.toMempoolLedgerEntry(childDeposit);
            expect(outRef(depositSource)).toEqual(makeOutRefCbor(0x13, 0));
            expect(outRef(childDepositSource)).toEqual(makeOutRefCbor(0x15, 0));
            const [spentByBlock, unspent, unrelatedInput] = [
              0x11, 0x12, 0x14,
            ].map(entry) as [
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
            ];
            const [rejectedOut0, rejectedOut1, childOut, unrelatedOut] = [
              0x21, 0x22, 0x31, 0x41,
            ].map(entry) as [
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
            ];
            const committed = [
              spentByBlock,
              unspent,
              depositSource,
              unrelatedInput,
              childDepositSource,
            ];
            yield* MempoolLedgerDB.insert(committed);

            const rejected = pendingTx(
              0x02,
              [spentByBlock, unspent, depositSource],
              [rejectedOut0, rejectedOut1],
            );
            const child = pendingTx(
              0x03,
              [rejectedOut0, childDepositSource],
              [childOut],
            );
            const unrelated = pendingTx(0x04, [unrelatedInput], [unrelatedOut]);
            yield* MempoolDB.insertMultipleCore([rejected, unrelated]);
            yield* ProcessedMempoolDB.insertTx({
              [TxUtils.Columns.TX_ID]: child.txId,
              [TxUtils.Columns.TX]: child.txCbor,
            });
            yield* MempoolDB.applyLedgerEffectsCore([child]);
            yield* MempoolTxDeltasDB.upsertMany(
              [rejected, child, unrelated].map(({ txId, spent, produced }) => ({
                txId,
                spent,
                produced,
              })),
            );
            const depositStatus = (id: Buffer) =>
              DepositsDB.retrieveByEventId(id).pipe(
                Effect.map((row) =>
                  Option.map(row, (value) => value[DepositsDB.Columns.STATUS]),
                ),
              );
            // Admission consumed both deposits.
            expect(
              yield* depositStatus(deposit[DepositsDB.Columns.ID]),
            ).toEqual(Option.some(DepositsDB.Status.Consumed));
            expect(
              yield* depositStatus(childDeposit[DepositsDB.Columns.ID]),
            ).toEqual(Option.some(DepositsDB.Status.Consumed));

            const outcome = yield* persistCommitStageRejectedTransactions({
              rejectionEntries: [
                {
                  [TxRejectionsDB.Columns.TX_ID]: rejected.txId,
                  [TxRejectionsDB.Columns.REJECT_CODE]:
                    COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
                  [TxRejectionsDB.Columns.REJECT_DETAIL]: "withdrawn input",
                },
              ],
              // The block consumes `spentByBlock` and leaves every other
              // committed output unspent.
              resolveInputPostState: commitStageInputPostState({
                baseLedgerOutputs: new Map(
                  committed.map((e) => [
                    outRef(e).toString("hex"),
                    e[LedgerUtils.Columns.OUTPUT],
                  ]),
                ),
                insertedOutputs: new Map(),
                spentOutRefHexes: new Set([
                  outRef(spentByBlock).toString("hex"),
                ]),
              }),
            });

            expect(outcome).toMatchObject({
              _tag: "Persisted",
              ledgerChanged: true,
            });
            const ledger = (yield* MempoolLedgerDB.retrieve)
              .map((row) => ({
                [MempoolLedgerDB.Columns.TX_ID]:
                  row[MempoolLedgerDB.Columns.TX_ID],
                [MempoolLedgerDB.Columns.OUTREF]:
                  row[MempoolLedgerDB.Columns.OUTREF],
                [MempoolLedgerDB.Columns.OUTPUT]:
                  row[MempoolLedgerDB.Columns.OUTPUT],
                [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]:
                  row[MempoolLedgerDB.Columns.SOURCE_EVENT_ID],
              }))
              .sort((a, b) =>
                Buffer.compare(
                  a[MempoolLedgerDB.Columns.OUTREF],
                  b[MempoolLedgerDB.Columns.OUTREF],
                ),
              );
            const expectedRow = (
              e: LedgerUtils.Entry,
              sourceEventId: Buffer | null,
            ) => ({
              [MempoolLedgerDB.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
              [MempoolLedgerDB.Columns.OUTREF]: outRef(e),
              [MempoolLedgerDB.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
              [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: sourceEventId,
            });
            expect(ledger).toEqual([
              expectedRow(unspent, null),
              expectedRow(depositSource, deposit[DepositsDB.Columns.ID]),
              expectedRow(
                childDepositSource,
                childDeposit[DepositsDB.Columns.ID],
              ),
              expectedRow(unrelatedOut, null),
            ]);
            // Both restored deposit outputs are spendable again.
            expect(
              yield* depositStatus(deposit[DepositsDB.Columns.ID]),
            ).toEqual(Option.some(DepositsDB.Status.Projected));
            expect(
              yield* depositStatus(childDeposit[DepositsDB.Columns.ID]),
            ).toEqual(Option.some(DepositsDB.Status.Projected));
            expect(
              (yield* retrieveAllMempool).map((row) =>
                row[TxUtils.Columns.TX_ID].toString("hex"),
              ),
            ).toEqual([unrelated.txId.toString("hex")]);
            expect(yield* ProcessedMempoolDB.retrieve).toEqual([]);
            expect(
              (yield* TxRejectionsDB.retrieveByTxId(child.txId)).map((row) => [
                row[TxRejectionsDB.Columns.REJECT_CODE],
                row[TxRejectionsDB.Columns.REJECT_DETAIL],
              ]),
            ).toEqual([
              [
                COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
                `Transaction spends L2 outref ${outRef(rejectedOut0).toString(
                  "hex",
                )}, an output of transaction ${rejected.txId.toString(
                  "hex",
                )}, which was rejected at commit`,
              ],
            ]);
          }),
        ),
    );

    it.effect(
      "persists exact rejection journals for distinct admitted transactions",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const attempts = Array.from({ length: 8 }, (_, index) =>
              makeScriptProofSubmitTx(index + 100),
            );
            const submit = (proofEnvelope: Buffer) =>
              submitThroughRouter(proofEnvelope, Effect.void).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, nodeConfig),
              );
            for (const attempt of attempts) {
              const admitted = yield* submit(attempt.proofEnvelope);
              if (admitted.status !== 202) {
                throw new Error(
                  `script admission failed: ${JSON.stringify(admitted)}`,
                );
              }
            }

            const leaseOwner = "database-test:rejected-transactions";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: attempts.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(attempts.length);
            yield* TxAdmissionsDB.markRejected({
              rows: claimed,
              leaseOwner,
              rejectedTxs: claimed.map((row) => ({
                txId: row.tx_id,
                code: RejectCodes.PlutusEvaluationUnavailable,
                detail: "deliberate rejection journal regression",
              })),
            });

            for (const attempt of attempts) {
              const stored = yield* TxAdmissionsDB.getByTxId(attempt.txId);
              expect(stored?.status).toBe(TxAdmissionsDB.Status.Rejected);
              expect(stored?.tx_canonical_cbor).toEqual(
                attempt.txCanonicalCbor,
              );
              expect(
                (yield* TxRejectionsDB.retrieveByTxId(attempt.txId)).map(
                  (row) => [
                    row[TxRejectionsDB.Columns.REJECT_CODE],
                    row[TxRejectionsDB.Columns.REJECT_DETAIL],
                  ],
                ),
              ).toEqual([
                [
                  RejectCodes.PlutusEvaluationUnavailable,
                  "deliberate rejection journal regression",
                ],
              ]);
            }
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "atomically accepts canonical script transaction bytes and preserves exact duplicates",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const attempt = makeScriptProofSubmitTx(211);
            const fixtureNormalization = normalizeSubmitTxCanonicalCborToNative(
              attempt.txCanonicalCbor,
            );
            if (!fixtureNormalization.ok) {
              throw new Error(fixtureNormalization.detail);
            }
            const submit = () =>
              submitThroughRouter(attempt.proofEnvelope, Effect.void).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, nodeConfig),
              );
            const admitted = yield* submit();
            if (admitted.status !== 202) {
              throw new Error(
                `script admission failed: ${JSON.stringify(admitted)}`,
              );
            }

            const leaseOwner = "database-test:accepted-script-transaction";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(1);
            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: [
                {
                  txId: attempt.txId,
                  txCbor: attempt.txCanonicalCbor,
                  spent: [],
                  produced: [],
                },
              ],
            });

            const stored = yield* TxAdmissionsDB.getByTxId(attempt.txId);
            expect(stored?.status).toBe(TxAdmissionsDB.Status.Accepted);
            expect(stored?.tx_canonical_cbor).toEqual(attempt.txCanonicalCbor);
            expect(stored?.tx_full_hash_v1).toEqual(
              computeMidgardNativeTxFullHashFromCanonicalCbor(
                attempt.txCanonicalCbor,
              ),
            );
            expect(yield* MempoolDB.retrieveTxCborByHash(attempt.txId)).toEqual(
              attempt.txCanonicalCbor,
            );

            const duplicate = yield* submit();
            expect(duplicate.status).toBe(200);
            expect(duplicate.body).toMatchObject({
              txId: attempt.txIdHex,
              status: TxAdmissionsDB.Status.Accepted,
              duplicate: true,
            });
            expect(
              (yield* TxAdmissionsDB.getByTxId(attempt.txId))?.request_count,
            ).toBe(2n);
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "preserves validating status and exact payload bytes when acceptance fails",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const attempt = makeScriptProofSubmitTx(223);
            const admitted = yield* submitThroughRouter(
              attempt.proofEnvelope,
              Effect.void,
            ).pipe(
              Effect.provideService(SqlClient.SqlClient, admissionSql),
              Effect.provideService(NodeConfig, nodeConfig),
            );
            expect(admitted.status).toBe(202);

            const leaseOwner = "database-test:accepted-transaction-rollback";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(1);
            const sql = yield* SqlClient.SqlClient;
            yield* sql`INSERT INTO mempool (tx_id, tx)
            VALUES (${attempt.txId}, ${attempt.txCanonicalCbor})`;

            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: [
                  {
                    txId: attempt.txId,
                    txCbor: attempt.txCanonicalCbor,
                    spent: [],
                    produced: [],
                  },
                ],
              }),
            );
            expect(result._tag).toBe("Left");

            const stored = yield* TxAdmissionsDB.getByTxId(attempt.txId);
            expect(stored?.status).toBe(TxAdmissionsDB.Status.Validating);
            expect(stored?.tx_canonical_cbor).toEqual(attempt.txCanonicalCbor);
            expect(stored?.lease_owner).toBe(leaseOwner);
            expect(yield* MempoolDB.retrieveTxCborByHash(attempt.txId)).toEqual(
              attempt.txCanonicalCbor,
            );
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "preserves canonical reference-input transaction bytes on acceptance",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const attempt = makeReferenceScriptProofSubmitTx(227);
            const admitted = yield* submitThroughRouter(
              attempt.proofEnvelope,
              Effect.void,
            ).pipe(
              Effect.provideService(SqlClient.SqlClient, admissionSql),
              Effect.provideService(NodeConfig, nodeConfig),
            );
            if (admitted.status !== 202) {
              throw new Error(
                `reference-script admission failed: ${JSON.stringify(admitted)}`,
              );
            }

            const leaseOwner = "database-test:accepted-reference-script";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: [
                {
                  txId: attempt.txId,
                  txCbor: attempt.txCanonicalCbor,
                  spent: [],
                  produced: [],
                },
              ],
            });

            const stored = yield* TxAdmissionsDB.getByTxId(attempt.txId);
            expect(stored?.status).toBe(TxAdmissionsDB.Status.Accepted);
            expect(stored?.tx_canonical_cbor).toEqual(attempt.txCanonicalCbor);
            expect(yield* MempoolDB.retrieveTxCborByHash(attempt.txId)).toEqual(
              attempt.txCanonicalCbor,
            );
            expect(
              decodeMidgardNativeTxFullFromCanonicalCbor(
                stored!.tx_canonical_cbor,
              ).body.referenceInputsPreimageCbor,
            ).toEqual(encodeCbor([attempt.referenceOutRef]));
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "preserves exact submit-router HTTP parity for new, duplicate, conflict, and backlog-full requests",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const baseConfig = yield* NodeConfig;
            let testConfig = {
              ...baseConfig,
              MAX_DURABLE_ADMISSION_BACKLOG: 2,
              SUBMIT_INGRESS_MAX_CONCURRENCY: 32,
            };
            const submit = (txCanonicalCbor: Buffer) =>
              submitThroughRouter(
                wrapNativeSubmitTx(txCanonicalCbor),
                Effect.void,
              ).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, testConfig),
              );

            const firstTx = makeNativeSubmitTx();
            const first = yield* submit(firstTx.txCanonicalCbor);
            expectSubmitBody(first, {
              status: 202,
              txIdHex: firstTx.txIdHex,
              duplicate: false,
            });
            const duplicate = yield* submit(firstTx.txCanonicalCbor);
            expectSubmitBody(duplicate, {
              status: 200,
              txIdHex: firstTx.txIdHex,
              duplicate: true,
            });
            expect(duplicate.body.firstSeenAt).toBe(first.body.firstSeenAt);
            const stored = yield* TxAdmissionsDB.getByTxId(firstTx.txId);
            expect(stored?.request_count).toBe(2n);

            const conflictTx = makeNativeSubmitTx();
            yield* TxAdmissionsDB.admit({
              txId: conflictTx.txId,
              txCanonicalCbor: Buffer.concat([
                conflictTx.txCanonicalCbor,
                Buffer.from([0]),
              ]),
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const conflict = yield* submit(conflictTx.txCanonicalCbor);
            expect(conflict.status).toBe(409);
            expect(conflict.body).toEqual({
              error: "E_TX_ID_BYTES_CONFLICT",
              message: expect.stringContaining(conflictTx.txIdHex),
              txId: conflictTx.txIdHex,
            });

            const globals = yield* Globals;
            yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
              ADMISSION_BACKLOG_BASE: 2n,
              ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
              ADMISSION_BACKLOG_IN_FLIGHT: 0n,
              ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
            });
            const duplicateWhileFull = yield* submit(firstTx.txCanonicalCbor);
            expectSubmitBody(duplicateWhileFull, {
              status: 200,
              txIdHex: firstTx.txIdHex,
              duplicate: true,
            });
            const newWhileFullTx = makeNativeSubmitTx();
            const newWhileFull = yield* submit(newWhileFullTx.txCanonicalCbor);
            expect(newWhileFull).toEqual({
              status: 503,
              body: {
                error: "Durable submission admission backlog is full",
                backlog: "2",
                maxBacklog: "2",
              },
              retryAfter: "5",
            });

            const identicalTx = makeNativeSubmitTx();
            testConfig = {
              ...testConfig,
              MAX_DURABLE_ADMISSION_BACKLOG: 100,
            };
            yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
              ADMISSION_BACKLOG_BASE: 0n,
              ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
              ADMISSION_BACKLOG_IN_FLIGHT: 0n,
              ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
            });
            const identical = yield* Effect.all(
              Array.from({ length: 12 }, () =>
                submit(identicalTx.txCanonicalCbor),
              ),
              { concurrency: "unbounded" },
            );
            expect(
              identical.filter((result) => result.status === 202),
            ).toHaveLength(1);
            expect(
              identical.filter((result) => result.status === 200),
            ).toHaveLength(11);
            expect(
              identical.every(
                (result) =>
                  result.body.txId === identicalTx.txIdHex &&
                  result.body.status === TxAdmissionsDB.Status.Queued,
              ),
            ).toBe(true);
            expect(
              (yield* TxAdmissionsDB.getByTxId(identicalTx.txId))
                ?.request_count,
            ).toBe(12n);
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "atomically caps aggregate pending transaction bytes across parallel submissions",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const admissionSql = yield* AdmissionSql;
            const baseConfig = yield* NodeConfig;
            const attempts = Array.from({ length: 32 }, () =>
              makeNativeSubmitTx(),
            );
            const transactionBytes = attempts[0]!.txCanonicalCbor.length;
            expect(
              attempts.every(
                (attempt) =>
                  attempt.txCanonicalCbor.length === transactionBytes,
              ),
            ).toBe(true);
            const maxBacklogBytes = transactionBytes * 4;
            const testConfig = {
              ...baseConfig,
              MAX_DURABLE_ADMISSION_BACKLOG: 100,
              MAX_DURABLE_ADMISSION_BACKLOG_BYTES: maxBacklogBytes,
              SUBMIT_INGRESS_MAX_CONCURRENCY: 64,
            };
            const submit = (txCanonicalCbor: Buffer) =>
              submitThroughRouter(
                wrapNativeSubmitTx(txCanonicalCbor),
                Effect.void,
              ).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, testConfig),
              );
            const results = yield* Effect.all(
              attempts.map((attempt) => submit(attempt.txCanonicalCbor)),
              { concurrency: "unbounded" },
            );
            const accepted = results
              .map((result, index) => ({ result, index }))
              .filter(({ result }) => result.status === 202);
            const denied = results.filter((result) => result.status === 503);
            expect(accepted).toHaveLength(4);
            expect(denied).toHaveLength(28);
            expect(
              denied.every(
                ({ body }) =>
                  body.error ===
                    "Durable submission admission byte backlog is full" &&
                  body.maxBacklogBytes === maxBacklogBytes.toString(),
              ),
            ).toBe(true);

            const sql = yield* SqlClient.SqlClient;
            const pending = yield* sql<{
              readonly bytes: string;
              readonly count: string;
            }>`SELECT
              COALESCE(
                SUM(octet_length(payload.tx_canonical_cbor)),
                0
              )::text AS bytes,
              COUNT(*)::text AS count
            FROM tx_admissions admission
            INNER JOIN tx_admission_payloads payload
              ON payload.tx_id = admission.tx_id
            WHERE admission.status IN ('queued', 'validating')`;
            expect(pending[0]).toEqual({
              bytes: maxBacklogBytes.toString(),
              count: "4",
            });

            const duplicate = yield* submit(
              attempts[accepted[0]!.index]!.txCanonicalCbor,
            );
            expect(duplicate.status).toBe(200);
            expect(duplicate.body.duplicate).toBe(true);
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "holds a stale refresh, bounds parallel distinct HTTP admits, then recovers after one refresh",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const originalGlobals = yield* Globals;
            const backlogGauge = yield* SubscriptionRef.make(
              yield* Ref.get(originalGlobals.ADMISSION_BACKLOG_GAUGE),
            );
            const globals = Globals.make({
              ...originalGlobals,
              ADMISSION_BACKLOG_GAUGE: backlogGauge,
            });
            const admissionSql = yield* AdmissionSql;
            const baseConfig = yield* NodeConfig;
            const testConfig = {
              ...baseConfig,
              MAX_DURABLE_ADMISSION_BACKLOG: 5,
              ADMISSION_BACKLOG_REFRESH_MS: 10,
              SUBMIT_INGRESS_MAX_CONCURRENCY: 16,
            };
            const submit = (txCanonicalCbor: Buffer) =>
              submitThroughRouter(
                wrapNativeSubmitTx(txCanonicalCbor),
                Effect.void,
              ).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, testConfig),
                Effect.provideService(Globals, globals),
              );
            const maxBacklog = 5;
            for (let index = 0; index < 3; index += 1) {
              yield* TxAdmissionsDB.admit({
                txId: databaseTxHash(`admission.http-frozen-seed-${index}`),
                txCanonicalCbor: databaseFixtureBytes(
                  `admission.http-frozen-seed-${index}`,
                  64,
                ),
                submitSource: "native",
                currentBacklog: BigInt(index),
                maxBacklog,
              });
            }
            yield* refreshAdmissionBacklogGauge.pipe(
              Effect.provideService(Globals, globals),
            );
            expect(
              yield* readAdmissionBacklogGauge.pipe(
                Effect.provideService(Globals, globals),
              ),
            ).toBe(3n);

            const unfreeze = yield* Deferred.make<void>();
            const refreshFiber = yield* Effect.fork(
              admissionBacklogGaugeFiber(
                Schedule.spaced(Duration.millis(10)),
                Deferred.await(unfreeze),
              ).pipe(
                Effect.provideService(NodeConfig, testConfig),
                Effect.provideService(Globals, globals),
              ),
            );
            yield* Effect.gen(function* () {
              const attempts = Array.from({ length: 10 }, () =>
                makeNativeSubmitTx(),
              );
              const results = yield* Effect.all(
                attempts.map((tx) => submit(tx.txCanonicalCbor)),
                { concurrency: "unbounded" },
              ).pipe(
                Effect.timeoutFail({
                  duration: Duration.seconds(10),
                  onTimeout: () =>
                    new Error(
                      "Parallel stale-gauge HTTP submissions exceeded 10 seconds",
                    ),
                }),
              );
              expect(
                results.filter((result) => result.status === 202),
              ).toHaveLength(2);
              const rejected = results.filter(
                (result) => result.status === 503,
              );
              expect(rejected).toHaveLength(8);
              expect(
                rejected.every(
                  (result) =>
                    result.body.backlog === "5" &&
                    result.body.maxBacklog === "5",
                ),
              ).toBe(true);
              expect(yield* TxAdmissionsDB.countBacklog).toBe(5n);

              const sql = yield* SqlClient.SqlClient;
              // Preserve admission's monotone timestamps when the fixture drains
              // the backlog through a different database connection.
              yield* sql`UPDATE ${sql(TxAdmissionsDB.tableName)}
              SET status = 'accepted',
                  terminal_at = GREATEST(NOW(), first_seen_at, last_seen_at, updated_at),
                  updated_at = GREATEST(NOW(), first_seen_at, last_seen_at, updated_at)
              WHERE status IN ('queued', 'validating')`;
              yield* Deferred.succeed(unfreeze, undefined);
              // Observe the refresh's real PostgreSQL completion, not elapsed
              // virtual time: TestClock cannot make the COUNT query finish.
              yield* backlogGauge.changes.pipe(
                Stream.filter(
                  (state) =>
                    state.ADMISSION_BACKLOG_BASE === 0n &&
                    state.ADMISSION_BACKLOG_LOCAL_DELTA === 0n &&
                    state.ADMISSION_BACKLOG_IN_FLIGHT === 0n,
                ),
                Stream.runHead,
              );
              expect(
                yield* readAdmissionBacklogGauge.pipe(
                  Effect.provideService(Globals, globals),
                ),
              ).toBe(0n);
              const recoveryTx = makeNativeSubmitTx();
              expect((yield* submit(recoveryTx.txCanonicalCbor)).status).toBe(
                202,
              );
            }).pipe(
              Effect.ensuring(
                Deferred.succeed(unfreeze, undefined).pipe(
                  Effect.ignore,
                  Effect.zipRight(Fiber.interruptFork(refreshFiber)),
                ),
              ),
            );
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect(
      "keeps submit latency isolated while the batch pool is held and labels the resumed drain",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const batchSql = yield* BatchSql;
            const admissionSql = yield* AdmissionSql;
            const nodeConfig = yield* NodeConfig;
            const ingressTestConfig = {
              ...nodeConfig,
              SUBMIT_INGRESS_MAX_CONCURRENCY: 64,
            };
            const processorActivity = yield* SubscriptionRef.make(0);
            const globals = Globals.make({
              ...(yield* Globals),
              TX_QUEUE_PROCESSOR_ACTIVE: processorActivity,
            });
            const validationStarted = yield* Deferred.make<void>();
            const finishValidation = yield* Deferred.make<void>();
            let validationRuns = 0;
            const cache = yield* makeMempoolLedgerCacheService(
              globals,
              MempoolLedgerDB.retrieveSpendable.pipe(
                Effect.provideService(SqlClient.SqlClient, batchSql),
              ),
            );
            const validationPool: ValidationPoolService = {
              ...testingInlineCekVerdicts,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              poolSize: 1,
              ready: Effect.void,
              stats: Effect.succeed({
                busyWorkers: 0,
                queueDepth: 0,
                oldestInFlightAgeMs: 0,
                liveWorkers: 1,
                restartingWorkers: 0,
              }),
              runPhaseAChunk: (txs) =>
                Effect.sync(() => {
                  validationRuns += 1;
                }).pipe(
                  Effect.zipRight(
                    Deferred.succeed(validationStarted, undefined),
                  ),
                  Effect.zipRight(Deferred.await(finishValidation)),
                  Effect.as({
                    accepted: [],
                    rejected: txs.map((tx) => ({
                      txId: tx.txId,
                      code: RejectCodes.InvalidSignature,
                      detail: "phase1 pool-isolation hold",
                    })),
                  }),
                ),
            };
            const lucid = {
              api: {
                currentSlot: () => 0,
                // The processor builds script contexts in this slot mapping.
                config: () => ({
                  slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
                }),
              },
            } as unknown as Lucid;
            attachTestL1Access(lucid.api, 0); // the processor reads l1SlotNow
            const submit = (
              txCanonicalCbor: Buffer,
              wake: Effect.Effect<void, never, TxQueueWakeRequirements>,
            ) =>
              submitThroughRouter(
                wrapNativeSubmitTx(txCanonicalCbor),
                wake,
              ).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
                Effect.provideService(NodeConfig, ingressTestConfig),
                Effect.provideService(Globals, globals),
                Effect.provideService(ValidationPool, validationPool),
                Effect.provideService(MempoolLedgerCache, cache),
                Effect.provideService(Lucid, lucid),
              );
            const percentile99 = (samples: readonly number[]): number =>
              [...samples].sort((left, right) => left - right)[
                Math.max(0, Math.ceil(samples.length * 0.99) - 1)
              ] ?? 0;
            const measure = (
              count: number,
              wake: Effect.Effect<void, never, TxQueueWakeRequirements>,
            ) =>
              Effect.all(
                Array.from({ length: count }, () =>
                  Effect.gen(function* () {
                    const tx = makeNativeSubmitTx();
                    const startedAt = performance.now();
                    const response = yield* submit(tx.txCanonicalCbor, wake);
                    expect(response.status).toBe(202);
                    return performance.now() - startedAt;
                  }),
                ),
                { concurrency: "unbounded" },
              );

            // The driver applied a view: the drain holds a follower permit.
            const gate = yield* Effect.flatMap(
              writeFollowerTip(10),
              openFollowerWriteGateAt,
            ).pipe(Effect.provideService(SqlClient.SqlClient, batchSql));
            yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
              ...local,
              epoch: gate.epoch,
            }));
            yield* measure(24, Effect.void);
            const releases = yield* Effect.forEach(
              Array.from({ length: nodeConfig.POSTGRES_BATCH_POOL_SIZE }),
              () => Deferred.make<void>(),
            );
            const acquired = yield* Effect.forEach(releases, () =>
              Deferred.make<void, unknown>(),
            );
            // A holder that fails before acquiring (for example a batch
            // connection that exceeds connect_timeout) fails its `acquired`
            // signal, so the wait below surfaces the failure instead of
            // blocking until the test timeout.
            const holders = yield* Effect.forEach(releases, (release, index) =>
              Effect.fork(
                batchSql
                  .withTransaction(
                    Effect.gen(function* () {
                      yield* batchSql`SELECT 1`;
                      yield* Deferred.succeed(acquired[index]!, undefined);
                      yield* Deferred.await(release);
                    }),
                  )
                  .pipe(
                    Effect.tapErrorCause((cause) =>
                      Deferred.failCause(acquired[index]!, cause),
                    ),
                  ),
              ),
            );
            const releaseHolders = Effect.forEach(releases, (release) =>
              Deferred.succeed(release, undefined),
            ).pipe(
              Effect.zipRight(Effect.forEach(holders, Fiber.interrupt)),
              Effect.zipRight(Deferred.succeed(finishValidation, undefined)),
              Effect.asVoid,
            );
            yield* Effect.gen(function* () {
              yield* Effect.forEach(acquired, Deferred.await);

              // All batch connections remain held until after admission completes,
              // so admission routed through that pool would block here. Only the
              // absolute bound is asserted: a ratio against the unsaturated p99
              // compared two 24-sample p99s and flaked under host load.
              const saturatedP99 = percentile99(
                yield* measure(24, requestTxQueueProcessorWakeup),
              );
              expect(saturatedP99).toBeLessThanOrEqual(1_000);

              const activity = yield* admissionSql<{
                readonly application_name: string;
                readonly state: string;
                readonly count: number;
              }>`SELECT application_name, state, COUNT(*)::int AS count
            FROM pg_stat_activity
            WHERE datname = current_database()
              AND application_name IN ('midgard-node-admission', 'midgard-node-batch')
            GROUP BY application_name, state`;
              expect(
                activity.some(
                  (row) =>
                    row.application_name === "midgard-node-admission" &&
                    row.count >= 1,
                ),
              ).toBe(true);
              expect(
                activity
                  .filter(
                    (row) => row.application_name === "midgard-node-batch",
                  )
                  .reduce((sum, row) => sum + row.count, 0),
              ).toBe(nodeConfig.POSTGRES_BATCH_POOL_SIZE);
              yield* TestClock.adjust(Duration.millis(300));
              expect(validationRuns).toBe(0);
              expect(
                yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE),
              ).toBeGreaterThan(0);

              yield* Deferred.succeed(releases[0]!, undefined);
              // Pool release and PostgreSQL queries complete on real I/O time.
              // The validation stub signals that the production drain resumed.
              yield* Deferred.await(validationStarted);
              expect(validationRuns).toBeGreaterThan(0);
              const batchBackends = yield* admissionSql<{
                readonly application_name: string;
                readonly count: number;
              }>`SELECT application_name, COUNT(*)::int AS count
            FROM pg_stat_activity
            WHERE datname = current_database()
              AND application_name = 'midgard-node-batch'
            GROUP BY application_name`;
              expect(batchBackends[0]?.count).toBe(
                nodeConfig.POSTGRES_BATCH_POOL_SIZE,
              );

              yield* Effect.forEach(releases.slice(1), (release) =>
                Deferred.succeed(release, undefined),
              );
              yield* Effect.forEach(holders, Fiber.join);
              yield* Deferred.succeed(finishValidation, undefined);
              yield* processorActivity.changes.pipe(
                Stream.filter((active) => active === 0),
                Stream.runHead,
              );
              expect(yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE)).toBe(0);
            }).pipe(Effect.ensuring(releaseHolders));
          }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
        ),
    );

    it.effect("round-trips ordered bytea arrays without changing bytes", () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const pg = sql as PgClient;
          const duplicate = Buffer.from([
            0x00, 0xff, 0x80, 0x7f, 0x5c, 0x27, 0x00,
          ]);
          const expected = [
            Buffer.alloc(32, 0x00),
            duplicate,
            Buffer.alloc(32, 0xff),
            Buffer.from(duplicate),
          ];
          const rows = yield* sql<{
            readonly value: Buffer;
            readonly ordinal: number;
          }>`SELECT value, ordinality::int AS ordinal
          FROM unnest(${pg.array(
            expected.map((value) => `\\x${value.toString("hex")}`),
          )}::bytea[])
            WITH ORDINALITY AS input_bytes(value, ordinality)
          ORDER BY ordinality`;

          expect(rows.map((row) => row.ordinal)).toStrictEqual([1, 2, 3, 4]);
          expect(rows.map((row) => row.value)).toStrictEqual(expected);
        }),
      ),
    );

    it.effect(
      "accepts a 2048-row compact batch through bytea-array predicates",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const rowCount = 2_048;
            const sql = yield* SqlClient.SqlClient;
            const validTxCanonicalCbor = makeNativeSubmitTx().txCanonicalCbor;
            const txs = Array.from({ length: rowCount }, (_, index) => {
              const label = `admission.bulk-array-${index.toString()}`;
              const txId = databaseTxHash(label);
              const txCanonicalCbor = validTxCanonicalCbor;
              const source = {
                [LedgerUtils.Columns.TX_ID]: databaseTxHash(`${label}.source`),
                [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                  `${label}.source`,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  `${label}.source-output`,
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: address1,
              } satisfies LedgerUtils.Entry;
              const produced = {
                [LedgerUtils.Columns.TX_ID]: txId,
                [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                  `${label}.produced`,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  `${label}.produced-output`,
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: address1,
              } satisfies LedgerUtils.Entry;
              return { txId, txCanonicalCbor, source, produced };
            });
            yield* sql`INSERT INTO ${sql(TxAdmissionsDB.tableName)} ${sql.insert(
              txs.map(({ txId }) => ({
                tx_id: txId,
                status: TxAdmissionsDB.Status.Queued,
                submit_source: "native",
              })),
            )}`;
            yield* sql`INSERT INTO ${sql(
              TxAdmissionsDB.payloadTableName,
            )} ${sql.insert(
              txs.map(({ txId, txCanonicalCbor }) => ({
                tx_id: txId,
                tx_canonical_cbor: txCanonicalCbor,
                tx_full_hash_v1:
                  computeMidgardNativeTxFullHashFromCanonicalCbor(
                    txCanonicalCbor,
                  ),
              })),
            )}`;
            yield* MempoolLedgerDB.insert(txs.map(({ source }) => source));

            const leaseOwner = "database-test:bulk-array-accept";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: rowCount,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(rowCount);
            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: txs.map(
                ({ txId, txCanonicalCbor, source, produced }) => ({
                  txId,
                  txCbor: txCanonicalCbor,
                  spent: [source[LedgerUtils.Columns.OUTREF]],
                  produced: [produced],
                }),
              ),
            });

            const accepted = yield* sql<{
              readonly count: number;
            }>`SELECT COUNT(*)::int AS count
            FROM ${sql(TxAdmissionsDB.tableName)}
            WHERE status = ${TxAdmissionsDB.Status.Accepted}`;
            expect(accepted[0]?.count).toBe(rowCount);
            expect(yield* MempoolDB.retrieveTxCount).toBe(BigInt(rowCount));
            const inlinePayloads = yield* sql<{
              readonly tx_id: Buffer;
              readonly tx: Buffer;
            }>`SELECT tx_id, tx FROM mempool`;
            expect(
              new Map(
                inlinePayloads.map((entry) => [
                  entry.tx_id.toString("hex"),
                  entry.tx.toString("hex"),
                ]),
              ),
            ).toEqual(
              new Map(
                txs.map((entry) => [
                  entry.txId.toString("hex"),
                  entry.txCanonicalCbor.toString("hex"),
                ]),
              ),
            );
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(ledger).toHaveLength(rowCount);
            expect(
              new Set(ledger.map((entry) => entry.outref.toString("hex"))),
            ).toEqual(
              new Set(
                txs.map(({ produced }) =>
                  produced[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ),
            );
            expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(
              rowCount * 2,
            );
          }),
        ),
    );

    it.effect(
      "preserves binary produced columns and explicit or default timestamps",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const binary = (length: number, seed: number): Buffer => {
              const value = Buffer.from(
                Array.from(
                  { length },
                  (_, index) => (seed + index * 131) & 0xff,
                ),
              );
              value[0] = 0x00;
              value[1] = 0xff;
              value[2] = 0x80;
              return value;
            };
            const validTxCanonicalCbor = makeNativeSubmitTx().txCanonicalCbor;
            const txs = [0, 1].map((index) => {
              const txId = binary(32, 17 + index);
              const txCanonicalCbor = validTxCanonicalCbor;
              const source = {
                [LedgerUtils.Columns.TX_ID]: binary(32, 101 + index),
                [LedgerUtils.Columns.OUTREF]: binary(36, 131 + index),
                [LedgerUtils.Columns.OUTPUT]: binary(80, 151 + index),
                [LedgerUtils.Columns.ADDRESS]: address1,
              } satisfies LedgerUtils.Entry;
              return { txId, txCanonicalCbor, source };
            });
            yield* Effect.all(
              txs.map(({ txId, txCanonicalCbor }) =>
                TxAdmissionsDB.admit({
                  txId,
                  txCanonicalCbor,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded", discard: true },
            );
            yield* MempoolLedgerDB.insert(txs.map(({ source }) => source));
            const leaseOwner = "database-test:produced-array-binary";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: txs.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const explicitTimestamp = new Date("2026-07-10T11:22:33.456Z");
            const produced = [
              {
                [LedgerUtils.Columns.TX_ID]: txs[0]!.txId,
                [LedgerUtils.Columns.OUTREF]: binary(36, 191),
                [LedgerUtils.Columns.OUTPUT]: binary(96, 211),
                [LedgerUtils.Columns.ADDRESS]: address1,
                [LedgerUtils.Columns.TIMESTAMPTZ]: explicitTimestamp,
              },
              {
                [LedgerUtils.Columns.TX_ID]: txs[1]!.txId,
                [LedgerUtils.Columns.OUTREF]: binary(36, 231),
                [LedgerUtils.Columns.OUTPUT]: binary(96, 251),
                [LedgerUtils.Columns.ADDRESS]: address2,
              },
            ] satisfies readonly LedgerUtils.Entry[];
            const sql = yield* SqlClient.SqlClient;
            const before = yield* sql<{
              readonly now: Date;
            }>`SELECT clock_timestamp() AS now`;
            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: txs.map((tx, index) => ({
                txId: tx.txId,
                txCbor: tx.txCanonicalCbor,
                spent: [tx.source[LedgerUtils.Columns.OUTREF]],
                produced: [produced[index]!],
              })),
            });
            const after = yield* sql<{
              readonly now: Date;
            }>`SELECT clock_timestamp() AS now`;

            const persisted = yield* MempoolLedgerDB.retrieveByTxOutRefs(
              produced.map((entry) => entry[LedgerUtils.Columns.OUTREF]),
            );
            expect(persisted).toHaveLength(2);
            const byOutref = new Map(
              persisted.map((entry) => [
                entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
                entry,
              ]),
            );
            for (const entry of produced) {
              const actual = byOutref.get(
                entry[LedgerUtils.Columns.OUTREF].toString("hex"),
              );
              expect(actual?.[MempoolLedgerDB.Columns.TX_ID]).toEqual(
                entry[LedgerUtils.Columns.TX_ID],
              );
              expect(actual?.[MempoolLedgerDB.Columns.OUTPUT]).toEqual(
                entry[LedgerUtils.Columns.OUTPUT],
              );
              expect(actual?.[MempoolLedgerDB.Columns.ADDRESS]).toBe(
                entry[LedgerUtils.Columns.ADDRESS],
              );
            }
            expect(
              byOutref
                .get(produced[0]![LedgerUtils.Columns.OUTREF].toString("hex"))
                ?.[MempoolLedgerDB.Columns.TIMESTAMPTZ].getTime(),
            ).toBe(explicitTimestamp.getTime());
            const defaultTimestamp = byOutref.get(
              produced[1]![LedgerUtils.Columns.OUTREF].toString("hex"),
            )?.[MempoolLedgerDB.Columns.TIMESTAMPTZ];
            expect(defaultTimestamp?.getTime()).toBeGreaterThanOrEqual(
              before[0]!.now.getTime(),
            );
            expect(defaultTimestamp?.getTime()).toBeLessThanOrEqual(
              after[0]!.now.getTime(),
            );
          }),
        ),
    );

    it.effect(
      "rolls back duplicate produced outrefs under strict uniqueness",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const inputs = [0, 1].map((index) => ({
              txId: databaseTxHash(
                `admission.produced-array-conflict-${index.toString()}`,
              ),
              txCanonicalCbor: databaseFixtureBytes(
                `admission.produced-array-conflict-${index.toString()}`,
                64,
              ),
              source: {
                ...ledgerEntry1,
                [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                  `admission.produced-array-conflict-source-${index.toString()}`,
                ),
              } satisfies LedgerUtils.Entry,
            }));
            yield* Effect.all(
              inputs.map(({ txId, txCanonicalCbor }) =>
                TxAdmissionsDB.admit({
                  txId,
                  txCanonicalCbor,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded", discard: true },
            );
            yield* MempoolLedgerDB.insert(inputs.map(({ source }) => source));
            const leaseOwner = "database-test:produced-array-conflict";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: inputs.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const duplicateOutref = Buffer.concat([
              Buffer.from([0x00, 0xff, 0x80, 0x00]),
              databaseFixtureBytes("admission.produced-array-conflict", 32),
            ]);
            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: inputs.map((input, index) => ({
                  txId: input.txId,
                  txCbor: input.txCanonicalCbor,
                  spent: [input.source[LedgerUtils.Columns.OUTREF]],
                  produced: [
                    {
                      [LedgerUtils.Columns.TX_ID]: input.txId,
                      [LedgerUtils.Columns.OUTREF]: duplicateOutref,
                      [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                        `admission.produced-array-conflict-output-${index.toString()}`,
                        80,
                      ),
                      [LedgerUtils.Columns.ADDRESS]: address1,
                    },
                  ],
                })),
              }),
            );

            expect(result._tag).toBe("Left");
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(
              new Set(ledger.map((entry) => entry.outref.toString("hex"))),
            ).toEqual(
              new Set(
                inputs.map(({ source }) =>
                  source[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ),
            );
            const admissions = yield* Effect.forEach(inputs, ({ txId }) =>
              TxAdmissionsDB.getByTxId(txId),
            );
            expect(
              admissions.every(
                (entry) => entry?.status === TxAdmissionsDB.Status.Validating,
              ),
            ).toBe(true);
            expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
          }),
        ),
    );

    it.effect(
      "rolls back the complete accept transaction on a mempool membership conflict",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("admission.membership-conflict");
            const txCanonicalCbor = databaseFixtureBytes(
              "admission.membership-conflict-cbor",
              64,
            );
            const source = {
              ...ledgerEntry1,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.membership-conflict-source",
              ),
            } satisfies LedgerUtils.Entry;
            const produced = {
              ...ledgerEntry2,
              [LedgerUtils.Columns.TX_ID]: txId,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.membership-conflict-produced",
              ),
            } satisfies LedgerUtils.Entry;
            yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            yield* MempoolLedgerDB.insert([source]);
            const leaseOwner = "database-test:membership-conflict";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${txId}, ${txCanonicalCbor})`;

            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: [
                  {
                    txId,
                    txCbor: txCanonicalCbor,
                    spent: [source[LedgerUtils.Columns.OUTREF]],
                    produced: [produced],
                  },
                ],
              }),
            );

            expect(result._tag).toBe("Left");
            expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
              TxAdmissionsDB.Status.Validating,
            );
            expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(
              ledger.map((entry) => entry.outref.toString("hex")),
            ).toStrictEqual([
              source[LedgerUtils.Columns.OUTREF].toString("hex"),
            ]);
            expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
          }),
        ),
    );

    it.effect(
      "preserves new, duplicate, conflict, and backlog-full admission semantics",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("admission.state-machine");
            const txCbor = databaseFixtureBytes("admission.state-machine", 64);
            const first = yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor: txCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            expect(first.kind).toBe("new");
            expect(first.entry.request_count).toBe(1n);

            const duplicate = yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor: txCbor,
              submitSource: "native",
              currentBacklog: 10n,
              maxBacklog: 10,
            });
            expect(duplicate.kind).toBe("duplicate");
            expect(duplicate.entry.request_count).toBe(2n);
            expect(duplicate.entry.first_seen_at.getTime()).toBe(
              first.entry.first_seen_at.getTime(),
            );

            const conflict = yield* Effect.either(
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.state-machine-conflict",
                  64,
                ),
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            );
            expect(conflict._tag).toBe("Left");
            if (conflict._tag === "Left") {
              expect(conflict.left._tag).toBe("TxAdmissionConflictError");
            }

            const backlogFull = yield* Effect.either(
              TxAdmissionsDB.admit({
                txId: databaseTxHash("admission.backlog-full"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.backlog-full",
                  64,
                ),
                submitSource: "native",
                currentBacklog: 10n,
                maxBacklog: 10,
              }),
            );
            expect(backlogFull._tag).toBe("Left");
            if (backlogFull._tag === "Left") {
              expect(backlogFull.left._tag).toBe("TxAdmissionBacklogFullError");
              if (backlogFull.left._tag === "TxAdmissionBacklogFullError") {
                expect(backlogFull.left.backlog).toBe(10n);
              }
            }
          }),
        ),
    );

    it.effect("arbitrates parallel identical admissions exactly once", () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.concurrent");
          const txCanonicalCbor = databaseFixtureBytes(
            "admission.concurrent",
            64,
          );
          const results = yield* Effect.all(
            Array.from({ length: 12 }, () =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 100,
              }),
            ),
            { concurrency: "unbounded" },
          );
          expect(
            results.filter((result) => result.kind === "new"),
          ).toHaveLength(1);
          expect(
            results.filter((result) => result.kind === "duplicate"),
          ).toHaveLength(11);
          const row = yield* TxAdmissionsDB.getByTxId(txId);
          expect(row?.request_count).toBe(12n);
        }),
      ),
    );

    it.effect(
      "resolves an identical reserved-batch insert that waited on a concurrent commit as a duplicate",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const firstInserted = yield* Deferred.make<number>();
            const secondStarted = yield* Deferred.make<number>();
            const releaseFirst = yield* Deferred.make<void>();
            const request = {
              txId: databaseTxHash("admission.reserved-concurrent"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.reserved-concurrent",
                64,
              ),
              submitSource: "native" as const,
            };
            const firstFiber = yield* Effect.fork(
              sql.withTransaction(
                Effect.gen(function* () {
                  const [backend] = yield* sql<{
                    readonly pid: number;
                  }>`SELECT pg_backend_pid()::int AS pid`;
                  const outcomes = yield* TxAdmissionsDB.admitReservedBatch([
                    request,
                  ]);
                  yield* Deferred.succeed(firstInserted, backend!.pid);
                  yield* Deferred.await(releaseFirst);
                  return outcomes;
                }),
              ),
            );
            const firstPid = yield* Deferred.await(firstInserted);
            const secondFiber = yield* Effect.fork(
              sql.withTransaction(
                Effect.gen(function* () {
                  const [backend] = yield* sql<{
                    readonly pid: number;
                  }>`SELECT pg_backend_pid()::int AS pid`;
                  yield* Deferred.succeed(secondStarted, backend!.pid);
                  return yield* TxAdmissionsDB.admitReservedBatch([request]);
                }),
              ),
            );
            const secondPid = yield* Deferred.await(secondStarted);
            let observedBlockedInsert = false;
            for (let attempt = 0; attempt < 100; attempt += 1) {
              const [blocking] = yield* sql<{
                readonly blocked: boolean;
              }>`SELECT ${firstPid} = ANY(pg_blocking_pids(${secondPid})) AS blocked`;
              if (blocking?.blocked === true) {
                observedBlockedInsert = true;
                break;
              }
              yield* Effect.promise(
                () => new Promise<void>((resolve) => setTimeout(resolve, 10)),
              );
            }
            expect(observedBlockedInsert).toBe(true);
            yield* Deferred.succeed(releaseFirst, undefined);
            const [first, second] = yield* Effect.all(
              [Fiber.join(firstFiber), Fiber.join(secondFiber)],
              { concurrency: "unbounded" },
            );
            expect(first[0]?._tag).toBe("Success");
            expect(second[0]?._tag).toBe("Success");
            if (first[0]?._tag === "Success") {
              expect(first[0].result.kind).toBe("new");
            }
            if (second[0]?._tag === "Success") {
              expect(second[0].result.kind).toBe("duplicate");
            }
            expect(
              (yield* TxAdmissionsDB.getByTxId(request.txId))?.request_count,
            ).toBe(2n);
          }),
        ),
    );

    it.effect(
      "orders overlapping opposite-order reserved batches without deadlock",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`CREATE OR REPLACE FUNCTION phase1_admission_order_barrier()
            RETURNS trigger
            LANGUAGE plpgsql
            AS $$
            BEGIN
              PERFORM pg_advisory_xact_lock(
                hashtextextended(encode(NEW.tx_id, 'hex'), 0)
              );
              PERFORM pg_sleep(0.05);
              RETURN NEW;
            END;
            $$`;
            yield* sql`CREATE TRIGGER phase1_admission_order_barrier
            BEFORE INSERT ON tx_admissions
            FOR EACH ROW
            EXECUTE FUNCTION phase1_admission_order_barrier()`;
            const requestA = {
              txId: databaseTxHash("admission.reserved-order-a"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.reserved-order-a",
                64,
              ),
              submitSource: "native" as const,
            };
            const requestB = {
              txId: databaseTxHash("admission.reserved-order-b"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.reserved-order-b",
                64,
              ),
              submitSource: "native" as const,
            };
            const start = yield* Deferred.make<void>();
            const run = (requests: readonly (typeof requestA)[]) =>
              sql.withTransaction(
                Effect.gen(function* () {
                  yield* sql`SET LOCAL lock_timeout = '5s'`;
                  yield* sql`SET LOCAL statement_timeout = '10s'`;
                  yield* Deferred.await(start);
                  return yield* TxAdmissionsDB.admitReservedBatch(requests);
                }),
              );
            const forward = yield* Effect.fork(run([requestA, requestB]));
            const reverse = yield* Effect.fork(run([requestB, requestA]));
            yield* Deferred.succeed(start, undefined);
            const outcomes = [
              ...(yield* Fiber.join(forward)),
              ...(yield* Fiber.join(reverse)),
            ];
            for (const request of [requestA, requestB]) {
              const kinds = outcomes
                .filter(
                  (outcome) =>
                    outcome._tag === "Success" &&
                    outcome.result.entry.tx_id.equals(request.txId),
                )
                .map((outcome) =>
                  outcome._tag === "Success" ? outcome.result.kind : "conflict",
                )
                .sort();
              expect(kinds).toEqual(["duplicate", "new"]);
              expect(
                (yield* TxAdmissionsDB.getByTxId(request.txId))?.request_count,
              ).toBe(2n);
            }
          }),
        ),
    );

    it.effect(
      "does not overshoot a stale live count when local admits fill the cap",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const maxBacklog = 5;
            let accepted = 0;
            let rejected = 0;
            for (let index = 0; index < maxBacklog + 3; index += 1) {
              const result = yield* Effect.either(
                TxAdmissionsDB.admit({
                  txId: databaseTxHash(
                    `admission.stale-gauge-${index.toString()}`,
                  ),
                  txCanonicalCbor: databaseFixtureBytes(
                    `admission.stale-gauge-${index.toString()}`,
                    64,
                  ),
                  submitSource: "native",
                  currentBacklog: yield* readAdmissionBacklogGauge,
                  maxBacklog,
                }),
              );
              if (result._tag === "Right") {
                accepted += 1;
                yield* noteLocalAdmit;
              } else {
                rejected += 1;
              }
            }
            expect(accepted).toBe(maxBacklog);
            expect(rejected).toBe(3);
            expect(yield* TxAdmissionsDB.countBacklog).toBe(BigInt(maxBacklog));
          }).pipe(Effect.provide(Globals.Default)),
        ),
    );

    it.effect("does not overshoot the cap under parallel distinct admits", () =>
      isolatedDb(
        Effect.gen(function* () {
          const maxBacklog = 5;
          const results = yield* Effect.all(
            Array.from({ length: 16 }, (_, index) =>
              Effect.gen(function* () {
                const reservation =
                  yield* reserveAdmissionBacklogSlot(maxBacklog);
                const result = yield* Effect.either(
                  TxAdmissionsDB.admit({
                    txId: databaseTxHash(
                      `admission.parallel-cap-${index.toString()}`,
                    ),
                    txCanonicalCbor: databaseFixtureBytes(
                      `admission.parallel-cap-${index.toString()}`,
                      64,
                    ),
                    submitSource: "native",
                    currentBacklog: reservation.currentBacklog,
                    maxBacklog,
                  }),
                );
                if (reservation.reserved) {
                  if (result._tag === "Right" && result.right.kind === "new") {
                    yield* commitAdmissionBacklogSlot;
                  } else {
                    yield* releaseAdmissionBacklogSlot;
                  }
                }
                return result;
              }),
            ),
            { concurrency: "unbounded" },
          );

          expect(
            results.filter(
              (result) =>
                result._tag === "Right" && result.right.kind === "new",
            ),
          ).toHaveLength(maxBacklog);
          expect(
            results.filter(
              (result) =>
                result._tag === "Left" &&
                result.left._tag === "TxAdmissionBacklogFullError",
            ),
          ).toHaveLength(results.length - maxBacklog);
          expect(yield* TxAdmissionsDB.countBacklog).toBe(BigInt(maxBacklog));
          expect(yield* readAdmissionBacklogGauge).toBe(BigInt(maxBacklog));
        }).pipe(Effect.provide(Globals.Default)),
      ),
    );

    it.effect("batch-updates per-row rejection metadata under one lease", () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            {
              txId: databaseTxHash("admission.reject-1"),
              txCanonicalCbor: databaseFixtureBytes("admission.reject-1", 64),
            },
            {
              txId: databaseTxHash("admission.reject-2"),
              txCanonicalCbor: databaseFixtureBytes("admission.reject-2", 64),
            },
          ];
          yield* Effect.all(
            inputs.map((input) =>
              TxAdmissionsDB.admit({
                ...input,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded" },
          );
          const leaseOwner = "database-test:rejections";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 2,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          yield* TxAdmissionsDB.markRejected({
            rows: claimed,
            leaseOwner,
            rejectedTxs: [
              {
                txId: inputs[0]!.txId,
                code: RejectCodes.InputNotFound,
                detail: "missing input",
              },
              {
                txId: inputs[1]!.txId,
                code: RejectCodes.DoubleSpend,
                detail: null,
              },
            ],
          });

          const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
          const second = yield* TxAdmissionsDB.getByTxId(inputs[1]!.txId);
          expect(first?.status).toBe(TxAdmissionsDB.Status.Rejected);
          expect(first?.reject_code).toBe(RejectCodes.InputNotFound);
          expect(first?.reject_detail).toBe("missing input");
          expect(second?.status).toBe(TxAdmissionsDB.Status.Rejected);
          expect(second?.reject_code).toBe(RejectCodes.DoubleSpend);
          expect(second?.reject_detail).toBeNull();
        }),
      ),
    );

    it.effect(
      "claims disjoint leases for two concurrent validation loops",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const inputs = Array.from({ length: 8 }, (_, index) => ({
              txId: databaseTxHash(`admission.parallel-claim-${index}`),
              txCanonicalCbor: databaseFixtureBytes(
                `admission.parallel-claim-${index}`,
                64,
              ),
            }));
            yield* Effect.all(
              inputs.map((input) =>
                TxAdmissionsDB.admit({
                  ...input,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 20,
                }),
              ),
              { concurrency: "unbounded" },
            );
            const [left, right] = yield* Effect.all(
              [
                TxAdmissionsDB.claimBatch({
                  limit: 4,
                  leaseOwner: "parallel-loop:left",
                  leaseDurationMs: 30_000,
                }),
                TxAdmissionsDB.claimBatch({
                  limit: 4,
                  leaseOwner: "parallel-loop:right",
                  leaseDurationMs: 30_000,
                }),
              ],
              { concurrency: "unbounded" },
            );
            const leftIds = new Set(
              left.map((entry) => entry.tx_id.toString("hex")),
            );
            const rightIds = new Set(
              right.map((entry) => entry.tx_id.toString("hex")),
            );
            expect(left).toHaveLength(4);
            expect(right).toHaveLength(4);
            expect([...leftIds].some((txId) => rightIds.has(txId))).toBe(false);
            expect(new Set([...leftIds, ...rightIds]).size).toBe(8);
          }),
        ),
    );

    it.effect(
      "loads exact payloads after a lightweight ordered claim and fails closed on loss",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const inputs = Array.from({ length: 3 }, (_, index) => ({
              txId: databaseTxHash(`admission.split-claim-${index}`),
              txCanonicalCbor: databaseFixtureBytes(
                `admission.split-claim-${index}`,
                64 + index,
              ),
            }));
            for (const input of inputs) {
              yield* TxAdmissionsDB.admit({
                ...input,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              });
            }
            const leaseOwner = "database-test:split-claim";
            const claimed = yield* TxAdmissionsDB.claimBatchLease({
              limit: inputs.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(inputs.length);
            expect("tx_canonical_cbor" in claimed[0]!).toBe(false);

            const loaded = yield* TxAdmissionsDB.loadClaimedPayloads({
              claimed,
              leaseOwner,
            });
            expect(loaded.map((entry) => entry.tx_id)).toEqual(
              inputs.map((input) => input.txId),
            );
            expect(loaded.map((entry) => entry.tx_canonical_cbor)).toEqual(
              inputs.map((input) => input.txCanonicalCbor),
            );

            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM tx_admission_payloads
            WHERE tx_id = ${inputs[1]!.txId}`;
            const missingPayload = yield* Effect.either(
              TxAdmissionsDB.loadClaimedPayloads({ claimed, leaseOwner }),
            );
            expect(missingPayload._tag).toBe("Left");
            const stillLeased = yield* TxAdmissionsDB.getByTxId(
              inputs[0]!.txId,
            );
            expect(stillLeased?.status).toBe(TxAdmissionsDB.Status.Validating);
            expect(stillLeased?.lease_owner).toBe(leaseOwner);

            yield* TxAdmissionsDB.releaseForRetry({
              txIds: claimed.map((entry) => entry.tx_id),
              leaseOwner,
              baseDelayMs: 0,
              maxDelayMs: 0,
            });
            const recovered = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
            expect(recovered?.status).toBe(TxAdmissionsDB.Status.Queued);
            expect(recovered?.lease_owner).toBeNull();
          }),
        ),
    );

    it.effect("keeps relaxed claim durability transaction-local", () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const admissionSql = yield* AdmissionSql;
          const readSetting = (client: SqlClient.SqlClient) =>
            client<{ readonly synchronous_commit: string }>`
            SHOW synchronous_commit`.pipe(
              Effect.map((rows) => rows[0]?.synchronous_commit),
            );
          const txId = databaseTxHash("admission.local-sync-setting");
          yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor: databaseFixtureBytes(
              "admission.local-sync-setting",
              64,
            ),
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          expect(yield* readSetting(sql)).toBe("on");
          expect(yield* readSetting(admissionSql)).toBe("on");
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner: "local-sync-setting",
            leaseDurationMs: 30_000,
          });
          expect(claimed).toHaveLength(1);
          expect(yield* readSetting(sql)).toBe("on");
          expect(yield* readSetting(admissionSql)).toBe("on");
        }),
      ),
    );

    it.effect(
      "orders duplicate arrival sequences by tx id without a full-history unique index",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txIds = [
              databaseTxHash("admission.arrival-tie-c"),
              databaseTxHash("admission.arrival-tie-a"),
              databaseTxHash("admission.arrival-tie-b"),
            ];
            yield* Effect.all(
              txIds.map((txId) =>
                TxAdmissionsDB.admit({
                  txId,
                  txCanonicalCbor: databaseFixtureBytes(
                    `admission.arrival-tie-${txId.toString("hex")}`,
                    64,
                  ),
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded" },
            );
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE tx_admissions SET arrival_seq = 7`;
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 3,
              leaseOwner: "arrival-tie",
              leaseDurationMs: 30_000,
            });
            expect(claimed.map((row) => row.tx_id.toString("hex"))).toEqual(
              [...txIds]
                .sort(Buffer.compare)
                .map((txId) => txId.toString("hex")),
            );

            const indexes = yield* sql<{
              readonly indexname: string;
              readonly indexdef: string;
            }>`SELECT indexname, indexdef
            FROM pg_indexes
            WHERE tablename = 'tx_admissions'`;
            expect(
              indexes.some(
                (index) => index.indexname === "tx_admissions_arrival_seq_key",
              ),
            ).toBe(false);
            expect(
              indexes.find(
                (index) =>
                  index.indexname === "idx_tx_admissions_queued_arrival",
              )?.indexdef,
            ).toContain("(arrival_seq, tx_id)");
          }),
        ),
    );

    it.effect(
      "releases a validation lease after a worker infrastructure crash",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("admission.worker-crash");
            const txCanonicalCbor = makeNativeSubmitTx().txCanonicalCbor;
            yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const leaseOwner = "worker-crash:lease";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const exit = yield* Effect.exit(
              withAdmissionLeaseRecovery(
                Effect.die(
                  new ValidationWorkerError({
                    message: "worker crashed during UPLC evaluation",
                  }),
                ),
                TxAdmissionsDB.releaseForRetry({
                  txIds: claimed.map((entry) => entry.tx_id),
                  leaseOwner,
                  baseDelayMs: 0,
                  maxDelayMs: 0,
                }),
              ),
            );
            expect(exit._tag).toBe("Failure");
            const recovered = yield* TxAdmissionsDB.getByTxId(txId);
            expect(recovered?.status).toBe(TxAdmissionsDB.Status.Queued);
            expect(recovered?.lease_owner).toBeNull();
            expect(recovered?.lease_expires_at).toBeNull();

            const retryLeaseOwner = "worker-crash:retry";
            const retryClaim = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner: retryLeaseOwner,
              leaseDurationMs: 30_000,
            });
            const source = {
              ...ledgerEntry1,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.worker-crash-source",
              ),
            };
            const produced = {
              ...ledgerEntry1,
              [LedgerUtils.Columns.TX_ID]: txId,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.worker-crash-produced",
              ),
            };
            yield* MempoolLedgerDB.insert([source]);
            yield* TxAdmissionsDB.markAccepted({
              rows: retryClaim,
              leaseOwner: retryLeaseOwner,
              processedTxs: [
                {
                  txId,
                  txCbor: txCanonicalCbor,
                  spent: [source[LedgerUtils.Columns.OUTREF]],
                  produced: [produced],
                },
              ],
            });
            yield* (yield* WriteBehind).flushNow;

            const sql = yield* SqlClient.SqlClient;
            const membership = yield* sql<{
              readonly tx: Buffer;
            }>`SELECT tx FROM mempool WHERE tx_id = ${txId}`;
            expect(membership).toHaveLength(1);
            expect(membership[0]?.tx).toEqual(txCanonicalCbor);
            expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
              TxAdmissionsDB.Status.Accepted,
            );
            yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;
            expect(yield* MempoolDB.retrieveTxCborByHash(txId)).toEqual(
              txCanonicalCbor,
            );
            expect(
              yield* MempoolDB.retrieveTxCborsByHashes([txId]),
            ).toStrictEqual([txCanonicalCbor]);

            const page = yield* MempoolDB.retrievePage({ limit: 10 });
            const selection = selectCommitTxCandidates({
              mempoolTxs: page.entries,
              processedMempoolTxs: [],
            });
            expect(selection.sourceTable).toBe("mempool");
            expect(selection.candidateTxHashes).toStrictEqual([txId]);
            expect(
              selection.candidateTxs.map((entry) => entry.tx),
            ).toStrictEqual([txCanonicalCbor]);
            expect(
              (yield* AddressHistoryDB.retrieve(address1)).map(toHex),
            ).toContain(toHex(txCanonicalCbor));
          }),
        ),
    );

    it.effect(
      "survives SIGKILL after accept commit and positively decodes the lost write-behind delta after restart",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const tx = makeNativeSubmitTx();
            const processed = yield* breakDownTx(tx.txCanonicalCbor);
            expect(processed.txId).toEqual(tx.txId);
            expect(processed.spent).toHaveLength(1);
            expect(processed.produced.length).toBeGreaterThan(0);
            const source = {
              ...ledgerEntry1,
              [LedgerUtils.Columns.OUTREF]: processed.spent[0]!,
            } satisfies LedgerUtils.Entry;
            yield* MempoolLedgerDB.insert([source]);
            yield* TxAdmissionsDB.admit({
              txId: tx.txId,
              txCanonicalCbor: tx.txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const leaseOwner = "phase1-write-behind-crash";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(1);

            const helper = bundleChildProcessHelper(
              "./helpers/phase1-write-behind-crash-process.ts",
            );
            const crashInput = JSON.stringify({
              txIdHex: tx.txIdHex,
              txCanonicalCborHex: tx.txCanonicalCbor.toString("hex"),
              leaseOwner,
              spentOutrefHexes: processed.spent.map((value) =>
                value.toString("hex"),
              ),
              produced: processed.produced.map((entry) => ({
                txIdHex: entry[LedgerUtils.Columns.TX_ID].toString("hex"),
                outrefHex: entry[LedgerUtils.Columns.OUTREF].toString("hex"),
                outputHex: entry[LedgerUtils.Columns.OUTPUT].toString("hex"),
                address: entry[LedgerUtils.Columns.ADDRESS],
              })),
            });
            const crashMarker = "phase1_accept_committed_before_write_behind";
            const supervised = yield* Effect.promise(() =>
              superviseHostProcess({
                service: "phase1-write-behind-crash",
                command: process.execPath,
                args: [helper],
                cwd: path.resolve(databaseTestDirectory, ".."),
                env: {
                  ...databaseChildProcessEnv(),
                  PHASE1_WRITE_BEHIND_CRASH_INPUT: crashInput,
                },
                envInheritance: "none",
                rawLogPath: `${helper}.log`,
                timeoutMs: 10_000,
                maxRestarts: 0,
                terminateOnOutput: { marker: crashMarker, signal: "SIGKILL" },
              }),
            );
            expect(supervised.status).toBe("restart_budget_exhausted");
            expect(supervised.attempts[0]?.signal).toBe("SIGKILL");
            expect(supervised.attempts[0]?.outputTermination).toMatchObject({
              marker: crashMarker,
              signal: "SIGKILL",
            });

            expect((yield* TxAdmissionsDB.getByTxId(tx.txId))?.status).toBe(
              TxAdmissionsDB.Status.Accepted,
            );
            expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
            expect(
              (yield* MempoolTxDeltasDB.retrieveByTxIds([tx.txId])).size,
            ).toBe(0);
            expect(yield* AddressHistoryDB.retrieve(address1)).toEqual([]);

            // A new runtime has no in-memory write-behind queue. The durable
            // mempool row carries canonical CBOR inline, so the commit fallback
            // can reconstruct the exact delta without an admission-table join.
            const page = yield* MempoolDB.retrievePage({ limit: 1 });
            expect(page.entries).toHaveLength(1);
            const restarted = yield* resolveTxDeltaForCommit(
              page.entries[0]!,
              undefined,
            );
            expect(restarted._tag).toBe("Decoded");
            if (restarted._tag === "Decoded") {
              expect(restarted.spent).toStrictEqual(processed.spent);
              expect(restarted.produced).toStrictEqual(
                processed.produced.map((entry) => ({
                  [LedgerUtils.Columns.OUTREF]:
                    entry[LedgerUtils.Columns.OUTREF],
                  [LedgerUtils.Columns.OUTPUT]:
                    entry[LedgerUtils.Columns.OUTPUT],
                })),
              );
            }
            expect(yield* AddressHistoryDB.retrieve(address1)).toEqual([]);
          }),
        ),
    );

    it.effect(
      "atomically accepts multiple rows and consumes the matching deposit source",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry({
              [DepositsDB.Columns.PROJECTED_HEADER_HASH]: databaseFixtureBytes(
                "admission.array-deposit.header",
                28,
              ),
              [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
            });
            yield* insertDeposits([deposit]);
            const depositSource =
              yield* DepositsDB.toMempoolLedgerEntry(deposit);
            const normalSource = {
              ...ledgerEntry2,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.array-deposit.normal-source",
              ),
            } satisfies LedgerUtils.Entry;
            yield* MempoolLedgerDB.insert([depositSource, normalSource]);

            const validTxCanonicalCbor = makeNativeSubmitTx().txCanonicalCbor;
            const inputs = [
              {
                txId: databaseTxHash("admission.array-deposit.first"),
                txCanonicalCbor: validTxCanonicalCbor,
                source: depositSource,
              },
              {
                txId: databaseTxHash("admission.array-deposit.second"),
                txCanonicalCbor: validTxCanonicalCbor,
                source: normalSource,
              },
            ];
            yield* Effect.all(
              inputs.map(({ txId, txCanonicalCbor }) =>
                TxAdmissionsDB.admit({
                  txId,
                  txCanonicalCbor,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded", discard: true },
            );
            const leaseOwner = "database-test:array-deposit-accept";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: inputs.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const produced = inputs.map((input, index) => ({
              [LedgerUtils.Columns.TX_ID]: input.txId,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                `admission.array-deposit.produced-${index.toString()}`,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                `admission.array-deposit.output-${index.toString()}`,
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: address1,
            }));
            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: inputs.map((input, index) => ({
                txId: input.txId,
                txCbor: input.txCanonicalCbor,
                spent: [input.source[LedgerUtils.Columns.OUTREF]],
                produced: [produced[index]!],
              })),
            });

            const refreshedDeposit = yield* DepositsDB.retrieveByEventId(
              deposit[DepositsDB.Columns.ID],
            );
            expect(Option.isSome(refreshedDeposit)).toBe(true);
            if (Option.isSome(refreshedDeposit)) {
              expect(refreshedDeposit.value[DepositsDB.Columns.STATUS]).toBe(
                DepositsDB.Status.Consumed,
              );
            }
            expect(yield* MempoolDB.retrieveTxCount).toBe(2n);
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(
              new Set(ledger.map((entry) => entry.outref.toString("hex"))),
            ).toEqual(
              new Set(
                produced.map((entry) =>
                  entry[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ),
            );
            const accepted = yield* Effect.forEach(inputs, ({ txId }) =>
              TxAdmissionsDB.getByTxId(txId),
            );
            expect(
              accepted.every(
                (entry) => entry?.status === TxAdmissionsDB.Status.Accepted,
              ),
            ).toBe(true);
          }),
        ),
    );

    it.effect(
      "rolls back deposit consumption when bytea-array lease counts mismatch",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry({
              [DepositsDB.Columns.PROJECTED_HEADER_HASH]: databaseFixtureBytes(
                "admission.array-deposit-rollback.header",
                28,
              ),
              [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
            });
            yield* insertDeposits([deposit]);
            const depositSource =
              yield* DepositsDB.toMempoolLedgerEntry(deposit);
            const normalSource = {
              ...ledgerEntry2,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                "admission.array-deposit-rollback.normal-source",
              ),
            } satisfies LedgerUtils.Entry;
            yield* MempoolLedgerDB.insert([depositSource, normalSource]);
            const inputs = [
              {
                txId: databaseTxHash("admission.array-deposit-rollback.first"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.array-deposit-rollback.first",
                  64,
                ),
                source: depositSource,
              },
              {
                txId: databaseTxHash("admission.array-deposit-rollback.second"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.array-deposit-rollback.second",
                  64,
                ),
                source: normalSource,
              },
            ];
            yield* Effect.all(
              inputs.map(({ txId, txCanonicalCbor }) =>
                TxAdmissionsDB.admit({
                  txId,
                  txCanonicalCbor,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded", discard: true },
            );
            const leaseOwner = "database-test:array-deposit-rollback";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: inputs.length,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE ${sql(TxAdmissionsDB.tableName)}
            SET lease_owner = 'other-owner'
            WHERE tx_id = ${inputs[1]!.txId}`;
            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: inputs.map((input, index) => ({
                  txId: input.txId,
                  txCbor: input.txCanonicalCbor,
                  spent: [input.source[LedgerUtils.Columns.OUTREF]],
                  produced: [
                    {
                      [LedgerUtils.Columns.TX_ID]: input.txId,
                      [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                        `admission.array-deposit-rollback.produced-${index.toString()}`,
                      ),
                      [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                        `admission.array-deposit-rollback.output-${index.toString()}`,
                        80,
                      ),
                      [LedgerUtils.Columns.ADDRESS]: address1,
                    },
                  ],
                })),
              }),
            );

            expect(result._tag).toBe("Left");
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
            const refreshedDeposit = yield* DepositsDB.retrieveByEventId(
              deposit[DepositsDB.Columns.ID],
            );
            expect(Option.isSome(refreshedDeposit)).toBe(true);
            if (Option.isSome(refreshedDeposit)) {
              expect(refreshedDeposit.value[DepositsDB.Columns.STATUS]).toBe(
                DepositsDB.Status.Projected,
              );
            }
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(
              new Set(ledger.map((entry) => entry.outref.toString("hex"))),
            ).toEqual(
              new Set(
                [depositSource, normalSource].map((entry) =>
                  entry[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ),
            );
            expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
          }),
        ),
    );

    it.effect(
      "rolls back a batch rejection when the lease count mismatches",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const inputs = [
              {
                txId: databaseTxHash("admission.reject-mismatch-1"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.reject-mismatch-1",
                  64,
                ),
              },
              {
                txId: databaseTxHash("admission.reject-mismatch-2"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.reject-mismatch-2",
                  64,
                ),
              },
            ];
            yield* Effect.all(
              inputs.map((input) =>
                TxAdmissionsDB.admit({
                  ...input,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded" },
            );
            const leaseOwner = "database-test:rejection-mismatch";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 2,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE tx_admissions SET lease_owner = 'other-owner' WHERE tx_id = ${inputs[1]!.txId}`;

            const result = yield* Effect.either(
              TxAdmissionsDB.markRejected({
                rows: claimed,
                leaseOwner,
                rejectedTxs: [
                  {
                    txId: inputs[0]!.txId,
                    code: RejectCodes.InputNotFound,
                    detail: "first",
                  },
                  {
                    txId: inputs[1]!.txId,
                    code: RejectCodes.DoubleSpend,
                    detail: "second",
                  },
                ],
              }),
            );
            expect(result._tag).toBe("Left");
            const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
            expect(first?.status).toBe(TxAdmissionsDB.Status.Validating);
            const rejectionCount = yield* sql<{
              readonly count: string;
            }>`SELECT COUNT(*)::text AS count FROM tx_rejections`;
            expect(rejectionCount[0]?.count).toBe("0");
          }),
        ),
    );

    it.effect(
      "copies the durable canonical CBOR on the fallback accepted path",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("admission.accept-fallback-inline");
            const txCanonicalCbor = makeNativeSubmitTx().txCanonicalCbor;
            yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const leaseOwner = "database-test:accept-fallback-inline";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });

            yield* TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: [
                {
                  txId,
                  txCbor: txCanonicalCbor,
                  spent: [],
                  produced: [],
                },
              ],
            });

            const sql = yield* SqlClient.SqlClient;
            const memberships = yield* sql<{ readonly tx: Buffer }>`
            SELECT tx FROM mempool WHERE tx_id = ${txId}`;
            expect(memberships).toEqual([{ tx: txCanonicalCbor }]);
            expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
              TxAdmissionsDB.Status.Accepted,
            );
            yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;
            expect(yield* MempoolDB.retrieveTxCborByHash(txId)).toEqual(
              txCanonicalCbor,
            );
          }),
        ),
    );

    it.effect(
      "refuses terminal acceptance when the durable admission payload is missing",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("admission.accept-missing-payload");
            const txCanonicalCbor = databaseFixtureBytes(
              "admission.accept-missing-payload",
              64,
            );
            yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const leaseOwner = "database-test:accept-missing-payload";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;

            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs: [
                  {
                    txId,
                    txCbor: txCanonicalCbor,
                    spent: [],
                    produced: [],
                  },
                ],
              }),
            );
            expect(result._tag).toBe("Left");
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
            const admissions = yield* sql<{
              readonly status: TxAdmissionsDB.Status;
              readonly lease_owner: string | null;
            }>`SELECT status, lease_owner FROM tx_admissions WHERE tx_id = ${txId}`;
            expect(admissions).toHaveLength(1);
            expect(admissions[0]?.status).toBe(
              TxAdmissionsDB.Status.Validating,
            );
            expect(admissions[0]?.lease_owner).toBe(leaseOwner);
          }),
        ),
    );

    it.effect(
      "rolls back the compact accepted fast path when the lease count mismatches",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const inputs = [
              {
                txId: databaseTxHash("admission.accept-mismatch-1"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.accept-mismatch-1",
                  64,
                ),
              },
              {
                txId: databaseTxHash("admission.accept-mismatch-2"),
                txCanonicalCbor: databaseFixtureBytes(
                  "admission.accept-mismatch-2",
                  64,
                ),
              },
            ];
            yield* Effect.all(
              inputs.map((input) =>
                TxAdmissionsDB.admit({
                  ...input,
                  submitSource: "native",
                  currentBacklog: 0n,
                  maxBacklog: 10,
                }),
              ),
              { concurrency: "unbounded" },
            );
            yield* MempoolLedgerDB.insert([ledgerEntry1, ledgerEntry2]);
            const leaseOwner = "database-test:accept-mismatch";
            const claimed = yield* TxAdmissionsDB.claimBatch({
              limit: 2,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE tx_admissions
            SET lease_owner = 'other-owner'
            WHERE tx_id = ${inputs[1]!.txId}`;

            const processedTxs: readonly ProcessedTx[] = inputs.map(
              (input, index) => {
                const source = index === 0 ? ledgerEntry1 : ledgerEntry2;
                return {
                  txId: input.txId,
                  txCbor: input.txCanonicalCbor,
                  spent: [source[LedgerUtils.Columns.OUTREF]],
                  produced: [
                    {
                      ...source,
                      [LedgerUtils.Columns.TX_ID]: input.txId,
                      [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                        `admission.accept-mismatch-produced-${index.toString()}`,
                      ),
                    },
                  ],
                };
              },
            );
            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: claimed,
                leaseOwner,
                processedTxs,
              }),
            );
            expect(result._tag).toBe("Left");
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
            const ledger = yield* MempoolLedgerDB.retrieveSpendable;
            expect(
              new Set(ledger.map((entry) => entry.outref.toString("hex"))),
            ).toEqual(
              new Set(
                [ledgerEntry1, ledgerEntry2].map((entry) =>
                  entry[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ),
            );
            const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
            expect(first?.status).toBe(TxAdmissionsDB.Status.Validating);
            expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
          }),
        ),
    );
  });
};
