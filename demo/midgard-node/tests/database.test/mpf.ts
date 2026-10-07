import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  BlocksDB,
  CommitBuildCalibrationDB,
  ConfirmedLedgerDB,
  DepositsDB,
  LedgerUtils,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../../src/database/index.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
} from "../../src/mpf/index.js";
import {
  applyConfirmedLedgerDelta,
  applyConfirmedLedgerDeltaChainTransaction,
  decodeConfirmedLedgerDelta,
  materializeConfirmedLedgerSnapshot,
} from "../../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { finalizeConfirmedMergeTransaction } from "../../src/transactions/state-queue/merge-to-confirmed-state.js";
import { buildDaPayloadInsert } from "../../src/workers/commit-block-header/da-payload.js";
import { resolvePendingJournalLedgerState } from "../../src/workers/commit-block-header/pending-journal.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from ".././midgard-output-helpers.js";
import { address1, isolatedDb, makeDepositEntry } from "./fixtures.js";

export const registerMpfTests = () => {
  describe("Phase 3 MPF durable state", () => {
    it.effect("persists EWMA and keeps audit divergence sticky", () =>
      isolatedDb(
        Effect.gen(function* () {
          const before = yield* CommitBuildCalibrationDB.retrieve;
          const updated = yield* CommitBuildCalibrationDB.update(2.5);
          expect(updated.msPerTxEwma).toBe(2.5);
          expect(updated.sampleCount).toBe(before.sampleCount + 1n);

          yield* MpfEngineStateDB.recordLedgerAudit({
            rootHex: SDK.EMPTY_MERKLE_TREE_ROOT,
            diverged: true,
          });
          yield* MpfEngineStateDB.recordLedgerAudit({
            rootHex: SDK.EMPTY_MERKLE_TREE_ROOT,
            diverged: false,
          });
          expect(
            (yield* MpfEngineStateDB.assertLedgerAuditHealthy.pipe(
              Effect.either,
            ))._tag,
          ).toBe("Left");
          yield* MpfEngineStateDB.acknowledgeCleanLedgerAudit(
            SDK.EMPTY_MERKLE_TREE_ROOT,
          );
          expect(
            (yield* MpfEngineStateDB.assertLedgerAuditHealthy.pipe(
              Effect.either,
            ))._tag,
          ).toBe("Right");
          expect(
            yield* MpfEngineStateDB.acquireLedgerStoreLease({
              owner: "audit:test",
              ttlMs: 60_000,
            }),
          ).toBe(true);
          expect(
            yield* MpfEngineStateDB.acquireLedgerStoreLease({
              owner: "commit:test",
              ttlMs: 60_000,
            }),
          ).toBe(false);
          yield* MpfEngineStateDB.releaseLedgerStoreLease("audit:test");
        }),
      ),
    );

    it.live(
      "renews ordered state-queue and MPF leases and excludes merge/commit competitors",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const result = yield* StateQueueMutationLeasesDB.tryWithLease(
              "mpf-audit-test",
              (stateQueueToken) =>
                Effect.gen(function* () {
                  const mpfResult =
                    yield* MpfEngineStateDB.tryWithLedgerStoreLease(
                      "mpf-audit-test",
                      (mpfOwner) =>
                        Effect.gen(function* () {
                          yield* Effect.sleep("2500 millis"); // 2.5 TTLs: needs renewal
                          yield* StateQueueMutationLeasesDB.revalidate(
                            stateQueueToken,
                          );
                          yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                            mpfOwner,
                          );
                          const mergeAttempt =
                            yield* StateQueueMutationLeasesDB.tryAcquire({
                              holder: "merge-test",
                              ttlMs: 100,
                            });
                          expect(mergeAttempt._tag).toBe("Busy");
                          expect(
                            yield* MpfEngineStateDB.acquireLedgerStoreLease({
                              owner: "commit-test",
                              ttlMs: 100,
                            }),
                          ).toBe(false);
                        }),
                      { ttlMs: 1_000, renewIntervalMs: 50 },
                    );
                  expect(mpfResult._tag).toBe("Ran");
                }),
              { ttlMs: 1_000, renewIntervalMs: 50 },
            );
            expect(result._tag).toBe("Ran");
          }),
        ),
    );

    it.effect(
      "replays a depth-three ledger delta chain from confirmed state in a parent-plus-child confirmed merge transaction",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
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
            const [a, untouched, c, d, e] = [1, 2, 3, 4, 5].map(entry) as [
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
              LedgerUtils.Entry,
            ];
            yield* ConfirmedLedgerDB.insertMultiple([a, untouched]);
            const states = [
              [a, untouched],
              [untouched, c],
              [untouched, d],
              [untouched, d, e],
            ];
            const roots = yield* Effect.forEach(
              states,
              computeLedgerMpfRootFromLedgerEntries,
            );
            const headerValues: SDK.Header[] = [];
            const headers: Buffer[] = [];
            for (let index = 0; index < 3; index += 1) {
              const header: SDK.Header = {
                prevUtxosRoot: roots[index]!,
                utxosRoot: roots[index + 1]!,
                withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                withdrawalCount: 0n,
                forcedTransactionCount: 0n,
                l2TransactionCount: 0n,
                depositCount: 0n,
                totalEventCount: 0n,
                transitionStepCount: 0n,
                validationTraceCount: 0n,
                startTime: BigInt(index * 2_000),
                endTime: BigInt(index * 2_000 + 1_000),
                blockSlot: BigInt(index),
                expectedNetworkId: 0n,
                minFeeA: 0n,
                minFeeB: 0n,
                prevHeaderHash:
                  index === 0
                    ? "00".repeat(28)
                    : headers[index - 1]!.toString("hex"),
                operatorVkey: "11".repeat(28),
                protocolVersion: 1n,
              };
              headerValues.push(header);
              headers.push(
                Buffer.from(yield* SDK.hashBlockHeader(header), "hex"),
              );
            }
            const prepare = (
              index: number,
              spent: readonly Buffer[],
              produced: readonly LedgerUtils.Entry[],
            ) =>
              Effect.gen(function* () {
                yield* PendingBlockFinalizationsDB.preparePendingSubmission({
                  headerHash: headers[index]!,
                  headerCbor: Buffer.from(
                    LucidData.to(
                      headerValues[index]! as never,
                      SDK.Header as never,
                    ),
                    "hex",
                  ),
                  metadata: {
                    deploymentMarker: makeDeploymentMarker("de".repeat(32)),
                    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
                    stateQueueLeaseToken: `phase3-${index.toString()}`,
                    baseSnapshotId: `phase3-${index.toString()}`,
                    baseTailOutRef: `phase3#${index.toString()}`,
                    baseTailHeaderHash:
                      index === 0 ? Buffer.alloc(28) : headers[index - 1]!,
                    baseTailDatumCbor: "d87980",
                    baseRoots: {
                      utxosRoot: roots[index]!,
                      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    },
                    blockStartTime: new Date(index * 2_000),
                    expectedRoots: {
                      utxosRoot: roots[index + 1]!,
                      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    },
                    expectedCounts: {
                      withdrawalCount: 0n,
                      forcedTransactionCount: 0n,
                      l2TransactionCount: 0n,
                      depositCount: 0n,
                      totalEventCount: 0n,
                      transitionStepCount: 0n,
                      validationTraceCount: 0n,
                    },
                  },
                  blockEndTime: new Date(index * 2_000 + 1_000),
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
                    spent,
                    produced: produced.map((item) => ({
                      [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
                        item[LedgerUtils.Columns.OUTREF],
                      [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
                        item[LedgerUtils.Columns.OUTPUT],
                    })),
                  },
                  utxoPayloadAggregate: ledgerPayloadAggregateFromEntries(
                    states[index + 1]!,
                  ),
                });
                yield* sql`UPDATE ${sql(
                  PendingBlockFinalizationsDB.tableName,
                )} SET status = ${PendingBlockFinalizationsDB.Status.Finalized}
              WHERE header_hash = ${headers[index]!}`;
              });
            yield* prepare(0, [a[LedgerUtils.Columns.OUTREF]], [c]);
            yield* prepare(1, [c[LedgerUtils.Columns.OUTREF]], [d]);
            yield* prepare(2, [], [e]);

            const found =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headers[2]!,
              );
            expect(found._tag).toBe("Some");
            if (found._tag === "None") return;
            expect(found.value.utxoPayloadAggregate).toEqual(
              ledgerPayloadAggregateFromEntries(states[3]!),
            );
            const snapshot = yield* materializeConfirmedLedgerSnapshot(
              found.value,
            );
            expect(snapshot.root).toBe(roots[3]);
            expect(
              snapshot.entries.map((item) =>
                item[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ).toEqual(
              states[3]!.map((item) =>
                item[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            );
            expect(snapshot.deltaChain).toHaveLength(3);
            const wrongBase = yield* Effect.either(
              applyConfirmedLedgerDeltaChainTransaction({
                ...snapshot,
                baseRoot: roots[1]!,
              }),
            );
            expect(wrongBase._tag).toBe("Left");
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(
                yield* ConfirmedLedgerDB.retrieve,
              ),
            ).toBe(roots[0]);
            const wrongFinal = yield* Effect.either(
              applyConfirmedLedgerDeltaChainTransaction({
                ...snapshot,
                root: roots[2]!,
              }),
            );
            expect(wrongFinal._tag).toBe("Left");
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(
                yield* ConfirmedLedgerDB.retrieve,
              ),
            ).toBe(roots[0]);
            const transactionallyRecovered =
              yield* applyConfirmedLedgerDeltaChainTransaction(snapshot);
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(
                transactionallyRecovered,
              ),
            ).toBe(roots[3]);
            yield* ConfirmedLedgerDB.clear;
            yield* ConfirmedLedgerDB.insertMultiple([...states[0]!]);

            const fullUtxos = snapshot.entries.map((item) => ({
              outref: item[LedgerUtils.Columns.OUTREF],
              output: item[LedgerUtils.Columns.OUTPUT],
            }));
            const identityInsert = yield* buildDaPayloadInsert({
              record: found.value,
              utxos: fullUtxos,
              envelope: { mode: "identity", zstdLevel: 3 },
            });
            const zstdInsert = yield* buildDaPayloadInsert({
              record: found.value,
              utxos: fullUtxos,
              envelope: { mode: "zstd", zstdLevel: 3 },
            });
            const identityUnwrapped = yield* Effect.tryPromise({
              try: () =>
                unwrapDaPayload(identityInsert.payload_cbor, {
                  maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
                }),
              catch: (cause) => cause,
            });
            const zstdUnwrapped = yield* Effect.tryPromise({
              try: () =>
                unwrapDaPayload(zstdInsert.payload_cbor, {
                  maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
                }),
              catch: (cause) => cause,
            });
            const expectedPayloadOutrefs = states[3]!
              .map((item) => item[LedgerUtils.Columns.OUTREF].toString("hex"))
              .sort();
            for (const payloadBytes of [
              identityUnwrapped.innerBytes,
              zstdUnwrapped.innerBytes,
            ]) {
              const payload = SDK.decodeDaPayload(payloadBytes);
              expect(
                payload.block_body.utxos.map(([outref]) => outref),
              ).toEqual(expectedPayloadOutrefs);
              expect(payload.block_body.header.utxosRoot).toBe(roots[3]);
            }

            for (let index = 0; index < headers.length; index += 1) {
              const header = headers[index]!;
              const journal =
                yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header);
              if (journal._tag === "None") throw new Error("missing journal");
              const delta = yield* decodeConfirmedLedgerDelta(journal.value);
              if (delta === undefined) throw new Error("missing delta");
              yield* applyConfirmedLedgerDelta(delta);
              const confirmedAtStep = yield* ConfirmedLedgerDB.retrieve;
              expect(
                confirmedAtStep.map((item) =>
                  item[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              ).toEqual(
                states[index + 1]!.map((item) =>
                  item[LedgerUtils.Columns.OUTREF].toString("hex"),
                ),
              );
              expect(
                yield* computeLedgerMpfRootFromLedgerEntries(confirmedAtStep),
              ).toBe(roots[index + 1]);
            }
            const confirmedAfter = yield* ConfirmedLedgerDB.retrieve;
            expect(
              confirmedAfter.some((item) =>
                item[LedgerUtils.Columns.OUTREF].equals(
                  untouched[LedgerUtils.Columns.OUTREF],
                ),
              ),
            ).toBe(true);
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(confirmedAfter),
            ).toBe(roots[3]);

            yield* ConfirmedLedgerDB.clear;
            yield* ConfirmedLedgerDB.insertMultiple([...states[0]!]);
            const parentChildJournal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headers[1]!,
              );
            if (parentChildJournal._tag === "None") {
              throw new Error("missing parent-child journal");
            }
            const parentChildSnapshot =
              yield* materializeConfirmedLedgerSnapshot(
                parentChildJournal.value,
              );
            expect(parentChildSnapshot.deltaChain).toHaveLength(2);

            const utxoSet = (entries: readonly LedgerUtils.Entry[]) =>
              entries
                .map(
                  (item) =>
                    `${item[LedgerUtils.Columns.OUTREF].toString("hex")}:${item[
                      LedgerUtils.Columns.OUTPUT
                    ].toString("hex")}`,
                )
                .sort();
            const mergeHeaderHash = headers[1]!;
            const successTxHash = Buffer.alloc(32, 0x61);
            const successDeposit = makeDepositEntry({
              [DepositsDB.Columns.PROJECTED_HEADER_HASH]: mergeHeaderHash,
              [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
            });
            yield* BlocksDB.insert(mergeHeaderHash, [successTxHash]);
            yield* DepositsDB.insertEntries([successDeposit]);
            yield* finalizeConfirmedMergeTransaction({
              headerHash: mergeHeaderHash,
              snapshot: parentChildSnapshot,
              projectedDepositEventIds: [successDeposit[DepositsDB.Columns.ID]],
              projectedWithdrawalEventIds: [],
              projectedForcedTransactionEventIds: [],
            });
            const mergedConfirmed = yield* ConfirmedLedgerDB.retrieve;
            expect(utxoSet(mergedConfirmed)).toEqual(utxoSet(states[2]!));
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(mergedConfirmed),
            ).toBe(roots[2]);
            expect(
              (yield* BlocksDB.retrieveTxHashesByHeaderHash(
                mergeHeaderHash,
              )).map((hash) => hash.toString("hex")),
            ).toEqual([]);
            const consumedDeposit = yield* DepositsDB.retrieveByEventId(
              successDeposit[DepositsDB.Columns.ID],
            );
            expect(consumedDeposit._tag).toBe("Some");
            if (consumedDeposit._tag === "Some") {
              expect(consumedDeposit.value[DepositsDB.Columns.STATUS]).toBe(
                DepositsDB.Status.Consumed,
              );
            }

            yield* ConfirmedLedgerDB.clear;
            yield* ConfirmedLedgerDB.insertMultiple([...states[0]!]);
            const failureTxHash = Buffer.alloc(32, 0x62);
            const failureDeposit = makeDepositEntry({
              [DepositsDB.Columns.PROJECTED_HEADER_HASH]: mergeHeaderHash,
              [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
            });
            yield* BlocksDB.insert(mergeHeaderHash, [failureTxHash]);
            yield* DepositsDB.insertEntries([failureDeposit]);
            const wrongBaseFinalization = yield* Effect.either(
              finalizeConfirmedMergeTransaction({
                headerHash: mergeHeaderHash,
                snapshot: {
                  ...parentChildSnapshot,
                  baseRoot: roots[1]!,
                },
                projectedDepositEventIds: [
                  failureDeposit[DepositsDB.Columns.ID],
                ],
                projectedWithdrawalEventIds: [],
                projectedForcedTransactionEventIds: [],
              }),
            );
            expect(wrongBaseFinalization._tag).toBe("Left");
            const confirmedAfterWrongBase = yield* ConfirmedLedgerDB.retrieve;
            expect(utxoSet(confirmedAfterWrongBase)).toEqual(
              utxoSet(states[0]!),
            );
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(
                confirmedAfterWrongBase,
              ),
            ).toBe(roots[0]);
            expect(
              (yield* BlocksDB.retrieveTxHashesByHeaderHash(
                mergeHeaderHash,
              )).map((hash) => hash.toString("hex")),
            ).toEqual([failureTxHash.toString("hex")]);
            const projectedAfterWrongBase = yield* DepositsDB.retrieveByEventId(
              failureDeposit[DepositsDB.Columns.ID],
            );
            expect(projectedAfterWrongBase._tag).toBe("Some");
            if (projectedAfterWrongBase._tag === "Some") {
              expect(
                projectedAfterWrongBase.value[DepositsDB.Columns.STATUS],
              ).toBe(DepositsDB.Status.Projected);
            }

            const wrongFinalRootFinalization = yield* Effect.either(
              finalizeConfirmedMergeTransaction({
                headerHash: mergeHeaderHash,
                snapshot: {
                  ...parentChildSnapshot,
                  root: roots[1]!,
                },
                projectedDepositEventIds: [
                  failureDeposit[DepositsDB.Columns.ID],
                ],
                projectedWithdrawalEventIds: [],
                projectedForcedTransactionEventIds: [],
              }),
            );
            expect(wrongFinalRootFinalization._tag).toBe("Left");
            const confirmedAfterWrongFinal = yield* ConfirmedLedgerDB.retrieve;
            expect(utxoSet(confirmedAfterWrongFinal)).toEqual(
              utxoSet(states[0]!),
            );
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(
                confirmedAfterWrongFinal,
              ),
            ).toBe(roots[0]);
            expect(
              (yield* BlocksDB.retrieveTxHashesByHeaderHash(
                mergeHeaderHash,
              )).map((hash) => hash.toString("hex")),
            ).toEqual([failureTxHash.toString("hex")]);
            const projectedAfterWrongFinal =
              yield* DepositsDB.retrieveByEventId(
                failureDeposit[DepositsDB.Columns.ID],
              );
            expect(projectedAfterWrongFinal._tag).toBe("Some");
            if (projectedAfterWrongFinal._tag === "Some") {
              expect(
                projectedAfterWrongFinal.value[DepositsDB.Columns.STATUS],
              ).toBe(DepositsDB.Status.Projected);
            }
          }),
        ),
    );

    it.effect(
      "materializes a first-block journal across an implicit genesis base",
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
            const genesis = entry(21);
            const deposit = entry(22);
            const selectedBaseUtxosRoot =
              yield* computeLedgerMpfRootFromLedgerEntries([genesis]);
            const finalEntries = [genesis, deposit];
            const expectedFinalUtxosRoot =
              yield* computeLedgerMpfRootFromLedgerEntries(finalEntries);
            const journalState = yield* resolvePendingJournalLedgerState({
              recordedBaseUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              selectedBaseUtxosRoot,
              expectedFinalUtxosRoot,
              expectedFinalEntryCount: finalEntries.length,
              implicitGenesisEntries: [genesis],
              transitionDelta: {
                spent: [],
                produced: [
                  {
                    outref: deposit[LedgerUtils.Columns.OUTREF],
                    output: deposit[LedgerUtils.Columns.OUTPUT],
                  },
                ],
              },
            });
            expect(journalState.ledgerDelta.spent).toEqual([]);
            expect(journalState.ledgerDelta.produced).toHaveLength(2);

            const unexplainedDivergence =
              yield* resolvePendingJournalLedgerState({
                recordedBaseUtxosRoot: "ff".repeat(32),
                selectedBaseUtxosRoot,
                expectedFinalUtxosRoot,
                expectedFinalEntryCount: finalEntries.length,
                implicitGenesisEntries: [genesis],
                transitionDelta: {
                  spent: [],
                  produced: [],
                },
              }).pipe(Effect.either);
            expect(unexplainedDivergence._tag).toBe("Left");

            const headerHash = Buffer.alloc(28, 23);
            yield* PendingBlockFinalizationsDB.preparePendingSubmission({
              headerHash,
              headerCbor: Buffer.from("d87980", "hex"),
              metadata: {
                deploymentMarker: makeDeploymentMarker("de".repeat(32)),
                consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
                stateQueueLeaseToken: "implicit-genesis-test",
                baseSnapshotId: "implicit-genesis-test",
                baseTailOutRef: "genesis#0",
                baseTailHeaderHash: Buffer.alloc(28),
                baseTailDatumCbor: "d87980",
                baseRoots: {
                  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                },
                blockStartTime: new Date(0),
                expectedRoots: {
                  utxosRoot: expectedFinalUtxosRoot,
                  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                },
                expectedCounts: {
                  withdrawalCount: 0n,
                  forcedTransactionCount: 0n,
                  l2TransactionCount: 0n,
                  depositCount: 1n,
                  totalEventCount: 1n,
                  transitionStepCount: 1n,
                  validationTraceCount: 0n,
                },
              },
              blockEndTime: new Date(1_000),
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
                spent: journalState.ledgerDelta.spent,
                produced: journalState.ledgerDelta.produced.map((produced) => ({
                  [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
                    produced.outref,
                  [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
                    produced.output,
                })),
              },
              utxoPayloadAggregate:
                ledgerPayloadAggregateFromEntries(finalEntries),
            });
            const journal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headerHash,
              );
            expect(journal._tag).toBe("Some");
            if (journal._tag === "None") return;
            const materialized = yield* materializeConfirmedLedgerSnapshot(
              journal.value,
            );
            expect(materialized.root).toBe(expectedFinalUtxosRoot);
            expect(materialized.entries).toHaveLength(finalEntries.length);
          }),
        ),
    );
  });
};
