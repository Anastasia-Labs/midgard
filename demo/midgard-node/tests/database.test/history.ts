import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import {
  Data as LucidData,
  datumToHash,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect } from "vitest";

import {
  DepositStatusCommandError,
  resolveDepositStatusProgram,
} from "../../src/commands/deposit-status.js";
import {
  DepositsDB,
  DepositSubmissionAttemptsDB,
  MempoolLedgerDB,
  WithdrawalsDB,
} from "../../src/database/index.js";
import { resolveIncludedDepositEntriesForWindow } from "../../src/mpf/index.js";
import { Lucid } from "../../src/services/lucid.js";
import { MidgardContracts } from "../../src/services/midgard-contracts.js";
import { reconcileDepositSubmissionAttemptProgram } from "../../src/transactions/submit-deposit.js";
import { loadRealMidgardContractsForTest } from ".././helpers/real-midgard-contracts.js";
import { insertDeposits, insertWithdrawals } from "../helpers/event-rows.js";
import {
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  isolatedDb,
  makeDepositEntry,
  makeDepositSubmissionAttempt,
  makeHistoryWithdrawalEntry,
} from "./fixtures.js";

export const registerHistoryTests = () => {
  describe("authenticated history pointer persistence", () => {
    /** A deposit row no follower admission identity has claimed. */
    const unassociated = <T extends object>(entry: T) => ({
      ...entry,
      l1_event_key: null,
      l1_origin_outref: null,
    });

    it.effect(
      "settles a submission attempt by authenticated history intent without writing the deposit row",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const contracts = yield* Effect.promise(() =>
              loadRealMidgardContractsForTest({
                txHash: "01".repeat(32),
                outputIndex: 0,
              }),
            );
            const deployment = SDK.eventHistoryDeploymentFromContracts(
              SDK.requireEventHistoryContracts(contracts).deposit,
            );
            const wallet = generateEmulatorAccount({ lovelace: 100_000_000n });
            const baseLucid = yield* Effect.promise(() =>
              makeLucid(new Emulator([wallet]), "Preprod"),
            );
            const eventId = { transactionId: "12".repeat(32), outputIndex: 0n };
            const idCbor = Buffer.from(
              LucidData.to(eventId, SDK.OutputReference),
              "hex",
            );
            const key = datumToHash(idCbor.toString("hex"));
            const node: SDK.EventHistoryNode = {
              position: { Key: [key] },
              next: null,
              protected_until: 0n,
              payload: {
                Order: {
                  facts: {
                    event_id: eventId,
                    inclusion_time: 1000n,
                    structural_lovelace: 2_000_000n,
                    structural_refund_key: "aa".repeat(28),
                    location: {
                      Inline: {
                        payload: {
                          DepositPayload: {
                            event: {
                              id: eventId,
                              info: {
                                l2_address: yield* SDK.addressDataFromBech32(
                                  wallet.address,
                                ),
                                l2_network_id: 0n,
                                l2_datum: null,
                              },
                            },
                          },
                        },
                      },
                    },
                  },
                },
              },
            };
            const root: UTxO = {
              txHash: "13".repeat(32),
              outputIndex: 0,
              address: deployment.address,
              assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
              datum: LucidData.to(
                {
                  position: "Root",
                  next: key,
                  protected_until: 0n,
                  payload: "RootContent",
                },
                SDK.EventHistoryNode,
              ),
            };
            const order: UTxO = {
              txHash: "14".repeat(32),
              outputIndex: 0,
              address: deployment.address,
              assets: { lovelace: 7_000_000n, [deployment.policyId + key]: 1n },
              datum: LucidData.to(node, SDK.EventHistoryNode),
            };
            // Reader/DB integration fixture; applied-policy acceptance and real
            // pointer transactions are covered by the public-builder emulator suite.
            let visible = [root, order];
            let unavailable = false;
            const api: LucidEvolution = {
              ...baseLucid,
              utxosAt: async (address) => {
                if (unavailable)
                  throw new Error("history provider unavailable");
                return address === deployment.address ? visible : [];
              },
            };
            const service = Lucid.make({
              api,
              referenceScriptsApi: baseLucid,
              operatorMainAddress: wallet.address,
              operatorMergeAddress: wallet.address,
              referenceScriptsWalletAddress: wallet.address,
              referenceScriptsAddress: wallet.address,
              submitSlotSnapshot: () =>
                Effect.die("not used by read-only reconciliation"),
              switchToOperatorsMainWallet: Effect.void,
              switchToOperatorsMergingWallet: Effect.void,
              switchToReferenceScriptWallet: Effect.void,
            });
            const admissionHash = databaseTxHash(
              "history-intent-original-admission",
            );
            const attempt = makeDepositSubmissionAttempt({
              txHash: admissionHash,
              eventId: idCbor,
            });
            yield* DepositSubmissionAttemptsDB.insertSubmitted({
              ...attempt,
              [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]:
                wallet.address,
              [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]:
                "5000000",
              [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]: {
                lovelace: "5000000",
              },
              [DepositSubmissionAttemptsDB.Columns.METADATA]: {
                ...attempt[DepositSubmissionAttemptsDB.Columns.METADATA],
                depositAddress: deployment.address,
                depositAssetName: key,
                depositAuthUnit: deployment.policyId + key,
                inclusionTime: 1000,
                structuralLovelace: "2000000",
              },
            });
            const reconcile = reconcileDepositSubmissionAttemptProgram(
              admissionHash.toString("hex"),
            ).pipe(
              Effect.provideService(Lucid, service),
              Effect.provideService(
                MidgardContracts,
                MidgardContracts.make({
                  ...contracts,
                  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
                }),
              ),
            );
            // The intent check settles the attempt only: the deposit row is
            // the L1 follower-change driver's to write (N1).
            const noDepositRow = Effect.map(
              DepositsDB.retrieveByEventId(idCbor),
              Option.isNone,
            );
            expect((yield* reconcile).status).toBe("reconciled_after_timeout");
            expect(yield* noDepositRow).toBe(true);
            const continued = {
              ...order,
              txHash: "15".repeat(32),
              outputIndex: 2,
            };
            visible = [root, continued];
            expect((yield* reconcile).status).toBe("reconciled_after_timeout");
            expect(yield* noDepositRow).toBe(true);
            visible = [
              {
                ...root,
                datum: LucidData.to(
                  {
                    position: "Root",
                    next: null,
                    protected_until: 0n,
                    payload: "RootContent",
                  },
                  SDK.EventHistoryNode,
                ),
              },
            ];
            expect((yield* reconcile).status).toBe("ambiguous");
            if (node.payload === "RootContent" || !("Order" in node.payload))
              throw new Error("Expected Order fixture");
            visible = [
              root,
              {
                ...continued,
                datum: LucidData.to(
                  {
                    ...node,
                    payload: {
                      Order: {
                        facts: {
                          ...node.payload.Order.facts,
                          inclusion_time: 2000n,
                        },
                      },
                    },
                  },
                  SDK.EventHistoryNode,
                ),
              },
            ];
            expect((yield* reconcile).status).toBe("ambiguous");
            expect(yield* noDepositRow).toBe(true);
            unavailable = true;
            const failure = yield* Effect.either(reconcile);
            expect(failure._tag).toBe("Left");
            if (failure._tag === "Left")
              expect(failure.left).toBeInstanceOf(SDK.LucidError);
          }),
        ),
    );

    it.effect(
      "resolves the submission hash, the deposit's immutable admission tx, after its Order moves",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            const admissionHash =
              deposit[DepositsDB.Columns.DEPOSIT_L1_TX_HASH];
            const attempt = makeDepositSubmissionAttempt({
              txHash: admissionHash,
              eventId: deposit[DepositsDB.Columns.ID],
            });
            yield* DepositSubmissionAttemptsDB.insertSubmitted(attempt);
            const stored =
              yield* DepositSubmissionAttemptsDB.retrieveByTxHash(
                admissionHash,
              );
            expect(Option.isSome(stored)).toBe(true);
            if (Option.isNone(stored)) throw new Error("Expected journal");
            expect(
              stored.value[DepositSubmissionAttemptsDB.Columns.METADATA],
            ).toEqual(attempt[DepositSubmissionAttemptsDB.Columns.METADATA]);
            expect(
              stored.value[DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS],
            ).toEqual(
              attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS],
            );
            expect(
              stored.value[
                DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS
              ],
            ).toEqual(
              attempt[DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS],
            );
            yield* insertDeposits([deposit]);
            // A list insertion moves the Order to another output; the row
            // keeps its admission tx (ruling 2) and refuses another one.
            const movedHash = databaseTxHash("status-pointer-continuation");
            const moved = yield* Effect.either(
              insertDeposits([
                {
                  ...deposit,
                  [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: movedHash,
                },
              ]),
            );
            expect(moved._tag).toBe("Left");
            expect(
              yield* resolveDepositStatusProgram({
                cardanoTxHash: admissionHash,
              }),
            ).toEqual(unassociated(deposit));
            expect(
              yield* resolveDepositStatusProgram({
                cardanoTxHash: admissionHash,
                eventId: deposit[DepositsDB.Columns.ID],
              }),
            ).toEqual(unassociated(deposit));
            expect(
              yield* DepositsDB.retrieveByCardanoTxHash(movedHash),
            ).toEqual([]);
          }),
        ),
    );

    for (const status of Object.values(DepositsDB.Status)) {
      it.effect(
        `keeps a deposit's admission tx and its ${status} projection on re-ingestion`,
        () =>
          isolatedDb(
            Effect.gen(function* () {
              const entry = makeDepositEntry();
              const header =
                status === DepositsDB.Status.Awaiting
                  ? null
                  : databaseFixtureBytes("history-pointer-header", 28);
              yield* insertDeposits([
                {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: status,
                  [DepositsDB.Columns.PROJECTED_HEADER_HASH]: header,
                },
              ]);
              const replacementHash = databaseTxHash(
                "history-pointer-deposit-replacement",
              );
              // The same event again keeps its projection; another admission
              // tx for it is refused (ruling 2).
              yield* insertDeposits([entry]);
              const replaced = yield* Effect.either(
                insertDeposits([
                  {
                    ...entry,
                    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: replacementHash,
                  },
                ]),
              );
              expect(replaced._tag).toBe("Left");
              expect(yield* DepositsDB.retrieveAllEntries()).toEqual([
                unassociated({
                  ...entry,
                  [DepositsDB.Columns.STATUS]: status,
                  [DepositsDB.Columns.PROJECTED_HEADER_HASH]: header,
                }),
              ]);
              expect(
                yield* DepositsDB.retrieveByCardanoTxHash(
                  entry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
                ),
              ).toHaveLength(1);
              expect(
                yield* DepositsDB.retrieveByCardanoTxHash(replacementHash),
              ).toEqual([]);
            }),
          ),
      );
    }
    for (const status of Object.values(WithdrawalsDB.Status)) {
      it.effect(
        `refreshes withdrawal location while preserving ${status} classification`,
        () =>
          isolatedDb(
            Effect.gen(function* () {
              yield* WithdrawalsDB.clear;
              const entry = makeHistoryWithdrawalEntry();
              const classified =
                status === WithdrawalsDB.Status.Awaiting
                  ? entry
                  : {
                      ...entry,
                      [WithdrawalsDB.Columns.STATUS]: status,
                      [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
                        databaseFixtureBytes("history-pointer-settlement", 64),
                      [WithdrawalsDB.Columns.VALIDITY]:
                        WithdrawalsDB.Validity.WithdrawalIsValid,
                      [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {
                        checked: true,
                      },
                      [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]:
                        databaseFixtureBytes("history-pointer-header", 28),
                    };
              yield* insertWithdrawals([classified]);
              const replacementHash = databaseTxHash(
                "history-pointer-withdrawal-replacement",
              );
              yield* insertWithdrawals([
                {
                  ...entry,
                  [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]:
                    replacementHash,
                  [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 3,
                },
              ]);
              const rows = yield* WithdrawalsDB.retrieveAllEntries();
              expect(rows).toHaveLength(1);
              expect(rows[0]![WithdrawalsDB.Columns.VALIDITY_DETAIL]).toEqual(
                classified[WithdrawalsDB.Columns.VALIDITY_DETAIL],
              );
              expect(rows[0]).toMatchObject({
                ...classified,
                [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: replacementHash,
                [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 3,
              });
              expect(
                yield* WithdrawalsDB.retrieveByCardanoTxHash(
                  entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH],
                ),
              ).toEqual([]);
            }),
          ),
      );
    }
    it.effect(
      "names only the L2 outrefs of withdrawals that are pending and not invalid",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const base = makeHistoryWithdrawalEntry();
            const withdrawal = (
              label: string,
              status: WithdrawalsDB.Status,
              validity: WithdrawalsDB.Validity | null,
              l2OutRef: Buffer = databaseOutputReferenceId(
                `pending-withdrawal-l2-${label}`,
                1n,
              ),
            ): WithdrawalsDB.Entry => ({
              ...base,
              [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(
                `pending-withdrawal-${label}`,
              ),
              [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
                `pending-withdrawal-l1-${label}`,
              ),
              [WithdrawalsDB.Columns.L2_OUTREF]: l2OutRef,
              [WithdrawalsDB.Columns.STATUS]: status,
              ...(validity === null
                ? {}
                : {
                    [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
                      databaseFixtureBytes(`pending-withdrawal-${label}`, 64),
                    [WithdrawalsDB.Columns.VALIDITY]: validity,
                    [WithdrawalsDB.Columns.VALIDITY_DETAIL]: { checked: true },
                    [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]:
                      databaseFixtureBytes(`pending-withdrawal-header`, 28),
                  }),
            });
            const unclassified = withdrawal(
              "unclassified",
              WithdrawalsDB.Status.Awaiting,
              null,
            );
            const projectedValid = withdrawal(
              "projected-valid",
              WithdrawalsDB.Status.Projected,
              WithdrawalsDB.Validity.WithdrawalIsValid,
            );
            yield* insertWithdrawals([
              unclassified,
              projectedValid,
              withdrawal(
                "projected-invalid",
                WithdrawalsDB.Status.Projected,
                WithdrawalsDB.Validity.SpentWithdrawalUtxo,
              ),
              withdrawal(
                "finalized-valid",
                WithdrawalsDB.Status.Finalized,
                WithdrawalsDB.Validity.WithdrawalIsValid,
              ),
              // An l2_outref that decodes to no output reference is skipped.
              withdrawal(
                "undecodable",
                WithdrawalsDB.Status.Awaiting,
                null,
                Buffer.from("00", "hex"),
              ),
            ]);
            expect(yield* WithdrawalsDB.retrieveAllEntries()).toHaveLength(5);

            const pending =
              yield* WithdrawalsDB.retrievePendingLedgerOutRefHexes;
            expect([...pending].sort()).toEqual(
              (yield* Effect.forEach(
                [unclassified, projectedValid],
                WithdrawalsDB.toLedgerOutRef,
              ))
                .map((outRef) => outRef.toString("hex"))
                .sort(),
            );
          }),
        ),
    );
    it.effect(
      "rolls back every deposit location when a batch changes immutable content",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const first = makeDepositEntry();
            const second = makeDepositEntry();
            yield* insertDeposits([first, second]);
            for (const field of [
              DepositsDB.Columns.INFO,
              DepositsDB.Columns.LEDGER_OUTPUT,
            ]) {
              const outcome = yield* Effect.either(
                insertDeposits([
                  {
                    ...first,
                    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: databaseTxHash(
                      "history-pointer-new",
                    ),
                  },
                  { ...second, [field]: Buffer.from("changed") },
                ]),
              );
              expect(outcome._tag).toBe("Left");
              expect(yield* DepositsDB.retrieveAllEntries()).toEqual([
                unassociated(first),
                unassociated(second),
              ]);
            }
            const outcome = yield* Effect.either(
              insertDeposits([
                {
                  ...first,
                  [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                    first[DepositsDB.Columns.INCLUSION_TIME].getTime() + 1,
                  ),
                },
              ]),
            );
            expect(outcome._tag).toBe("Left");
          }),
        ),
    );
    it.effect(
      "rejects withdrawal body or timing drift and rolls back the batch",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const first = makeHistoryWithdrawalEntry();
            const second = {
              ...first,
              [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(
                "history-pointer-second",
              ),
              [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 1,
            };
            yield* insertWithdrawals([first, second]);
            const before = yield* WithdrawalsDB.retrieveAllEntries();
            for (const field of [
              WithdrawalsDB.Columns.RAW_EVENT_INFO,
              WithdrawalsDB.Columns.L2_VALUE,
              WithdrawalsDB.Columns.REFUND_DATUM,
            ]) {
              const outcome = yield* Effect.either(
                insertWithdrawals([
                  {
                    ...first,
                    [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]:
                      databaseTxHash("history-pointer-new"),
                  },
                  { ...second, [field]: Buffer.from("changed") },
                ]),
              );
              expect(outcome._tag).toBe("Left");
              expect(yield* WithdrawalsDB.retrieveAllEntries()).toEqual(before);
            }
            const outcome = yield* Effect.either(
              insertWithdrawals([
                {
                  ...first,
                  [WithdrawalsDB.Columns.INCLUSION_TIME]: new Date(
                    first[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() + 1,
                  ),
                },
              ]),
            );
            expect(outcome._tag).toBe("Left");
          }),
        ),
    );
    it.effect(
      "rejects two competing locations for one event in a single observation",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const deposit = makeDepositEntry();
            const withdrawal = makeHistoryWithdrawalEntry();
            expect(
              (yield* Effect.either(
                insertDeposits([
                  deposit,
                  {
                    ...deposit,
                    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]:
                      databaseTxHash("competing"),
                  },
                ]),
              ))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(
                insertWithdrawals([
                  withdrawal,
                  {
                    ...withdrawal,
                    [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 2,
                  },
                ]),
              ))._tag,
            ).toBe("Left");
            expect(yield* DepositsDB.retrieveAllEntries()).toEqual([]);
            expect(yield* WithdrawalsDB.retrieveAllEntries()).toEqual([]);
          }),
        ),
    );
  });

  describe("DepositsDB and MempoolLedgerDB exact-once projection", () => {
    it.effect("rejects payload drift for the same deposit event_id", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const eventId = databaseOutputReferenceId(
            "deposits.payload-drift",
            0,
          );
          const first = makeDepositEntry({
            [DepositsDB.Columns.ID]: eventId,
          });
          const conflicting = makeDepositEntry({
            [DepositsDB.Columns.ID]: eventId,
            [DepositsDB.Columns.INFO]: databaseFixtureBytes(
              "deposits.payload-drift.conflicting-info",
              48,
            ),
          });

          yield* insertDeposits([first]);
          const result = yield* Effect.either(insertDeposits([conflicting]));

          expect(result._tag).toEqual("Left");
        }),
      ),
    );

    it.effect("retrieves one deposit by event_id", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const deposit = makeDepositEntry();
          yield* insertDeposits([deposit]);

          const retrieved = yield* DepositsDB.retrieveByEventId(
            deposit[DepositsDB.Columns.ID],
          );
          expect(retrieved._tag).toEqual("Some");
          if (retrieved._tag !== "Some") {
            throw new Error("expected deposit lookup to return a row");
          }

          expect(
            retrieved.value[DepositsDB.Columns.ID].equals(
              deposit[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
          expect(
            retrieved.value[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
              deposit[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
            ),
          ).toEqual(true);
        }),
      ),
    );

    it.effect(
      "retrieves deposits by Cardano tx hash in deterministic order",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const sharedCardanoTxHash = databaseTxHash(
              "deposits.shared-cardano-tx.deterministic-order",
            );
            const first = makeDepositEntry({
              [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                "2026-04-13T17:00:00.000Z",
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.deterministic-order.first",
                0,
              ),
            });
            const second = makeDepositEntry({
              [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                "2026-04-13T17:00:01.000Z",
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.deterministic-order.second",
                1,
              ),
            });
            yield* insertDeposits([second, first]);

            const retrieved =
              yield* DepositsDB.retrieveByCardanoTxHash(sharedCardanoTxHash);

            expect(retrieved).toHaveLength(2);
            expect(
              retrieved[0]?.[DepositsDB.Columns.ID].equals(
                first[DepositsDB.Columns.ID],
              ),
            ).toEqual(true);
            expect(
              retrieved[1]?.[DepositsDB.Columns.ID].equals(
                second[DepositsDB.Columns.ID],
              ),
            ).toEqual(true);
          }),
        ),
    );

    it.effect(
      "rejects ambiguous cardanoTxHash lookups and requires eventId to disambiguate",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const sharedCardanoTxHash = databaseTxHash(
              "deposits.shared-cardano-tx.ambiguous-status",
            );
            yield* insertDeposits([
              makeDepositEntry({
                [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                  "deposits.ambiguous.first",
                  0,
                ),
                [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
              }),
              makeDepositEntry({
                [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                  "deposits.ambiguous.second",
                  1,
                ),
                [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
              }),
            ]);

            const result = yield* Effect.either(
              resolveDepositStatusProgram({
                cardanoTxHash: sharedCardanoTxHash,
              }),
            );

            expect(result._tag).toEqual("Left");
            if (result._tag !== "Left") {
              throw new Error("expected ambiguous lookup to fail");
            }
            expect(result.left).toBeInstanceOf(DepositStatusCommandError);
            if (!(result.left instanceof DepositStatusCommandError)) {
              throw new Error("expected DepositStatusCommandError");
            }
            expect(result.left.status).toEqual(409);
          }),
        ),
    );

    it.effect(
      "allows eventId to disambiguate a shared cardanoTxHash lookup",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const sharedCardanoTxHash = databaseTxHash(
              "deposits.shared-cardano-tx.event-id-disambiguates",
            );
            const first = makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.disambiguates.first",
                0,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            });
            const second = makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.disambiguates.second",
                1,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            });
            yield* insertDeposits([first, second]);

            const resolved = yield* resolveDepositStatusProgram({
              eventId: second[DepositsDB.Columns.ID],
              cardanoTxHash: sharedCardanoTxHash,
            });

            expect(
              resolved[DepositsDB.Columns.ID].equals(
                second[DepositsDB.Columns.ID],
              ),
            ).toEqual(true);
          }),
        ),
    );

    it.effect(
      "projects a deposit into mempool_ledger exactly once by source_event_id",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            yield* insertDeposits([deposit]);
            const mempoolEntry =
              yield* DepositsDB.toMempoolLedgerEntry(deposit);

            yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);
            yield* DepositsDB.markAwaitingAsProjected([
              deposit[DepositsDB.Columns.ID],
            ]);

            const duplicateResult = yield* Effect.either(
              MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]),
            );
            expect(duplicateResult._tag).toEqual("Left");

            const mempoolRows = yield* MempoolLedgerDB.retrieve;
            expect(mempoolRows).toHaveLength(1);
            expect(
              mempoolRows[0]?.[MempoolLedgerDB.Columns.SOURCE_EVENT_ID]?.equals(
                deposit[DepositsDB.Columns.ID],
              ),
            ).toEqual(true);

            const projectedRows = yield* DepositsDB.retrieveProjectedEntries();
            expect(projectedRows).toHaveLength(1);
            expect(projectedRows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
              DepositsDB.Status.Projected,
            );
          }),
        ),
    );

    it.effect(
      "rejects source_event_id payload drift during idempotent projection reconciliation",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            yield* insertDeposits([deposit]);
            const mempoolEntry =
              yield* DepositsDB.toMempoolLedgerEntry(deposit);
            yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);

            const conflictingEntry = {
              ...mempoolEntry,
              [MempoolLedgerDB.Columns.OUTPUT]: databaseFixtureBytes(
                "deposits.projection-reconciliation.conflicting-output",
                80,
              ),
            };

            const result = yield* Effect.either(
              MempoolLedgerDB.reconcileDepositEntries([conflictingEntry]),
            );
            expect(result._tag).toEqual("Left");
          }),
        ),
    );

    it.effect(
      "assigns and clears a projected header hash for a projected deposit",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            const headerHash = databaseFixtureBytes(
              "deposits.projected-header",
              28,
            );
            yield* insertDeposits([deposit]);
            yield* DepositsDB.markAwaitingAsProjected([
              deposit[DepositsDB.Columns.ID],
            ]);
            yield* DepositsDB.markProjectedByEventIds(
              [deposit[DepositsDB.Columns.ID]],
              headerHash,
            );

            const assignedRows = yield* DepositsDB.retrieveAllEntries();
            expect(
              assignedRows[0]?.[
                DepositsDB.Columns.PROJECTED_HEADER_HASH
              ]?.equals(headerHash),
            ).toEqual(true);

            yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
              [deposit[DepositsDB.Columns.ID]],
              headerHash,
            );

            const clearedRows = yield* DepositsDB.retrieveAllEntries();
            expect(
              clearedRows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
            ).toBeNull();
          }),
        ),
    );

    it.effect(
      "treats projection claiming as idempotent once a deposit is already projected",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const deposit = makeDepositEntry();
            yield* insertDeposits([deposit]);
            yield* DepositsDB.markAwaitingAsProjected([
              deposit[DepositsDB.Columns.ID],
            ]);
            yield* DepositsDB.markAwaitingAsProjected([
              deposit[DepositsDB.Columns.ID],
            ]);

            const rows = yield* DepositsDB.retrieveAllEntries();
            expect(rows).toHaveLength(1);
            expect(rows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
              DepositsDB.Status.Projected,
            );
            expect(
              rows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
            ).toBeNull();
          }),
        ),
    );

    it.effect(
      "re-includes an overdue projected deposit whose earlier header assignment was abandoned",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
            const overdueProjectedDeposit = makeDepositEntry({
              [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                currentBlockStartTime.getTime() - 1_000,
              ),
              [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
            });
            yield* insertDeposits([overdueProjectedDeposit]);

            const included = yield* resolveIncludedDepositEntriesForWindow({
              currentBlockStartTime,
              effectiveEndTime: new Date(
                currentBlockStartTime.getTime() + 1_000,
              ),
            });

            expect(included).toHaveLength(1);
            expect(
              included[0]?.[DepositsDB.Columns.ID].equals(
                overdueProjectedDeposit[DepositsDB.Columns.ID],
              ),
            ).toEqual(true);
            expect(included[0]?.[DepositsDB.Columns.STATUS]).toEqual(
              DepositsDB.Status.Projected,
            );
          }),
        ),
    );

    it.effect(
      "fails closed when an overdue deposit was never projected before its window closed",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
            const overdueAwaitingDeposit = makeDepositEntry({
              [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                currentBlockStartTime.getTime() - 1_000,
              ),
            });
            yield* insertDeposits([overdueAwaitingDeposit]);

            const result = yield* Effect.either(
              resolveIncludedDepositEntriesForWindow({
                currentBlockStartTime,
                effectiveEndTime: new Date(
                  currentBlockStartTime.getTime() + 1_000,
                ),
              }),
            );

            expect(result._tag).toEqual("Left");
          }),
        ),
    );
  });
};
