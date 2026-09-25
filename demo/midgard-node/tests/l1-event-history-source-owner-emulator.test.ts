import { randomUUID } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { SqlClient } from "@effect/sql";
import { Deferred, Fiber, Runtime, Schedule } from "effect";
import { expect, it, vi } from "vitest";

import * as Journal from "../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { makeEventHistoryOwner } from "../src/services/event-history-owner.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  advanceEmulatorPastLatestBlockEndTime,
  assetsToValue,
  CML,
  commitConfirmRecoverAndMerge,
  concludePayoutProgram,
  Data,
  Database,
  Effect,
  ensureSeparateCollateralUtxo,
  expectedAuthenticatedEventRoot,
  fetchWithdrawalsOnceProgram,
  initializePayoutProgram,
  paymentCredentialOf,
  payoutStatusProgram,
  resolveEventSettlementProofProgram,
  runNodeCommandProgram,
  SDK,
  submitDepositWithDiagnostics,
  submitWithdrawalWithDiagnostics,
  utxosProgram,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";
import { submitHistoryObservation } from "./helpers/history-projection-observations.js";
import {
  makeRecordedHistoryTransport,
  openHistorySourceOwnerLifecycle,
} from "./helpers/history-source-owner-emulator.js";
import { provideDatabaseLayers } from "./utils.js";

/** Complete address-scope ledger scans, and the most ever acquired at once. */
const ledgerScans = (
  requests: readonly { socket: number; method: string }[],
) => {
  const acquired = new Set<number>();
  let count = 0;
  let maxInFlight = 0;
  for (const { socket, method } of requests) {
    if (method === "acquireLedgerState") {
      acquired.add(socket);
      maxInFlight = Math.max(maxInFlight, acquired.size);
    } else if (method === "releaseLedgerState") acquired.delete(socket);
    else if (method === "queryLedgerState/utxo") count += 1;
  }
  return { count, maxInFlight };
};

/** Successful node classification and actual mature merge establish both
 * settlement frontiers. The observation transport labels remain synthetic. */
it("runs the default history owner through actual bootstrap, continuation, retirement and fresh restart intersection", async () => {
  const h = await openHistorySourceOwnerLifecycle();
  const { fixture, lucidService, globals } = h;
  const context = { fixture, lucidService, globals };
  const lucid = fixture.operatorLucid;
  const pair = SDK.requireEventHistoryContracts(fixture.contracts);
  const command = <A>(effect: Parameters<typeof runNodeCommandProgram<A>>[0]) =>
    runNodeCommandProgram(effect, context);
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    vi.setSystemTime(fixture.emulator.now());
    const ownerAddress = await fixture.depositorLucid.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address: ownerAddress,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      ),
    );
    expect(deposits).toHaveLength(1);
    const deposit = deposits[0]!;
    await h.deployment.chain.awaitLedgerTime(
      Number(deposit.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    const depositBlock = await commitConfirmRecoverAndMerge(context);
    const depositRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      [{ key: deposit.idCbor, value: deposit.infoCbor }],
    );
    expect(depositBlock.queuedHeader.depositsRoot).toBe(depositRoot);
    const depositId = deposit.idCbor.toString("hex");
    const depositResolution = await command(
      resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId: deposit.idCbor,
      }),
    );
    expect(depositResolution.root).toBe(depositRoot);
    expect(depositResolution.settlementRefInput).toEqual(
      depositBlock.settlementUtxo,
    );
    await ensureSeparateCollateralUtxo(lucid);
    const absorbed = await command(
      absorbConfirmedDepositToReserveProgram({ eventId: depositId }),
    );
    expect(absorbed.details.depositOutRef).toBe(
      `${deposit.utxo.txHash}#${deposit.utxo.outputIndex}`,
    );
    expect(h.capture().history.deposits).toHaveLength(0);
    const reserve = (
      await lucid.utxosAt(fixture.contracts.reserve.spendingScriptAddress)
    ).find((output) => output.assets.lovelace === 12_000_000n);
    expect(reserve).toBeDefined();

    const l2State = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(l2State.utxoCount).toBe(1);
    expect(l2State.totals.lovelace).toBe(12_000_000n);
    const target = l2State.utxos[0]!;
    const ownerKey = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Preprod",
      }).paymentKey,
    );
    const ownerAddressData = await Effect.runPromise(
      SDK.addressDataFromBech32(ownerAddress),
    );
    const body: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: target.txHash,
        outputIndex: BigInt(target.outputIndex),
      },
      l2_owner: paymentCredentialOf(ownerAddress).hash,
      l2_value: assetsToValue({ lovelace: 12_000_000n }),
      l1_address: ownerAddressData,
      l1_datum: "NoDatum",
    };
    const submitted = await submitWithdrawalWithDiagnostics(fixture, {
      body,
      signature: SDK.signWithdrawalBody(ownerKey, body),
      refundAddress: ownerAddressData,
      refundDatum: "NoDatum",
    });
    const withdrawals = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
      ),
    );
    expect(withdrawals).toHaveLength(1);
    const withdrawal = withdrawals[0]!;
    expect(submitted.withdrawalEventId).toBe(withdrawal.idCbor.toString("hex"));
    await h.deployment.chain.awaitLedgerTime(
      Number(withdrawal.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    expect((await command(fetchWithdrawalsOnceProgram)).reconciledCount).toBe(
      1,
    );
    const withdrawalBlock = await commitConfirmRecoverAndMerge(context);
    const resolution = await command(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: withdrawal.idCbor,
      }),
    );
    expect(resolution.kind).toBe("withdrawal");
    if (resolution.kind !== "withdrawal")
      throw new Error("Expected withdrawal resolution");
    expect(resolution.validity).toBe("WithdrawalIsValid");
    expect(resolution.settlementRefInput).toEqual(
      withdrawalBlock.settlementUtxo,
    );
    expect(resolution.root).toBe(withdrawalBlock.queuedHeader.withdrawalsRoot);
    const emptyL2 = await Effect.runPromise(
      utxosProgram(ownerAddress).pipe(Effect.provide(Database.layer)),
    );
    expect(emptyL2.utxoCount).toBe(0);
    const eventId = withdrawal.idCbor.toString("hex");
    await command(initializePayoutProgram({ eventId }));
    expect(h.capture().history.withdrawals).toHaveLength(0);
    await ensureSeparateCollateralUtxo(lucid);
    const added = await command(addReserveFundsToPayoutProgram({ eventId }));
    expect(added.details.reserveOutRef).toBe(
      `${reserve!.txHash}#${reserve!.outputIndex}`,
    );
    expect((await command(payoutStatusProgram(eventId))).phase).toBe("funded");
    await command(concludePayoutProgram({ eventId }));
    expect((await command(payoutStatusProgram(eventId))).phase).toBe(
      "concluded",
    );
    expect(
      (await lucid.utxosAt(ownerAddress)).some(
        (output) => output.assets.lovelace === 12_000_000n,
      ),
    ).toBe(true);
    await h.observer.flush();
    expect(h.observer.pendingCount()).toBe(0);
    expect(
      h.transitions.map((transition) => ({
        kind: transition.kind,
        operation: transition.operation,
        reason: transition.retirement?.reason,
      })),
    ).toEqual([
      { kind: "deposit", operation: "InsertOrder", reason: undefined },
      { kind: "deposit", operation: "RetireOrder", reason: "absorbed" },
      { kind: "withdrawal", operation: "InsertOrder", reason: undefined },
      {
        kind: "withdrawal",
        operation: "RetireOrder",
        reason: "payout_initialized",
      },
    ]);
    for (const [kind, original] of [
      ["deposit", deposit],
      ["withdrawal", withdrawal],
    ] as const) {
      const admitted = h.transitions.find(
        (transition) =>
          transition.kind === kind && transition.admission !== undefined,
      )!.admission!;
      const retired = h.transitions.find(
        (transition) =>
          transition.kind === kind && transition.retirement !== undefined,
      )!.retirement!.event;
      expect(retired.idCbor).toBe(original.idCbor.toString("hex"));
      expect(retired.factsCbor).toBe(admitted.factsCbor);
      expect(retired.originalAssetsCbor).toBe(admitted.originalAssetsCbor);
    }
    // Fresh ordinary controls use sorted actual nonce keys to force an Order
    // continuation. They are not asserted to have been included on L2.
    const funding = fixture.depositorLucid;
    funding.overrideUTxOs(await funding.utxosAt(ownerAddress));
    const split = await funding
      .newTx()
      .pay.ToAddress(ownerAddress, { lovelace: 30_000_000n })
      .pay.ToAddress(ownerAddress, { lovelace: 30_000_000n })
      .pay.ToAddress(ownerAddress, { lovelace: 10_000_000n })
      .complete({ localUPLCEval: true });
    const splitReceipt = await submitHistoryObservation(funding, split);
    const nonces = (await funding.utxosAt(ownerAddress)).filter(
      (u) =>
        u.txHash === splitReceipt.transaction.txHash &&
        u.assets.lovelace === 30_000_000n,
    );
    expect(nonces).toHaveLength(2);
    const eventKey = (u: { txHash: string; outputIndex: number }) =>
      Effect.runSync(
        SDK.eventHistoryKey({
          transactionId: u.txHash,
          outputIndex: BigInt(u.outputIndex),
        }),
      );
    nonces.sort((a, b) => eventKey(a).localeCompare(eventKey(b)));
    const controls: SDK.DepositUTxO[] = [];
    for (const nonce of nonces) {
      const nodes = SDK.authenticateHistoryNodes(
        await funding.utxosAt(pair.deposit.list.spendingScriptAddress),
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      );
      const protectedUntil = nodes.reduce(
        (n, { node }) => (node.protected_until > n ? node.protected_until : n),
        0n,
      );
      await h.deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
      vi.setSystemTime(fixture.emulator.now());
      funding.overrideUTxOs(
        (await funding.utxosAt(ownerAddress)).filter(
          (u) =>
            u.datum == null &&
            u.scriptRef == null &&
            (!nonces.some(
              (n) => n.txHash === u.txHash && n.outputIndex === u.outputIndex,
            ) ||
              u.outputIndex === nonce.outputIndex),
        ),
      );
      const built = await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          funding,
          fixture.contracts,
          {
            nonceInput: nonce,
            l2Address: ownerAddress,
            l2Datum: null,
            lovelace: 5_000_000n,
            additionalAssets: {},
            referenceScripts: fixture.referenceScripts.deposit,
          },
        ),
      );
      await submitHistoryObservation(funding, built.tx);
      const orders = await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          funding,
          SDK.eventHistoryDeploymentFromContracts(pair.deposit),
        ),
      );
      controls.push(orders.find((o) => o.assetName === eventKey(nonce))!);
    }
    const currentControls = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        funding,
        SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      ),
    );
    const continued = currentControls.find(
      (o) => o.assetName === controls[0]!.assetName,
    )!;
    expect(continued.utxo.txHash).not.toBe(controls[0]!.utxo.txHash);
    expect(continued.facts).toEqual(controls[0]!.facts);
    expect(continued.originalAssets).toEqual(controls[0]!.originalAssets);
    await h.observer.flush();
    h.observer.restore();
    vi.useRealTimers();
    const transport = makeRecordedHistoryTransport(h);
    const initialIndex = transport.indexOf(deposit.utxo.txHash);
    transport.reveal(initialIndex);
    const changes: { kind: string; head: string; snapshotDigest: string }[] =
      [];
    const ownerEvidence = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor,event_history_block_applications,event_history_live_outputs,event_history_incarnations,event_history_replay_receipts,event_history_authority`;
          yield* sql`CREATE TABLE IF NOT EXISTS history_owner_applied_probe (ordinal bigserial PRIMARY KEY,kind text NOT NULL,head text NOT NULL,snapshot_digest text NOT NULL)`;
          yield* sql`TRUNCATE history_owner_applied_probe`;
          const cache = yield* makeMempoolLedgerCacheService(
            globals,
            MempoolLedgerDB.retrieveSpendable.pipe(
              Effect.provideService(SqlClient.SqlClient, sql),
            ),
          );
          // Holds one forward journal transaction open, so the test can observe
          // the producer gate while that source work is still pending.
          let forwardGate:
            | {
                readonly head: string;
                readonly entered: Deferred.Deferred<void>;
                readonly release: Deferred.Deferred<void>;
              }
            | undefined;
          const makeOwner = (
            refuseRepair = false,
            prepareCompletion?: () => Effect.Effect<void>,
          ) =>
            makeEventHistoryOwner({
              prepareCompletion,
              binding: h.binding,
              histories: pair,
              slotToUnixTime: lucid.slotToUnixTime,
              transport: transport.options,
              heartbeatIntervalMs: 100,
              retainedPointLimit: 128,
              maximumReceiptBytes: 16 * 1024 * 1024,
              leaseDurationMs: 60_000,
              rollbackHorizon: 2160,
              ownerToken: randomUUID(),
              expectedInitializationTransactionHash:
                h.deployment.initialization.txHash,
              cache,
              reconcile: (change) =>
                Effect.gen(function* () {
                  const gate = forwardGate;
                  if (
                    gate !== undefined &&
                    change.kind === "forward" &&
                    change.after.head.id === gate.head
                  ) {
                    yield* Deferred.succeed(gate.entered, undefined);
                    yield* Deferred.await(gate.release);
                  }
                  // A bounded SQL probe in the owner's mandatory transaction. This is
                  // deliberately not production L2/ingestion reconciliation coverage.
                  const loaded = yield* Journal.load(h.binding);
                  expect(loaded?.capture.snapshotDigest).toBe(
                    change.after.capture.snapshotDigest,
                  );
                  yield* sql`INSERT INTO history_owner_applied_probe (kind,head,snapshot_digest) VALUES (${refuseRepair ? "repair_refused" : change.kind},${change.after.head.id},${change.after.capture.snapshotDigest})`;
                  if (refuseRepair) {
                    const authority = yield* sql<{
                      state: string;
                    }>`SELECT state FROM event_history_authority`;
                    expect(authority[0]!.state).toBe("recovering");
                    return yield* Effect.fail(
                      new Error("Fixture dependent repair refused"),
                    );
                  }
                  changes.push({
                    kind: change.kind,
                    head: change.after.head.id,
                    snapshotDigest: change.after.capture.snapshotDigest,
                  });
                }),
            });
          type Owner = Effect.Effect.Success<ReturnType<typeof makeOwner>>;
          const awaitHead = (owner: Owner, index: number) =>
            owner
              .runProducer(() =>
                Effect.gen(function* () {
                  const loaded = yield* Journal.load(h.binding);
                  if (loaded?.head.id !== transport.points[index]!.point.id)
                    return yield* Effect.fail(
                      new Error("Owner is still following accepted branch"),
                    );
                  return loaded;
                }),
              )
              .pipe(
                Effect.retry(Schedule.spaced("10 millis")),
                Effect.raceFirst(
                  owner.awaitStopped.pipe(
                    Effect.zipRight(
                      Effect.fail(
                        new Error(
                          "Owner stopped before reaching its source tip",
                        ),
                      ),
                    ),
                  ),
                ),
                Effect.timeout("20 seconds"),
              );
          const last = transport.points.length - 1;
          const middle = initialIndex + Math.floor((last - initialIndex) / 2);
          expect(middle).toBeGreaterThan(initialIndex);
          expect(middle).toBeLessThan(last);
          const seededStage = yield* Effect.scoped(
            Effect.gen(function* () {
              const owner = yield* makeOwner();
              yield* owner.awaitReady;
              const seeded = yield* owner.runProducer(() =>
                Journal.load(h.binding),
              );
              expect(seeded).not.toBeNull();
              expect(seeded!.head.id).toBe(
                transport.points[initialIndex]!.point.id,
              );
              expect(seeded!.capture.history.deposits).toHaveLength(1);
              expect(seeded!.incarnations).toHaveLength(1);
              const initialReceipts = yield* sql<{
                count: number;
              }>`SELECT count(*)::integer AS count FROM event_history_replay_receipts`;
              expect(initialReceipts[0]!.count).toBe(
                initialIndex -
                  transport.indexOf(h.deployment.initialization.txHash) +
                  1,
              );
              // First start: the single complete scan, never concurrent.
              expect(ledgerScans(transport.requests)).toEqual({
                count: 1,
                maxInFlight: 1,
              });
              // Steady state: new blocks after readiness append at the head of
              // the open gate with no further ledger scan. While a forward block
              // is being journaled the gate stays open; a producer admitted then
              // commits behind the append and completes.
              forwardGate = {
                head: transport.points[middle]!.point.id,
                entered: yield* Deferred.make<void>(),
                release: yield* Deferred.make<void>(),
              };
              transport.reveal(middle);
              yield* Deferred.await(forwardGate.entered).pipe(
                Effect.timeout("20 seconds"),
              );
              // Asserted after release, so a failure cannot strand the append.
              const readyDuring = (yield* owner.frontier).ready;
              const during = yield* Effect.fork(
                Effect.either(owner.runProducer(() => Effect.void)),
              );
              yield* Deferred.succeed(forwardGate.release, undefined);
              forwardGate = undefined;
              expect(readyDuring).toBe(true);
              expect(
                (yield* Fiber.join(during).pipe(Effect.timeout("20 seconds")))
                  ._tag,
              ).toBe("Right");
              const followed = yield* awaitHead(owner, middle);
              expect(followed.head.id).toBe(transport.points[middle]!.point.id);
              expect(ledgerScans(transport.requests).count).toBe(1);
              yield* owner.close;
              return { seeded, initialReceipts };
            }),
          );
          // Restart from N with the chain at N+k: replay the k blocks and become
          // ready at the tip without a scan.
          transport.reveal(last);
          const first = yield* Effect.scoped(
            Effect.gen(function* () {
              const { seeded, initialReceipts } = seededStage;
              const owner = yield* makeOwner();
              yield* owner.awaitReady;
              const finished = (yield* owner.runProducer(() =>
                Journal.load(h.binding),
              ))!;
              expect(finished.head.id).toBe(transport.points[last]!.point.id);
              expect(ledgerScans(transport.requests).count).toBe(1);
              expect(finished.capture.history.deposits).toHaveLength(2);
              expect(finished.capture.history.withdrawals).toHaveLength(0);
              expect(finished.incarnations).toHaveLength(4);
              for (const original of [deposit, withdrawal]) {
                const event = finished.incarnations.find(
                  (i) => i.event.idCbor === original.idCbor.toString("hex"),
                )!;
                expect(event.placement?.retirement).toEqual(
                  expect.objectContaining({
                    at: expect.objectContaining({
                      transactionHash: expect.any(String),
                    }),
                  }),
                );
                expect(event.event.originalAssetsCbor).toBe(
                  Data.to(
                    SDK.assetsToValue(original.originalAssets),
                    SDK.Value,
                  ),
                );
                expect(event.event.factsCbor).toBe(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    plutusConstrFieldCbor(original.utxo.datum!, [3, 0]),
                  ),
                );
              }
              const control = finished.incarnations.find(
                (i) => i.event.idCbor === continued.idCbor.toString("hex"),
              )!;
              expect(control.placement?.current?.outRef).toEqual({
                txHash: continued.utxo.txHash,
                outputIndex: continued.utxo.outputIndex,
              });
              const receiptCount = yield* sql<{
                count: number;
              }>`SELECT count(*)::integer AS count FROM event_history_replay_receipts`;
              expect(receiptCount).toEqual(initialReceipts);
              yield* owner.close;
              const closeState = yield* sql<{
                state: string;
                generation: string;
                owner_token: string;
              }>`SELECT state,generation,owner_token FROM event_history_authority`;
              expect(closeState[0]!.state).toBe("suspended");
              expect(
                yield* Effect.either(owner.runProducer(() => Effect.void)),
              ).toMatchObject({ _tag: "Left" });
              return {
                seeded,
                finished,
                closeState,
                receiptCount: receiptCount[0]!.count,
              };
            }),
          );
          transport.hold();
          const restarted = yield* Effect.scoped(
            Effect.gen(function* () {
              const owner = yield* makeOwner();
              const runtime = yield* Effect.runtime<never>();
              const run = Runtime.runPromise(runtime);
              let ready = false;
              const readyPromise = run(owner.awaitReady).then(() => {
                ready = true;
              });
              yield* Effect.tryPromise(() =>
                vi.waitFor(() => expect(transport.heldCount()).toBe(1), {
                  timeout: 20_000,
                  interval: 10,
                }),
              );
              expect(ready).toBe(false);
              expect(
                yield* Effect.either(owner.runProducer(() => Effect.void)),
              ).toMatchObject({ _tag: "Left" });
              transport.release();
              yield* Effect.tryPromise(() => readyPromise);
              const loaded = yield* owner.runProducer(() =>
                Journal.load(h.binding),
              );
              expect(loaded).toEqual(first.finished);
              yield* owner.close;
              const closeState = yield* sql<{
                state: string;
                generation: string;
                owner_token: string;
              }>`SELECT state,generation,owner_token FROM event_history_authority`;
              expect(closeState[0]!.state).toBe("suspended");
              expect(BigInt(closeState[0]!.generation)).toBeGreaterThan(
                BigInt(first.closeState[0]!.generation),
              );
              expect(closeState[0]!.owner_token).not.toBe(
                first.closeState[0]!.owner_token,
              );
              expect(
                yield* Effect.either(owner.runProducer(() => Effect.void)),
              ).toMatchObject({ _tag: "Left" });
              return { loaded, closeState };
            }),
          );
          const refusedRepair = yield* Effect.scoped(
            Effect.gen(function* () {
              const before = yield* Journal.load(h.binding);
              const owner = yield* makeOwner(true);
              const readiness = yield* Effect.either(owner.awaitReady);
              expect(readiness).toMatchObject({ _tag: "Left" });
              expect(inspect(readiness, { depth: 12 })).toContain(
                "Fixture dependent repair refused",
              );
              expect(
                yield* Effect.either(owner.runProducer(() => Effect.void)),
              ).toMatchObject({ _tag: "Left" });
              const marker = yield* sql<{
                count: number;
              }>`SELECT count(*)::integer AS count FROM history_owner_applied_probe WHERE kind='repair_refused'`;
              expect(marker[0]!.count).toBe(0);
              const after = yield* Journal.load(h.binding);
              expect(after).toEqual(before);
              const authority = yield* sql<{
                state: string;
              }>`SELECT state FROM event_history_authority`;
              expect(authority[0]!.state).not.toBe("ready");
              yield* owner.close;
              const closed = yield* sql<{
                state: string;
              }>`SELECT state FROM event_history_authority`;
              expect(closed[0]!.state).toBe("suspended");
              expect(
                yield* Effect.either(owner.runProducer(() => Effect.void)),
              ).toMatchObject({ _tag: "Left" });
              return {
                error: inspect(readiness, { depth: 12 }),
                rolledBackProbeCount: marker[0]!.count,
                unchangedCheckpoint: after,
                authority,
                closed,
              };
            }),
          );
          const cancelledPreparations = [];
          for (const mode of ["close", "source-unavailable"] as const) {
            const outcome = yield* Effect.scoped(
              Effect.gen(function* () {
                const before = yield* Journal.load(h.binding);
                const entered = yield* Deferred.make<void>();
                const interrupted = yield* Deferred.make<void>();
                const owner = yield* makeOwner(false, () =>
                  Deferred.succeed(entered, undefined).pipe(
                    Effect.zipRight(Effect.never),
                    Effect.ensuring(Deferred.succeed(interrupted, undefined)),
                  ),
                );
                yield* Deferred.await(entered).pipe(
                  Effect.timeout("5 seconds"),
                );
                expect(
                  (yield* Effect.either(owner.runProducer(() => Effect.void)))
                    ._tag,
                ).toBe("Left");
                if (mode === "close") {
                  yield* owner.close.pipe(Effect.timeout("2 seconds"));
                } else {
                  transport.close();
                  expect((yield* Effect.either(owner.awaitStopped))._tag).toBe(
                    "Left",
                  );
                  yield* owner.close.pipe(Effect.timeout("2 seconds"));
                }
                yield* Deferred.await(interrupted).pipe(
                  Effect.timeout("2 seconds"),
                );
                expect((yield* Effect.either(owner.awaitReady))._tag).toBe(
                  "Left",
                );
                expect(yield* Journal.load(h.binding)).toEqual(before);
                const [state] = yield* sql<{
                  state: string;
                }>`SELECT state FROM event_history_authority`;
                expect(state!.state).toBe("suspended");
                return {
                  mode,
                  interrupted: true,
                  unchangedCheckpoint: true,
                  state: state!.state,
                };
              }),
            );
            cancelledPreparations.push(outcome);
          }
          // No restart with a non-empty journal ever scanned the ledger.
          expect(ledgerScans(transport.requests)).toEqual({
            count: 1,
            maxInFlight: 1,
          });
          const count = yield* sql<{
            count: number;
          }>`SELECT count(*)::integer AS count FROM event_history_replay_receipts`;
          expect(count[0]!.count).toBe(first.receiptCount);
          const replayReceipts =
            yield* sql`SELECT encode(binding_digest,'hex') AS binding_digest,encode(manifest_id,'hex') AS manifest_id,
            encode(block_hash,'hex') AS block_hash,block_slot,block_height,encode(parent_hash,'hex') AS parent_hash,
            encode(predecessor_hash,'hex') AS predecessor_hash,encode(activation_hash,'hex') AS activation_hash,blocks_replayed,
            receipt,encode(receipt_digest,'hex') AS receipt_digest,encode(range_digest,'hex') AS range_digest
            FROM event_history_replay_receipts ORDER BY block_height`;
          const applications =
            yield* sql`SELECT encode(binding_digest,'hex') AS binding_digest,encode(block_hash,'hex') AS block_hash,
            application_revision,encode(parent_hash,'hex') AS parent_hash,parent_application_revision,block_slot,block_height,
            encode(before_snapshot_digest,'hex') AS before_snapshot_digest,encode(after_snapshot_digest,'hex') AS after_snapshot_digest,
            ledger_receipt,encode(ledger_receipt_digest,'hex') AS ledger_receipt_digest,undo_record,encode(undo_digest,'hex') AS undo_digest,canonical
            FROM event_history_block_applications WHERE canonical ORDER BY block_height`;
          expect(replayReceipts).toHaveLength(first.receiptCount);
          expect(applications).toHaveLength(
            transport.points.length - initialIndex - 1,
          );
          const probe =
            yield* sql`SELECT kind,head,snapshot_digest FROM history_owner_applied_probe ORDER BY ordinal`;
          yield* sql`DROP TABLE history_owner_applied_probe`;
          return {
            first,
            restarted,
            cancelledPreparations,
            replayReceipts,
            applications,
            refusedRepair,
            probe,
            changes,
            requests: transport.requests,
            points: transport.points,
          };
        }),
      ),
    );
    expect(changes.some((c) => c.kind === "seed")).toBe(true);
    expect(changes.some((c) => c.kind === "forward")).toBe(true);
    expect(
      changes.filter((c) => c.kind === "resume").length,
    ).toBeGreaterThanOrEqual(2);
    const evidencePath = process.env.MIDGARD_HISTORY_SOURCE_OWNER_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            scope:
              "Real accepted lifecycle plus default source-owner bootstrap/forward/restart; synthetic transport over recorded provider snapshots; SQL probe reconciliation only, no production consumer repair claim",
            binding: h.binding,
            genesis: h.genesis,
            status: "passed",
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            deploymentInfoSha256: h.deploymentInfoSha256,
            protocolParameters: h.deployment.manifest.cardanoProtocolParameters,
            receipts: h.receipts,
            batches: h.batches,
            publicationTransactions: [...h.publications],
            ownerEvidence,
            transitions: h.transitions,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  } catch (cause) {
    const evidencePath = process.env.MIDGARD_HISTORY_SOURCE_OWNER_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            status: "failed",
            cause: inspect(cause, { depth: 12 }),
            manifestId: h.deployment.manifest.manifestId,
            blueprintSha256: h.deployment.manifest.artifacts.blueprintHash,
            receipts: h.receipts,
            batches: h.batches,
            transitions: h.transitions,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
    throw cause;
  } finally {
    h.observer.restore();
    vi.useRealTimers();
  }
});
