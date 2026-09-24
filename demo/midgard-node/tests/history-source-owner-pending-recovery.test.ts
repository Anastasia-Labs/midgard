import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber, Option, Schedule } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { sqlErrorToDatabaseError } from "../src/database/utils/common.js";
import { makeEventHistoryOwner } from "../src/services/event-history-owner.js";
import type { HistoryRecoveryPreparation } from "../src/services/event-history-recovery.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { historyOutputObservation } from "./helpers/history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";
import { openHistorySourceOwnerLifecycle } from "./helpers/history-source-owner-emulator.js";
import { provideDatabaseLayers } from "./utils.js";

// Accepted emulator initialization and actually observed empty intervals feed
// production source decoding, replay, journal and SQL authority. Branch ancestry
// and the pending disposition callback are controlled models. This does not
// establish a production disposition for an orphaned published L2 block.
it("retains streamed recovery evidence while pending, opens only the current frontier, and fails closed on source loss", async () => {
  const h = await openHistorySourceOwnerLifecycle();
  await h.observer.flush();
  h.observer.restore();
  vi.useRealTimers();
  const source = makeRollbackHistoryTransport(h);
  const pair = SDK.requireEventHistoryContracts(h.fixture.contracts);
  const addresses = [
    h.binding.hubAddress,
    ...Object.values(h.binding.deployments).flatMap((deployment) => [
      deployment.address,
      deployment.retentionAddress,
    ]),
  ];
  const interval = async () => {
    h.fixture.emulator.awaitBlock(1);
    return {
      observations: [],
      observedSlot: h.fixture.emulator.slot,
      observedHeight: h.fixture.emulator.blockHeight,
      outputs: (
        await Promise.all(
          addresses.map((address) => h.fixture.operatorLucid.utxosAt(address)),
        )
      )
        .flat()
        .map(historyOutputObservation),
    };
  };
  try {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.scoped(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger,
        deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits,
        pending_block_finalization_withdrawals, event_history_cursor,
        event_history_block_applications, event_history_live_outputs,
        event_history_incarnations, event_history_replay_receipts,
        event_history_authority CASCADE`;
            yield* sql`CREATE TABLE IF NOT EXISTS history_pending_recovery_probe
        (head text PRIMARY KEY, revision text NOT NULL)`;
            yield* sql`TRUNCATE history_pending_recovery_probe`;
            const dependent = sql<{
              head: string;
              revision: string;
            }>`SELECT head, revision
        FROM history_pending_recovery_probe ORDER BY head`;
            const cache = yield* makeMempoolLedgerCacheService(
              h.globals,
              dependent.pipe(
                Effect.map((rows): MempoolLedgerDB.EntryWithTimeStamp[] =>
                  rows.map(({ head }) => ({
                    tx_id: Buffer.from(head, "hex"),
                    outref: Buffer.concat([
                      Buffer.from(head, "hex"),
                      Buffer.alloc(4),
                    ]),
                    output: Buffer.from(head, "hex"),
                    address: "addr_test1_pending_recovery_probe",
                    source_event_id: null,
                    [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
                  })),
                ),
                sqlErrorToDatabaseError(
                  "history_pending_recovery_probe",
                  "load cache probe",
                ),
              ),
            );
            let pending = false;
            const reason = "Awaiting source evidence for dependent disposition";
            const prepared: string[] = [];
            let releaseAtSlot: number | undefined;
            let producerDrained = true;
            const pendingAttempts: {
              head: string;
              revision: string;
              preparation: HistoryRecoveryPreparation;
              completed: boolean;
            }[] = [];
            const makeOwner = () =>
              makeEventHistoryOwner({
                binding: h.binding,
                histories: pair,
                slotToUnixTime: h.fixture.operatorLucid.slotToUnixTime,
                transport: source.options,
                heartbeatIntervalMs: 100,
                retainedPointLimit: 128,
                maximumReceiptBytes: 16 * 1024 * 1024,
                leaseDurationMs: 60_000,
                ownerToken: randomUUID(),
                expectedInitializationTransactionHash:
                  h.deployment.initialization.txHash,
                cache,
                preparePendingReconciliation: (checkpoint, preparation) =>
                  Effect.gen(function* () {
                    yield* preparation.assertCurrent;
                    expect(producerDrained).toBe(true);
                    expect(pending).toBe(true);
                    expect(
                      Option.isNone(
                        yield* Effect.serviceOption(
                          SqlClient.TransactionConnection,
                        ),
                      ),
                    ).toBe(true);
                    expect(
                      (yield* Effect.either(
                        Authority.requireRecoveryTransaction,
                      ))._tag,
                    ).toBe("Left");
                    const authority = yield* Authority.retrieve;
                    expect(Option.isSome(authority)).toBe(true);
                    if (Option.isNone(authority))
                      throw new Error("Missing preparation authority");
                    expect(authority.value.state).toBe("recovering");
                    expect(authority.value.generation).toBe(
                      preparation.token.generation,
                    );
                    expect(authority.value.owner_token).toBe(
                      preparation.token.ownerToken,
                    );
                    expect(preparation.token.deploymentIdentity).toBe(
                      h.binding.manifestId,
                    );
                    expect(
                      (yield* Effect.either(cache.withClaimLock(Effect.void)))
                        ._tag,
                    ).toBe("Left");
                    const before = yield* dependent;
                    const attempt = {
                      head: checkpoint.head.id,
                      revision: checkpoint.revision,
                      preparation,
                      completed: false,
                    };
                    pendingAttempts.push(attempt);
                    // Controlled evidence-availability model, without SQL mutation:
                    // only the owner's subsequent reconcile may publish the repair.
                    if (checkpoint.head.slot === releaseAtSlot) pending = false;
                    expect(yield* dependent).toEqual(before);
                    yield* preparation.assertCurrent;
                    attempt.completed = true;
                  }),
                prepareCompletion: (checkpoint) =>
                  Effect.sync(() => {
                    expect(pending).toBe(false);
                    prepared.push(checkpoint.head.id);
                  }),
                reconcile: ({ after }) =>
                  Effect.gen(function* () {
                    const actual = yield* Journal.load(h.binding);
                    expect(actual?.revision).toBe(after.revision);
                    expect(actual?.capture.snapshotDigest).toBe(
                      after.capture.snapshotDigest,
                    );
                    // Pending changes no dependent SQL. The owner must still commit the
                    // authenticated history journal and consume subsequent source blocks.
                    if (pending) return { status: "pending" as const, reason };
                    yield* sql`DELETE FROM history_pending_recovery_probe`;
                    yield* sql`INSERT INTO history_pending_recovery_probe (head, revision)
            VALUES (${after.head.id}, ${after.revision})`;
                    return undefined;
                  }),
              });
            let owner = yield* makeOwner();
            const waitCheckpoint = (id: string) =>
              Effect.gen(function* () {
                const checkpoint = yield* Journal.load(h.binding);
                if (checkpoint?.head.id !== id)
                  return yield* Effect.fail(
                    new Error("Source has not reached checkpoint"),
                  );
                const status = yield* owner.reconciliationStatus;
                if (status === undefined)
                  return yield* Effect.fail(
                    new Error(
                      "Pending disposition has not been reauthenticated",
                    ),
                  );
                expect(status).toEqual({
                  status: "pending",
                  reason,
                });
                return checkpoint;
              }).pipe(
                Effect.retry(Schedule.spaced("10 millis")),
                Effect.timeout("15 seconds"),
              );
            const waitPendingAttempt = (id: string, after = 0) =>
              Effect.suspend(() => {
                const attempt = pendingAttempts
                  .slice(after)
                  .find((value) => value.head === id && value.completed);
                return attempt === undefined
                  ? Effect.fail(
                      new Error("Pending preparation has not completed"),
                    )
                  : Effect.succeed(attempt);
              }).pipe(
                Effect.retry(Schedule.spaced("10 millis")),
                Effect.timeout("15 seconds"),
              );
            const assertFenced = Effect.gen(function* () {
              const authority = yield* Authority.retrieve;
              expect(Option.isSome(authority) && authority.value.state).toBe(
                "recovering",
              );
              expect(
                (yield* Effect.either(
                  owner.runProducer(
                    () => sql`DELETE FROM history_pending_recovery_probe`,
                  ),
                ))._tag,
              ).toBe("Left");
              expect(
                (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
              ).toBe("Left");
            });
            yield* owner.awaitReady.pipe(Effect.timeout("30 seconds"));
            const ancestor = source.points.at(-1)!.point;
            h.batches.push(yield* Effect.promise(interval));
            const displaced = source.appendAccepted();
            yield* owner
              .awaitReadyAt(displaced)
              .pipe(Effect.timeout("15 seconds"));
            const originalDependent = yield* dependent;
            expect(originalDependent).toHaveLength(1);
            expect(originalDependent[0]!.head).toBe(displaced.id);
            const preparedBeforeRollback = [...prepared];
            const producerEntered = yield* Deferred.make<void>();
            const releaseProducer = yield* Deferred.make<void>();
            producerDrained = false;
            const draining = yield* Effect.fork(
              owner
                .runProducer(() =>
                  Effect.gen(function* () {
                    yield* Deferred.succeed(producerEntered, undefined);
                    yield* Deferred.await(releaseProducer);
                    producerDrained = true;
                  }),
                )
                .pipe(Effect.either),
            );
            yield* Deferred.await(producerEntered);
            pending = true;
            source.rollbackTo(ancestor.id);
            yield* Effect.gen(function* () {
              const claim = yield* Effect.either(
                cache.withClaimLock(Effect.void),
              );
              if (claim._tag !== "Left")
                return yield* Effect.fail(
                  new Error("Source has not fenced the old cache"),
                );
              return undefined;
            }).pipe(
              Effect.retry(Schedule.spaced("10 millis")),
              Effect.timeout("15 seconds"),
            );
            expect(pendingAttempts).toEqual([]);
            expect(yield* dependent).toEqual(originalDependent);
            yield* Deferred.succeed(releaseProducer, undefined);
            expect((yield* Fiber.join(draining))._tag).toBe("Left");
            const rolledBack = yield* waitCheckpoint(ancestor.id);
            const ancestorAttempt = yield* waitPendingAttempt(ancestor.id);
            yield* assertFenced;
            expect(yield* dependent).toEqual(originalDependent);
            expect(prepared).toEqual(preparedBeforeRollback);

            const laterOne = source.appendFork(yield* Effect.promise(interval));
            const checkpointOne = yield* waitCheckpoint(laterOne.id);
            yield* waitPendingAttempt(laterOne.id);
            expect(
              (yield* Effect.either(ancestorAttempt.preparation.assertCurrent))
                ._tag,
            ).toBe("Left");
            const laterTwo = source.appendFork(yield* Effect.promise(interval));
            const checkpointTwo = yield* waitCheckpoint(laterTwo.id);
            const beforeRestartAttempt = yield* waitPendingAttempt(laterTwo.id);
            expect(BigInt(checkpointOne.revision)).toBeGreaterThan(
              BigInt(rolledBack.revision),
            );
            expect(BigInt(checkpointTwo.revision)).toBeGreaterThan(
              BigInt(checkpointOne.revision),
            );
            const retained = yield* sql<{
              block_hash: Buffer;
            }>`SELECT block_hash
        FROM event_history_block_applications WHERE canonical
          AND block_hash IN (${Buffer.from(laterOne.id, "hex")}, ${Buffer.from(laterTwo.id, "hex")})`;
            expect(
              retained
                .map(({ block_hash }) => block_hash.toString("hex"))
                .sort(),
            ).toEqual([laterOne.id, laterTwo.id].sort());
            yield* assertFenced;
            expect(yield* dependent).toEqual(originalDependent);
            expect(prepared).toEqual(preparedBeforeRollback);

            // Restart against the persisted pending head. The in-memory status
            // begins empty, so it must be refreshed before native preparation.
            yield* owner.close;
            expect(
              (yield* Effect.either(
                beforeRestartAttempt.preparation.assertCurrent,
              ))._tag,
            ).toBe("Left");
            const attemptsBeforeRestart = pendingAttempts.length;
            owner = yield* makeOwner();
            const restartedPending = yield* waitCheckpoint(laterTwo.id);
            expect(restartedPending.revision).toBe(checkpointTwo.revision);
            const restartedAttempt = yield* waitPendingAttempt(
              laterTwo.id,
              attemptsBeforeRestart,
            );
            expect(restartedAttempt.preparation.token.ownerToken).not.toBe(
              beforeRestartAttempt.preparation.token.ownerToken,
            );
            expect(
              BigInt(restartedAttempt.preparation.token.generation),
            ).toBeGreaterThan(
              BigInt(beforeRestartAttempt.preparation.token.generation),
            );
            expect(restartedAttempt.revision).toBe(checkpointTwo.revision);
            yield* assertFenced;
            expect(yield* dependent).toEqual(originalDependent);
            expect(prepared).toEqual(preparedBeforeRollback);

            const currentBatch = yield* Effect.promise(interval);
            releaseAtSlot = currentBatch.observedSlot;
            const current = source.appendFork(currentBatch);
            const ready = yield* owner
              .awaitReadyAt(current)
              .pipe(Effect.timeout("15 seconds"));
            expect(ready.point).toEqual(current);
            expect(yield* owner.reconciliationStatus).toBeUndefined();
            const releaseAttempt = yield* waitPendingAttempt(current.id);
            expect(releaseAttempt.revision).toBe(ready.checkpointRevision);
            const readyAuthority = yield* Authority.retrieve;
            expect(
              Option.isSome(readyAuthority) && readyAuthority.value.generation,
            ).toBe(releaseAttempt.preparation.token.generation);
            expect(
              (yield* Effect.either(restartedAttempt.preparation.assertCurrent))
                ._tag,
            ).toBe("Left");
            expect(prepared.slice(preparedBeforeRollback.length)).toEqual([
              current.id,
            ]);
            expect(
              (yield* Effect.either(owner.awaitReadyAt(laterOne)))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(owner.awaitReadyAt(laterTwo)))._tag,
            ).toBe("Left");
            expect(yield* owner.runProducer(() => dependent)).toEqual([
              { head: current.id, revision: ready.checkpointRevision },
            ]);
            const cached = yield* cache.withClaimLock(
              cache.currentState.pipe(
                Effect.map((state) =>
                  [...state.values()].map((value) => value.toString("hex")),
                ),
              ),
            );
            expect(cached).toEqual([current.id]);

            releaseAtSlot = undefined;
            pending = true;
            source.rollbackTo(laterTwo.id);
            const attemptsBeforeFinalRollback = pendingAttempts.length;
            yield* waitCheckpoint(laterTwo.id);
            const lossAttempt = yield* waitPendingAttempt(
              laterTwo.id,
              attemptsBeforeFinalRollback,
            );
            yield* assertFenced;
            const beforeSourceLoss = yield* dependent;
            const preparedBeforeLoss = [...prepared];
            source.close();
            expect(
              (yield* owner.awaitStopped.pipe(
                Effect.either,
                Effect.timeout("15 seconds"),
              ))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(owner.awaitReadyAt(laterTwo)))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(owner.runProducer(() => Effect.void)))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
            ).toBe("Left");
            expect(yield* dependent).toEqual(beforeSourceLoss);
            expect(prepared).toEqual(preparedBeforeLoss);
            expect(
              (yield* Effect.either(lossAttempt.preparation.assertCurrent))
                ._tag,
            ).toBe("Left");
            expect((yield* Journal.load(h.binding))?.head.id).toBe(laterTwo.id);
          }),
        ),
      ),
    );
  } finally {
    source.close();
    h.observer.restore();
    vi.useRealTimers();
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DROP TABLE IF EXISTS history_pending_recovery_probe`;
        }),
      ),
    );
  }
});
