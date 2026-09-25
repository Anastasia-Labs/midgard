import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Deferred,
  Effect,
  Either,
  Fiber,
  HashMap,
  Logger,
  Option,
  Schedule,
} from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import {
  HISTORY_READY_MAXIMUM_LAG_BLOCKS,
  type HistoryOwnerChange,
  type HistoryOwnerCoverage,
  makeEventHistoryOwner,
} from "../src/services/event-history-owner.js";
import { assertHistoryProducer } from "../src/services/event-history-producer.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { historyOutputObservation } from "./helpers/history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";
import { openHistorySourceOwnerLifecycle } from "./helpers/history-source-owner-emulator.js";
import { provideDatabaseLayers } from "./utils.js";

// The recovery candidate predicate itself is exercised against real pending
// headers in event-history-retired-membership.test.ts. Here the owner's own
// wiring of its hold slot into retention is what is under test, so the slot
// may be pinned; unset, the real single-sourced query answers.
const recoveryHold = vi.hoisted(() => ({
  slot: undefined as number | undefined,
}));
vi.mock(
  "../src/services/history-signed-header-recovery.js",
  async (importOriginal) => {
    const { Effect } = await import("effect");
    const actual =
      await importOriginal<
        typeof import("../src/services/history-signed-header-recovery.js")
      >();
    return {
      ...actual,
      signedHeaderRecoveryHoldSlot: (bindingDigest: string) =>
        recoveryHold.slot === undefined
          ? actual.signedHeaderRecoveryHoldSlot(bindingDigest)
          : Effect.succeed(recoveryHold.slot),
    };
  },
);

const ledgerScans = (requests: readonly { method: string }[]) =>
  requests.filter(({ method }) => method === "queryLedgerState/utxo").length;
const networkTipReads = (requests: readonly { method: string }[]) =>
  requests.filter(({ method }) => method === "queryNetwork/tip").length;
const until = (condition: () => boolean) =>
  Effect.suspend(() =>
    condition() ? Effect.void : Effect.fail(new Error("Not yet")),
  ).pipe(
    Effect.retry(Schedule.spaced("10 millis")),
    Effect.timeout("15 seconds"),
  );
const truncate = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger,
    deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits,
    pending_block_finalization_withdrawals, event_history_cursor,
    event_history_block_applications, event_history_live_outputs,
    event_history_incarnations, event_history_replay_receipts,
    event_history_authority CASCADE`;
});

const openLifecycle = async () => {
  const h = await openHistorySourceOwnerLifecycle();
  await h.observer.flush();
  h.observer.restore();
  vi.useRealTimers();
  const addresses = [
    h.binding.hubAddress,
    ...Object.values(h.binding.deployments).flatMap((deployment) => [
      deployment.address,
      deployment.retentionAddress,
    ]),
  ];
  // An actually observed empty interval: one emulator block, complete outputs.
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
  return { h, interval };
};

type Gate = {
  entered: Deferred.Deferred<void>;
  release: Deferred.Deferred<void>;
};
const gate = Effect.gen(function* () {
  return {
    entered: yield* Deferred.make<void>(),
    release: yield* Deferred.make<void>(),
  } satisfies Gate;
});

// Accepted emulator initialization and observed empty intervals feed production
// source decoding, replay, journal and SQL authority. Branch ancestry, a lagging
// network-tip answer and the recovery hold slot are controlled models.
it("restarts near its head, appends at an open gate without recovery, ignores a lagging tip, bounds follower lag, holds retention visibly and refuses a rollback past its anchor", async () => {
  const { h, interval } = await openLifecycle();
  h.batches.push(await interval());
  const source = makeRollbackHistoryTransport(h);
  const logs: { message: string; annotations: Record<string, unknown> }[] = [];
  const capture = Logger.make(({ message, annotations }) => {
    if (HashMap.has(annotations, "event"))
      logs.push({
        message: (Array.isArray(message) ? message : [message]).join(" "),
        annotations: Object.fromEntries(annotations),
      });
  });
  try {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.scoped(
          Effect.gen(function* () {
            yield* truncate;
            const sql = yield* SqlClient.SqlClient;
            const cache = yield* makeMempoolLedgerCacheService(
              h.globals,
              MempoolLedgerDB.retrieveSpendable.pipe(
                Effect.provideService(SqlClient.SqlClient, sql),
              ),
            );
            const changes: {
              kind: HistoryOwnerChange["kind"];
              head: string;
            }[] = [];
            let completionGate: Gate | undefined;
            let forwardGate: (Gate & { id: string }) | undefined;
            // Full recoveries: prepareCompletion runs once per closed gate.
            let preparations = 0;
            const makeOwner = (rollbackHorizon = 2160) =>
              makeEventHistoryOwner({
                binding: h.binding,
                histories: SDK.requireEventHistoryContracts(
                  h.fixture.contracts,
                ),
                slotToUnixTime: h.fixture.operatorLucid.slotToUnixTime,
                transport: source.options,
                heartbeatIntervalMs: 100,
                retainedPointLimit: 128,
                maximumReceiptBytes: 16 * 1024 * 1024,
                leaseDurationMs: 60_000,
                rollbackHorizon,
                ownerToken: randomUUID(),
                expectedInitializationTransactionHash:
                  h.deployment.initialization.txHash,
                cache,
                reconcile: (change) =>
                  Effect.gen(function* () {
                    changes.push({
                      kind: change.kind,
                      head: change.after.head.id,
                    });
                    const held = forwardGate;
                    if (
                      held !== undefined &&
                      change.kind === "forward" &&
                      change.after.head.id === held.id
                    ) {
                      yield* Deferred.succeed(held.entered, undefined);
                      yield* Deferred.await(held.release);
                    }
                  }),
                prepareCompletion: () =>
                  Effect.gen(function* () {
                    preparations += 1;
                    const held = completionGate;
                    completionGate = undefined;
                    if (held === undefined) return;
                    yield* Deferred.succeed(held.entered, undefined);
                    yield* Deferred.await(held.release);
                  }),
              });
            const produces = (
              owner: Effect.Effect.Success<ReturnType<typeof makeOwner>>,
            ) =>
              Effect.either(owner.runProducer(() => Effect.void)).pipe(
                Effect.map((result) => result._tag),
              );
            const load = Journal.load(h.binding).pipe(
              Effect.map((checkpoint) => {
                if (checkpoint === null) throw new Error("Missing checkpoint");
                return checkpoint;
              }),
            );
            const extend = Effect.gen(function* () {
              return source.appendFork(yield* Effect.promise(interval));
            });
            const advance = Effect.gen(function* () {
              h.batches.push(yield* Effect.promise(interval));
              return source.appendAccepted();
            });
            const authority = Authority.retrieve.pipe(
              Effect.map((row) => {
                if (Option.isNone(row)) throw new Error("Missing authority");
                return row.value;
              }),
            );
            const frontierWhere = (
              owner: Effect.Effect.Success<ReturnType<typeof makeOwner>>,
              condition: (
                frontier: Effect.Effect.Success<typeof owner.frontier>,
              ) => boolean,
            ) =>
              owner.frontier.pipe(
                Effect.filterOrFail(condition, () => new Error("Not yet")),
                Effect.retry(Schedule.spaced("10 millis")),
                Effect.timeout("15 seconds"),
              );
            // Held inside a registered producer after it has read its coverage.
            const holdProducer = (
              owner: Effect.Effect.Success<ReturnType<typeof makeOwner>>,
            ) =>
              Effect.gen(function* () {
                const running = yield* Deferred.make<HistoryOwnerCoverage>();
                const release = yield* Deferred.make<void>();
                // A failed assertion must not leave close draining it forever.
                yield* Effect.addFinalizer(() =>
                  Deferred.succeed(release, undefined),
                );
                const fiber = yield* Effect.fork(
                  Effect.either(
                    owner.runProducer((token, assertCurrent, coverage) =>
                      Effect.gen(function* () {
                        yield* Deferred.succeed(running, coverage);
                        yield* Deferred.await(release);
                        yield* assertCurrent;
                        // Its SQL write: the Ready row lock plus the coverage
                        // check against the journal as it now stands.
                        yield* assertHistoryProducer({ token, coverage });
                        return coverage;
                      }),
                    ),
                  ),
                );
                const coverage = yield* Deferred.await(running).pipe(
                  Effect.timeout("15 seconds"),
                );
                return { coverage, release, fiber };
              });

            let owner = yield* makeOwner();
            yield* owner.awaitReady.pipe(Effect.timeout("30 seconds"));
            expect(ledgerScans(source.requests)).toBe(1);
            for (let n = 0; n < 3; n++) {
              h.batches.push(yield* Effect.promise(interval));
              yield* owner
                .awaitReadyAt(source.appendAccepted())
                .pipe(Effect.timeout("15 seconds"));
            }

            // 3. A heartbeat answering with an older, lower tip than ChainSync
            // already reported does not regress the frontier or readiness.
            const head = source.points.at(-1)!.point;
            source.pinNetworkTip(source.points.at(-2)!.point);
            const reads = networkTipReads(source.requests);
            yield* until(() => networkTipReads(source.requests) >= reads + 6);
            yield* Effect.sleep("50 millis");
            expect(yield* produces(owner)).toBe("Right");
            expect(
              (yield* owner.awaitReadyAt(head).pipe(Effect.timeout("1 second")))
                .point,
            ).toEqual(head);
            expect(yield* owner.frontier).toMatchObject({
              ready: true,
              headHeight: head.height,
              tipHeight: head.height,
              lagBlocks: 0,
            });
            source.pinNetworkTip(undefined);

            // Append. Appending at the head never closes the gate. A producer that
            // read its journaled prefix before two forward blocks completes
            // after them; readiness stays open throughout, and no append runs a
            // recovery (same generation, no preparation, no scan).
            const readiness: boolean[] = [];
            const sampler = yield* Effect.fork(
              owner.frontier.pipe(
                Effect.tap(({ ready }) =>
                  Effect.sync(() => readiness.push(ready)),
                ),
                Effect.repeat(Schedule.spaced("5 millis")),
              ),
            );
            const readyGeneration = (yield* authority).generation;
            const readyPreparations = preparations;
            const spanning = yield* holdProducer(owner);
            expect(spanning.coverage.point).toEqual(head);
            let appended = head;
            for (let n = 0; n < 2; n++) {
              appended = yield* advance;
              yield* owner
                .awaitReadyAt(appended)
                .pipe(Effect.timeout("15 seconds"));
            }
            yield* Deferred.succeed(spanning.release, undefined);
            const spanned = yield* Fiber.join(spanning.fiber);
            expect(spanned._tag).toBe("Right");
            yield* Fiber.interrupt(sampler);
            expect(readiness.length).toBeGreaterThan(0);
            expect(readiness.every(Boolean)).toBe(true);
            const afterAppends = yield* authority;
            expect(afterAppends.state).toBe("ready");
            expect(afterAppends.generation).toBe(readyGeneration);
            expect(afterAppends.point_hash?.toString("hex")).toBe(appended.id);
            expect(Number(afterAppends.point_slot)).toBe(appended.slot);
            expect(preparations).toBe(readyPreparations);
            expect(ledgerScans(source.requests)).toBe(1);

            // Lag. A source tip more than the bound ahead of the journal head
            // refuses new producers, visibly and without closing the gate;
            // catching up to within the bound admits them again.
            logs.length = 0;
            source.pinNetworkTip({
              id: "ab".repeat(32),
              slot: appended.slot + 1_000,
              height: appended.height + HISTORY_READY_MAXIMUM_LAG_BLOCKS + 1,
            });
            yield* frontierWhere(
              owner,
              ({ lagBlocks }) => lagBlocks > HISTORY_READY_MAXIMUM_LAG_BLOCKS,
            );
            expect(yield* owner.frontier).toMatchObject({
              ready: true,
              headHeight: appended.height,
              lagBlocks: HISTORY_READY_MAXIMUM_LAG_BLOCKS + 1,
            });
            const behind = yield* Effect.either(
              owner.runProducer(() => Effect.void),
            );
            expect(behind._tag).toBe("Left");
            expect(
              String(Either.getLeft(behind).pipe(Option.getOrThrow)),
            ).toMatch(/History follower is 6 blocks behind the source tip/);
            source.pinNetworkTip(undefined);
            appended = yield* advance;
            yield* owner
              .awaitReadyAt(appended)
              .pipe(Effect.timeout("15 seconds"));
            expect((yield* owner.frontier).lagBlocks).toBe(
              HISTORY_READY_MAXIMUM_LAG_BLOCKS,
            );
            expect(yield* produces(owner)).toBe("Right");
            expect(
              logs
                .filter(
                  ({ annotations }) =>
                    annotations.event === "history_follower_lag",
                )
                .map(({ annotations }) => annotations.state),
            ).toEqual(["lagging", "caught_up"]);
            expect(preparations).toBe(readyPreparations);

            // Rollback. A rollback still supersedes a producer in flight, and closes
            // the gate for exactly one recovery at the new branch.
            const interrupted = yield* holdProducer(owner);
            expect(interrupted.coverage.point).toEqual(appended);
            const survivor = source.points.at(-2)!.point;
            source.rollbackTo(survivor.id);
            yield* frontierWhere(owner, ({ ready }) => !ready);
            const branch = yield* extend;
            yield* Deferred.succeed(interrupted.release, undefined);
            const superseded = yield* Fiber.join(interrupted.fiber);
            expect(superseded._tag).toBe("Left");
            yield* owner
              .awaitReadyAt(branch)
              .pipe(Effect.timeout("15 seconds"));
            expect(preparations).toBe(readyPreparations + 1);
            expect(BigInt((yield* authority).generation)).toBeGreaterThan(
              BigInt(readyGeneration),
            );

            // 7. A one-block fork while offline rewinds exactly one block: the
            // restart offers nearby retained intersections, not just the anchor.
            yield* owner.close;
            const offline = yield* load;
            const parent = source.points.at(-2)!.point;
            source.rollbackTo(parent.id);
            const fork = yield* extend;
            changes.length = 0;
            const offered = source.requests.length;
            owner = yield* makeOwner();
            yield* owner.awaitReadyAt(fork).pipe(Effect.timeout("15 seconds"));
            expect(changes.filter(({ kind }) => kind === "rollback")).toEqual([
              { kind: "rollback", head: parent.id },
            ]);
            expect(changes.filter(({ kind }) => kind === "forward")).toEqual([
              { kind: "forward", head: fork.id },
            ]);
            const intersection = source.requests
              .slice(offered)
              .find(({ method }) => method === "findIntersection");
            const points = (
              (intersection?.params as { points?: { id: string }[] })?.points ??
              []
            ).map(({ id }) => id);
            expect(points[0]).toBe(offline.head.id);
            expect(points[1]).toBe(parent.id);
            expect(points.at(-1)).toBe(offline.anchor.id);
            expect(ledgerScans(source.requests)).toBe(1);

            // 2. A forward block delivered while the first convergence
            // completes does not supersede it: readiness opens at the converged
            // head, and the block then appends at the head of that open gate
            // without another recovery.
            yield* owner.close;
            const completion = yield* gate;
            completionGate = completion;
            owner = yield* makeOwner();
            yield* Deferred.await(completion.entered).pipe(
              Effect.timeout("15 seconds"),
            );
            const recovered = preparations;
            const next = yield* extend;
            const forwarded = yield* gate;
            forwardGate = { ...forwarded, id: next.id };
            // Delivered and accepted; queued behind the completing convergence.
            yield* Effect.sleep("200 millis");
            yield* Deferred.succeed(completion.release, undefined);
            expect(
              (yield* owner
                .awaitReadyAt(fork)
                .pipe(Effect.timeout("15 seconds"))).point,
            ).toEqual(fork);
            yield* Deferred.await(forwarded.entered).pipe(
              Effect.timeout("15 seconds"),
            );
            // Journaling the block inside the Ready generation: the gate stays.
            expect((yield* owner.frontier).ready).toBe(true);
            const completedGeneration = (yield* authority).generation;
            yield* Deferred.succeed(forwarded.release, undefined);
            forwardGate = undefined;
            yield* owner.awaitReadyAt(next).pipe(Effect.timeout("15 seconds"));
            expect(preparations).toBe(recovered);
            expect((yield* authority).generation).toBe(completedGeneration);
            expect(yield* produces(owner)).toBe("Right");

            // 4. The owner wires the recovery hold slot into retention, and a
            // hold keeping the anchor more than k behind is reported once per
            // transition and exposed as status.
            yield* owner.close;
            const pinned = (yield* load).anchor;
            recoveryHold.slot = pinned.slot;
            logs.length = 0;
            owner = yield* makeOwner(2);
            yield* owner.awaitReadyAt(next).pipe(Effect.timeout("15 seconds"));
            let latest = next;
            for (let n = 0; n < 4; n++) {
              latest = yield* extend;
              yield* owner
                .awaitReadyAt(latest)
                .pipe(Effect.timeout("15 seconds"));
            }
            const held = yield* load;
            expect(held.anchor).toEqual(pinned);
            const status = yield* owner.retentionHold;
            expect(status).toEqual({
              holdSlot: pinned.slot,
              anchorHeight: pinned.height,
              unheldAnchorHeight: latest.height - 2,
              heldBlocks: latest.height - 2 - pinned.height,
              rollbackHorizon: 2,
            });
            const holding = logs.filter(
              ({ annotations }) =>
                annotations.event === "history_retention_hold",
            );
            expect(holding).toHaveLength(1);
            expect(holding[0]!.annotations).toMatchObject({
              state: "holding",
              holdSlot: pinned.slot,
              anchorHeight: pinned.height,
            });
            recoveryHold.slot = undefined;
            latest = yield* extend;
            yield* owner
              .awaitReadyAt(latest)
              .pipe(Effect.timeout("15 seconds"));
            expect(yield* owner.retentionHold).toBeUndefined();
            expect(
              logs
                .filter(
                  ({ annotations }) =>
                    annotations.event === "history_retention_hold",
                )
                .map(({ annotations }) => annotations.state),
            ).toEqual(["holding", "released"]);
            const released = yield* load;
            expect(released.anchor.height).toBe(latest.height - 2);

            // 8. A rollback below the retained anchor is refused before any
            // retained block is undone.
            const below = source.points.find(
              ({ point }) => point.height === released.anchor.height - 1,
            )!.point;
            source.rollbackTo(below.id);
            const stopped = yield* owner.awaitStopped.pipe(
              Effect.flip,
              Effect.timeout("15 seconds"),
            );
            expect(String(stopped.cause)).toMatch(
              /History rollback exceeds retained journal ancestry/,
            );
            yield* owner.close;
            const refused = yield* load;
            expect(refused.head).toEqual(released.head);
            expect(refused.revision).toBe(released.revision);
            expect(refused.anchor).toEqual(released.anchor);
            expect(ledgerScans(source.requests)).toBe(1);
          }).pipe(Effect.provide(Logger.add(capture))),
        ),
      ),
    );
  } finally {
    recoveryHold.slot = undefined;
    source.close();
    h.observer.restore();
    vi.useRealTimers();
  }
}, 180_000);

// First start: the complete capture was taken at C, then the source switched
// to a sibling of C before the replay reached it. The replay must refuse to
// continue past C's slot rather than never seeding.
it("refuses a first start whose capture point left the chain before the replay reached it", async () => {
  const { h, interval } = await openLifecycle();
  // C is neither the activation block nor its successor.
  h.batches.push(await interval());
  h.batches.push(await interval());
  const sibling = await interval();
  const source = makeRollbackHistoryTransport(h);
  try {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.scoped(
          Effect.gen(function* () {
            yield* truncate;
            const sql = yield* SqlClient.SqlClient;
            const cache = yield* makeMempoolLedgerCacheService(
              h.globals,
              MempoolLedgerDB.retrieveSpendable.pipe(
                Effect.provideService(SqlClient.SqlClient, sql),
              ),
            );
            const captured = source.points.at(-1)!.point;
            const parent = source.points.at(-2)!.point;
            // The capture's snapshot is already acquired at C when the source
            // replaces C with a later sibling.
            source.beforeLedgerQuery(() => {
              source.rollbackTo(parent.id);
              source.appendFork(sibling);
            });
            const owner = yield* makeEventHistoryOwner({
              binding: h.binding,
              histories: SDK.requireEventHistoryContracts(h.fixture.contracts),
              slotToUnixTime: h.fixture.operatorLucid.slotToUnixTime,
              transport: source.options,
              heartbeatIntervalMs: 100,
              retainedPointLimit: 128,
              maximumReceiptBytes: 16 * 1024 * 1024,
              leaseDurationMs: 60_000,
              rollbackHorizon: 2160,
              ownerToken: randomUUID(),
              expectedInitializationTransactionHash:
                h.deployment.initialization.txHash,
              cache,
              reconcile: () => Effect.void,
            });
            const refused = yield* owner.awaitReady.pipe(
              Effect.flip,
              Effect.timeout("15 seconds"),
            );
            expect(String(refused.cause)).toMatch(
              /capture point left the chain before replay reached it/,
            );
            expect(source.points.at(-1)!.point.id).not.toBe(captured.id);
            expect(yield* Journal.load(h.binding)).toBeNull();
          }),
        ),
      ),
    );
  } finally {
    source.close();
    h.observer.restore();
    vi.useRealTimers();
  }
}, 120_000);
