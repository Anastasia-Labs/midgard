import { createHash, randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Schedule } from "effect";
import JSONBig from "json-bigint";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { loadCanonicalHistoryCoverage } from "../src/database/eventHistoryCanonicalCoverage.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { makeEventHistoryOwner } from "../src/services/event-history-owner.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { historyOutputObservation } from "./helpers/history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";
import { openHistorySourceOwnerLifecycle } from "./helpers/history-source-owner-emulator.js";
import { provideDatabaseLayers } from "./utils.js";

const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const digest = (text: string) => createHash("sha256").update(text).digest();
type Coverage = Effect.Effect.Success<
  ReturnType<typeof loadCanonicalHistoryCoverage>
>;

// Accepted emulator initialization and complete observed intervals are real;
// branch ancestry is the existing controlled transport model. Retained SQL is
// consumed only inside the production source owner's recovery transaction.
it("loads complete current-branch rosters and refuses stale or altered retained coverage", async () => {
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
  // Ensure the source must retain a non-activation block before its first
  // acquired checkpoint, as well as later journal applications.
  h.batches.push(await interval());
  const source = makeRollbackHistoryTransport(h);
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
            const cache = yield* makeMempoolLedgerCacheService(
              h.globals,
              MempoolLedgerDB.retrieveSpendable.pipe(
                Effect.provideService(SqlClient.SqlClient, sql),
              ),
            );
            const observed = new Map<string, Coverage>();
            const checkpoints = new Map<string, Journal.Checkpoint>();
            const tokens = new Map<string, Authority.Token>();
            const refusals: string[] = [];
            let checkCorruption = true;
            const owner = yield* makeEventHistoryOwner({
              binding: h.binding,
              histories: SDK.requireEventHistoryContracts(h.fixture.contracts),
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
              reconcile: ({ after }) =>
                Effect.gen(function* () {
                  const token = yield* Authority.requireRecoveryTransaction;
                  const coverage = yield* loadCanonicalHistoryCoverage(
                    h.binding,
                    after,
                  );
                  expect(coverage.ownerGeneration).toBe(token.generation);
                  expect(coverage.checkpointRevision).toBe(after.revision);
                  expect(coverage.snapshotDigest).toBe(
                    after.capture.snapshotDigest,
                  );
                  expect(coverage.bindingDigest).toBe(h.binding.digest);
                  expect(coverage.manifestId).toBe(h.binding.manifestId);
                  observed.set(after.head.id, coverage);
                  checkpoints.set(after.head.id, after);
                  tokens.set(after.head.id, token);
                  if (!checkCorruption || after.head.id === after.anchor.id)
                    return;
                  checkCorruption = false;

                  const stale = yield* Effect.either(
                    loadCanonicalHistoryCoverage(h.binding, {
                      ...after,
                      revision: (BigInt(after.revision) + 1n).toString(),
                    }),
                  );
                  expect(stale._tag).toBe("Left");
                  refusals.push("stale-checkpoint");
                  const foreign = yield* Effect.either(
                    loadCanonicalHistoryCoverage(
                      { ...h.binding, manifestId: "ff".repeat(32) },
                      after,
                    ),
                  );
                  expect(foreign._tag).toBe("Left");
                  refusals.push("foreign-deployment");

                  const [row] = yield* sql<{
                    ledger_receipt: string;
                    ledger_receipt_digest: Buffer;
                  }>`SELECT ledger_receipt, ledger_receipt_digest
                    FROM event_history_block_applications
                    WHERE binding_digest = ${Buffer.from(h.binding.digest, "hex")}
                      AND block_hash = ${Buffer.from(after.head.id, "hex")} AND canonical`;
                  expect(row).toBeDefined();
                  if (row === undefined)
                    throw new Error("Missing current application");
                  const restore = sql`UPDATE event_history_block_applications
                    SET ledger_receipt = ${row.ledger_receipt},
                      ledger_receipt_digest = ${row.ledger_receipt_digest}
                    WHERE binding_digest = ${Buffer.from(h.binding.digest, "hex")}
                      AND block_hash = ${Buffer.from(after.head.id, "hex")} AND canonical`;
                  yield* Effect.gen(function* () {
                    yield* sql`UPDATE event_history_block_applications
                      SET ledger_receipt = ${row.ledger_receipt + " "}
                      WHERE binding_digest = ${Buffer.from(h.binding.digest, "hex")}
                        AND block_hash = ${Buffer.from(after.head.id, "hex")} AND canonical`;
                    expect(
                      (yield* Effect.either(
                        loadCanonicalHistoryCoverage(h.binding, after),
                      ))._tag,
                    ).toBe("Left");
                    refusals.push("changed-receipt");
                    const missingRoster = lossless.parse(
                      row.ledger_receipt,
                    ) as {
                      block: { transactions?: unknown };
                    };
                    delete missingRoster.block.transactions;
                    const replacement = lossless.stringify(missingRoster);
                    yield* sql`UPDATE event_history_block_applications
                      SET ledger_receipt = ${replacement}, ledger_receipt_digest = ${digest(replacement)}
                      WHERE binding_digest = ${Buffer.from(h.binding.digest, "hex")}
                        AND block_hash = ${Buffer.from(after.head.id, "hex")} AND canonical`;
                    expect(
                      (yield* Effect.either(
                        loadCanonicalHistoryCoverage(h.binding, after),
                      ))._tag,
                    ).toBe("Left");
                    refusals.push("missing-roster-with-matching-digest");
                  }).pipe(Effect.ensuring(restore.pipe(Effect.orDie)));
                  expect(
                    yield* loadCanonicalHistoryCoverage(h.binding, after),
                  ).toEqual(coverage);
                }),
            });
            yield* owner.awaitReady.pipe(Effect.timeout("30 seconds"));
            const ancestor = source.points.at(-1)!.point;
            const initial = observed.get(ancestor.id)!;
            expect(initial).toBeDefined();
            expect(initial.blocks.length).toBeGreaterThan(1);
            expect(initial.activationTransactionHash).toBe(
              h.deployment.initialization.txHash,
            );
            expect(
              initial.blocks[0]!.transactions.map((tx) => tx.txHash),
            ).toContain(h.deployment.initialization.txHash);
            const assertSourceRosters = (coverage: Coverage) => {
              const expected = source.points.filter(
                ({ point }) =>
                  point.height >= coverage.start.height &&
                  point.height <= coverage.head.height,
              );
              expect(coverage.blocks).toEqual(
                expected.map((block) => ({
                  point: block.point,
                  parent: block.parent,
                  transactions: block.transactions.map((tx) => ({
                    txHash: tx.id,
                    spends: tx.spends,
                  })),
                })),
              );
            };
            assertSourceRosters(initial);
            // Registration/activation transactions share these complete block
            // rosters even though they do not mutate either event-history list.
            expect(
              initial.blocks
                .flatMap((block) => block.transactions)
                .some(
                  (transaction) =>
                    transaction.txHash !== h.deployment.initialization.txHash,
                ),
            ).toBe(true);
            expect(
              (yield* Effect.either(
                loadCanonicalHistoryCoverage(
                  h.binding,
                  checkpoints.get(ancestor.id)!,
                ),
              ))._tag,
            ).toBe("Left");
            const token = tokens.get(ancestor.id)!;
            const foreignGeneration = yield* Effect.either(
              Authority.withRecovery(
                {
                  ...token,
                  generation: (BigInt(token.generation) + 1n).toString(),
                },
                loadCanonicalHistoryCoverage(
                  h.binding,
                  checkpoints.get(ancestor.id)!,
                ),
              ),
            );
            expect(foreignGeneration._tag).toBe("Left");
            if (foreignGeneration._tag === "Left")
              expect(foreignGeneration.left.message).toBe(
                "History authority generation or owner changed",
              );

            h.batches.push(yield* Effect.promise(interval));
            const orphan = source.appendAccepted();
            yield* owner
              .awaitReadyAt(orphan)
              .pipe(Effect.timeout("15 seconds"));
            const extended = observed.get(orphan.id)!;
            assertSourceRosters(extended);
            expect(extended.blocks.at(-1)!.transactions).toEqual([]);
            expect(refusals).toEqual([
              "stale-checkpoint",
              "foreign-deployment",
              "changed-receipt",
              "missing-roster-with-matching-digest",
            ]);
            source.rollbackTo(ancestor.id);
            yield* Effect.gen(function* () {
              const checkpoint = yield* Journal.load(h.binding);
              if (checkpoint?.head.id !== ancestor.id)
                return yield* Effect.fail(
                  new Error("Rollback has not reached its ancestor"),
                );
            }).pipe(
              Effect.retry(Schedule.spaced("10 millis")),
              Effect.timeout("15 seconds"),
            );
            yield* owner
              .awaitReadyAt(ancestor)
              .pipe(Effect.timeout("15 seconds"));
            const fork = source.appendFork(yield* Effect.promise(interval));
            yield* owner.awaitReadyAt(fork).pipe(Effect.timeout("15 seconds"));
            const current = observed.get(fork.id)!;
            assertSourceRosters(current);
            expect(current.blocks.map((block) => block.point.id)).not.toContain(
              orphan.id,
            );
            expect(current.blocks.at(-1)!.point).toEqual(fork);
            expect(BigInt(current.ownerGeneration)).toBeGreaterThan(
              BigInt(extended.ownerGeneration),
            );
            const retained = yield* sql<{ canonical: boolean }>`SELECT canonical
              FROM event_history_block_applications
              WHERE binding_digest = ${Buffer.from(h.binding.digest, "hex")}
                AND block_hash = ${Buffer.from(orphan.id, "hex")}`;
            expect(retained).toEqual([{ canonical: false }]);
            yield* owner.close;
          }),
        ),
      ),
    );
  } finally {
    source.close();
    h.observer.restore();
    vi.useRealTimers();
  }
});
