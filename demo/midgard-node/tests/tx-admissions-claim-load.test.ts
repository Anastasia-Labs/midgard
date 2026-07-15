import "./utils.js";

import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Deferred, Duration, Effect, Fiber } from "effect";
import { beforeAll, describe, expect } from "vitest";

import { MigrationRunner, TxAdmissionsDB } from "@/database/index.js";

import {
  deterministicFixtureBytes,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
} from "./utils.js";

type AdmissionInput = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
};

const admissionInput = (label: string): AdmissionInput => ({
  txId: deterministicFixtureTxHash(`claim-load:${label}`),
  txCanonicalCbor: deterministicFixtureBytes(`claim-load:${label}`, 64),
});

const insertAdmissions = (inputs: readonly AdmissionInput[]) =>
  Effect.gen(function* () {
    for (const input of inputs) {
      const inserted = yield* TxAdmissionsDB.tryInsert({
        ...input,
        submitSource: "native",
      });
      expect(inserted).not.toBeNull();
    }
  });

const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
      return yield* effect;
    }),
  );

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      MigrationRunner.migrate({
        appVersion: "tx-admissions-claim-load-test",
        actor: "tx-admissions-claim-load-test",
      }),
    ),
  );
});

describe("durable admission lightweight claim and payload load", () => {
  it.effect(
    "claims oldest-first by physical row identity and loads the exact canonical order",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            admissionInput("oldest-third"),
            admissionInput("oldest-first"),
            admissionInput("oldest-second"),
          ];
          yield* insertAdmissions(inputs);
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 30 WHERE tx_id = ${inputs[0]!.txId}`;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 10 WHERE tx_id = ${inputs[1]!.txId}`;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 20 WHERE tx_id = ${inputs[2]!.txId}`;
          const future = yield* sql<{ readonly future: Date }>`
            UPDATE tx_admissions
            SET
              first_seen_at = NOW() + INTERVAL '2 seconds',
              last_seen_at = NOW() + INTERVAL '2 seconds',
              updated_at = NOW() + INTERVAL '2 seconds',
              next_attempt_at = NOW()
            WHERE tx_id = ${inputs[1]!.txId}
            RETURNING updated_at AS future`;

          const leaseOwner = "claim-load:oldest-first";
          const claimed = yield* TxAdmissionsDB.claimBatchLease({
            limit: inputs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const expected = [inputs[1]!, inputs[2]!, inputs[0]!];
          expect(claimed.map((entry) => entry.tx_id)).toStrictEqual(
            expected.map((entry) => entry.txId),
          );
          const validationStartedAt = claimed[0]!.validation_started_at;
          expect(validationStartedAt).not.toBeNull();
          if (validationStartedAt === null) return;
          expect(validationStartedAt.getTime()).toBeGreaterThanOrEqual(
            future[0]!.future.getTime(),
          );

          const loaded = yield* TxAdmissionsDB.loadClaimedPayloads({
            claimed,
            leaseOwner,
          });
          expect(loaded.map((entry) => entry.tx_id)).toStrictEqual(
            expected.map((entry) => entry.txId),
          );
          expect(loaded.map((entry) => entry.tx_canonical_cbor)).toStrictEqual(
            expected.map((entry) => entry.txCanonicalCbor),
          );
          const settings = yield* sql<{
            readonly synchronous_commit: string;
          }>`SHOW synchronous_commit`;
          expect(settings[0]?.synchronous_commit).toBe("on");
        }),
      ),
  );

  it.effect(
    "skips a locked oldest row without waiting and claims it after release",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            admissionInput("skip-locked-oldest"),
            admissionInput("skip-locked-second"),
            admissionInput("skip-locked-third"),
          ];
          yield* insertAdmissions(inputs);
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 10 WHERE tx_id = ${inputs[0]!.txId}`;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 20 WHERE tx_id = ${inputs[1]!.txId}`;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 30 WHERE tx_id = ${inputs[2]!.txId}`;

          const oldestLocked = yield* Deferred.make<void>();
          const releaseOldest = yield* Deferred.make<void>();
          const holder = yield* Effect.fork(
            sql.withTransaction(
              Effect.gen(function* () {
                const locked = yield* sql<{
                  readonly tx_id: Buffer;
                }>`SELECT tx_id
                  FROM tx_admissions
                  WHERE tx_id = ${inputs[0]!.txId}
                  FOR UPDATE`;
                expect(locked.map((row) => row.tx_id)).toStrictEqual([
                  inputs[0]!.txId,
                ]);
                yield* Deferred.succeed(oldestLocked, undefined);
                yield* Deferred.await(releaseOldest);
              }),
            ),
          );

          const skipped = yield* Effect.gen(function* () {
            yield* Deferred.await(oldestLocked).pipe(
              Effect.timeoutFail({
                duration: Duration.seconds(5),
                onTimeout: () =>
                  new Error("Timed out waiting for the oldest-row lock"),
              }),
            );
            return yield* TxAdmissionsDB.claimBatchLease({
              limit: 2,
              leaseOwner: "claim-load:skip-locked:later",
              leaseDurationMs: 30_000,
            }).pipe(
              Effect.timeoutFail({
                duration: Duration.seconds(5),
                onTimeout: () =>
                  new Error(
                    "Claim waited for the locked oldest row instead of skipping it",
                  ),
              }),
            );
          }).pipe(Effect.ensuring(Deferred.succeed(releaseOldest, undefined)));

          yield* Fiber.join(holder);
          expect(skipped.map((entry) => entry.tx_id)).toStrictEqual([
            inputs[1]!.txId,
            inputs[2]!.txId,
          ]);

          const oldest = yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner: "claim-load:skip-locked:oldest",
            leaseDurationMs: 30_000,
          });
          expect(oldest.map((entry) => entry.tx_id)).toStrictEqual([
            inputs[0]!.txId,
          ]);
        }),
      ),
  );

  it.effect("keeps two concurrent claim loops disjoint and complete", () =>
    isolatedDb(
      Effect.gen(function* () {
        const inputs = Array.from({ length: 8 }, (_, index) =>
          admissionInput(`disjoint-${index.toString()}`),
        );
        yield* insertAdmissions(inputs);
        const [left, right] = yield* Effect.all(
          [
            TxAdmissionsDB.claimBatchLease({
              limit: 4,
              leaseOwner: "claim-load:disjoint:left",
              leaseDurationMs: 30_000,
            }),
            TxAdmissionsDB.claimBatchLease({
              limit: 4,
              leaseOwner: "claim-load:disjoint:right",
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
        expect(new Set([...leftIds, ...rightIds])).toEqual(
          new Set(inputs.map((entry) => entry.txId.toString("hex"))),
        );
      }),
    ),
  );

  it.effect(
    "fails closed for duplicate, extra, and foreign-owner claimed-id sets",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            admissionInput("set-first"),
            admissionInput("set-second"),
            admissionInput("set-foreign"),
          ];
          yield* insertAdmissions(inputs);
          const leaseOwner = "claim-load:set-owner";
          const claimed = yield* TxAdmissionsDB.claimBatchLease({
            limit: 2,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const foreign = yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner: "claim-load:foreign-owner",
            leaseDurationMs: 30_000,
          });
          expect(
            yield* Effect.either(
              TxAdmissionsDB.loadClaimedPayloads({
                claimed: [claimed[0]!, claimed[0]!],
                leaseOwner,
              }),
            ),
          ).toMatchObject({ _tag: "Left" });
          expect(
            yield* Effect.either(
              TxAdmissionsDB.loadClaimedPayloads({
                claimed: [claimed[0]!],
                leaseOwner,
              }),
            ),
          ).toMatchObject({ _tag: "Left" });
          expect(
            yield* Effect.either(
              TxAdmissionsDB.loadClaimedPayloads({
                claimed: [claimed[0]!, foreign[0]!],
                leaseOwner,
              }),
            ),
          ).toMatchObject({ _tag: "Left" });

          const exact = yield* TxAdmissionsDB.loadClaimedPayloads({
            claimed,
            leaseOwner,
          });
          expect(exact.map((entry) => entry.tx_id)).toStrictEqual(
            claimed.map((entry) => entry.tx_id),
          );
        }),
      ),
  );

  it.effect("fails closed when a claimed payload is missing", () =>
    isolatedDb(
      Effect.gen(function* () {
        const inputs = [
          admissionInput("missing-first"),
          admissionInput("missing-second"),
        ];
        yield* insertAdmissions(inputs);
        const leaseOwner = "claim-load:missing";
        const claimed = yield* TxAdmissionsDB.claimBatchLease({
          limit: inputs.length,
          leaseOwner,
          leaseDurationMs: 30_000,
        });
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${claimed[1]!.tx_id}`;

        expect(
          yield* Effect.either(
            TxAdmissionsDB.loadClaimedPayloads({ claimed, leaseOwner }),
          ),
        ).toMatchObject({ _tag: "Left" });
        const stillLeased = yield* sql<{
          readonly status: string;
          readonly lease_owner: string | null;
        }>`SELECT status, lease_owner FROM tx_admissions ORDER BY arrival_seq`;
        expect(stillLeased).toEqual([
          { status: "validating", lease_owner: leaseOwner },
          { status: "validating", lease_owner: leaseOwner },
        ]);
      }),
    ),
  );

  it.effect(
    "requeues an expired lease and reclaims it without resetting age",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const input = admissionInput("expired");
          yield* insertAdmissions([input]);
          const initial = yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner: "claim-load:expired:first",
            leaseDurationMs: 1,
          });
          const validationStartedAt = initial[0]!.validation_started_at;
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions SET lease_expires_at = NOW() - INTERVAL '1 second' WHERE tx_id = ${input.txId}`;
          expect(yield* TxAdmissionsDB.requeueExpiredLeases).toBe(1);

          const reclaimed = yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner: "claim-load:expired:second",
            leaseDurationMs: 30_000,
          });
          expect(reclaimed.map((entry) => entry.tx_id)).toStrictEqual([
            input.txId,
          ]);
          expect(reclaimed[0]!.validation_started_at).toEqual(
            validationStartedAt,
          );
          const row = yield* TxAdmissionsDB.getByTxId(input.txId);
          expect(row).toMatchObject({
            status: TxAdmissionsDB.Status.Validating,
            lease_owner: "claim-load:expired:second",
            attempt_count: 2,
          });
        }),
      ),
  );

  it.effect("releases and reclaims a lease after a worker crash", () =>
    isolatedDb(
      Effect.gen(function* () {
        const input = admissionInput("worker-crash");
        yield* insertAdmissions([input]);
        const crashedOwner = "claim-load:worker-crash";
        const crashed = yield* TxAdmissionsDB.claimBatchLease({
          limit: 1,
          leaseOwner: crashedOwner,
          leaseDurationMs: 30_000,
        });
        yield* TxAdmissionsDB.releaseForRetry({
          txIds: crashed.map((entry) => entry.tx_id),
          leaseOwner: crashedOwner,
          delayMs: 0,
        });

        const retryOwner = "claim-load:worker-retry";
        const reclaimed = yield* TxAdmissionsDB.claimBatchLease({
          limit: 1,
          leaseOwner: retryOwner,
          leaseDurationMs: 30_000,
        });
        const loaded = yield* TxAdmissionsDB.loadClaimedPayloads({
          claimed: reclaimed,
          leaseOwner: retryOwner,
        });
        expect(loaded).toHaveLength(1);
        expect(loaded[0]?.tx_id).toEqual(input.txId);
        expect(loaded[0]?.tx_canonical_cbor).toEqual(input.txCanonicalCbor);
        const row = yield* TxAdmissionsDB.getByTxId(input.txId);
        expect(row).toMatchObject({
          status: TxAdmissionsDB.Status.Validating,
          lease_owner: retryOwner,
          attempt_count: 2,
        });
      }),
    ),
  );
});
