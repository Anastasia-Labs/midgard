import "./utils.js";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1(
          [],
        ),
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
    "fails closed when the persisted V1 full-transaction commitment is corrupt",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const input = admissionInput("corrupt-full-hash");
          yield* insertAdmissions([input]);
          const leaseOwner = "claim-load:corrupt-full-hash";
          const claimed = yield* TxAdmissionsDB.claimBatchLease({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admission_payloads
            SET tx_full_hash_v1 = decode(repeat('00', 32), 'hex')
            WHERE tx_id = ${input.txId}`;

          const loaded = yield* Effect.either(
            TxAdmissionsDB.loadClaimedPayloads({ claimed, leaseOwner }),
          );
          expect(loaded).toMatchObject({
            _tag: "Left",
            left: {
              message:
                "Admission payload canonical V1 full-transaction commitment does not match its persisted bytes",
            },
          });
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
          baseDelayMs: 0,
          maxDelayMs: 0,
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

  it.effect(
    "backs off worker retries exponentially per admission and caps the delay",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const input = admissionInput("worker-retry-backoff");
          yield* insertAdmissions([input]);
          const sql = yield* SqlClient.SqlClient;
          const expectedRetryDelaysMs = [250, 500, 1_000, 1_000];

          for (const [
            index,
            expectedRetryDelayMs,
          ] of expectedRetryDelaysMs.entries()) {
            const leaseOwner = `claim-load:worker-retry-backoff:${index}`;
            const claimed = yield* TxAdmissionsDB.claimBatchLease({
              limit: 1,
              leaseOwner,
              leaseDurationMs: 30_000,
            });
            expect(claimed).toHaveLength(1);

            yield* TxAdmissionsDB.releaseForRetry({
              txIds: [input.txId],
              leaseOwner,
              baseDelayMs: 250,
              maxDelayMs: 1_000,
            });

            const scheduled = yield* sql<{
              readonly attempt_count: number;
              readonly retry_delay_ms: number | string;
            }>`SELECT
                attempt_count,
                EXTRACT(EPOCH FROM (next_attempt_at - updated_at)) * 1000
                  AS retry_delay_ms
              FROM tx_admissions
              WHERE tx_id = ${input.txId}`;
            expect(scheduled[0]?.attempt_count).toBe(index + 1);
            expect(Number(scheduled[0]?.retry_delay_ms)).toBe(
              expectedRetryDelayMs,
            );

            yield* sql`UPDATE tx_admissions
              SET next_attempt_at = NOW()
              WHERE tx_id = ${input.txId}`;
          }
        }),
      ),
  );

  // A duplicate submission of a still-queued transaction rewrites that row
  // (last_seen_at/updated_at/request_count) while leaving it queued and
  // claimable, which moves the row to a new physical location. The dangerous
  // interleaving is narrow: the duplicate must commit after the claim
  // statement has taken its snapshot but before that statement locks the row.
  // A statement-level gate pins exactly that window here, because the claim's
  // own UPDATE fires it before any candidate is locked and it parks the claim
  // on an advisory lock the duplicate submission holds. A claim identifying
  // its candidates by physical location then joins against a tuple version its
  // snapshot cannot see and returns nothing while the row stays queued;
  // identifying them by tx_id claims every row it locked.
  const GATE_LOCK_KEY = 8_140_255;
  // Live clock: the gate handshake polls and bounds itself with real timeouts.
  it.live(
    "claims a queued row whose duplicate submission moved it after the claim snapshot",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const inputs = Array.from({ length: 4 }, (_, index) =>
            admissionInput(`rewrite-race-${index.toString()}`),
          );
          yield* insertAdmissions(inputs);
          const fullHashes = yield* Effect.all(
            inputs.map((input) =>
              sql<{
                readonly tx_full_hash_v1: Buffer;
              }>`SELECT tx_full_hash_v1
                  FROM tx_admission_payloads
                  WHERE tx_id = ${input.txId}`.pipe(
                Effect.map((rows) => rows[0]!.tx_full_hash_v1),
              ),
            ),
          );
          const sidecar = encodeMidgardCekProgramMaterialSidecarV1([]);

          const scenario = Effect.gen(function* () {
            yield* sql.unsafe(
              `CREATE FUNCTION claim_snapshot_gate() RETURNS trigger
                 LANGUAGE plpgsql AS $gate$
                 BEGIN
                   PERFORM pg_advisory_xact_lock(${GATE_LOCK_KEY.toString()});
                   RETURN NULL;
                 END
                 $gate$`,
            );
            yield* sql`CREATE TRIGGER claim_snapshot_gate_trigger
              BEFORE UPDATE ON tx_admissions
              FOR EACH STATEMENT EXECUTE FUNCTION claim_snapshot_gate()`;

            const gateHeld = yield* Deferred.make<void>();
            const releaseGate = yield* Deferred.make<void>();
            // The duplicate submission holds the gate, waits until the claim
            // is parked on it, and only then rewrites the queued rows. Its
            // commit therefore lands strictly inside the claim's window.
            const duplicate = yield* Effect.fork(
              sql.withTransaction(
                Effect.gen(function* () {
                  yield* sql`SELECT pg_advisory_xact_lock(${GATE_LOCK_KEY})`;
                  yield* Deferred.succeed(gateHeld, undefined);
                  yield* Deferred.await(releaseGate);
                  yield* Effect.forEach(
                    inputs,
                    (input, index) =>
                      // The production duplicate-submission path.
                      TxAdmissionsDB.touchDuplicate({
                        txId: input.txId,
                        txFullHashV1: fullHashes[index]!,
                        txCanonicalCbor: input.txCanonicalCbor,
                        programMaterialSidecarCbor: sidecar,
                      }),
                    { discard: true },
                  );
                }),
              ),
            );

            const claimed = yield* Effect.gen(function* () {
              yield* Deferred.await(gateHeld).pipe(
                Effect.timeoutFail({
                  duration: Duration.seconds(10),
                  onTimeout: () =>
                    new Error("Timed out holding the statement gate"),
                }),
              );
              const claim = yield* Effect.fork(
                TxAdmissionsDB.claimBatchLease({
                  limit: inputs.length,
                  leaseOwner: "claim-load:rewrite-race",
                  leaseDurationMs: 30_000,
                }),
              );
              yield* Effect.iterate(false, {
                while: (parked) => !parked,
                body: () =>
                  sql<{
                    readonly parked: boolean;
                  }>`SELECT EXISTS (
                      SELECT 1
                      FROM pg_locks
                      WHERE locktype = 'advisory'
                        AND NOT granted
                        AND ((classid::bigint << 32) | objid::bigint) =
                          ${GATE_LOCK_KEY}
                    ) AS parked`.pipe(
                    Effect.map((rows) => rows[0]!.parked),
                    Effect.tap((parked) =>
                      parked ? Effect.void : Effect.sleep(Duration.millis(5)),
                    ),
                  ),
              }).pipe(
                Effect.timeoutFail({
                  duration: Duration.seconds(15),
                  onTimeout: () =>
                    new Error("Claim never parked on the statement gate"),
                }),
              );
              yield* Deferred.succeed(releaseGate, undefined);
              return yield* Fiber.join(claim).pipe(
                Effect.timeoutFail({
                  duration: Duration.seconds(15),
                  onTimeout: () =>
                    new Error("Claim never resumed after the gate released"),
                }),
              );
            }).pipe(Effect.ensuring(Deferred.succeed(releaseGate, undefined)));
            yield* Fiber.join(duplicate);
            return claimed;
          }).pipe(
            Effect.ensuring(
              sql
                .unsafe(
                  `DROP TRIGGER IF EXISTS claim_snapshot_gate_trigger
                     ON tx_admissions`,
                )
                .pipe(
                  Effect.andThen(
                    sql.unsafe(`DROP FUNCTION IF EXISTS claim_snapshot_gate()`),
                  ),
                  Effect.orDie,
                ),
            ),
          );

          const claimed = yield* scenario;
          expect(claimed.map((entry) => entry.tx_id)).toStrictEqual(
            inputs.map((input) => input.txId),
          );

          const rows = yield* sql<{
            readonly status: string;
            readonly lease_owner: string | null;
            readonly request_count: string;
          }>`SELECT
              status::text AS status,
              lease_owner,
              request_count::text AS request_count
            FROM tx_admissions
            ORDER BY arrival_seq`;
          // Every locked row transitioned, and each carries the duplicate
          // submission that moved it, so the claim really did resolve rows the
          // rewrite had relocated.
          expect(rows.map((row) => row.status)).toStrictEqual(
            inputs.map(() => "validating"),
          );
          expect(rows.map((row) => row.lease_owner)).toStrictEqual(
            inputs.map(() => "claim-load:rewrite-race"),
          );
          expect(rows.map((row) => row.request_count)).toStrictEqual(
            inputs.map(() => "2"),
          );
        }),
      ),
  );
});
