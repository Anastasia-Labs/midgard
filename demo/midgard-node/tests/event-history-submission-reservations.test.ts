import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Clock, Effect, Option, Redacted } from "effect";
import { describe, expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { DatabaseError } from "../src/database/utils/common.js";
import {
  attempt,
  hash,
  holdings,
  input,
  leftBehind,
  run,
  untilAdvisoryLock,
} from "./event-history-submission-reservations.fixture.js";
import { testDatabaseName } from "./test-env.js";
import { provideDatabaseLayers } from "./utils.js";

const pending = (row: Journal.Row, txHashes: readonly string[], ttl = 5_000) =>
  ({ ...row.checkpoint, pending: attempt(txHashes, ttl) }) as const;

describe("history submission input reservations", () => {
  it("holds a pending attempt's inputs and releases them once it settles, keeping the nonce", async () => {
    const [head, fund] = [hash(randomUUID()), hash(randomUUID())];
    await run(
      Effect.gen(function* () {
        const row = yield* Journal.reserve(input());
        const saved = yield* Journal.saveCheckpoint(
          row,
          pending(row, [head, fund]),
        );
        expect(yield* holdings(row.submission_id)).toEqual(
          [`${head}#0`, `${fund}#0`, row.nonce_out_ref].sort(),
        );
        yield* Journal.saveCheckpoint(saved, {
          ...row.checkpoint,
          admission: saved.checkpoint.pending!,
        });
        expect(yield* holdings(row.submission_id)).toEqual([row.nonce_out_ref]);
      }),
    );
  });

  it("releases the old list head when an attempt settles and is rebuilt against a new one", async () => {
    const [oldHead, newHead, fund] = [1, 2, 3].map(() => hash(randomUUID()));
    await run(
      Effect.gen(function* () {
        const row = yield* Journal.reserve(input());
        const first = yield* Journal.saveCheckpoint(
          row,
          pending(row, [oldHead!, fund!]),
        );
        const settled = yield* Journal.saveCheckpoint(first, row.checkpoint);
        yield* Journal.saveCheckpoint(settled, pending(row, [newHead!, fund!]));
        expect(yield* holdings(row.submission_id)).toEqual(
          [`${newHead}#0`, `${fund}#0`, row.nonce_out_ref].sort(),
        );
        // Another submission now reserves the old head without any holder.
        const other = yield* Journal.reserve(input());
        yield* Journal.saveCheckpoint(other, pending(other, [oldHead!]));
        expect(yield* holdings(other.submission_id)).toContain(`${oldHead}#0`);
      }),
    );
  });

  it.each([
    { holder: "has no pending attempt", spends: undefined },
    { holder: "has a pending attempt spending other inputs", spends: "other" },
  ])(
    "takes over an input left behind by a holder that $holder, never its nonce",
    async ({ spends }) => {
      const head = hash(randomUUID());
      await run(
        Effect.gen(function* () {
          let holder = yield* Journal.reserve(input());
          if (spends !== undefined)
            holder = yield* Journal.saveCheckpoint(
              holder,
              pending(holder, [hash(randomUUID())]),
            );
          yield* leftBehind(`${head}#0`, holder.submission_id);
          const taker = yield* Journal.reserve(input());
          const nonce = yield* Effect.either(
            Journal.saveCheckpoint(
              taker,
              pending(taker, [holder.request.nonce.txHash]),
            ),
          );
          if (nonce._tag !== "Left") throw new Error("Took over a nonce");
          expect(nonce.left).toMatchObject({
            _tag: "HistoryInputReservedError",
            holder: holder.submission_id,
          });
          yield* Journal.saveCheckpoint(taker, pending(taker, [head]));
          expect(yield* holdings(taker.submission_id)).toContain(`${head}#0`);
          expect(yield* holdings(holder.submission_id)).not.toContain(
            `${head}#0`,
          );
        }),
      );
    },
  );

  it("takes over a holder's input only once its pending attempt's TTL is below the observed tip slot", async () => {
    const [head, own] = [hash(randomUUID()), hash(randomUUID())];
    await run(
      Effect.gen(function* () {
        const holder = yield* Journal.reserve(input());
        const held = yield* Journal.saveCheckpoint(
          holder,
          pending(holder, [head], 1_000),
        );
        const taker = yield* Journal.reserve(input());
        // Still landable: no tip observed, or a tip not past the TTL.
        for (const tip of [undefined, 999, 1_000]) {
          const refused = yield* Effect.either(
            Journal.saveCheckpoint(taker, pending(taker, [head, own]), tip),
          );
          if (refused._tag !== "Left") throw new Error("Took a live input");
          expect(refused.left).toBeInstanceOf(
            Journal.HistoryInputReservedError,
          );
          expect(refused.left).toMatchObject({
            outRef: `${head}#0`,
            holder: holder.submission_id,
          });
          expect(yield* holdings(taker.submission_id)).toEqual([
            taker.nonce_out_ref,
          ]);
        }
        const took = yield* Journal.saveCheckpoint(
          taker,
          pending(taker, [head, own]),
          1_001,
        );
        expect(yield* holdings(taker.submission_id)).toEqual(
          [`${head}#0`, `${own}#0`, taker.nonce_out_ref].sort(),
        );
        expect(yield* holdings(holder.submission_id)).toEqual([
          holder.nonce_out_ref,
        ]);
        // The holder's own journal is untouched until it reconciles.
        const stored = yield* Journal.retrieve(holder.submission_id);
        expect(Option.isSome(stored) && stored.value).toEqual(held);
        // Publications are bounded like admissions, so a dead holder's
        // pending publication frees its funding the same way.
        const funding = hash(randomUUID());
        const publisher = yield* Journal.reserve(input());
        yield* Journal.saveCheckpoint(publisher, {
          ...publisher.checkpoint,
          pending: attempt([funding], 1_000, "Publication"),
        });
        const early = yield* Effect.either(
          Journal.saveCheckpoint(took, pending(took, [funding]), 1_000),
        );
        expect(early._tag === "Left" && early.left).toMatchObject({
          _tag: "HistoryInputReservedError",
          holder: publisher.submission_id,
        });
        const published = yield* Journal.saveCheckpoint(
          took,
          pending(took, [funding]),
          1_001,
        );
        expect(yield* holdings(publisher.submission_id)).toEqual([
          publisher.nonce_out_ref,
        ]);
        // No durable submission journals a TTL-less body any more; one that a
        // journal still holds (written before publications were bounded) could
        // land at any time, so it is never taken over.
        const forever = hash(randomUUID());
        const other = yield* Journal.reserve(input());
        const legacyCheckpoint = {
          ...other.checkpoint,
          pending: attempt([forever], undefined, "Publication"),
        };
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE event_history_submissions
          SET checkpoint = CAST(${JSON.stringify(legacyCheckpoint)} AS TEXT)::JSONB
          WHERE submission_id = ${other.submission_id}`;
        yield* leftBehind(`${forever}#0`, other.submission_id);
        const refused = yield* Effect.either(
          Journal.saveCheckpoint(
            published,
            pending(published, [forever]),
            Number.MAX_SAFE_INTEGER,
          ),
        );
        if (refused._tag !== "Left") throw new Error("Took a TTL-less input");
        expect(refused.left).toMatchObject({
          _tag: "HistoryInputReservedError",
          holder: other.submission_id,
        });
      }),
    );
  });

  it("gives exactly one of two racing takers an expired holder's input", async () => {
    for (let round = 0; round < 5; round++) {
      const head = hash(randomUUID());
      await run(
        Effect.gen(function* () {
          const holder = yield* Journal.reserve(input());
          yield* Journal.saveCheckpoint(holder, pending(holder, [head], 1_000));
          const takers = [
            yield* Journal.reserve(input()),
            yield* Journal.reserve(input()),
          ];
          const outcomes = yield* Effect.all(
            takers.map((taker) =>
              Effect.either(
                Journal.saveCheckpoint(
                  taker,
                  pending(taker, [head, hash(randomUUID())]),
                  2_000,
                ),
              ),
            ),
            { concurrency: "unbounded" },
          );
          const winner = outcomes.findIndex(({ _tag }) => _tag === "Right");
          expect(outcomes.filter(({ _tag }) => _tag === "Right")).toHaveLength(
            1,
          );
          const lost = outcomes[1 - winner]!;
          if (lost._tag !== "Left") throw new Error("Two winners");
          expect(lost.left).toMatchObject({
            _tag: "HistoryInputReservedError",
            holder: takers[winner]!.submission_id,
          });
          expect(yield* holdings(takers[winner]!.submission_id)).toContain(
            `${head}#0`,
          );
          expect(yield* holdings(takers[1 - winner]!.submission_id)).toEqual([
            takers[1 - winner]!.nonce_out_ref,
          ]);
        }),
      );
    }
  });

  it("hides only inputs their holder can no longer spend from nonce selection", async () => {
    const [live, expired, left] = [1, 2, 3].map(() => hash(randomUUID()));
    await run(
      Effect.gen(function* () {
        const liveHolder = yield* Journal.reserve(input());
        yield* Journal.saveCheckpoint(
          liveHolder,
          pending(liveHolder, [live!], 5_000),
        );
        const deadHolder = yield* Journal.reserve(input());
        yield* Journal.saveCheckpoint(
          deadHolder,
          pending(deadHolder, [expired!], 1_000),
        );
        yield* leftBehind(`${left}#0`, liveHolder.submission_id);
        const wallet = liveHolder.wallet_address;
        const nonces = [liveHolder.nonce_out_ref, deadHolder.nonce_out_ref];
        // Without an observed tip only the left-behind input is free.
        expect([...(yield* Journal.reservedInputs(wallet))]).toEqual(
          expect.arrayContaining([...nonces, `${live}#0`, `${expired}#0`]),
        );
        expect(yield* Journal.reservedInputs(wallet)).not.toContain(
          `${left}#0`,
        );
        const atTip = yield* Journal.reservedInputs(wallet, 1_001);
        expect([...atTip]).toEqual(
          expect.arrayContaining([...nonces, `${live}#0`]),
        );
        expect(atTip).not.toContain(`${expired}#0`);
        expect(atTip).not.toContain(`${left}#0`);
      }),
    );
  });

  it("takes over a new nonce only from a holder that can no longer spend it, never another submission's nonce", async () => {
    await run(
      Effect.gen(function* () {
        const holder = yield* Journal.reserve(input());
        const [expired, left] = [hash(randomUUID()), hash(randomUUID())];
        yield* Journal.saveCheckpoint(
          holder,
          pending(holder, [expired], 1_000),
        );
        yield* leftBehind(`${left}#0`, holder.submission_id);
        const live =
          "History transaction input is reserved by another submission";
        for (const [outRef, tip, message] of [
          [
            holder.nonce_out_ref,
            2_000,
            "Failed to reserve history submission nonce",
          ],
          [`${expired}#0`, 1_000, live],
          [`${expired}#0`, undefined, live],
        ] as const) {
          const refused = yield* Effect.either(
            Journal.reserve({ ...input(), nonce_out_ref: outRef }, tip),
          );
          if (refused._tag !== "Left") throw new Error("Nonce taken over");
          expect(refused.left).toBeInstanceOf(DatabaseError);
          expect(refused.left.message).toBe(message);
          expect(yield* holdings(holder.submission_id)).toContain(outRef);
        }
        for (const [outRef, tip] of [
          [`${expired}#0`, 1_001],
          [`${left}#0`, undefined],
        ] as const) {
          const taker = yield* Journal.reserve(
            { ...input(), nonce_out_ref: outRef },
            tip,
          );
          expect(yield* holdings(taker.submission_id)).toEqual([outRef]);
          expect(yield* holdings(holder.submission_id)).not.toContain(outRef);
        }
      }),
    );
  });

  it("holds another submission's checkpoint while a nonce is chosen, then refuses it that nonce", async () => {
    const [chosen, funder] = [input(), input()];
    const funderRow = await Effect.runPromise(
      provideDatabaseLayers(Journal.reserve(funder)),
    );
    // The funder builds with the chosen nonce, read as unreserved, and saves
    // after the chooser read it and before the chooser reserves it.
    let saving: Promise<unknown> | undefined;
    await run(
      Effect.asVoid(
        Journal.choosingNonce(
          chosen.wallet_address,
          Effect.gen(function* () {
            saving = Effect.runPromise(
              provideDatabaseLayers(
                Effect.either(
                  Journal.saveCheckpoint(
                    funderRow,
                    pending(funderRow, [chosen.request.nonce.txHash]),
                  ),
                ),
              ),
            );
            yield* Effect.promise(() => untilAdvisoryLock(false));
            return yield* Journal.reserve(chosen);
          }),
        ),
      ),
    );
    const saved = await saving!;
    expect(saved).toMatchObject({
      _tag: "Left",
      left: { _tag: "HistoryInputReservedError", outRef: chosen.nonce_out_ref },
    });
    await run(
      Effect.gen(function* () {
        for (const { submission_id, nonce_out_ref } of [chosen, funder])
          expect(yield* holdings(submission_id)).toEqual([nonce_out_ref]);
      }),
    );
  });

  it("frees the wallet from a chooser that outlasts its bound on the wall clock, whatever the caller's clock, and fails having reserved nothing", async () => {
    const [chosen, saver] = [input(), input()];
    const saverRow = await Effect.runPromise(
      provideDatabaseLayers(Journal.reserve(saver)),
    );
    const [order, started] = [[] as string[], Date.now()];
    // The caller's clock never advances, as a test clock nobody adjusts.
    const frozen: Clock.Clock = {
      [Clock.ClockTypeId]: Clock.ClockTypeId,
      unsafeCurrentTimeMillis: () => started,
      currentTimeMillis: Effect.succeed(started),
      unsafeCurrentTimeNanos: () => BigInt(started) * 1_000_000n,
      currentTimeNanos: Effect.succeed(BigInt(started) * 1_000_000n),
      sleep: () => Effect.never,
    };
    const choosing = Effect.runPromise(
      provideDatabaseLayers(
        Effect.either(
          Journal.choosingNonce(
            chosen.wallet_address,
            // A hung provider call: it outlasts the bound on the wall clock.
            Effect.zipRight(
              Effect.promise(
                () => new Promise((resolve) => setTimeout(resolve, 30_000)),
              ),
              Effect.zipRight(
                Effect.sync(() => order.push("chose")),
                Journal.reserve(chosen),
              ),
            ),
            500,
          ),
        ).pipe(Effect.withClock(frozen)),
      ),
    );
    await untilAdvisoryLock(true);
    await run(
      Effect.asVoid(
        Journal.saveCheckpoint(
          saverRow,
          pending(saverRow, [hash(randomUUID())]),
        ),
      ),
    );
    // The save waited only for the bound, never for the chooser's call.
    expect(Date.now() - started).toBeLessThan(30_000);
    expect(await choosing).toMatchObject({
      _tag: "Left",
      left: { _tag: "DatabaseError" },
    });
    expect(order).toEqual([]);
    expect(
      Option.isNone(
        await Effect.runPromise(
          provideDatabaseLayers(Journal.retrieve(chosen.submission_id)),
        ),
      ),
    ).toBe(true);
  }, 60_000);

  it("sets a nonce choice's idle-in-transaction timeout to the bound plus a margin, for that transaction only", async () => {
    const { wallet_address } = input();
    const idleTimeout = Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ pid: number; timeout: string }>`SELECT pg_backend_pid()
        AS pid, current_setting('idle_in_transaction_session_timeout') AS timeout`,
    );
    // One connection, so every read is on the session that made the choice.
    const [before, during, after] = await Effect.runPromise(
      Effect.provide(
        Effect.all([
          idleTimeout,
          Journal.choosingNonce(wallet_address, idleTimeout),
          idleTimeout,
        ]),
        PgClient.layer({
          host: process.env.POSTGRES_HOST ?? "127.0.0.1",
          port: Number(process.env.POSTGRES_PORT ?? "5433"),
          username: process.env.POSTGRES_USER ?? "postgres",
          password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
          database: testDatabaseName(),
          maxConnections: 1,
        }),
      ),
    );
    expect(new Set([before, during, after].map(([row]) => row!.pid)).size).toBe(
      1,
    );
    expect(during[0]!.timeout).toBe("90s");
    expect(before[0]!.timeout).not.toBe("90s");
    expect(after[0]!.timeout).toBe(before[0]!.timeout);
  });

  it("names every nonce of the wallet's submissions, and none of their pending inputs", async () => {
    const fund = hash(randomUUID());
    await run(
      Effect.gen(function* () {
        const rows = [
          yield* Journal.reserve(input()),
          yield* Journal.reserve(input()),
        ];
        yield* Journal.saveCheckpoint(rows[0]!, pending(rows[0]!, [fund]));
        const nonces = yield* Journal.reservedNonces(rows[0]!.wallet_address);
        for (const row of rows) expect(nonces).toContain(row.nonce_out_ref);
        expect(nonces).not.toContain(`${fund}#0`);
      }),
    );
  });
});
