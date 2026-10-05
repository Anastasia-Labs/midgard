import { createHash, randomUUID } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { DatabaseError } from "../src/database/utils/common.js";
import type { Database } from "../src/services/database.js";
import { provideDatabaseLayers } from "./utils.js";

const hash = (seed: string) => createHash("sha256").update(seed).digest("hex");

const input = (): Omit<Journal.Row, "revision"> => {
  const nonce = hash(randomUUID());
  return {
    submission_id: `reservation-${randomUUID()}`,
    kind: "Deposit",
    policy_id: "aa".repeat(28),
    wallet_address: "reservation-test-wallet",
    intent_hash: "bb".repeat(32),
    nonce_out_ref: `${nonce}#0`,
    request: {
      payloadCbor: "d87980",
      reclaimAuthCbor: "d87980",
      assets: { lovelace: "25000000" },
      structuralLovelace: "2500000",
      structuralRefundKey: "cc".repeat(28),
      nonce: {
        txHash: nonce,
        outputIndex: 0,
        address: "reservation-test-wallet",
        assets: { lovelace: "30000000" },
      },
    },
    checkpoint: { requestHash: "dd".repeat(32) },
  };
};

/** A completed body spending `txHashes`#0, with a TTL slot when given. */
const attempt = (
  txHashes: readonly string[],
  ttl?: number,
  phase: SDK.EventHistorySubmissionAttempt["phase"] = "Admission",
): SDK.EventHistorySubmissionAttempt => {
  const transactionCbor = `84a${ttl === undefined ? 3 : 4}008${txHashes.length}${txHashes
    .map((txHash) => `825820${txHash}00`)
    .join("")}01800200${
    ttl === undefined ? "" : `031a${ttl.toString(16).padStart(8, "0")}`
  }a0f5f6`;
  return {
    phase,
    outputIndex: 0,
    transactionCbor,
    txHash: CML.hash_transaction(
      CML.Transaction.from_cbor_hex(transactionCbor).body(),
    ).to_hex(),
  };
};

const pending = (row: Journal.Row, txHashes: readonly string[], ttl?: number) =>
  ({ ...row.checkpoint, pending: attempt(txHashes, ttl) }) as const;

const holdings = (submissionId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ out_ref: string }>`SELECT out_ref
      FROM event_history_submission_inputs WHERE submission_id = ${submissionId}`;
    return rows.map((row) => row.out_ref).sort();
  });

/** A reservation row as the code before release left it behind. */
const leftBehind = (outRef: string, submissionId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO event_history_submission_inputs (out_ref, submission_id)
      VALUES (${outRef}, ${submissionId})`;
  });

const run = <E>(
  effect: Effect.Effect<void, E, SqlClient.SqlClient | Database>,
) => Effect.runPromise(provideDatabaseLayers(effect));

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
        yield* Journal.saveCheckpoint(other, {
          ...other.checkpoint,
          pending: attempt([forever], undefined, "Publication"),
        });
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

  it("hides only inputs their holder can no longer spend from nonce and funding selection", async () => {
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
});
