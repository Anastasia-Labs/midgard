import { randomUUID } from "node:crypto";

import type { IntentStatus } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import * as IntentJournal from "../src/services/intent-journal.js";
import {
  noOpenAttempt,
  settleAttempts,
} from "../src/services/settlement.status.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));
const deploymentId = "a1".repeat(32);
const owner = () => ({
  deploymentId,
  walletAddress: "settlement-wallet",
  token: randomUUID(),
});
const ready = Effect.gen(function* () {
  const token = yield* Authority.acquire({
    deploymentIdentity: deploymentId,
    ownerToken: randomUUID(),
    leaseDurationMs: 60_000,
  });
  yield* Authority.publishReady(token, {
    point: { slot: 10, id: "b1".repeat(32) },
    snapshotDigest: "c1".repeat(32),
  });
  return token;
});
const insertJob = (eventId = "01") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
    VALUES (${deploymentId}, 'deposit', ${eventId}, 'absorb')`;
  });
const attempt = (
  eventId = "01",
  hash = "d1".repeat(32),
): Journal.SettlementAttempt => ({
  deployment_id: deploymentId,
  kind: "deposit",
  event_id: eventId,
  phase: "absorb",
  tx_hash: hash,
  signed_cbor: "test-journal-body",
  required_outputs: [1, 2],
  fee_inputs: [`${hash === "d1".repeat(32) ? "e1".repeat(32) : hash}#0`],
  status: "pending",
});
const depths = { confirmationDepth: 3, securityParameter: 10 };
/**
 * The intent journal's derived status per tx hash (absent: not journaled,
 * or pruned), as `readIntentStatus` answers it; the follower's own
 * derivation is covered in `settlement-derived-status.test.ts`.
 */
const statuses = new Map<string, IntentStatus>();
const landed = (depth: number): IntentStatus => ({
  kind: "landed",
  slot: 100,
  height: 50,
  depth,
});
const live: IntentStatus = { kind: "live", inputsAvailable: true };
const jobPhase = (eventId = "01") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      phase: string;
    }>`SELECT phase FROM settlement_jobs WHERE event_id = ${eventId}`;
    return rows[0]?.phase;
  });
const attemptStatus = (hash = "d1".repeat(32)) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      status: string;
    }>`SELECT status FROM settlement_attempts WHERE tx_hash = ${hash}`;
    return rows[0]?.status;
  });
beforeEach(() => {
  statuses.clear();
  vi.spyOn(IntentJournal, "readIntentStatus").mockImplementation((hash) =>
    Effect.succeed(statuses.get(hash) ?? null),
  );
  return run(resetApplicationTables);
});
afterEach(() => {
  vi.restoreAllMocks();
});

describe("settlement durable submission journal", () => {
  it("enqueues finalized events transactionally and excludes invalid withdrawals", async () => {
    await run(
      Effect.gen(function* () {
        yield* ready;
        const sql = yield* SqlClient.SqlClient;
        const empty = Buffer.alloc(0),
          id = Buffer.from("01", "hex"),
          hash = Buffer.alloc(32),
          ownerHash = Buffer.alloc(28);
        yield* sql`INSERT INTO deposits_utxos (event_id, event_info, inclusion_time, deposit_l1_tx_hash, ledger_tx_id, ledger_output, ledger_address, status)
        VALUES (${id}, ${empty}, clock_timestamp(), ${hash}, ${hash}, ${empty}, 'test-address', 'awaiting')`;
        expect(yield* Journal.nextJob(owner())).toBeUndefined();
        yield* sql`UPDATE deposits_utxos SET status = 'consumed'`;
        expect((yield* Journal.nextJob(owner()))?.kind).toBe("deposit");
        for (const [event, validity] of [
          ["02", "WithdrawalIsValid"],
          ["03", "IncorrectWithdrawalSignature"],
        ]) {
          yield* sql`INSERT INTO withdrawal_utxos
          (event_id, raw_event_info, settlement_event_info, inclusion_time, withdrawal_l1_tx_hash, withdrawal_l1_output_index, asset_name, l2_outref, l2_owner, l2_value, l1_address, l1_datum, refund_address, refund_datum, validity, status)
          VALUES (${Buffer.from(event!, "hex")}, ${empty}, ${empty}, clock_timestamp(), ${hash}, ${Number(event)}, ${id}, ${empty}, ${ownerHash}, ${empty}, ${empty}, ${empty}, ${empty}, ${empty}, ${validity!}, 'finalized')`;
        }
        yield* sql`UPDATE deposits_utxos SET status = 'consumed'`;
        const jobs =
          yield* sql`SELECT kind, event_id FROM settlement_jobs ORDER BY event_id`;
        expect(jobs).toEqual([
          { kind: "deposit", event_id: "01" },
          { kind: "withdrawal", event_id: "02" },
        ]);
      }),
    );
  });
  it("takes the next phase once the attempt is cd deep, and a rollback below cd reverts it", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Journal.saveAttempt(actor, attempt(), Effect.void);
        // Live, then landed short of cd: not confirmed, and it blocks.
        statuses.set(attempt().tx_hash, live);
        expect((yield* settleAttempts(actor, depths))?.attempt.tx_hash).toBe(
          attempt().tx_hash,
        );
        statuses.set(attempt().tx_hash, landed(2));
        expect((yield* settleAttempts(actor, depths))?.level).toBe("open");
        expect(yield* jobPhase()).toBe("absorb");
        // cd deep: safe; the job takes the next phase, nothing blocks.
        statuses.set(attempt().tx_hash, landed(3));
        expect(yield* settleAttempts(actor, depths)).toBeUndefined();
        expect(yield* jobPhase()).toBe("complete");
        // A rollback deeper than cd: it is live again, so the job
        // returns to the attempt's phase and the attempt blocks again.
        statuses.set(attempt().tx_hash, live);
        expect((yield* settleAttempts(actor, depths))?.attempt.tx_hash).toBe(
          attempt().tx_hash,
        );
        expect(yield* jobPhase()).toBe("absorb");
        // Relanded past k: final by derivation, still not stored final
        // (only the follower's prune step stores it).
        statuses.set(attempt().tx_hash, landed(11));
        expect(yield* settleAttempts(actor, depths)).toBeUndefined();
        expect(yield* jobPhase()).toBe("complete");
        expect(yield* attemptStatus()).toBe("pending");
      }),
    );
  });
  it("refuses new work while an attempt reads short of cd, so a fee coin a rollback restored is never reused", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob("01");
        yield* insertJob("02");
        yield* Journal.saveAttempt(
          actor,
          attempt(),
          noOpenAttempt(actor, depths),
        );
        statuses.set(attempt().tx_hash, landed(3));
        yield* settleAttempts(actor, depths);
        // A rollback restores the first attempt's fee coin before the next
        // body is journaled: the check under the owner-row lock refuses it.
        statuses.set(attempt().tx_hash, landed(2));
        const reusing = {
          ...attempt("02", "d2".repeat(32)),
          fee_inputs: attempt().fee_inputs,
        };
        const refused = yield* Effect.either(
          Journal.saveAttempt(actor, reusing, noOpenAttempt(actor, depths)),
        );
        expect(refused._tag).toBe("Left");
        expect(yield* attemptStatus("d2".repeat(32))).toBeUndefined();
        // Relanded cd deep: the next body is journaled.
        statuses.set(attempt().tx_hash, landed(4));
        yield* Journal.saveAttempt(
          actor,
          reusing,
          noOpenAttempt(actor, depths),
        );
        expect(yield* attemptStatus("d2".repeat(32))).toBe("pending");
      }),
    );
  });
  it("preserves a signed attempt across database scopes and holds history retention until it is final", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* insertJob("02");
        yield* Journal.saveAttempt(actor, attempt(), Effect.void);
      }),
    );
    const [stored] = await run(Journal.openAttempts(deploymentId));
    expect(stored).toMatchObject({
      ...attempt(),
      job_phase: "absorb",
      latest: true,
    });
    expect(await run(Journal.settlementRetentionHoldSlot(deploymentId))).toBe(
      10,
    );
    await run(
      Effect.gen(function* () {
        // Safe is not final: the hold stays while it may still revert.
        statuses.set(attempt().tx_hash, landed(3));
        yield* settleAttempts(actor, depths);
        expect(yield* Journal.settlementRetentionHoldSlot(deploymentId)).toBe(
          10,
        );
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE settlement_attempts SET status = 'final'`;
        expect(
          yield* Journal.settlementRetentionHoldSlot(deploymentId),
        ).toBeUndefined();
        // A final attempt of a complete job is no longer read.
        expect(yield* Journal.openAttempts(deploymentId)).toEqual([]);
      }),
    );
  });
  it("advances a job whose attempt was stored final while the tick did not run", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Journal.saveAttempt(actor, attempt(), Effect.void);
        // Pruned from the journal past k: it derives nothing any more.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE settlement_attempts SET status = 'final'`;
        expect(yield* settleAttempts(actor, depths)).toBeUndefined();
        expect(yield* jobPhase()).toBe("complete");
        expect(yield* Journal.openAttempts(deploymentId)).toEqual([]);
      }),
    );
  });
  it("expires an attempt back to its phase, and keys the job's phase to its latest attempt only", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        const first = attempt();
        const second = attempt("01", "d2".repeat(32));
        yield* Journal.saveAttempt(actor, first, Effect.void);
        yield* Journal.saveAttempt(actor, second, Effect.void);
        const open = yield* Journal.openAttempts(deploymentId);
        expect(open.map((a) => [a.tx_hash, a.latest])).toEqual([
          [first.tx_hash, false],
          [second.tx_hash, true],
        ]);
        // The older attempt settling does not move the job; it is not latest.
        statuses.set(first.tx_hash, landed(3));
        statuses.set(second.tx_hash, live);
        expect((yield* settleAttempts(actor, depths))?.attempt.tx_hash).toBe(
          second.tx_hash,
        );
        expect(yield* jobPhase()).toBe("absorb");
        yield* Journal.expireAttempt(actor, second);
        expect(yield* attemptStatus(second.tx_hash)).toBe("expired");
        // The expired attempt leaves the set; the first is latest again.
        expect(
          (yield* Journal.openAttempts(deploymentId)).map((a) => [
            a.tx_hash,
            a.latest,
          ]),
        ).toEqual([[first.tx_hash, true]]);
        expect(yield* settleAttempts(actor, depths)).toBeUndefined();
        expect(yield* jobPhase()).toBe("complete");
      }),
    );
  });
  it("fences a stale worker and refuses wallet rebinding", async () => {
    const first = owner(),
      second = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(first);
        yield* insertJob();
        expect((yield* Effect.either(Journal.renew(second)))._tag).toBe("Left");
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE settlement_owners SET lease_until = clock_timestamp() - interval '1 second'`;
        yield* Journal.renew(second);
        expect(
          (yield* Effect.either(
            Journal.saveAttempt(first, attempt(), Effect.void),
          ))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(
            Journal.renew({ ...second, walletAddress: "different-wallet" }),
          ))._tag,
        ).toBe("Left");
        yield* Journal.saveAttempt(second, attempt(), Effect.void);
      }),
    );
  });
  it("does not checkpoint new work while the canonical history owner is recovering", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        const history = yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Authority.beginRecovery(history, "test rollback");
        expect(
          (yield* Effect.either(
            Journal.saveAttempt(actor, attempt(), Effect.void),
          ))._tag,
        ).toBe("Left");
        expect(yield* Journal.openAttempts(deploymentId)).toEqual([]);
      }),
    );
  });
});
