import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import {
  reconcileRestoredSettlementFees,
  reconcileSettlementReceipts,
} from "../src/services/settlement.js";
import * as publicationProvider from "../src/transactions/reference-publication-provider.js";
import { provideDatabaseLayers } from "./utils.js";

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
  recovery: false,
});
beforeEach(() =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE settlement_attempts, settlement_jobs, settlement_owners, event_history_authority, deposits_utxos, withdrawal_utxos CASCADE`;
    }),
  ),
);

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
        expect(yield* Journal.nextJob(owner(), "0")).toBeUndefined();
        yield* sql`UPDATE deposits_utxos SET status = 'consumed'`;
        expect((yield* Journal.nextJob(owner(), "0"))?.kind).toBe("deposit");
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
  it("waits for a restored indexer before reactivating an old confirmed receipt", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Journal.saveAttempt(actor, attempt());
        yield* Journal.finishAttempt(
          actor,
          attempt(),
          "confirmed",
          "complete",
          "0",
        );
      }),
    );
    let caughtUp = false;
    const barrier = vi
      .spyOn(publicationProvider, "synchronizePublicationIndexerPoint")
      .mockImplementation(async () => {
        caughtUp = true;
        return { slot: 1000, id: "ee".repeat(32) };
      });
    const transactionStatus = vi.fn(async (txHash: string) =>
      caughtUp
        ? {
            status: "confirmed" as const,
            txHash,
            confirmation: { txHash, blockHash: "ee".repeat(32), slot: 11 },
          }
        : { status: "not_found" as const, txHash },
    );
    try {
      expect(
        await run(
          reconcileSettlementReceipts(
            actor,
            {
              deployment_id: deploymentId,
              kind: "deposit",
              event_id: "01",
              phase: "complete",
              failures: 0,
              verified_generation: "0",
            },
            { transactionStatus },
          ),
        ),
      ).toBe(true);
      expect(barrier).toHaveBeenCalledOnce();
      expect(transactionStatus).toHaveBeenCalledOnce();
      expect(await run(Journal.pending(deploymentId))).toBeUndefined();
    } finally {
      barrier.mockRestore();
    }
  });
  it("does not trust an old-fork confirmation while the indexer catches up after rollback", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Journal.saveAttempt(actor, attempt());
        yield* Journal.finishAttempt(
          actor,
          attempt(),
          "confirmed",
          "complete",
          "0",
        );
      }),
    );
    let caughtUp = false;
    const barrier = vi
      .spyOn(publicationProvider, "synchronizePublicationIndexerPoint")
      .mockImplementation(async () => {
        caughtUp = true;
        return { slot: 1000, id: "ee".repeat(32) };
      });
    const transactionStatus = vi.fn(async (txHash: string) =>
      caughtUp
        ? { status: "not_found" as const, txHash }
        : {
            status: "confirmed" as const,
            txHash,
            confirmation: { txHash, blockHash: "bb".repeat(32), slot: 11 },
          },
    );
    try {
      expect(
        await run(
          reconcileSettlementReceipts(
            actor,
            {
              deployment_id: deploymentId,
              kind: "deposit",
              event_id: "01",
              phase: "complete",
              failures: 0,
              verified_generation: "0",
            },
            { transactionStatus },
          ),
        ),
      ).toBe(false);
      expect(barrier).toHaveBeenCalledOnce();
      expect((await run(Journal.pending(deploymentId)))?.recovery).toBe(true);
    } finally {
      barrier.mockRestore();
    }
  });
  it("gives new work a turn ahead of old completed-history recovery without starving either queue", async () => {
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* insertJob("01");
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE settlement_jobs SET phase = 'complete', due_at = clock_timestamp() - interval '1 day', verified_generation = 0`;
        yield* insertJob("02");
        expect((yield* Journal.nextJob(owner(), "1", false))?.event_id).toBe(
          "02",
        );
        expect((yield* Journal.nextJob(owner(), "1", true))?.event_id).toBe(
          "01",
        );
        yield* sql`UPDATE settlement_jobs SET verified_generation = 1 WHERE event_id = '01'`;
        expect((yield* Journal.nextJob(owner(), "1", true))?.event_id).toBe(
          "02",
        );
      }),
    );
  });
  it("recovers a rolled-back receipt before fresh work can reuse its restored fee coin", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob("01");
        yield* Journal.saveAttempt(actor, attempt());
        yield* Journal.finishAttempt(
          actor,
          attempt(),
          "confirmed",
          "complete",
          "0",
        );
        yield* insertJob("02");
        // Refuse a fresh body even if the rollback occurs during its build.
        expect(
          (yield* Effect.either(
            Journal.saveAttempt(actor, {
              ...attempt("02", "d2".repeat(32)),
              fee_inputs: attempt().fee_inputs,
            }),
          ))._tag,
        ).toBe("Left");
        expect(yield* Journal.pending(deploymentId)).toBeUndefined();
        expect((yield* Journal.nextJob(actor, "1", false))?.event_id).toBe(
          "02",
        );
      }),
    );
    const barrier = vi
      .spyOn(publicationProvider, "synchronizePublicationIndexerPoint")
      .mockResolvedValue({ slot: 1000, id: "ee".repeat(32) });
    const utxosAt = vi.fn(async () => [
      {
        txHash: "e1".repeat(32),
        outputIndex: 0,
        address: actor.walletAddress,
        assets: { lovelace: 10_000_000n },
      },
    ]);
    const transactionStatus = vi.fn(async (txHash: string) => ({
      status: "not_found" as const,
      txHash,
    }));
    try {
      expect(
        await run(
          reconcileRestoredSettlementFees(actor, {
            utxosAt,
            transactionStatus,
          }),
        ),
      ).toBe(false);
      expect(barrier).toHaveBeenCalledOnce();
      expect((await run(Journal.pending(deploymentId)))?.tx_hash).toBe(
        attempt().tx_hash,
      );
      expect((await run(Journal.pending(deploymentId)))?.recovery).toBe(true);
    } finally {
      barrier.mockRestore();
    }
  });
  it("preserves a signed attempt across database scopes and excludes another job until reconciliation", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* insertJob("02");
        yield* Journal.saveAttempt(actor, attempt());
      }),
    );
    const stored = await run(Journal.pending(deploymentId));
    expect(stored).toMatchObject(attempt());
    expect(await run(Journal.settlementRetentionHoldSlot(deploymentId))).toBe(
      10,
    );
    await run(
      Effect.gen(function* () {
        expect(
          (yield* Effect.either(
            Journal.saveAttempt(actor, attempt("02", "d2".repeat(32))),
          ))._tag,
        ).toBe("Left");
        yield* Journal.finishAttempt(
          actor,
          attempt(),
          "confirmed",
          "complete",
          "0",
        );
        expect(
          yield* Journal.settlementRetentionHoldSlot(deploymentId),
        ).toBeUndefined();
        yield* Journal.saveAttempt(actor, attempt("02", "d2".repeat(32)));
      }),
    );
    expect((await run(Journal.pending(deploymentId)))?.event_id).toBe("02");
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
          (yield* Effect.either(Journal.saveAttempt(first, attempt())))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(
            Journal.renew({ ...second, walletAddress: "different-wallet" }),
          ))._tag,
        ).toBe("Left");
        yield* Journal.saveAttempt(second, attempt());
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
          (yield* Effect.either(Journal.saveAttempt(actor, attempt())))._tag,
        ).toBe("Left");
        expect(yield* Journal.pending(deploymentId)).toBeUndefined();
      }),
    );
  });
  it("queues a rolled-back receipt without losing the body or trusting the new generation", async () => {
    const actor = owner();
    await run(
      Effect.gen(function* () {
        yield* ready;
        yield* Journal.renew(actor);
        yield* insertJob();
        yield* Journal.saveAttempt(actor, attempt());
        yield* Journal.finishAttempt(
          actor,
          attempt(),
          "confirmed",
          "complete",
          "0",
        );
        expect(
          yield* Journal.settlementRetentionHoldSlot(deploymentId),
        ).toBeUndefined();
        yield* Journal.resumeReceipt(actor, attempt());
        const pending = yield* Journal.pending(deploymentId);
        expect(pending?.recovery).toBe(true);
        expect(pending?.signed_cbor).toBe(attempt().signed_cbor);
        if (pending === undefined) throw new Error("Missing recovered attempt");
        yield* Journal.finishAttempt(actor, pending, "expired", "absorb", "1");
        expect((yield* Journal.nextJob(actor, "1"))?.verified_generation).toBe(
          "-1",
        );
      }),
    );
  });
});
