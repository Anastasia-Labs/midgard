/**
 * Every operator queues a settlement job for every event, so another
 * operator often settles it first. The job reads that settlement from the
 * follower's facts and completes once it is cd deep, as it does from its
 * own attempts; while it is shallower the job waits and counts no failure,
 * and a rollback that undoes it puts the job back on its own path.
 *
 * The node's own earlier attempt never reopens a job completed this way.
 *
 * The settlement worker's tick runs against a migrated node database with
 * a real follower store over it, fed a simulated chain whose settlement
 * transactions are built raw.
 */

import { afterAll, afterEach, describe, expect, it } from "vitest";

import { SETTLEMENT_JOB_FAILURE_CAP } from "../src/services/settlement-retry.js";
import {
  admissionTx,
  eventOrder,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import {
  CD,
  closeScenarios,
  dropScenarioDatabases,
  eventIdHex,
  K,
  nonceRef,
  payoutTxs,
  queuedDeposit,
  queuedWithdrawal,
  QUIET,
  scenario,
} from "./helpers/settlement-settled-elsewhere.js";

afterEach(closeScenarios);
afterAll(dropScenarioDatabases);

describe("a settlement another operator landed first", () => {
  it("completes a deposit job once a foreign absorb is cd deep, with no failure and nothing stuck", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("waits on a foreign absorb shallower than cd without counting a failure", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.idle(CD - 2);
    for (let i = 0; i < SETTLEMENT_JOB_FAILURE_CAP; i += 1) await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "absorb", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("puts the job back on its own path when the foreign absorb is rolled back", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "absorb", ...QUIET });

    // The absorb is rolled back: nothing is settled, so the job builds its
    // own absorb again (here refused: this node never projected the event).
    await s.driver.backward(1);
    await s.tick();
    const reopened = await s.job(eventId);
    expect(reopened).toMatchObject({ phase: "absorb", failures: 1 });
    expect(reopened?.last_error).toMatch(
      new RegExp(`^settlement deposit ${eventId} absorb: `, "u"),
    );
    expect(reopened?.last_error).not.toMatch(/retired/u);
  });

  it("completes a withdrawal job once another operator's payout conclusion is cd deep", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund, conclude } = payoutTxs(s, withdrawal, order);
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.idle(CD);
    await s.driver.forward([conclude]);
    // The conclusion is shallower than cd: the job waits.
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "initialize", ...QUIET });

    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("never completes a withdrawal job whose foreign payout is initialized or funded but not concluded", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund } = payoutTxs(s, withdrawal, order);
    // The event is retired cd deep, but the payout output is unspent: the
    // job takes its own path (refused here: this node never projected the
    // withdrawal).
    await s.driver.forward([initialize]);
    await s.idle(CD);
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("initialize");

    // Funded cd deep: the first payout output is spent, its successor is
    // not; the payout is still open.
    await s.driver.forward([fund]);
    await s.idle(CD);
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("initialize");
  });

  it("keeps a job complete that its own settled initialize precedes, after another operator funded and concluded the payout", async () => {
    const s = await scenario();
    // Another deposit, absorbed by another operator; its job is queued
    // later, to be due before the withdrawal's.
    const deposit = eventOrder("deposit", nonceRef());
    const [admitted] = await s.driver.forward([
      admissionTx(deposit, s.driver.chain.nonce()),
    ]);
    await s.driver.forward([
      retirementTx(
        deposit,
        { txHash: admitted!, index: 0 },
        "absorbed",
        s.driver.chain.nonce(),
      ),
    ]);
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund, conclude } = payoutTxs(s, withdrawal, order);
    // This node's initialize lands and stays its latest attempt; another
    // operator funds and concludes the payout.
    await s.ownAttempt("withdrawal", eventId, "initialize", initialize);
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.driver.forward([conclude]);
    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });

    // At the same tip, the own initialize safe but not final: the deposit's
    // job is due first, so the tick builds that one, and the completed
    // withdrawal job must still read complete after it.
    const depositId = eventIdHex(deposit.nonce);
    await s.queue("deposit", depositId);
    await s.tick();
    expect(await s.job(depositId)).toEqual({ phase: "complete", ...QUIET });
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });

    // More than k deep, the prune step stores the attempt final and prunes
    // the retirement; the job stays complete.
    await s.idle(K + 1);
    await s.pruneAll();
    await s.tick();
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });
});
