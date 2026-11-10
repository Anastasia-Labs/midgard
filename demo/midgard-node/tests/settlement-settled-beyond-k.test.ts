/**
 * A node down for more than k blocks comes back to settlements another
 * operator landed meanwhile: the follower has pruned the settling facts
 * (#869). The job reads the event as retired more than k deep and
 * completes, instead of building again for an event that is gone and
 * retrying forever. Pruning never completes a job whose event is still
 * open, or one the follower never admitted.
 */
import "./utils.js";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import { SETTLEMENT_JOB_FAILURE_CAP } from "../src/services/settlement-retry.js";
import { eventOrder, retirementTx } from "./helpers/l1-events-chain.js";
import {
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

describe("a settlement another operator landed while the node was down for more than k blocks", () => {
  it("completes a deposit job whose foreign absorb the follower has pruned, with no failure and nothing stuck", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.idle(K + 1);
    await s.pruneAll();
    for (let i = 0; i < SETTLEMENT_JOB_FAILURE_CAP; i += 1) await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("completes a withdrawal job whose foreign payout the follower has pruned from initialize to conclusion", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund, conclude } = payoutTxs(s, withdrawal, order);
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.driver.forward([conclude]);
    await s.idle(K + 1);
    await s.pruneAll();
    for (let i = 0; i < SETTLEMENT_JOB_FAILURE_CAP; i += 1) await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("never completes a withdrawal job whose pruned initialize left the payout unconcluded", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund } = payoutTxs(s, withdrawal, order);
    // Retired and funded more than k deep: the event's rows and the spent
    // first payout output are pruned, the funded one is still live.
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.idle(K + 1);
    await s.pruneAll();
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("initialize");
  });

  it("never completes a job whose event the follower never admitted", async () => {
    const s = await scenario();
    const eventId = eventIdHex(eventOrder("deposit", nonceRef()).nonce);
    await s.queue("deposit", eventId);
    await s.idle(K + 1);
    await s.pruneAll();
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("absorb");
  });
});
