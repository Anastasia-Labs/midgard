/**
 * The node's §8.4 operator-set predicates (I1), each in both polarities:
 * an operator transition's intent is wanted while the operator-set
 * projection still allows the transition, and is `false` once the facts
 * rule it out. The key names the operator (`<verb>:...:<key>`); a node
 * without its operator key, or a bond recovery for another operator,
 * throws (held, never abandoned).
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  FOREIGN,
  openPredicateScenario,
  OWN,
  type PredicateScenario,
} from "./helpers/intent-predicates-scenario.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";

const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;
beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

let spares = 0;
const open = async () => {
  spares = 0;
  return openPredicateScenario(fixture, opened);
};

/** Journals an operator intent under `workflowKey` (each spends its own spare). */
const intent = (s: PredicateScenario, family: string, workflowKey: string) =>
  s.record(family, workflowKey, s.spend([s.spare(spares++)]), null);

describe("the node's operator-set predicates over the follower's projections", () => {
  it("register: wanted while the operator is neither registered nor active", async () => {
    const s = await open();
    const register = await intent(s, "register", `register:${FOREIGN}`);
    expect(await s.verdict(register)).toBe(true);
    await s.land(s.lists.insert(s.live(), "registered", FOREIGN));
    expect(await s.verdict(register)).toBe(false);
  });

  it("activate: wanted while registered and not yet active", async () => {
    const s = await open();
    const activate = await intent(s, "activate", `activate:${FOREIGN}`);
    expect(await s.verdict(activate)).toBe(false);
    await s.land(s.lists.insert(s.live(), "registered", FOREIGN));
    expect(await s.verdict(activate)).toBe(true);
    await s.land(s.lists.insert(s.live(), "active", FOREIGN));
    expect(await s.verdict(activate)).toBe(false);
  });

  it("deregister: wanted while registered", async () => {
    const s = await open();
    const deregister = await intent(s, "deregister", `deregister:${FOREIGN}`);
    expect(await s.verdict(deregister)).toBe(false);
    await s.land(s.lists.insert(s.live(), "registered", FOREIGN));
    expect(await s.verdict(deregister)).toBe(true);
    await s.land(s.lists.remove(s.live(), "registered", FOREIGN));
    expect(await s.verdict(deregister)).toBe(false);
  });

  it("retire: wanted while active", async () => {
    const s = await open();
    const retire = await intent(s, "retire", `retire:voluntary:${OWN}`);
    expect(await s.verdict(retire)).toBe(false);
    await s.land(s.lists.insert(s.live(), "active", OWN));
    expect(await s.verdict(retire)).toBe(true);
    await s.land(s.lists.remove(s.live(), "active", OWN));
    expect(await s.verdict(retire)).toBe(false);
  });

  it("recover_bond: wanted while this operator's retired node is live; another operator's throws", async () => {
    const s = await open();
    const recover = await intent(s, "recover_bond", `recover_bond:${OWN}`);
    expect(await s.verdict(recover)).toBe(false);
    await s.land(s.lists.insert(s.live(), "retired", OWN));
    expect(await s.verdict(recover)).toBe(true);
    await s.land(s.lists.remove(s.live(), "retired", OWN));
    expect(await s.verdict(recover)).toBe(false);
    const foreign = await intent(s, "recover_bond", `recover_bond:${FOREIGN}`);
    await expect(s.verdict(foreign)).rejects.toThrow("not this operator's");
  });

  it("exit (duplicate slash): wanted while the duplicate registered node is listed", async () => {
    const s = await open();
    const exit = await intent(s, "exit", `exit:slash_duplicate:${FOREIGN}`);
    expect(await s.verdict(exit)).toBe(false);
    await s.land(s.lists.insert(s.live(), "registered", FOREIGN));
    expect(await s.verdict(exit)).toBe(true);
    await s.land(s.lists.remove(s.live(), "registered", FOREIGN));
    expect(await s.verdict(exit)).toBe(false);
  });

  it("takeover: wanted while the skipped operator still holds a shift that started before the new one", async () => {
    const s = await open();
    await s.land(s.lists.shift(s.live(), FOREIGN, 1_000n));
    const takeover = await intent(s, "takeover", `takeover:${FOREIGN}:2000`);
    expect(await s.verdict(takeover)).toBe(true);
    await s.land(s.lists.shift(s.live(), OWN, 2_000n));
    expect(await s.verdict(takeover)).toBe(false);
    const early = await intent(s, "takeover", `takeover:${OWN}:2000`);
    expect(await s.verdict(early)).toBe(false);
  });

  it("scheduler refresh: wanted while the scheduler output it spends is live", async () => {
    const s = await open();
    const scheduler = s.lists.liveScheduler(s.live())!;
    const refresh = await intent(
      s,
      "scheduler_refresh",
      `scheduler:${scheduler.outRef.txHash.toString("hex")}#${scheduler.outRef.index.toString()}`,
    );
    expect(await s.verdict(refresh)).toBe(true);
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    expect(await s.verdict(refresh)).toBe(false);
  });

  it("without the operator key, an operator transition throws", async () => {
    const s = await open();
    const register = await intent(s, "register", `register:${FOREIGN}`);
    await expect(s.verdict(register, { operatorSet: null })).rejects.toThrow(
      "operator key is unreadable",
    );
  });
});
