/**
 * An expired shift whose successor is idle, against the real compiled
 * scheduler validator.
 *
 * An ended shift passes only to its one legal successor (tail → … → head,
 * then a rewind to the tail), and anyone may submit that refresh. The
 * operator that holds commit work drives the node's own scheduler alignment;
 * when it is not the successor it pushes the turn one hop, waits for that
 * pushed shift to end and repeats, until it appoints itself. The idle
 * operators submit nothing.
 */

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveSchedulerRefreshWitnessSelection } from "../src/workers/utils/scheduler-refresh.resolve-scheduler-refresh-witness-selection.js";
import {
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
} from "./helpers/operator-inactivity.js";
import {
  activeOperator,
  alignForCommit,
  expectPushTo,
  expectSelfAppointment,
  expectWaitForShiftEnd,
  initScenario,
  letShiftEnd,
  walletLovelace,
} from "./helpers/scheduler-successor-scenario.js";

describe("an idle scheduler successor does not freeze block production", () => {
  it("two operators: the tail holds work, pushes the turn to the idle head, then rewinds to itself", async () => {
    const scenario = await initScenario(2);
    const { fixture } = scenario;
    const [head, tail] = fixture.operators;
    if (head === undefined || tail === undefined) {
      throw new Error("Expected two fixture operators");
    }
    const appointed = await appointFirstSchedulerOperator(fixture);
    expect(appointed.operatorKeyHash).toBe(tail.keyHash);
    await letShiftEnd(fixture);

    const headLovelace = await walletLovelace(scenario, head.keyHash);
    const tailBeforePush = await walletLovelace(scenario, tail.keyHash);
    await expectPushTo(scenario, tail.keyHash, head.keyHash);
    // The work-holder paid for the push; the idle head paid nothing.
    expect(await walletLovelace(scenario, tail.keyHash)).toBeLessThan(
      tailBeforePush,
    );
    expect(await walletLovelace(scenario, head.keyHash)).toBe(headLovelace);

    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, tail.keyHash);
    expect(await walletLovelace(scenario, head.keyHash)).toBe(headLovelace);
  });

  it("two operators at genesis: the head holds work, appoints the idle tail first, then advances to itself", async () => {
    const scenario = await initScenario(2);
    const { fixture } = scenario;
    const [head, tail] = fixture.operators;
    if (head === undefined || tail === undefined) {
      throw new Error("Expected two fixture operators");
    }
    expect(await fetchSchedulerDatum(fixture)).toBe("NoActiveOperators");
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    // The first appointment backdates its lower bound by 30 s, before the
    // instant the Lucid instances were made, so let the clock run first.
    fixture.emulator.awaitSlot(60);

    // Only the tail may be appointed first, and anyone may appoint it.
    await expectPushTo(scenario, head.keyHash, tail.keyHash);
    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, head.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);
  });

  it("three operators: a work-holder two hops away pushes one hop at a time, by advance and by rewind", async () => {
    const scenario = await initScenario(3);
    const { fixture } = scenario;
    const [first, middle, last] = fixture.operators;
    if (first === undefined || middle === undefined || last === undefined) {
      throw new Error("Expected three fixture operators");
    }
    // Rotation: last (tail) → middle → first (head) → rewind → last.
    const appointed = await appointFirstSchedulerOperator(fixture);
    expect(appointed.operatorKeyHash).toBe(last.keyHash);
    await letShiftEnd(fixture);

    // The head holds work two hops away: two advances.
    await expectPushTo(scenario, first.keyHash, middle.keyHash);
    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, first.keyHash);

    // Then the middle operator holds work, two hops away again: the push is
    // the head's rewind to the tail, then an advance to itself.
    await letShiftEnd(fixture);
    await expectPushTo(scenario, middle.keyHash, last.keyHash);
    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, middle.keyHash);
  });

  it("two operators at a shift end, both holding work: the push that loses to the successor's own takeover does not hold the commit tick", async () => {
    // The tail's own L1 node accepts its push, but the head's takeover of the
    // same scheduler input lands first, so the chain never includes the push.
    let takeOver: (() => Promise<void>) | undefined;
    let droppedPush: string | undefined;
    const scenario = await initScenario(2, {
      operator: 1,
      submit: async (emulator, tx) => {
        if (takeOver === undefined) return emulator.submitTx(tx);
        const landFirst = takeOver;
        takeOver = undefined;
        await landFirst();
        droppedPush = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(tx).body(),
        ).to_hex();
        return droppedPush;
      },
    });
    const { fixture } = scenario;
    const [head, tail] = fixture.operators;
    if (head === undefined || tail === undefined) {
      throw new Error("Expected two fixture operators");
    }
    await appointFirstSchedulerOperator(fixture);
    await letShiftEnd(fixture);
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    takeOver = () => expectSelfAppointment(scenario, head.keyHash);

    // A commitment tick holds the L1 control plane for at most 180 s; the
    // losing push must give it back long before that.
    const held = new Promise<"held">((resolve) =>
      setTimeout(() => resolve("held"), 60_000).unref(),
    );
    const outcome = await Promise.race([
      alignForCommit(scenario, tail.keyHash),
      held,
    ]);
    expect(droppedPush).toBeDefined();
    if (outcome === "held") {
      throw new Error("The losing push held the commit tick for 60 s");
    }
    const taken = activeOperator(await fetchSchedulerDatum(fixture));
    expect(taken.operator).toBe(head.keyHash);
    expectWaitForShiftEnd(fixture, outcome.result, taken.startTime);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);

    // The rotation continues: the tail takes the next turn.
    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, tail.keyHash);
  });

  it("two operators at a shift end, both holding work: the push its L1 node refuses because the successor's takeover already spent its input is a lost race, not an error", async () => {
    // The head's takeover of the same scheduler input lands just before the
    // tail's push reaches the tail's L1 node, which refuses the push.
    let takeOver: (() => Promise<void>) | undefined;
    let refused: unknown;
    const scenario = await initScenario(2, {
      operator: 1,
      submit: async (emulator, tx) => {
        if (takeOver === undefined) return emulator.submitTx(tx);
        const landFirst = takeOver;
        takeOver = undefined;
        await landFirst();
        try {
          return await emulator.submitTx(tx);
        } catch (error) {
          refused = error;
          throw error;
        }
      },
    });
    const { fixture } = scenario;
    const [head, tail] = fixture.operators;
    if (head === undefined || tail === undefined) {
      throw new Error("Expected two fixture operators");
    }
    await appointFirstSchedulerOperator(fixture);
    await letShiftEnd(fixture);
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    takeOver = () => expectSelfAppointment(scenario, head.keyHash);

    // The successor's turn is on chain, which is all the push was for: the
    // alignment goes on from it at once, without the exact-transaction wait
    // a refusal with no such state gets.
    const held = new Promise<"held">((resolve) =>
      setTimeout(() => resolve("held"), 60_000).unref(),
    );
    const outcome = await Promise.race([
      alignForCommit(scenario, tail.keyHash),
      held,
    ]);
    expect(String(refused)).toMatch(/already spent/);
    if (outcome === "held") {
      throw new Error("The refused push held the commit tick for 60 s");
    }
    const taken = activeOperator(await fetchSchedulerDatum(fixture));
    expect(taken.operator).toBe(head.keyHash);
    expectWaitForShiftEnd(fixture, outcome.result, taken.startTime);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);

    // The rotation continues: the tail takes the next turn.
    await letShiftEnd(fixture);
    await expectSelfAppointment(scenario, tail.keyHash);
  });

  it("refuses to push from a current operator that is in no active-operators node", async () => {
    const { fixture } = await initScenario(2);
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const tail = fixture.operators[1];
    if (tail === undefined) {
      throw new Error("Expected two fixture operators");
    }
    expect(() =>
      resolveSchedulerRefreshWitnessSelection({
        currentOperator: "ff".repeat(28),
        targetOperator: tail.keyHash,
        activeNodes: snapshot.active,
        registeredNodes: snapshot.registered,
        allowGenesisRewind: false,
      }),
    ).toThrow(/has no successor in the active-operators list/);
  });
});
