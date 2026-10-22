/**
 * The validation-trace defender killed and restarted, on the real
 * deployment in the emulator, over the node's follower, intent journal and
 * S6: wherever the node stops, the restarted one builds nothing again for a
 * key whose journaled transaction is live, S6 alone sends the journaled
 * bytes, and the outcome is the one without the crash.
 *
 * - Killed once a Defend is journaled, before the chain holds it, with an
 *   operator wallet that funds that one Defend alone: S6 sends it after the
 *   restart, and it lands in time; waiting on it is no shortfall.
 * - Killed once the chain holds the Defend, before it lands: nothing is
 *   sent again, and it lands.
 * - Killed mid-bisection, once a reveal is journaled: S6 sends it after the
 *   restart, and the game goes on from it with our clock above zero.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  challengerReveal,
  falseClaimMaterial,
  landWith,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import { defendKey } from "../src/validation-trace-defense/play.js";
import { validationTraceDefenseTick } from "../src/validation-trace-defense/tick.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  type DefenseScene,
  drainBase,
  openDefenseScene,
  startedAtOf,
} from "./helpers/validation-trace-defense-scene.js";

const databases = testDatabases();
const opened: DefenseScene[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map(({ env }) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

/** The scene with a false claim on our passing block, followed. */
const openWithClaim = async () => {
  const s = await openDefenseScene(databases);
  opened.push(s);
  const claim = await openFalseClaim(
    s.scene,
    await falseClaimMaterial(s.scene),
    40,
  );
  await s.env.follow();
  return { s, claim };
};

/** A tick of the restarted node with the chain held still. */
const stillTick = (s: DefenseScene) =>
  s.inNode(validationTraceDefenseTick(s.port));

/** The restart takes a few slots of L1 time; nothing lands meanwhile. */
const restartLater = async (s: DefenseScene) => {
  s.scene.h.emulator.awaitSlot(3);
  await s.restart();
  expect(
    await s.inNode(StateQueueMutationLeasesDB.retrieveActive()),
  ).toBeUndefined();
};

const threadOf = (s: DefenseScene, unit: string) => {
  const utxo = s.liveByUnit(unit)!;
  const { data } = Data.from(utxo.datum!, SDK.CaseThreadDatum);
  return { utxo, thread: data! };
};

/** The Defend journaled under the case's key landed in time, alone, and
 * our node and the case's thread are at it; then our first reveal follows. */
const defendedOnce = async (
  s: DefenseScene,
  claim: Awaited<ReturnType<typeof openWithClaim>>["claim"],
  defendBytes: Buffer,
) => {
  const [defend, ...more] = await s.journaled(defendKey(claim.caseUnit));
  expect(more).toStrictEqual([]);
  expect(defend!.txCbor).toStrictEqual(defendBytes);
  expect(defend!.status).toBe("landed");
  expect(s.liveByUnit(claim.caseUnit)!.txHash).toBe(defend!.txHash);
  expect(s.liveByUnit(s.scene.operator.nodeUnit)!.txHash).toBe(defend!.txHash);
  expect(s.landed(defend!.txHash).at).toBeLessThan(
    startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
  );
  expect(await s.tick()).toMatchObject([
    { kind: "reveal", outcome: "submitted" },
  ]);
  expect(await s.env.stage.run()).toStrictEqual([]);
};

describe("the validation-trace defender across a restart", () => {
  it("leaves a Defend journaled before the chain held it to S6, which sends it after the restart", async () => {
    const { s, claim } = await openWithClaim();
    await drainBase(s, 100_000_000n);
    const bytes = await s.tickKilledAt(s.killAtSubmit("before"));
    expect(await s.journaled(defendKey(claim.caseUnit))).toMatchObject([
      { txCbor: bytes, status: "live" },
    ]);
    expect(s.pending()).toBe(false);
    await restartLater(s);

    const sentBefore = s.env.sent.length;
    expect(await stillTick(s)).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "pending" },
    ]);
    // Waiting on its live attempt is not a shortfall.
    expect((await s.readiness()).reasons).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    expect(await s.env.stage.run()).toStrictEqual([]);
    expect(s.env.sent.slice(sentBefore)).toStrictEqual([bytes]);
    await s.land();
    await defendedOnce(s, claim, bytes);
  });

  it("sends nothing again for a Defend the chain held before the restart, and it lands", async () => {
    const { s, claim } = await openWithClaim();
    const bytes = await s.tickKilledAt(s.killAtSubmit("after"));
    expect(s.pending()).toBe(true);
    await s.restart();
    expect(
      await s.inNode(StateQueueMutationLeasesDB.retrieveActive()),
    ).toBeUndefined();

    const sentBefore = s.env.sent.length;
    expect(await stillTick(s)).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "pending" },
    ]);
    // Waiting on its live attempt is not a shortfall.
    expect((await s.readiness()).reasons).toStrictEqual([]);
    expect(await s.env.stage.run()).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    await s.land();
    await defendedOnce(s, claim, bytes);
  });

  it("leaves a reveal journaled mid-bisection to S6, and the game goes on from it after the restart", async () => {
    const { s, claim } = await openWithClaim();
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    const [defended] = await s.scene.h.funderLucid.utxosByOutRef([
      { txHash: s.liveByUnit(claim.caseUnit)!.txHash, outputIndex: 1 },
    ]);
    const threadUnit = Object.keys(defended!.assets).find(
      (unit) => unit !== "lovelace",
    )!;
    // Our first reveal, and the challenger's answer.
    expect(await s.tick()).toStrictEqual([
      { kind: "reveal", subject: threadUnit, outcome: "submitted" },
    ]);
    await s.land();
    await landWith(
      s.scene.h.proverLucid,
      await challengerReveal(
        s.scene,
        threadOf(s, threadUnit).utxo,
        claim.trace,
      ),
    );
    await s.env.follow();

    // Killed once our second reveal is journaled.
    const before = threadOf(s, threadUnit);
    const key = `reveal:${threadUnit}:${before.thread.round.toString()}`;
    const bytes = await s.tickKilledAt(s.killAtSubmit("before"));
    expect(await s.journaled(key)).toMatchObject([
      { txCbor: bytes, status: "live" },
    ]);
    await restartLater(s);

    const sentBefore = s.env.sent.length;
    expect(await stillTick(s)).toStrictEqual([
      { kind: "reveal", subject: threadUnit, outcome: "pending" },
    ]);
    // Waiting on its live attempt is not a shortfall.
    expect((await s.readiness()).reasons).toStrictEqual([]);
    expect(await s.env.stage.run()).toStrictEqual([]);
    expect(s.env.sent.slice(sentBefore)).toStrictEqual([bytes]);
    await s.land();

    const [reveal, ...more] = await s.journaled(key);
    expect(more).toStrictEqual([]);
    expect(reveal!.status).toBe("landed");
    const after = threadOf(s, threadUnit);
    expect(after.utxo.txHash).toBe(reveal!.txHash);
    expect(after.thread.round).toBe(before.thread.round);
    expect(after.thread.turn).toHaveProperty("AwaitingChallenger");
    expect(after.thread.defender_clock_ms).toBeGreaterThan(0n);
    // The challenger's turn: the restarted node plays nothing.
    expect(await s.tick()).toStrictEqual([]);
    expect(await s.env.stage.run()).toStrictEqual([]);
  });
});
