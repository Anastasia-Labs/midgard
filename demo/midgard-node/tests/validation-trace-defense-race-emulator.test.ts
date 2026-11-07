/**
 * Another active operator defends a case on our block first, on the real
 * deployment in the emulator, over the node's follower and intent journal:
 *
 * - its Defend lands while ours is being built over the case it spends, so
 *   ours is refused, the tick fails with it, and the state-queue lease is
 *   released;
 * - its Defend lands, and our follower sees it, while our tick waits for
 *   the L1 control plane: ours is not built over the case it spent;
 * - either way, the next tick rebuilds ours joining the case's defenders
 *   after it, and it lands in time;
 * - after two other operators' Defends, ours joins both, and lands.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  type Defender,
  defendPending,
  enrollSuccessor,
  falseClaimMaterial,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import { validationTraceDefenseTick } from "../src/validation-trace-defense/tick.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  type DefenseScene,
  inputsOf,
  openDefenseScene,
  outRefOf,
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

/** Another active operator, enrolled after the one keyed `lowerKey`. */
const enrollOther = async (
  s: DefenseScene,
  lowerKey: string,
): Promise<Defender> => {
  const successor = await enrollSuccessor(s.scene.h as never, lowerKey);
  return {
    vkey: successor.keyHash,
    nodeUnit: successor.activeNodeUnit,
    lucid: successor.lucid,
  };
};

/** The scene with a second active operator, and a false claim opened on
 * our passing block. */
const openWithOther = async () => {
  const s = await openDefenseScene(databases);
  opened.push(s);
  const other = await enrollOther(s, s.scene.operator.vkey);
  const claim = await openFalseClaim(
    s.scene,
    await falseClaimMaterial(s.scene),
    40,
  );
  await s.env.follow();
  return { s, other, claim };
};

/** The next tick lands our Defend over `otherDefend`, in time. */
const landsAfter = async (
  s: DefenseScene,
  claim: Awaited<ReturnType<typeof openWithOther>>["claim"],
  otherDefend: UTxO,
) => {
  expect(await s.tick()).toStrictEqual([
    { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
  ]);
  const ours = s.liveByUnit(claim.caseUnit)!;
  expect(ours.txHash).not.toBe(otherDefend.txHash);
  const landed = s.landed(ours.txHash);
  expect(inputsOf(landed.cbor)).toContain(outRefOf(otherDefend));
  expect(landed.at).toBeLessThan(
    startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
  );
  // Our Defend joined the case's defenders, and our node holds it.
  const rootOf = (utxo: UTxO) =>
    Data.from(utxo.datum!, SDK.Case).defenders_root;
  expect(rootOf(ours)).not.toBe(rootOf(otherDefend));
  expect(s.liveByUnit(s.scene.operator.nodeUnit)!.txHash).toBe(ours.txHash);
};

describe("the validation-trace defender beside another defender", () => {
  it("is refused when another operator's Defend lands as ours is built, then rebuilds over it", async () => {
    const { s, other, claim } = await openWithOther();
    // The other operator's Defend lands as ours is built over the case it
    // spends, inside our Defend's validity: our follower has not seen it.
    const { emulator } = s.scene.h;
    const sending: Defender = {
      ...other,
      lucid: Object.assign(Object.create(other.lucid) as typeof other.lucid, {
        awaitTx: async () => true,
      }),
    };
    let raced: string | undefined;
    const racing = {
      ...s.port,
      references: Effect.promise(async () => {
        if (raced === undefined) {
          raced = await defendPending(s.scene, claim.caseUtxo, sending);
          // The next block, not a whole block's slots later.
          emulator.awaitSlot(20 - (emulator.slot % 20));
        }
        return s.references;
      }),
    };
    const sentBefore = s.env.sent.length;
    await expect(s.inNode(validationTraceDefenseTick(racing))).rejects.toThrow(
      /missing from emulator ledger/u,
    );
    expect(raced).toBeDefined();
    expect(emulator.transactionHistory[raced!]?.status).toBe("confirmed");
    // Ours never reached the chain, and nothing holds the lease.
    expect(s.env.sent.length).toBe(sentBefore);
    expect(
      await s.inNode(StateQueueMutationLeasesDB.retrieveActive()),
    ).toBeUndefined();
    const otherDefend = s.liveByUnit(claim.caseUnit)!;
    expect(otherDefend.txHash).toBe(raced);

    await s.env.follow();
    await landsAfter(s, claim, otherDefend);
  });

  it("builds nothing over a case another operator's Defend spent while it waited for the control plane, then rebuilds over it", async () => {
    const { s, other, claim } = await openWithOther();
    // Another scope holds the L1 control plane; our tick decides, and waits.
    const plane = await s.holdControlPlane();
    const sentBefore = s.env.sent.length;
    const ticking = s.inNode(validationTraceDefenseTick(s.port));
    await s.defendWaitsForPlane();
    expect(
      (await s.health()).dueDefends.map(({ subject }) => subject),
    ).toStrictEqual([claim.caseUnit]);
    const raced = await defendPending(s.scene, claim.caseUtxo, other);
    await s.env.follow();
    await plane.release();
    expect(await ticking).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "moved" },
    ]);
    expect(s.env.sent.length).toBe(sentBefore);
    const otherDefend = s.liveByUnit(claim.caseUnit)!;
    expect(otherDefend.txHash).toBe(raced);
    await landsAfter(s, claim, otherDefend);
  });

  it("joins two other operators' Defends on the case, and lands", async () => {
    const { s, other, claim } = await openWithOther();
    const third = await enrollOther(s, other.vkey);
    await defendPending(s.scene, claim.caseUtxo, other);
    await defendPending(s.scene, s.liveByUnit(claim.caseUnit)!, third, [
      other.vkey,
    ]);
    await s.env.follow();
    await landsAfter(s, claim, s.liveByUnit(claim.caseUnit)!);
  });
});
