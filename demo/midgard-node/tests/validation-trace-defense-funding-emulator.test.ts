/**
 * How the defending operator funds its moves, on the real deployment in the
 * emulator, over the node's follower and wallet views:
 *
 * - with too little in the operator wallet and the pocket for a Defend, the
 *   tick names what it lacks, fails readiness and builds nothing; once the
 *   wallet is topped up, the Defend lands;
 * - our reveals take their collateral and any fee from the pocket alone,
 *   and keep our clock above zero down to the one-step resolution;
 * - past the case's duration our thread expires into the pocket;
 * - with the operator wallet alone short of a Defend, a pocket leftover
 *   funds it, and the pocket keeps an output to post as collateral.
 */
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core";
import { isPlainFundingOutput } from "@al-ft/midgard-fault-proofs";
import {
  challengerReveal,
  falseClaimMaterial,
  landWith,
  measureWithinTransactionLimits,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { shouldSkipForDueValidationTraceDefense } from "../src/fibers/block-commitment.validation-trace-defense-pause.js";
import { VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE } from "../src/validation-trace-defense/funding.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  at,
  type DefenseScene,
  drainBase,
  inputsOf,
  lovelace,
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

const open = async () => {
  const s = await openDefenseScene(databases);
  opened.push(s);
  return s;
};

const claimOn = async (s: DefenseScene, stepCount: number) => {
  const claim = await openFalseClaim(
    s.scene,
    await falseClaimMaterial(s.scene),
    stepCount,
  );
  await s.env.follow();
  return claim;
};

/** The challenger pays the operator wallet `amount`. */
const topUpBase = async (s: DefenseScene, amount: bigint) => {
  const { proverLucid } = s.scene.h;
  await landWith(
    proverLucid,
    await proverLucid
      .newTx()
      .pay.ToAddress(s.scene.funder.address, { lovelace: amount })
      .complete(),
  );
  await s.env.follow();
};

const threadOf = (s: DefenseScene, unit: string) => {
  const utxo = s.liveByUnit(unit);
  if (utxo === undefined) return undefined;
  const { data } = Data.from(utxo.datum!, SDK.CaseThreadDatum);
  return { utxo, thread: data! };
};

const collateralOf = (txCbor: Buffer): string[] => {
  const collateral = CML.Transaction.from_cbor_hex(txCbor.toString("hex"))
    .body()
    .collateral_inputs();
  return Array.from({ length: collateral?.len() ?? 0 }, (_, index) => {
    const input = collateral!.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

describe("the validation-trace defender's funding", () => {
  it("names what an underfunded Defend lacks and builds nothing, then lands it once topped up", async () => {
    const s = await open();
    await drainBase(s, 3_000_000n);
    const claim = await claimOn(s, 40);
    const sentBefore = s.env.sent.length;
    expect(await s.tick()).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    const health = await s.health();
    const short = health.states.find(
      ({ state }) => state === "validation-trace-defense-underfunded",
    );
    expect(short).toMatchObject({
      subject: claim.caseUnit,
      failsReadiness: true,
    });
    expect(short!.shortfallLovelace!).toBeGreaterThan(0n);
    expect(health.dueDefends).toStrictEqual([]);
    expect((await s.readiness()).reasons).toStrictEqual([
      `validation-trace-defense-underfunded:${claim.caseUnit}:${short!.deadline!.toString()}:${short!.shortfallLovelace!.toString()}`,
    ]);
    // An underfunded Defend never pauses commits.
    expect(await s.inNode(shouldSkipForDueValidationTraceDefense)).toBe(false);

    await topUpBase(
      s,
      short!.shortfallLovelace! + VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE,
    );
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  });

  it("reveals from the pocket down to the one-step resolution with our clock above zero, and expires into the pocket", async () => {
    const s = await open();
    const claim = await claimOn(s, 40);
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    // The Defend left the pocket one collateral output, and our thread.
    const [kept, ...noMore] = at(s, s.pocketAddress);
    expect(noMore).toStrictEqual([]);
    const defendHash = s.liveByUnit(claim.caseUnit)!.txHash;
    const [defended] = await s.scene.h.funderLucid.utxosByOutRef([
      { txHash: defendHash, outputIndex: 1 },
    ]);
    const threadUnit = Object.keys(defended!.assets).find(
      (unit) => unit !== "lovelace",
    )!;

    const fits: ReturnType<typeof measureWithinTransactionLimits>[] = [];
    for (let move = 0; move < 40; move += 1) {
      const live = threadOf(s, threadUnit)!;
      const { turn } = live.thread;
      if (turn === "ReadyForOneStep") break;
      if (typeof turn !== "string" && "AwaitingOperator" in turn) {
        const pocket = new Set(at(s, s.pocketAddress).map(outRefOf));
        expect(await s.tick()).toStrictEqual([
          { kind: "reveal", subject: threadUnit, outcome: "submitted" },
        ]);
        if (s.pending()) await s.land();
        const moved = threadOf(s, threadUnit)!;
        const reveal = s.landed(moved.utxo.txHash);
        // Beside our thread, only pocket outputs: as inputs and as collateral.
        const collateral = collateralOf(reveal.cbor);
        expect(collateral.length).toBeGreaterThan(0);
        for (const ref of [
          ...collateral,
          ...inputsOf(reveal.cbor).filter((ref) => ref !== outRefOf(live.utxo)),
        ])
          expect(pocket.has(ref)).toBe(true);
        expect(moved.thread.defender_clock_ms).toBeGreaterThan(0n);
        fits.push(measureWithinTransactionLimits(reveal.cbor.toString("hex")));
      } else {
        await landWith(
          s.scene.h.proverLucid,
          await challengerReveal(s.scene, live.utxo, claim.trace),
        );
        await s.env.follow();
      }
    }
    const resolved = threadOf(s, threadUnit)!;
    expect(resolved.thread.turn).toBe("ReadyForOneStep");
    expect(resolved.thread.defender_clock_ms).toBeGreaterThan(0n);
    expect(fits.length).toBeGreaterThan(0);
    const most = (pick: (fit: (typeof fits)[number]) => bigint) =>
      fits.reduce((max, fit) => (pick(fit) > max ? pick(fit) : max), 0n);
    console.info(
      `reveals: ${fits.length.toString()}, at most ${most((fit) => BigInt(fit.completeSignedBytes)).toString()} bytes, ${most((fit) => fit.executionMemory).toString()} mem, ${most((fit) => fit.executionSteps).toString()} steps`,
    );

    // Past the case's duration, our thread expires into the pocket.
    const expiresAfter =
      startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;
    s.scene.h.emulator.awaitSlot(
      Math.ceil((expiresAfter - s.scene.h.emulator.now()) / 1000) + 1,
    );
    const pocketBefore = new Set(at(s, s.pocketAddress).map(outRefOf));
    expect(await s.tick()).toStrictEqual([
      { kind: "expiry", subject: threadUnit, outcome: "submitted" },
    ]);
    if (s.pending()) await s.land();
    expect(s.liveByUnit(threadUnit)).toBeUndefined();
    const fresh = at(s, s.pocketAddress).filter(
      (utxo) => !pocketBefore.has(outRefOf(utxo)),
    );
    const expiryTx = s.landed(fresh[0]!.txHash);
    expect(inputsOf(expiryTx.cbor)).toContain(outRefOf(resolved.utxo));
    const leftover = fresh.reduce((max, utxo) =>
      (utxo.assets.lovelace ?? 0n) > (max.assets.lovelace ?? 0n) ? utxo : max,
    );
    expect(leftover.assets.lovelace ?? 0n).toBeGreaterThan(
      lovelace([kept!]) * 2n,
    );
    const expiryFit = measureWithinTransactionLimits(
      expiryTx.cbor.toString("hex"),
    );
    console.info(
      `expiry: ${expiryFit.completeSignedBytes.toString()} bytes, ${expiryFit.executionMemory.toString()} mem, ${expiryFit.executionSteps.toString()} steps`,
    );
  });

  it("funds a Defend from a pocket leftover when the operator wallet alone is short", async () => {
    const s = await open();
    // A leftover in the pocket, as an expiry leaves it.
    const { proverLucid } = s.scene.h;
    const paid = await landWith(
      proverLucid,
      await proverLucid
        .newTx()
        .pay.ToAddress(s.pocketAddress, { lovelace: 100_000_000n })
        .complete(),
    );
    await s.env.follow();
    const [leftover] = at(s, s.pocketAddress);
    expect(leftover!.txHash).toBe(paid.hash);
    await drainBase(s, 20_000_000n);
    const claim = await claimOn(s, 40);
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    const defend = s.landed(s.liveByUnit(claim.caseUnit)!.txHash);
    expect(inputsOf(defend.cbor)).toContain(outRefOf(leftover!));
    // The pocket keeps a plain output our reveals can post as collateral.
    expect(
      at(s, s.pocketAddress).some(
        (utxo) =>
          isPlainFundingOutput(utxo) &&
          (utxo.assets.lovelace ?? 0n) >= 5_000_000n,
      ),
    ).toBe(true);
  });
});
