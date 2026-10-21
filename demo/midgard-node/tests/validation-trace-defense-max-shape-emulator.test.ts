/**
 * The defending operator's whole thread at the consensus-maximum shape, on
 * the real deployment in the emulator: our passing block's script runs as
 * long as admission allows (its memory at the per-transaction cap), and the
 * case's step count is the largest a descriptor takes, and the claim takes
 * the bisection's longest path to the one-step resolution.
 *
 * - The Defend lands within the transaction limits, and while our trace of
 *   the event is built the node's main thread stays live: a timer standing
 *   in for the commit tick keeps firing.
 * - Our reveals land within the limits down to the one-step resolution,
 *   with our clock above zero.
 * - Past the case's duration our thread expires into the pocket, within the
 *   limits.
 */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  validationTraceDepthForStepCount,
} from "@al-ft/midgard-core";
import {
  ADMITTED_TX_EX_UNITS,
  challengerReveal,
  falseClaimMaterial,
  landWith,
  LONGEST_PASSING_SCRIPT,
  measureWithinTransactionLimits,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { testDatabases } from "./helpers/l1-events-store.js";
import {
  at,
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

/** The largest step count a descriptor takes. */
const MAX_STEP_COUNT = MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount;

/** The commit tick's stand-in: how often it asks for the main thread. */
const TIMER_MS = 50;
/** The longest the main thread may be held while our trace is built. */
const MAX_TIMER_GAP_MS = 1_000;

const threadOf = (s: DefenseScene, unit: string) => {
  const utxo = s.liveByUnit(unit);
  if (utxo === undefined) return undefined;
  const { data } = Data.from(utxo.datum!, SDK.CaseThreadDatum);
  return { utxo, thread: data! };
};

type Fit = ReturnType<typeof measureWithinTransactionLimits>;
const fitLine = (label: string, fit: Fit) =>
  `${label}: ${fit.completeSignedBytes.toString()} bytes, ${fit.executionMemory.toString()} mem, ${fit.executionSteps.toString()} steps`;

/** Runs `run` while a `TIMER_MS` timer fires; the longest gap between
 * firings. */
const withTimer = async <A>(run: () => Promise<A>) => {
  let last = performance.now();
  let longest = 0;
  let firings = 0;
  const timer = setInterval(() => {
    const now = performance.now();
    longest = Math.max(longest, now - last);
    last = now;
    firings += 1;
  }, TIMER_MS);
  const started = performance.now();
  try {
    const result = await run();
    return {
      result,
      longest: Math.max(longest, performance.now() - last),
      firings,
      ms: performance.now() - started,
    };
  } finally {
    clearInterval(timer);
  }
};

describe("the validation-trace defender at the consensus-maximum shape", () => {
  it("defends, reveals to the one-step resolution and expires, each within the limits, with the main thread live while it builds", async () => {
    const s = await openDefenseScene(databases, undefined, {
      passingSpend: {
        script: LONGEST_PASSING_SCRIPT,
        exUnits: ADMITTED_TX_EX_UNITS,
      },
    });
    opened.push(s);
    const material = await falseClaimMaterial(s.scene);
    // The passing script ran to the per-transaction memory cap.
    expect(material.honest.terminal.executionMemory).toBeLessThanOrEqual(
      ADMITTED_TX_EX_UNITS[0],
    );
    expect(material.honest.terminal.executionMemory).toBeGreaterThan(
      ADMITTED_TX_EX_UNITS[0] - 100_000n,
    );
    // The claim agrees with our trace everywhere past our honest end but at
    // its own end, so every round moves right: the bisection's longest path.
    const claim = await openFalseClaim(
      s.scene,
      material,
      MAX_STEP_COUNT,
      Number(material.honest.claim.stepCount),
    );
    await s.env.follow();

    // The Defend, our trace built in the tick that sends it.
    const defending = await withTimer(() => s.tick());
    expect(defending.result).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    console.info(
      `Defend tick: ${Math.round(defending.ms).toString()} ms, ${defending.firings.toString()} timer firings, longest gap ${Math.round(defending.longest).toString()} ms; honest trace ${material.honest.claim.stepCount.toString()} steps, ${material.honest.terminal.executionMemory.toString()} mem`,
    );
    expect(defending.longest).toBeLessThan(MAX_TIMER_GAP_MS);
    const defendHash = s.liveByUnit(claim.caseUnit)!.txHash;
    console.info(
      fitLine(
        "Defend",
        measureWithinTransactionLimits(
          s.landed(defendHash).cbor.toString("hex"),
        ),
      ),
    );
    const [defended] = await s.scene.h.funderLucid.utxosByOutRef([
      { txHash: defendHash, outputIndex: 1 },
    ]);
    const threadUnit = Object.keys(defended!.assets).find(
      (unit) => unit !== "lovelace",
    )!;

    // Every reveal of ours, down to the one-step resolution.
    const fits: Fit[] = [];
    for (let move = 0; move < 4 * 32; move += 1) {
      const live = threadOf(s, threadUnit)!;
      const { turn } = live.thread;
      if (turn === "ReadyForOneStep") break;
      if (typeof turn !== "string" && "AwaitingOperator" in turn) {
        expect(await s.tick()).toStrictEqual([
          { kind: "reveal", subject: threadUnit, outcome: "submitted" },
        ]);
        if (s.pending()) await s.land();
        const moved = threadOf(s, threadUnit)!;
        expect(moved.utxo.txHash).not.toBe(live.utxo.txHash);
        fits.push(
          measureWithinTransactionLimits(
            s.landed(moved.utxo.txHash).cbor.toString("hex"),
          ),
        );
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
    expect(fits.length).toBe(validationTraceDepthForStepCount(MAX_STEP_COUNT));
    const clockLeft = resolved.thread.defender_clock_ms;
    expect(clockLeft).toBeGreaterThan(0n);
    const most = (pick: (fit: Fit) => bigint) =>
      fits.reduce((max, fit) => (pick(fit) > max ? pick(fit) : max), 0n);
    console.info(
      `reveals: ${fits.length.toString()}, at most ${most((fit) => BigInt(fit.completeSignedBytes)).toString()} bytes, ${most((fit) => fit.executionMemory).toString()} mem, ${most((fit) => fit.executionSteps).toString()} steps; our clock left ${clockLeft.toString()} ms`,
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
    const [fresh] = at(s, s.pocketAddress).filter(
      (utxo) => !pocketBefore.has(outRefOf(utxo)),
    );
    const expiry = s.landed(fresh!.txHash);
    expect(inputsOf(expiry.cbor)).toContain(outRefOf(resolved.utxo));
    console.info(
      fitLine(
        "expiry",
        measureWithinTransactionLimits(expiry.cbor.toString("hex")),
      ),
    );
  }, 1_800_000);
});
