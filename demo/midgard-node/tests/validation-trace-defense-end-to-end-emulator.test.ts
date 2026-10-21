/**
 * The defending operator against the challenger, end to end on the real
 * deployment in the emulator: the node's defender, through its own ticks,
 * against the fault-proofs package's challenger.
 *
 * - A false claim on our passing block: the node defends and answers every
 *   reveal down to the one-step resolution, its thread expires into the
 *   pocket past the case's duration, the case closes DefenderWon with the
 *   challenger's bond going to the treasury (a close paying it back to the
 *   challenger is refused), and the block stays in the queue with no
 *   fraud-proof record against it.
 * - The honest claim on our failing block, opened by the challenger's
 *   workflow runner: the node names its own fault, never defends and never
 *   fails readiness, and the runner closes the case Convicted past its
 *   defense deadline.
 */
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core";
import {
  expectStateQueueHeaderOrder,
  makeRunner,
  runnerDeployment,
  runUntil,
  VALIDATION_TRACE_CASE_OPEN,
  validationTraceFraudProofUnit,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-challenger-runner";
import {
  challengerReveal,
  defenderWonCaseClose,
  falseClaimMaterial,
  landWith,
  network,
  openFalseClaim,
  protocolTreasury,
  type Scene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
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

/** The false claim's step count. */
const STEP_COUNT = 40;

const threadOf = (s: DefenseScene, unit: string) => {
  const utxo = s.liveByUnit(unit);
  if (utxo === undefined) return undefined;
  const { data } = Data.from(utxo.datum!, SDK.CaseThreadDatum);
  return { utxo, thread: data! };
};

const caseDatumOf = (utxo: UTxO) => Data.from(utxo.datum!, SDK.Case);

/** The queue still holds the scene's three blocks after their parent. */
const queueHoldsTheBlocks = (scene: Scene) =>
  expectStateQueueHeaderOrder({
    lucid: scene.h.proverLucid,
    contracts: scene.h.contracts,
    expectedHeaderHashes: [
      scene.failing.evidence.header.prevHeaderHash,
      scene.failing.evidence.headerHash,
      scene.passing.evidence.headerHash,
      scene.forced.evidence.headerHash,
    ],
  });

/** Every live output holding `unit`. */
const holding = (scene: Scene, unit: string): UTxO[] =>
  Object.values(scene.h.emulator.ledger)
    .filter(({ spent, utxo }) => !spent && (utxo.assets[unit] ?? 0n) > 0n)
    .map(({ utxo }) => utxo);

describe("the validation-trace defender against the challenger", () => {
  it("defends a false claim on our passing block to the one-step resolution and expiry, and the case closes DefenderWon with the block kept", async () => {
    const s = await openDefenseScene(databases);
    opened.push(s);
    const { scene } = s;
    const claim = await openFalseClaim(
      scene,
      await falseClaimMaterial(scene),
      STEP_COUNT,
    );
    await s.env.follow();

    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    const defendHash = s.liveByUnit(claim.caseUnit)!.txHash;
    const [defended] = await scene.h.funderLucid.utxosByOutRef([
      { txHash: defendHash, outputIndex: 1 },
    ]);
    const threadUnit = Object.keys(defended!.assets).find(
      (unit) => unit !== "lovelace",
    )!;

    // Both sides reveal, the node through its ticks, down to the one-step
    // resolution.
    for (let move = 0; move < 4 * 32; move += 1) {
      const live = threadOf(s, threadUnit)!;
      const { turn } = live.thread;
      if (turn === "ReadyForOneStep") break;
      if (typeof turn !== "string" && "AwaitingOperator" in turn) {
        expect(await s.tick()).toStrictEqual([
          { kind: "reveal", subject: threadUnit, outcome: "submitted" },
        ]);
        if (s.pending()) await s.land();
      } else {
        await landWith(
          scene.h.proverLucid,
          await challengerReveal(scene, live.utxo, claim.trace),
        );
        await s.env.follow();
      }
    }
    const resolved = threadOf(s, threadUnit)!;
    expect(resolved.thread.turn).toBe("ReadyForOneStep");
    expect(resolved.thread.defender_clock_ms).toBeGreaterThan(0n);

    // Past the case's duration, our thread expires into the pocket.
    s.scene.h.emulator.awaitSlot(
      Math.ceil(
        (startedAtOf(claim.caseUtxo) +
          MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS -
          scene.h.emulator.now()) /
          1000,
      ) + 1,
    );
    const pocketBefore = new Set(at(s, s.pocketAddress).map(outRefOf));
    expect(await s.tick()).toStrictEqual([
      { kind: "expiry", subject: threadUnit, outcome: "submitted" },
    ]);
    if (s.pending()) await s.land();
    expect(s.liveByUnit(threadUnit)).toBeUndefined();
    const [paid] = at(s, s.pocketAddress).filter(
      (utxo) => !pocketBefore.has(outRefOf(utxo)),
    );
    expect(inputsOf(s.landed(paid!.txHash).cbor)).toContain(
      outRefOf(resolved.utxo),
    );

    // The case still counts our thread: it closes DefenderWon.
    const caseUtxo = s.liveByUnit(claim.caseUnit)!;
    expect(caseDatumOf(caseUtxo).open_threads).toBe(1n);
    const [bond] = await scene.h.proverLucid.utxosByOutRef([
      { txHash: claim.caseUtxo.txHash, outputIndex: 1 },
    ]);
    const claimant = credentialToAddress(network, {
      type: "Key",
      hash: caseDatumOf(caseUtxo).bounty_claimant,
    });
    // A close paying the bond back to the challenger is refused.
    await expect(
      defenderWonCaseClose({
        scene,
        lucid: scene.h.proverLucid,
        caseUtxo,
        bond: bond!,
        bondTo: claimant,
      }),
    ).rejects.toThrow(/failed script execution/u);
    const closed = await landWith(
      scene.h.proverLucid,
      await defenderWonCaseClose({
        scene,
        lucid: scene.h.proverLucid,
        caseUtxo,
        bond: bond!,
      }),
    );
    await s.env.follow();
    expect(holding(scene, claim.caseUnit)).toStrictEqual([]);
    const [forfeited, remainder] = await scene.h.proverLucid.utxosByOutRef([
      { txHash: closed.hash, outputIndex: 0 },
      { txHash: closed.hash, outputIndex: 1 },
    ]);
    expect(forfeited!.address).toBe(protocolTreasury());
    expect(forfeited!.assets).toStrictEqual({
      lovelace: bond!.assets.lovelace,
    });
    expect(remainder!.address).toBe(claimant);

    // The block stays, with no fraud-proof record against it.
    await queueHoldsTheBlocks(scene);
    expect(
      holding(
        scene,
        validationTraceFraudProofUnit(
          runnerDeployment(scene),
          scene.passing.evidence.headerHash,
        ),
      ),
    ).toStrictEqual([]);
    expect(await s.tick()).toStrictEqual([]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  }, 1_800_000);

  it("never defends the honest claim the challenger's runner opens on our failing block, names its own fault without failing readiness, and the runner convicts", async () => {
    const s = await openDefenseScene(databases);
    opened.push(s);
    const { scene } = s;
    const failing = scene.failing.evidence.headerHash;
    const runner = makeRunner(scene);

    const { result: open } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("reason" in open)) throw new Error("no reason");
    const [, caseUnit, deadline] =
      /case ([0-9a-f]+) is open at \S+ until its defense deadline ([0-9]+)$/u.exec(
        open.reason,
      )!;

    // The node names its own fault, a detail that never fails readiness,
    // and sends nothing.
    expect(await s.tick()).toStrictEqual([]);
    expect(
      (await s.health()).states.find(({ subject }) => subject === caseUnit),
    ).toMatchObject({
      state: "validation-trace-defense-operator-fault",
      failsReadiness: false,
    });
    expect((await s.readiness()).reasons).toStrictEqual([]);

    // Up to its defense deadline the node still sends nothing.
    const { lucid } = s.port;
    scene.h.emulator.awaitSlot(
      lucid.unixTimeToSlot(Number(deadline)) - lucid.currentSlot(),
    );
    expect(await s.tick()).toStrictEqual([]);
    expect((await s.readiness()).reasons).toStrictEqual([]);

    // Past it, the runner closes the case Convicted. It observes no removal
    // of the block, so it stops once its record is on chain.
    scene.h.emulator.awaitSlot(1);
    const proofUnit = validationTraceFraudProofUnit(
      runnerDeployment(scene),
      failing,
    );
    for (let run = 0; run < 12; run += 1) {
      if (holding(scene, proofUnit).length > 0) break;
      await runner.runOnce(scene.failing);
    }
    const [record] = holding(scene, proofUnit);
    expect(record!.address).toBe(
      scene.h.contracts.fraudProof.spendingScriptAddress,
    );
    expect(
      Data.from(record!.datum!, SDK.FraudProofTokenDatum).fraud_prover,
    ).toBe(scene.h.proverSigner.paymentKeyHash);
    // The case convicted, so no thread was ever opened on it: its NFT is
    // locked beside the record.
    expect(
      holding(scene, caseUnit!).map(({ address }) => address),
    ).toStrictEqual([scene.h.contracts.fraudProof.spendingScriptAddress]);

    await s.env.follow();
    expect(await s.tick()).toStrictEqual([]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  }, 1_800_000);
});
