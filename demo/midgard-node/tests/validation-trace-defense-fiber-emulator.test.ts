/**
 * The defending operator's tick on the real deployment in the emulator,
 * over the node's follower, intent journal and landed rows, with the
 * operator's own wallet (its pocket selected) playing:
 *
 * - a case opened on our block is answered by a Defend, held under the
 *   state-queue lease until it lands, well before
 *   the operator's side clock runs out; the operator's next commit spends
 *   the node the Defend left, and keeps a bond hold at least the Defend's;
 * - while a signed commit is unresolved no Defend is built, and the due
 *   Defend does not count as waiting, also when the commit was signed while
 *   the Defend waited for the control plane; once the commit lands and is
 *   reconciled, the Defend spends the node the commit left;
 * - a case on our block with no active-operator node of ours names a
 *   state that fails readiness, and nothing is built;
 * - once our side clock has run out no Defend is built, commits do not
 *   pause, and the case is named late;
 * - with no L1 follower view the tick does nothing and names nothing.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  commitSceneSuccessor,
  falseClaimMaterial,
  measureWithinTransactionLimits,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import { SqlClient } from "@effect/sql";
import {
  generateEmulatorAccount,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { StateQueueMutationLeasesDB } from "../src/database/index.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { shouldSkipForDueValidationTraceDefense } from "../src/fibers/block-commitment.validation-trace-defense-pause.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import { VALIDATION_TRACE_DEFENSE_LEASE_HOLDER } from "../src/validation-trace-defense/health.js";
import { validationTraceDefenseTick } from "../src/validation-trace-defense/tick.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  bondUnlockTimeOf,
  type DefenseScene,
  inputsOf,
  openDefenseScene,
  outRefOf,
  startedAtOf,
} from "./helpers/validation-trace-defense-scene.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";

const databases = testDatabases();
const opened: DefenseScene[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map(({ env }) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

/** The scene with a false claim opened on our passing block. */
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

/** The Defend that left `caseUnit`'s live case output. */
const defendOf = (s: DefenseScene, caseUnit: string) => {
  const caseOutput = s.liveByUnit(caseUnit);
  if (caseOutput === undefined) throw new Error("the case is not live");
  return { hash: caseOutput.txHash, ...s.landed(caseOutput.txHash) };
};

/** Journals a signed commit of `commitHeader`, unresolved; its intended hash. */
const prepareSignedCommit = async (s: DefenseScene, commitHeader: Buffer) => {
  const intended = Buffer.alloc(32, 0x5c);
  await s.inNode(
    Effect.gen(function* () {
      yield* PendingBlockFinalizationsDB.preparePendingSubmission({
        ...journalFixture(commitHeader),
        preparedTxHash: intended,
      });
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE pending_block_finalizations
        SET intended_tx_hash = ${intended}, signed_tx_cbor = ${Buffer.from("84a0a0f5f6", "hex")}
        WHERE header_hash = ${commitHeader}`;
    }).pipe(Effect.provideService(FollowerWriteFixture, true)),
  );
  return intended;
};

describe("the validation-trace defender's tick", () => {
  it("lands the Defend of a case on our block before our clock runs out, and our next commit keeps its bond hold", async () => {
    const { s, claim } = await openWithClaim();
    const holders: (string | undefined)[] = [];
    const played = await s.tick(s.port, async () => {
      const lease = await s.inNode(StateQueueMutationLeasesDB.retrieveActive());
      holders.push(lease?.holder);
    });
    expect(played).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    // The Defend held the state-queue lease until it landed, and released it.
    expect(holders).toStrictEqual([VALIDATION_TRACE_DEFENSE_LEASE_HOLDER]);
    expect(
      await s.inNode(StateQueueMutationLeasesDB.retrieveActive()),
    ).toBeUndefined();
    const defend = defendOf(s, claim.caseUnit);
    expect(defend.hash).not.toBe(claim.caseUtxo.txHash);
    expect(inputsOf(defend.cbor)).toContain(outRefOf(claim.caseUtxo));
    expect(defend.at).toBeLessThan(
      startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
    );
    const fit = measureWithinTransactionLimits(defend.cbor.toString("hex"));
    console.info(
      `Defend: ${fit.completeSignedBytes.toString()} bytes, ${fit.executionMemory.toString()} mem, ${fit.executionSteps.toString()} steps`,
    );
    // The Defend left our node.
    const defendedNode = s.liveByUnit(s.scene.operator.nodeUnit)!;
    expect(defendedNode.txHash).toBe(defend.hash);
    const defendHold = bondUnlockTimeOf(defendedNode)!;

    // Defended: no Defend is due, and readiness is clean; the next tick
    // plays our first reveal.
    expect(await s.tick()).toStrictEqual([
      expect.objectContaining({ kind: "reveal", outcome: "submitted" }),
    ]);
    const health = await s.health();
    expect(health.dueDefends).toStrictEqual([]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
    expect(await s.inNode(shouldSkipForDueValidationTraceDefense)).toBe(false);

    const commit = await commitSceneSuccessor(s.scene);
    await s.env.follow();
    expect(s.landed(commit.activeNode.txHash).cbor).toBeDefined();
    expect(inputsOf(s.landed(commit.activeNode.txHash).cbor)).toContain(
      outRefOf(defendedNode),
    );
    expect(bondUnlockTimeOf(commit.activeNode)!).toBeGreaterThanOrEqual(
      defendHold,
    );
  });

  it("builds no Defend while a signed commit is unresolved, and defends over the commit once it is reconciled", async () => {
    const { s, claim } = await openWithClaim();
    const commitHeader = header("defender-commit-in-flight");
    const intended = await prepareSignedCommit(s, commitHeader);
    // The commit's fiber may hold the L1 control plane meanwhile: the
    // Defend does not wait on it.
    const plane = await s.holdControlPlane();
    const sentBefore = s.env.sent.length;
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "commit_in_flight" },
    ]);
    await plane.release();
    expect(s.env.sent.length).toBe(sentBefore);
    const waiting = await s.health();
    expect(waiting.dueDefends.map(({ subject }) => subject)).toStrictEqual([
      claim.caseUnit,
    ]);
    expect(waiting.defendDueSinceMs).toBeNull();

    // The commit lands, and the node reconciles its journal.
    const commit = await commitSceneSuccessor(s.scene);
    await s.env.follow();
    await s.inNode(
      PendingBlockFinalizationsDB.markSubmitted(commitHeader, intended).pipe(
        Effect.provideService(FollowerWriteFixture, true),
      ),
    );

    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    const defend = defendOf(s, claim.caseUnit);
    expect(inputsOf(defend.cbor)).toContain(outRefOf(commit.activeNode));
    expect(s.liveByUnit(s.scene.operator.nodeUnit)!.txHash).toBe(defend.hash);
    expect(defend.at).toBeLessThan(
      startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
    );
  });

  it("builds no Defend over a commit signed while it waited for the control plane", async () => {
    const { s, claim } = await openWithClaim();
    const plane = await s.holdControlPlane();
    const sentBefore = s.env.sent.length;
    const ticking = s.inNode(validationTraceDefenseTick(s.port));
    await s.defendWaitsForPlane();
    // The commit holding the control plane signs and journals its block.
    await prepareSignedCommit(s, header("defender-commit-under-plane"));
    await plane.release();
    expect(await ticking).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "commit_in_flight" },
    ]);
    expect(s.env.sent.length).toBe(sentBefore);
    expect(s.liveByUnit(claim.caseUnit)!.txHash).toBe(claim.caseUtxo.txHash);
  });

  it("names a case on our block with no active node of ours as failing readiness, and builds nothing", async () => {
    const { s, claim } = await openWithClaim();
    const stranger = getAddressDetails(
      generateEmulatorAccount({ lovelace: 0n }).address,
    ).paymentCredential!.hash;
    const sentBefore = s.env.sent.length;
    expect(await s.tick({ ...s.port, ourVkh: stranger })).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    const health = await s.health();
    expect(health.dueDefends).toStrictEqual([]);
    expect(health.states).toContainEqual(
      expect.objectContaining({
        state: "validation-trace-defense-not-active",
        subject: claim.caseUnit,
        failsReadiness: true,
      }),
    );
    expect((await s.readiness()).reasons).toContainEqual(
      expect.stringMatching(/^validation-trace-defense-not-active:/u),
    );
    // With our node, the same case is answered.
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  });

  it("builds no Defend once our side clock has run out, and names it late", async () => {
    const { s, claim } = await openWithClaim();
    const { emulator } = s.scene.h;
    const deadline =
      startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;
    emulator.awaitSlot(Math.ceil((deadline - emulator.now()) / 1000) + 1);
    const sentBefore = s.env.sent.length;
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "past_deadline" },
    ]);
    expect(s.env.sent.length).toBe(sentBefore);
    expect(s.liveByUnit(claim.caseUnit)!.txHash).toBe(claim.caseUtxo.txHash);
    expect(
      await s.inNode(StateQueueMutationLeasesDB.retrieveActive()),
    ).toBeUndefined();
    // Past its deadline the Defend is no longer due: commits do not pause.
    expect((await s.health()).dueDefends).toStrictEqual([]);
    expect(await s.inNode(shouldSkipForDueValidationTraceDefense)).toBe(false);
    expect((await s.readiness()).reasons).toStrictEqual([
      `validation-trace-defense-late:${claim.caseUnit}:${deadline.toString()}`,
    ]);
  });

  it("does nothing and names nothing with no L1 follower view", async () => {
    const { s } = await openWithClaim();
    const sentBefore = s.env.sent.length;
    const played = await s.inNode(
      validationTraceDefenseTick(s.port).pipe(
        Effect.provide(IntentJournalWithoutFollower),
      ),
    );
    expect(played).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    const health = await s.health();
    expect(health.following).toBe(false);
    expect(health.states).toStrictEqual([]);
    expect(health.dueDefends).toStrictEqual([]);
    expect(await s.readiness()).toStrictEqual({ reasons: [], details: [] });
    expect(await s.inNode(shouldSkipForDueValidationTraceDefense)).toBe(false);
  });
});
