/**
 * Whichever lands wins for the validation-trace defender's Defend, on the
 * real deployment in the emulator, over the node's follower, intent
 * journal and S6: a replacement is built only once the attempt before it
 * is dead, shares its case and our node with it, and so only one of them
 * can ever land.
 *
 * - A Defend whose submission was lost and whose validity passed unlanded
 *   is replaced by one that lands in time; the old one cannot land.
 * - A Defend that landed on a fork the chain rolls back is live again: the
 *   node builds nothing, S6 sends the journaled bytes, and they land.
 * - When the rolled-back Defend's validity has passed, the node builds a
 *   replacement, which lands in time; the old one cannot land.
 * - When the case's own opening is rolled back, the Defend on it is never
 *   sent again and dies once its validity passes, and nothing is built.
 *
 * A fork is the follower's alone: a Defend lands on it through a
 * submission the emulator never sees, applied to the follower as a block.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  falseClaimMaterial,
  landWith,
  openFalseClaim,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import { CML, generateEmulatorAccount } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { defendKey } from "../src/validation-trace-defense/play.js";
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

/** The scene with a false claim on our passing block, followed; and the
 * follower's height before the claim opened. */
const openWithClaim = async () => {
  const s = await openDefenseScene(databases);
  opened.push(s);
  await s.env.follow();
  const beforeClaim = s.env.tipHeight();
  const claim = await openFalseClaim(
    s.scene,
    await falseClaimMaterial(s.scene),
    40,
  );
  await s.env.follow();
  return { s, claim, beforeClaim };
};
type Claim = Awaited<ReturnType<typeof openWithClaim>>["claim"];

/** A tick with the chain held still. */
const stillTick = (s: DefenseScene) =>
  s.inNode(validationTraceDefenseTick(s.port));

/**
 * A tick whose Defend lands on a fork only: its bytes, applied to the
 * follower as a block the emulator never sees, and the follower's height
 * before that block.
 */
const defendOnFork = async (s: DefenseScene, claim: Claim) => {
  const onFork = s.forkOnlySubmit();
  const beforeFork = s.env.tipHeight();
  const ticking = stillTick(s);
  const bytes = await onFork;
  await s.env.applyForkBlock([bytes]);
  expect(await ticking).toStrictEqual([
    { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
  ]);
  return { bytes, beforeFork };
};

/** The last slot `bytes` is valid in. */
const validToSlot = (bytes: Buffer): number =>
  Number(CML.Transaction.from_cbor_bytes(bytes).body().ttl()!);

/** The emulator's L1 time passes `bytes`' validity, and a block lands and is followed. */
const pastValidity = async (s: DefenseScene, bytes: Buffer) => {
  const { emulator, proverLucid } = s.scene.h;
  emulator.awaitSlot(validToSlot(bytes) - emulator.slot + 1);
  await landWith(
    proverLucid,
    await proverLucid
      .newTx()
      .pay.ToAddress(generateEmulatorAccount({ lovelace: 0n }).address, {
        lovelace: 2_000_000n,
      })
      .complete(),
  );
  await s.env.follow();
};

const hashOf = (bytes: Buffer) =>
  CML.hash_transaction(CML.Transaction.from_cbor_bytes(bytes).body()).to_hex();

/** The replacement of `old` landed in time, sharing the case and our node
 * with it; `old` is dead (spent by its replacement) and cannot land. */
const replacedBy = async (s: DefenseScene, claim: Claim, old: Buffer) => {
  const caseRef = outRefOf(claim.caseUtxo);
  expect(inputsOf(old)).toContain(caseRef);
  const ours = s.liveByUnit(claim.caseUnit)!;
  expect(ours.txHash).not.toBe(hashOf(old));
  const replacement = s.landed(ours.txHash);
  const shared = inputsOf(replacement.cbor).filter((ref) =>
    inputsOf(old).includes(ref),
  );
  expect(shared).toContain(caseRef);
  // The case and our node, at least: our node holds the replacement now.
  expect(shared.length).toBeGreaterThanOrEqual(2);
  expect(s.liveByUnit(s.scene.operator.nodeUnit)!.txHash).toBe(ours.txHash);
  expect(replacement.at).toBeLessThan(
    startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
  );
  await expect(
    s.scene.h.emulator.submitTx(old.toString("hex")),
  ).rejects.toThrow();
  const sentBefore = s.env.sent.length;
  expect(await s.env.stage.run()).toStrictEqual([]);
  expect(s.env.sent.length).toBe(sentBefore);
  expect(
    Object.fromEntries(
      (await s.journaled(defendKey(claim.caseUnit))).map(
        ({ txHash, status }) => [txHash, status],
      ),
    ),
  ).toStrictEqual({ [hashOf(old)]: "conflicted", [ours.txHash]: "landed" });
};

describe("whichever of the validation-trace defender's Defends lands wins", () => {
  it("replaces a Defend whose validity passed unlanded, and the old one cannot land", async () => {
    const { s, claim } = await openWithClaim();
    // The submission is lost: no block ever holds it.
    const lost = s.forkOnlySubmit();
    const ticking = stillTick(s);
    const bytes = await lost;
    await pastValidity(s, bytes);
    expect(await ticking).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "submitted" },
    ]);
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    await replacedBy(s, claim, bytes);
  });

  it("builds nothing for a rolled-back Defend while S6 sends its journaled bytes, and they land", async () => {
    const { s, claim } = await openWithClaim();
    const { bytes, beforeFork } = await defendOnFork(s, claim);
    await s.env.rewindTo(beforeFork);
    const key = defendKey(claim.caseUnit);
    expect(await s.journaled(key)).toMatchObject([{ status: "live" }]);

    const sentBefore = s.env.sent.length;
    expect(await stillTick(s)).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "pending" },
    ]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
    expect(await s.env.stage.run()).toStrictEqual([]);
    expect(s.env.sent.slice(sentBefore)).toStrictEqual([bytes]);
    await s.land();
    expect(await s.env.stage.run()).toStrictEqual([]);

    expect(await s.journaled(key)).toMatchObject([
      { txHash: hashOf(bytes), status: "landed" },
    ]);
    expect(s.liveByUnit(claim.caseUnit)!.txHash).toBe(hashOf(bytes));
    expect(s.landed(hashOf(bytes)).at).toBeLessThan(
      startedAtOf(claim.caseUtxo) + MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
    );
    expect(await s.tick()).toMatchObject([
      { kind: "reveal", outcome: "submitted" },
    ]);
  });

  it("replaces a rolled-back Defend once its validity has passed, and the old one cannot land", async () => {
    const { s, claim } = await openWithClaim();
    const { bytes, beforeFork } = await defendOnFork(s, claim);
    await s.env.rewindTo(beforeFork);
    await pastValidity(s, bytes);
    const sentBefore = s.env.sent.length;
    expect(await s.env.stage.run()).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    expect(await s.tick()).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    await replacedBy(s, claim, bytes);
  });

  it("never sends a Defend again once the case's opening is rolled back, and builds nothing", async () => {
    const { s, claim, beforeClaim } = await openWithClaim();
    const { bytes } = await defendOnFork(s, claim);
    // The fork the follower stays on holds no case: the emulator's chain,
    // which still does, is never followed again.
    await s.env.rewindTo(beforeClaim);
    const key = defendKey(claim.caseUnit);
    const sentBefore = s.env.sent.length;
    const nothingBuilt = async () => {
      expect(await s.env.stage.run()).toStrictEqual([]);
      expect(s.env.sent.length).toBe(sentBefore);
      expect(await stillTick(s)).toStrictEqual([]);
      const health = await s.health();
      expect(health.dueDefends).toStrictEqual([]);
      expect(health.states).toStrictEqual([]);
      expect((await s.readiness()).reasons).toStrictEqual([]);
      expect(await s.journaled(key)).toHaveLength(1);
    };
    await nothingBuilt();
    // Past its validity on the fork, it is dead.
    const { emulator } = s.scene.h;
    emulator.awaitSlot(validToSlot(bytes) - emulator.slot + 1);
    await s.env.applyForkBlock([]);
    expect(await s.journaled(key)).toMatchObject([{ status: "expired" }]);
    await nothingBuilt();
  });
});
