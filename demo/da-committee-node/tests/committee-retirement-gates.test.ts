import { describe, expect, it } from "vitest";

import type { PromiseCapacityPoint } from "../src/availability/promise-capacity-evidence.js";
import {
  planRetirement,
  retirementPointKey,
  type RetirementVerifiedFacts,
} from "../src/store/retirement-transition.js";
import {
  descendantPoint,
  expiryPoint,
  horizonMs,
  oldPoint,
  retentionFixture,
} from "./helpers/committee-retirement.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

/**
 * Plan §5.1, §15 C3: retirement deletes a cohort only at finality, depth
 * k + 1 (`isFinal`: depth > k). It has three finality gates: the checkpoint
 * (the horizon point the source re-reads), every point the cohort's rows
 * name, and the horizon point the plan covers. Each test below closes
 * exactly one of them at depth k and leaves the others final.
 */
const k = 2160;
/** The block that certified each seeded row's capacity, deep in the chain. */
const certified = {
  slot: 1_500_000,
  blockHash: "78".repeat(32),
  blockNo: 2400,
};

const fixture = async () => {
  const f = await retentionFixture(await openTestCommitteeStore(), {
    observed: oldPoint,
    certified,
    submissionTxHash: "88".repeat(32),
  });
  f.proofs.set(retirementPointKey(certified), certified);
  await f.compact();
  const seeded = await f.seed(1);
  return { ...f, headerHash: seeded.header.headerHash };
};

describe("the checkpoint gate", () => {
  it("retires nothing while the checkpoint is k deep, with every row's point final", async () => {
    const f = await fixture();
    f.setPoint(expiryPoint);
    await f.compact();
    expect((await f.store.getRetirementFloor())?.checkpoint?.point).toEqual(
      expiryPoint,
    );
    // Every row's point is final here; only the checkpoint is at depth k.
    f.setPoint({ ...descendantPoint, blockNo: expiryPoint.blockNo + k - 1 });
    await expect(f.compact()).resolves.toEqual([]);
    expect(await f.store.getDaPayload(f.headerHash)).toBeDefined();
    f.setPoint({ ...descendantPoint, blockNo: expiryPoint.blockNo + k });
    await expect(f.compact()).resolves.toEqual([f.headerHash]);
    f.journal.close();
  });
});

describe("the plan's finality gates", () => {
  /** The verified facts at `boundary` for the seeded header, horizon `horizon`. */
  const factsAt = (
    f: Awaited<ReturnType<typeof fixture>>,
    boundaryBlockNo: number,
    horizon: PromiseCapacityPoint,
  ): RetirementVerifiedFacts => ({
    binding: f.binding,
    boundary: { ...descendantPoint, blockNo: boundaryBlockNo },
    canonicalTimeMs: horizonMs + 2_000_000,
    horizonPoint: horizon,
    horizonTimeMs: horizonMs + 1_000_000,
    pinnedHeaderHashes: new Set(),
    financialHeaderHashes: new Set(),
    canonicalPoints: new Map(
      [oldPoint, certified].map((p) => [retirementPointKey(p), p]),
    ),
    submittedTransactionPoints: new Map([["88".repeat(32), oldPoint]]),
    pointsBeyondRetention: new Set(),
    headerLandingPoints: new Map([[f.headerHash, oldPoint]]),
    landingsBeyondRetention: new Set(),
    submissionsBeyondRetention: new Set(),
  });
  const plan = async (
    f: Awaited<ReturnType<typeof fixture>>,
    facts: RetirementVerifiedFacts,
  ) =>
    planRetirement(
      (await f.store.readRetirementSnapshot()).data,
      facts,
      new Set(),
      () => false,
    );
  /** A final horizon point just below the certifying block. */
  const belowCertified = {
    slot: certified.slot - 1,
    blockHash: "9a".repeat(32),
    blockNo: certified.blockNo - 1,
  };

  it("waits while a point its rows name is k deep, with the horizon final", async () => {
    const f = await fixture();
    await expect(
      plan(f, factsAt(f, certified.blockNo + k - 1, belowCertified)),
    ).resolves.toEqual({});
    await expect(
      plan(f, factsAt(f, certified.blockNo + k, certified)),
    ).resolves.toMatchObject({
      plan: { headerHashes: [f.headerHash], floor: { point: certified } },
    });
    f.journal.close();
  });

  it("refuses a horizon point k deep, with every row's point final", async () => {
    const f = await fixture();
    const boundary = certified.blockNo + k;
    const horizon = {
      slot: certified.slot + 1,
      blockHash: "9b".repeat(32),
      blockNo: boundary - k + 1,
    };
    await expect(plan(f, factsAt(f, boundary, horizon))).rejects.toThrow(
      "Retirement horizon point does not cover every removed checkpoint",
    );
    await expect(
      plan(f, factsAt(f, boundary + 1, horizon)),
    ).resolves.toMatchObject({
      plan: { headerHashes: [f.headerHash], floor: { point: horizon } },
    });
    f.journal.close();
  });
});
