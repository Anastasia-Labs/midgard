import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyWithDaBondPoolChurnRetry,
  DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS,
  isDaBondPoolAttestationSkip,
} from "../src/transactions/da-attestation.js";

const HEADER_HASH = "ab".repeat(28);

class ApplyFailure extends Error {}

/**
 * A pool read that returns the given outrefs in order (the last one repeats),
 * or fails with a pool refusal where the entry is an error.
 */
const poolReads = (
  sequence: readonly (string | SDK.DaAttestationBuildError)[],
) => {
  let reads = 0;
  const read = Effect.suspend(() => {
    const entry = sequence[Math.min(reads, sequence.length - 1)];
    reads += 1;
    return typeof entry === "string"
      ? Effect.succeed(entry)
      : Effect.fail(entry);
  });
  return { read, count: () => reads };
};

/** An apply that fails with the given errors in order, then succeeds. */
const applies = (failures: readonly Error[]) => {
  let attempts = 0;
  const apply = Effect.suspend(() => {
    const failure = failures[attempts];
    attempts += 1;
    return failure === undefined
      ? Effect.succeed(`applied-${attempts.toString()}`)
      : Effect.fail(failure);
  });
  return { apply, count: () => attempts };
};

const run = <A, E>(effect: Effect.Effect<A, E>) =>
  Effect.runPromise(Effect.either(effect));

describe("DA attestation apply pool churn retry (G8)", () => {
  it("rebuilds the apply when the pool outref moved under a failed attempt", async () => {
    const pool = poolReads(["pool#0", "pool#1"]);
    const apply = applies([new ApplyFailure("reference input already spent")]);
    const outcome = await run(
      applyWithDaBondPoolChurnRetry({
        headerHash: HEADER_HASH,
        readPoolOutRef: pool.read,
        apply: apply.apply,
      }),
    );
    expect(outcome).toMatchObject({ _tag: "Right", right: "applied-2" });
    expect(apply.count()).toBe(2);
  });

  it("propagates the original failure without retrying when the pool did not move", async () => {
    const failure = new ApplyFailure("state queue node changed");
    const pool = poolReads(["pool#0"]);
    const apply = applies([failure]);
    const outcome = await run(
      applyWithDaBondPoolChurnRetry({
        headerHash: HEADER_HASH,
        readPoolOutRef: pool.read,
        apply: apply.apply,
      }),
    );
    expect(outcome).toMatchObject({ _tag: "Left", left: failure });
    expect(apply.count()).toBe(1);
  });

  it("stops after the bounded number of attempts while the pool keeps moving", async () => {
    let reads = 0;
    const movingPool = Effect.sync(() => {
      reads += 1;
      return `pool#${reads.toString()}`;
    });
    const failures = Array.from(
      { length: DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS + 2 },
      (_, index) =>
        new ApplyFailure(`spent reference input ${index.toString()}`),
    );
    const apply = applies(failures);
    const outcome = await run(
      applyWithDaBondPoolChurnRetry({
        headerHash: HEADER_HASH,
        readPoolOutRef: movingPool,
        apply: apply.apply,
      }),
    );
    expect(apply.count()).toBe(DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS);
    expect(outcome).toMatchObject({
      _tag: "Left",
      left: failures[DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS - 1],
    });
  });

  it("turns a pool that stopped backing after a failed attempt into a pool refusal", async () => {
    const refusal = new SDK.DaAttestationBuildError({
      reason: "pool-withdrawing",
      message: "The DA bond pool is withdrawing",
      cause: "test",
    });
    const pool = poolReads(["pool#0", refusal]);
    const apply = applies([new ApplyFailure("reference input already spent")]);
    const outcome = await run(
      applyWithDaBondPoolChurnRetry({
        headerHash: HEADER_HASH,
        readPoolOutRef: pool.read,
        apply: apply.apply,
      }),
    );
    expect(outcome).toMatchObject({ _tag: "Left", left: refusal });
    expect(isDaBondPoolAttestationSkip(refusal)).toBe(true);
    expect(apply.count()).toBe(1);
  });

  it("does not retry an apply refused for the pool", async () => {
    const refusal = new SDK.DaAttestationBuildError({
      reason: "pool-under-backed",
      message: "The DA bond pool backs less than one DA bond",
      cause: "test",
    });
    const pool = poolReads(["pool#0", "pool#1"]);
    const apply = applies([refusal]);
    const outcome = await run(
      applyWithDaBondPoolChurnRetry({
        headerHash: HEADER_HASH,
        readPoolOutRef: pool.read,
        apply: apply.apply,
      }),
    );
    expect(outcome).toMatchObject({ _tag: "Left", left: refusal });
    expect(apply.count()).toBe(1);
    expect(pool.count()).toBe(1);
  });

  it("classifies only the three pool reasons as a skip", () => {
    const skip = (reason: SDK.DaAttestationBuildFailureReason) =>
      isDaBondPoolAttestationSkip(
        new SDK.DaAttestationBuildError({ reason, message: reason, cause: "" }),
      );
    expect(skip("pool-under-backed")).toBe(true);
    expect(skip("pool-unavailable")).toBe(true);
    expect(skip("pool-withdrawing")).toBe(true);
    expect(skip("committee_rotated")).toBe(false);
    expect(skip("validity_range_past_deadline")).toBe(false);
    expect(isDaBondPoolAttestationSkip(new Error("pool-withdrawing"))).toBe(
      false,
    );
  });
});
