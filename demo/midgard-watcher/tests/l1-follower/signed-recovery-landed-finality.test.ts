/**
 * The watcher's L1 source says a landed transaction of ours is final only
 * at the release finality depth (the policy's confirmationDepth, 2 here, the
 * tip being 1 deep), never on first sight of its landing, and not only at
 * the deeper recovery depth (k + 2 = 5) that expiries and spends wait for.
 * One block deep it is landed and not final, so the workflow keeps
 * following it; a rollback that drops it reads it absent with every input
 * live, and the recovery resubmits the same signed bytes; landed again, it
 * turns final once it is release-deep. While no stored block is that deep
 * its finality cannot be read at all.
 */
import {
  FraudProofL1UnavailableError,
  fraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs/workflow/release-finality-policy";
import { isFinalAtReleaseDepth } from "@al-ft/midgard-fault-proofs/workflow/signed-transaction-reconciliation";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import { expect, it } from "vitest";

import { harness, T } from "../support/l1-follower-raw-reads-fixture.js";
import {
  nodeDouble,
  RECOVERY_DEPTH,
  RELEASE,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

const RELEASE_DEPTH = RELEASE.policy.confirmationDepth;

/** Our attempt, spending a funding output `above` blocks deep, or, with no
 * `above`, an output from outside the follower's view, so that it can land
 * in the first block above the origin. */
/** RELEASE with a deeper release depth (still short of the recovery
 * depth), so that a landing in the first block above the origin has no
 * stored block that deep under it. */
const deeper = (
  confirmationDepth: number,
): VerifiedFraudProofReleaseFinalityPolicy => {
  const policy = { ...RELEASE.policy, confirmationDepth };
  return {
    ...RELEASE,
    policy,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  };
};

const scene = async (above?: number, release = RELEASE) => {
  const h = await harness();
  let input = h.chain.outsideInput();
  if (above !== undefined) {
    const funding: SimTx = {
      inputs: [input],
      outputs: [{ address: T, lovelace: 2_000_000n }],
      nonce: h.chain.nonce(),
    };
    const { hashes } = await h.forward([funding]);
    for (let block = 0; block < above; block += 1) await h.forward([]);
    input = { txHash: Buffer.from(hashes[0]!, "hex"), index: 0 };
  }
  const attempt: SimTx = {
    inputs: [input],
    outputs: [{ address: T, lovelace: 1_500_000n }],
    nonce: h.chain.nonce(),
  };
  const double = nodeDouble();
  const recovery = fraudProofSignedTransactionRecovery(
    sourceOver(h, h.reads(), double.node),
    release,
  );
  const signed = signedOf(attempt);
  return {
    h,
    attempt,
    double,
    recovery,
    signed,
    observe: () => recovery.observeSignedTransaction(signed),
  };
};

it("holds a landing 1 block deep not final, reads it absent with the same bytes to resubmit once a rollback drops it, and holds it final only once it is release-deep", async () => {
  const { h, attempt, double, recovery, signed, observe } = await scene(4);

  const { point: firstLanding } = await h.forward([attempt]);
  const shallow = await observe();
  expect(shallow).toMatchObject({
    status: "included",
    inclusionPoint: firstLanding,
    canonicalPoint: firstLanding,
  });
  expect(BigInt(shallow.releaseFinalPoint.blockNo)).toBe(
    BigInt(firstLanding.blockNo) - BigInt(RELEASE_DEPTH - 1),
  );
  expect(isFinalAtReleaseDepth(shallow)).toBe(false);

  // A rollback drops it: absent, its input live, to be sent again as it is.
  await h.backward(1);
  const dropped = await observe();
  expect(dropped).toMatchObject({
    ...signed,
    status: "rebroadcast",
    canonicalPoint: h.tipPoint(),
  });
  expect(isFinalAtReleaseDepth(dropped)).toBe(false);
  double.calls.length = 0;
  await expect(
    recovery.rebroadcastSignedTransaction({
      ...signed,
      authorizeResubmission: async () => undefined,
    }),
  ).resolves.toBe(signed.transactionHash);
  expect(double.calls).toEqual([`submit:${signed.signedTransactionCborHex}`]);

  // The same bytes land again: final only at release depth, which is less
  // than the recovery depth.
  expect(RELEASE_DEPTH).toBeLessThan(RECOVERY_DEPTH);
  const { point: landing } = await h.forward([attempt]);
  // 1 deep up to one block short of release depth.
  const shortOfRelease = Array.from(
    { length: RELEASE_DEPTH - 1 },
    (_, i) => i + 1,
  );
  for (const depth of shortOfRelease) {
    const observed = await observe();
    expect(observed, `${depth.toString()} deep`).toMatchObject({
      status: "included",
      inclusionPoint: landing,
    });
    expect(isFinalAtReleaseDepth(observed), `${depth.toString()} deep`).toBe(
      false,
    );
    await h.forward([]);
  }
  const deep = await observe();
  expect(deep).toMatchObject({
    status: "included",
    inclusionPoint: landing,
    releaseFinalPoint: landing,
  });
  expect(isFinalAtReleaseDepth(deep)).toBe(true);
});

it("cannot read a landing's finality while no stored block is release-deep", async () => {
  const depth = RELEASE_DEPTH + 1;
  expect(depth).toBeLessThan(RECOVERY_DEPTH);
  const { h, attempt, observe } = await scene(undefined, deeper(depth));
  await h.forward([attempt]);
  // The origin is stored: the chain is 2 blocks long, short of 3.
  await expect(observe()).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
  // 2 deep: readable, not final.
  await h.forward([]);
  expect(isFinalAtReleaseDepth(await observe())).toBe(false);
  // 3 deep: final.
  await h.forward([]);
  expect(isFinalAtReleaseDepth(await observe())).toBe(true);
});
