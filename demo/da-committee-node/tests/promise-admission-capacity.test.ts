import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import {
  promiseCapacityEvidenceKey,
  type PromiseCapacityPoint,
} from "../src/availability/promise-capacity-evidence.js";
import { retiredPromiseCutoffs } from "../src/availability/promise-cutoff-source.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

// Signs one promise, then wires the committee's capacity source to a
// controllable selected chain over that promise's cutoff.
const signedPromiseOnControlledChain = async () => {
  const f = await promiseAdmissionFixture();
  const state = { activeStore: f.store, orphan: false };
  const service = f.service();
  await service.initialize();
  expect(await service.tick()).toMatchObject({
    signedHeaders: 1,
    errors: [],
  });
  const signed = (await f.store.listDaSignatures())[0]!;
  const old = (await f.store.getStateQueueHeader(signed.headerHash))!;
  const candidate =
    signed.headerHash === f.first.headerHash ? f.second : f.first;
  const cutoff = Number(
    old.header.endTime +
      BigInt(SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms),
  );
  const firstSlot = Math.ceil(cutoff / 1000);
  let boundary: PromiseCapacityPoint = {
    slot: firstSlot,
    blockNo: 100,
    blockHash: "12".repeat(32),
  };
  const identity = {
    deploymentFingerprint: f.config.deploymentFingerprint,
    contractManifestId: String(f.config.contractDeploymentInfo.manifestId),
    actorId: f.source.policyAuthority.binding!.actorId,
  };
  f.source.readSnapshot = async () => ({
    ...f.getSnapshot(),
    canonicalTimeMs: boundary.slot * 1000,
    retiredCommitmentDigests: await retiredPromiseCutoffs({
      store: state.activeStore,
      ...identity,
      recoveryDepth: 2160,
      boundary,
      canonicalTimeMs: boundary.slot * 1000,
      slotTimeMs: (slot) => slot * 1000,
      liabilities: [
        {
          headerHash: old.headerHash,
          commitmentDigest: signed.availabilityCommitmentDigest,
          cutoffTimeMs: cutoff,
        },
      ],
      readCanonicalPoint: async (point) =>
        state.orphan && point.blockHash === "12".repeat(32)
          ? null
          : { point, tip: boundary },
      assertCurrent: f.assertCurrent,
    }),
  });
  const record = (await f.store.getStateQueueHeader(candidate.headerHash))!;
  const expected = deriveExpectedDaAvailabilityCommitment({
    authority: {
      deploymentIdentity: f.config.hubOraclePolicyId,
      responseGeometry: f.config.availabilityChallenge.responseGeometry,
    },
    headerHash: candidate.headerHash,
    payloadCborHex: candidate.payloadCbor.toString("hex"),
  });
  return {
    f,
    state,
    service,
    firstSlot,
    setBoundary: (next: PromiseCapacityPoint) => {
      boundary = next;
    },
    check: () =>
      f.admission(state.activeStore).check({
        record,
        commitment: expected.commitment,
        commitmentDigest: expected.commitmentDigest,
      }),
    evidence: () =>
      state.activeStore.getPromiseCapacityEvidence(
        promiseCapacityEvidenceKey({
          ...identity,
          commitmentDigest: signed.availabilityCommitmentDigest,
        }),
      ),
  };
};

describe("persisted capacity certificate at the actual committee signing boundary", () => {
  it("signs the next promise once the old cutoff is observed, certifies it only beyond k, and fences a crossed protected floor after restart", async () => {
    const { f, state, service, firstSlot, setBoundary, check, evidence } =
      await signedPromiseOnControlledChain();
    // Confirmed but unretired: the observed cutoff no longer consumes
    // capacity, although it is not yet certified past the recovery depth.
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(2);
    expect(await f.store.listDaSignatures()).toHaveLength(2);
    expect(await evidence()).toMatchObject({ retirementKind: "open_cutoff" });
    expect((await evidence())?.certifiedAt).toBeUndefined();
    setBoundary({
      slot: firstSlot + 2159,
      blockNo: 2259,
      blockHash: "34".repeat(32),
    });
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    // With nothing left to sign, the admission check drives the source.
    expect((await check()).decision).toMatchObject({ status: "admitted" });
    expect((await evidence())?.certifiedAt).toBeUndefined();
    setBoundary({
      slot: firstSlot + 2160,
      blockNo: 2260,
      blockHash: "56".repeat(32),
    });
    expect((await check()).decision).toMatchObject({ status: "admitted" });
    expect((await evidence())?.certifiedAt?.blockNo).toBe(2260);
    expect(f.sign).toHaveBeenCalledTimes(2);
    // A rollback that removes the certified negative checkpoint exceeds the
    // supported monotonic floor: capacity cannot be reused on the new fork.
    state.orphan = true;
    expect((await check()).decision).toMatchObject({
      status: "incomplete_evidence",
    });
    // Persisted floor-cross evidence, rather than this process's memo, fences
    // subsequent admission even if the provider reports the old point again.
    await state.activeStore.close();
    state.activeStore = await f.open();
    state.orphan = false;
    expect((await check()).decision).toMatchObject({
      status: "incomplete_evidence",
      reason: expect.stringContaining("previously crossed"),
    });
    expect(f.sign).toHaveBeenCalledTimes(2);
  });
  it("charges a released promise again when its cutoff observation rolls back within k, and admits once the cutoff is re-observed", async () => {
    const { state, firstSlot, setBoundary, check, evidence } =
      await signedPromiseOnControlledChain();
    expect((await check()).decision).toMatchObject({ status: "admitted" });
    expect((await evidence())?.certifiedAt).toBeUndefined();
    // A shallow fork whose tip precedes the cutoff removes the observation.
    state.orphan = true;
    setBoundary({
      slot: firstSlot - 1,
      blockNo: 99,
      blockHash: "78".repeat(32),
    });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
    });
    // The cutoff is observed on the new fork and releases capacity again.
    setBoundary({
      slot: firstSlot,
      blockNo: 101,
      blockHash: "9a".repeat(32),
    });
    expect((await check()).decision).toMatchObject({ status: "admitted" });
    expect(await evidence()).toMatchObject({
      point: { blockNo: 101, blockHash: "9a".repeat(32) },
      retirementKind: "open_cutoff",
    });
    expect((await evidence())?.certifiedAt).toBeUndefined();
  });
});
