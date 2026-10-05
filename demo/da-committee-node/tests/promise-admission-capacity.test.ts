import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import type { PromiseCapacityPoint } from "../src/availability/promise-capacity-evidence.js";
import { retiredPromiseCutoffs } from "../src/availability/promise-cutoff-source.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import {
  promiseAdmissionFixture,
  stores,
} from "./helpers/promise-admission.js";

describe("persisted capacity certificate at the actual committee signing boundary", () => {
  it("refuses the next promise at k, signs it beyond k, and fences a crossed protected floor after restart", async () => {
    const f = await promiseAdmissionFixture();
    let activeStore = f.store;
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
    let orphan = false;
    f.source.readSnapshot = async () => ({
      ...f.getSnapshot(),
      canonicalTimeMs: boundary.slot * 1000,
      retiredCommitmentDigests: await retiredPromiseCutoffs({
        store: activeStore,
        deploymentFingerprint: f.config.deploymentFingerprint,
        contractManifestId: String(f.config.contractDeploymentInfo.manifestId),
        actorId: f.source.policyAuthority.binding!.actorId,
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
          orphan && point.blockHash === "12".repeat(32)
            ? null
            : { point, tip: boundary },
        assertCurrent: f.assertCurrent,
      }),
    });
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    boundary = {
      slot: firstSlot + 2160,
      blockNo: 2260,
      blockHash: "34".repeat(32),
    };
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(1);
    boundary = {
      slot: firstSlot + 2161,
      blockNo: 2261,
      blockHash: "56".repeat(32),
    };
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(2);
    expect(await f.store.listDaSignatures()).toHaveLength(2);
    // A rollback that removes the certified negative checkpoint exceeds the
    // supported monotonic floor: capacity cannot be reused on the new fork.
    orphan = true;
    const record = (await f.store.getStateQueueHeader(candidate.headerHash))!;
    const expected = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: f.config.hubOraclePolicyId,
        responseGeometry: f.config.availabilityChallenge.responseGeometry,
      },
      headerHash: candidate.headerHash,
      payloadCborHex: candidate.payloadCbor.toString("hex"),
    });
    const check = () =>
      f.admission(activeStore).check({
        record,
        commitment: expected.commitment,
        commitmentDigest: expected.commitmentDigest,
      });
    expect((await check()).decision).toMatchObject({
      status: "incomplete_evidence",
    });
    // Persisted floor-cross evidence, rather than this process's memo, fences
    // subsequent admission even if the provider reports the old point again.
    await activeStore.close();
    stores.delete(activeStore);
    activeStore = await f.open();
    orphan = false;
    expect((await check()).decision).toMatchObject({
      status: "incomplete_evidence",
      reason: expect.stringContaining("previously crossed"),
    });
    expect(f.sign).toHaveBeenCalledTimes(2);
  });
});
