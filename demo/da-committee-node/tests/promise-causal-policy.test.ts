import { describe, expect, it } from "vitest";

import {
  committeePromiseCausalPolicyDigest,
  verifyCommitteePromiseCausalPolicy,
} from "../src/availability/promise-causal-policy.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";
import { testPromiseCausalPolicy } from "./helpers/promise-causal-policy.js";

describe("configured causal admission authority", () => {
  it("charges the entire protected restoration set and five aggregate attempts without a scalar-policy conversion", () => {
    const f = testPromiseCausalPolicy();
    expect(f.authority.status()).toMatchObject({ status: "conditional" });
    expect(f.authority.policy(f.workload)).toBeUndefined();
    expect(f.authority.futureIntentRows?.(f.workload)).toBe(9);
    const full = {
      ...f.workload,
      outstandingPromises: 72,
      tranches: 72,
      publications: 144,
    };
    expect(f.authority.futureIntentRows?.(full)).toBe(648);
    expect(
      f.authority.envelope?.({ ...full, outstandingPromises: 73 }),
    ).toBeUndefined();
    expect(
      f.authority.envelope?.({ ...full, journalEntries: 1025 }),
    ).toBeUndefined();
  });
  it.each(["schema", "source", "enforcement", "retry", "calibration"])(
    "refuses mismatched %s before establishing a signing capability",
    (kind) => {
      const f = testPromiseCausalPolicy();
      const artifact = structuredClone(f.artifact);
      if (kind === "schema") Reflect.set(artifact, "schemaVersion", 1);
      if (kind === "source")
        Reflect.set(artifact, "sourceBinding", {
          ...artifact.sourceBinding,
          rollbackGeneration: 1,
        });
      if (kind === "enforcement") Reflect.set(artifact, "enforcement", []);
      if (kind === "retry")
        Reflect.set(artifact, "causal", {
          ...artifact.causal,
          aggregateAllowedFailedAttempts: 6,
        });
      if (kind === "calibration")
        Reflect.set(artifact, "calibrationEvidenceDigest", "00".repeat(32));
      const authority = verifyCommitteePromiseCausalPolicy({
        ...f.input,
        artifact,
        trustedPolicyDigest: committeePromiseCausalPolicyDigest(artifact),
      });
      expect(authority.status()).toMatchObject({ status: "unavailable" });
      expect(authority.envelope?.(f.workload)).toBeUndefined();
    },
  );
  it("latches clock, rollback and explicit stage breaches", () => {
    const f = testPromiseCausalPolicy();
    f.setClock(1000, 3000);
    expect(f.authority.status()).toMatchObject({ status: "unavailable" });
    f.setClock(4000, 3000);
    expect(f.authority.status()).toMatchObject({ status: "unavailable" });
    const rollback = testPromiseCausalPolicy();
    rollback.rollback();
    expect(rollback.authority.status()).toMatchObject({
      reason: "rollback_generation_changed",
    });
    const breach = testPromiseCausalPolicy();
    breach.authority.breach("submit_cap_breached");
    expect(breach.authority.status()).toMatchObject({
      reason: "submit_cap_breached",
    });
  });
  it("uses the real verified-commitment signing path, preserves old promises, and refuses a second current timing slot", async () => {
    const f = await promiseAdmissionFixture();
    const causal = testPromiseCausalPolicy(f.source.policyAuthority.binding);
    f.source.policyAuthority = causal.authority;
    f.source.readSnapshot = async () => ({
      ...f.getSnapshot(),
      boundary: {
        ...f.getSnapshot().boundary,
        actorStateDigest: "12".repeat(32),
        schedulingEvidenceDigest: "34".repeat(32),
      },
      currentSchedulingCommitmentDigests: new Set(
        (await f.store.listDaSignatures()).map(
          (row) => row.availabilityCommitmentDigest,
        ),
      ),
      retainedAttempts: [],
    });
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(1);
    const old = await f.store.listDaSignatures();
    f.source.readSnapshot = async () => ({
      ...f.getSnapshot(),
      boundary: {
        ...f.getSnapshot().boundary,
        actorStateDigest: "12".repeat(32),
        schedulingEvidenceDigest: "34".repeat(32),
      },
      currentSchedulingCommitmentDigests: new Set(),
      retainedAttempts: [],
    });
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(await f.store.listDaSignatures()).toHaveLength(2);
    expect(
      (await f.store.listDaSignatures()).find(
        (row) => row.headerHash === old[0]!.headerHash,
      ),
    ).toEqual(old[0]);
  });
});
