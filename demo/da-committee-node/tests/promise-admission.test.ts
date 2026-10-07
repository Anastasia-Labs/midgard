import { describe, expect, it, vi } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import { retainedSignedPayload } from "./committee-service.test/fixtures.js";
import { promiseAdmissionFixture as fixture } from "./helpers/promise-admission.js";

describe("committee fresh promise admission", () => {
  it("persists the first liability and refuses a second after restart and failed publication", async () => {
    const f = await fixture();
    const initial = f.service();
    await initial.initialize();
    expect(await initial.tick()).toMatchObject({
      signedHeaders: 1,
      skippedHeaders: 1,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(1);
    const signed = (await f.store.listDaSignatures())[0]!;
    await f.store.saveDaSignature({
      ...signed,
      broadcastStatus: "post_failed",
      source: "peer",
    });
    expect(await initial.readinessSnapshot()).toMatchObject({
      ready: false,
      promiseAdmission: {
        status: "insufficient_capacity",
        requiredMs: 900_000,
        candidateHeaderHash:
          signed.headerHash === f.first.headerHash
            ? f.second.headerHash
            : f.first.headerHash,
      },
    });
    await f.store.close();
    const reopened = await f.open();
    const restarted = f.service(reopened);
    await restarted.initialize();
    expect(await restarted.tick()).toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 2,
    });
    expect(f.sign).toHaveBeenCalledTimes(1);
    expect((await reopened.listDaSignatures()).length).toBe(1);
  });

  it("unknown runtime bounds refuse new signing while exact old signatures republish and bytes remain readable", async () => {
    const f = await fixture();
    const initial = f.service();
    await initial.initialize();
    expect(await initial.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    const signed = (await f.store.listDaSignatures())[0]!;
    const signedFixture =
      signed.headerHash === f.first.headerHash ? f.first : f.second;
    const candidate =
      signed.headerHash === f.first.headerHash ? f.second : f.first;
    const publishSignature = vi.fn(async () => "posted" as const);
    const oldOnly = new CommitteeService({
      config: {
        ...f.config,
        availabilityPromiseAdoption: {
          policyArtifactPath: "/explicit/adopted/artifact",
          trustedPolicyDigest: "00".repeat(32),
          resourceProfilePath: "/explicit/adopted/resources",
          trustedResourceProfileDigest: "00".repeat(32),
          calibrationEvidencePath: "/explicit/adopted/calibration",
          trustedCalibrationEvidenceDigest: "00".repeat(32),
          faultModelPath: "/explicit/adopted/faults",
          trustedFaultModelDigest: "00".repeat(32),
        },
      },
      store: f.store,
      stateQueueProvider: f.provider,
      payloadSource: f.payloadSource,
      signer: {
        publicKeyHex: f.signerValidation.signerPublicKeyHex,
        sign: f.sign,
      },
      signerValidation: f.signerValidation,
      coordinator: { publishSignature },
      writeEvent: () => {},
    });
    await oldOnly.initialize();
    expect(await oldOnly.readinessSnapshot()).toMatchObject({
      ready: false,
      promiseAdmissionPolicy: {
        status: "unavailable",
        reason: "finite_runtime_policy_unavailable",
      },
    });
    expect(await oldOnly.tick()).toMatchObject({ signedHeaders: 0 });
    expect(publishSignature).toHaveBeenCalledTimes(1);
    expect(f.sign).toHaveBeenCalledTimes(1);
    expect(
      await retainedSignedPayload(f.store, f.config, signed.headerHash),
    ).toEqual(signedFixture.payloadCbor);
    expect(await oldOnly.readinessSnapshot()).toMatchObject({
      ready: false,
      promiseAdmission: {
        status: "incomplete_evidence",
        reason: "finite_runtime_policy_unavailable",
        candidateHeaderHash: candidate.headerHash,
      },
    });
  });

  it("fences a changed canonical generation before the actual signing callback", async () => {
    const f = await fixture();
    f.assertCurrent.mockRejectedValue(new Error("generation changed"));
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 2,
    });
    expect(f.sign).not.toHaveBeenCalled();
    expect(await f.store.listDaSignatures()).toEqual([]);
    expect(await service.readinessSnapshot()).toMatchObject({
      promiseAdmission: { reason: "canonical_admission_boundary_changed" },
    });
  });

  it("holds unresolved actor resources and incomplete discovery without deleting retained bytes", async () => {
    const f = await fixture();
    f.setSnapshot({
      ...f.getSnapshot(),
      blocking: { kind: "unresolved", reason: "pending durable intent" },
    });
    const service = f.service();
    await service.initialize();
    await service.tick();
    expect(f.sign).not.toHaveBeenCalled();
    expect(await service.readinessSnapshot()).toMatchObject({
      promiseAdmission: {
        status: "unbounded_blocking",
        reason: "pending durable intent",
      },
    });
    expect(
      (await f.store.getDaPayload(f.first.headerHash))?.validationStatus,
    ).toBe("verified");
    f.setSnapshot({
      ...f.getSnapshot(),
      complete: false,
      blocking: { kind: "bounded", remainingMs: 0 },
    });
    await service.tick();
    expect(f.sign).not.toHaveBeenCalled();
    expect(await service.readinessSnapshot()).toMatchObject({
      promiseAdmission: {
        status: "incomplete_evidence",
        reason: "canonical_admission_evidence_incomplete",
      },
    });
  });

  it("retains restorable potential work past elapsed cutoff and releases only with k-safe proof", async () => {
    const f = await fixture();
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    const signed = (await f.store.listDaSignatures())[0]!;
    const candidate =
      signed.headerHash === f.first.headerHash ? f.second : f.first;
    const header = (await f.store.getStateQueueHeader(candidate.headerHash))!;
    const expected = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: f.config.hubOraclePolicyId,
        responseGeometry: f.config.availabilityChallenge.responseGeometry,
      },
      headerHash: candidate.headerHash,
      payloadCborHex: candidate.payloadCbor.toString("hex"),
    });
    const check = () =>
      f.admission().check({
        record: header,
        commitment: expected.commitment,
        commitmentDigest: expected.commitmentDigest,
      });
    f.setSnapshot({
      ...f.getSnapshot(),
      canonicalTimeMs: Number.MAX_SAFE_INTEGER,
    });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900_000,
    });
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 2,
    });
    expect(f.sign).toHaveBeenCalledTimes(1);
    // The source unit independently proves the durable >k certificate.
    f.setSnapshot({
      ...f.getSnapshot(),
      retiredCommitmentDigests: new Set([signed.availabilityCommitmentDigest]),
    });
    expect((await check()).decision.status).toBe("admitted");
    f.setSnapshot({
      ...f.getSnapshot(),
      canonicalTimeMs: 0,
      retiredCommitmentDigests: undefined,
      boundary: { ...f.getSnapshot().boundary, rollbackGeneration: 1 },
    });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900_000,
    });
    expect((await f.store.listDaSignatures()).length).toBe(1);
  });

  it("does not count a malformed durable own-signer record as zero", async () => {
    const f = await fixture();
    const service = f.service();
    await service.initialize();
    await service.tick();
    const signed = (await f.store.listDaSignatures())[0]!;
    const list = vi
      .spyOn(f.store, "listDaSignatures")
      .mockResolvedValue([{ ...signed, payloadHash: "00".repeat(32) }]);
    const record = (await f.store.getStateQueueHeader(f.second.headerHash))!;
    const expected = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: f.config.hubOraclePolicyId,
        responseGeometry: f.config.availabilityChallenge.responseGeometry,
      },
      headerHash: record.headerHash,
      payloadCborHex: f.second.payloadCbor.toString("hex"),
    });
    expect(
      (
        await f.admission().check({
          record,
          commitment: expected.commitment,
          commitmentDigest: expected.commitmentDigest,
        })
      ).decision.status,
    ).toBe("incomplete_evidence");
    list.mockRestore();
  });
});
