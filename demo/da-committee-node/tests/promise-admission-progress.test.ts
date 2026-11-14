import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { availabilityParametersFromConfig } from "../src/availability/factory.availability-responder-operations.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import { utxo } from "./helpers/availability-challenge.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";
import { testPromiseRuntimePolicy } from "./helpers/promise-runtime-policy.js";

const progressFixture = async () => {
  const f = await promiseAdmissionFixture();
  const service = f.service();
  await service.initialize();
  expect(await service.tick()).toMatchObject({ signedHeaders: 1, errors: [] });
  const signed = (await f.store.listDaSignatures())[0]!;
  const payload = signed.headerHash === f.first.headerHash ? f.first : f.second;
  const candidate =
    signed.headerHash === f.first.headerHash ? f.second : f.first;
  const commitment = SDK.parseDaAvailabilityCommitmentCbor(
    signed.availabilityCommitmentCbor,
    SDK.availabilityResponseGeometry(
      f.config.availabilityChallenge.responseGeometry,
    ),
  );
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    commitment,
    challengerFundingOutRef: {
      transactionId: "99".repeat(32),
      outputIndex: 0n,
    },
    challenger: "aa".repeat(28),
    openedAt: 1000n,
    parameters: availabilityParametersFromConfig(f.config),
  });
  const tranche = plan.trancheThreads[0]!;
  if (!("Active" in tranche))
    throw new Error("Fixture expected an active tranche");
  const publications = SDK.planDaAvailabilityPublications({
    commitment,
    payload: payload.payloadCbor,
    challengeAssetName: plan.challengeAssetName,
  });
  const receipt = SDK.advanceDaAvailabilityTranche({
    active: tranche,
    publication: publications[0]!.publications[0]!,
    responseGeometry: commitment.response_geometry,
    inclusiveValidityUpper: 3000n,
    carrierOutputIndex: 1n,
  });
  const challenge = {
    record: { utxo: utxo(0), datum: plan.record },
    terminal: { utxo: utxo(1), datum: plan.terminalAccumulator },
    queue: utxo(2),
    tranches: [{ utxo: utxo(3), datum: receipt }],
  };
  // Two promises at 450 s each (900 s) exceed the 880 s testing response window.
  f.source.policyAuthority = testPromiseRuntimePolicy(
    {
      id: "bounded-progress-fixture",
      envelopeId: "test-only",
      publishStepMs: 150000,
      settleStepMs: 150000,
      closeStepMs: 150000,
      discoveryAndClockMarginMs: 0,
      supportedRecoveryMs: 0,
    },
    {
      deploymentFingerprint: f.config.deploymentFingerprint,
      contractManifestId: String(f.config.contractDeploymentInfo.manifestId),
    },
  ).authority;
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
    challenge,
    check: () =>
      f.admission().check({
        record,
        commitment: expected.commitment,
        commitmentDigest: expected.commitmentDigest,
      }),
  };
};

describe("canonical active promise progress", () => {
  it("reserves full restorable demand across shallow publication and settlement progress", async () => {
    const { f, challenge, check } = await progressFixture();
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900000,
    });
    f.setSnapshot({ ...f.getSnapshot(), challenges: [challenge] });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900000,
      publications: 2,
      settlements: 2,
      closes: 2,
      obligations: 2,
    });
    f.setSnapshot({
      ...f.getSnapshot(),
      canonicalTimeMs: Number.MAX_SAFE_INTEGER,
    });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900000,
      obligations: 2,
    });
    f.setSnapshot({
      ...f.getSnapshot(),
      challenges: [
        {
          ...challenge,
          tranches: [],
          terminal: {
            ...challenge.terminal,
            datum: { ...challenge.terminal.datum, next_tranche_index: 1n },
          },
        },
      ],
    });
    expect((await check()).decision).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 900000,
      settlements: 2,
      closes: 2,
    });
  });
  it("holds missing tranches, unknown scan resources, missing old bytes and a breached policy", async () => {
    const { f, challenge, check } = await progressFixture();
    f.setSnapshot({
      ...f.getSnapshot(),
      challenges: [{ ...challenge, tranches: [] }],
    });
    expect((await check()).decision.status).toBe("incomplete_evidence");
    f.setSnapshot({
      ...f.getSnapshot(),
      challenges: [challenge],
      resourceWorkload: undefined,
    });
    expect((await check()).decision.reason).toBe(
      "runtime_workload_evidence_unavailable",
    );
    f.setSnapshot({
      ...f.getSnapshot(),
      resourceWorkload: {
        walletInputs: 1,
        journalEntries: 0,
        challengeRecords: 1,
        storeRecords: 0,
        storeEncodedBytes: 0,
      },
    });
    const missingBytes = vi
      .spyOn(f.store, "getDaPayload")
      .mockResolvedValue(undefined);
    expect((await check()).decision.status).toBe("incomplete_evidence");
    missingBytes.mockRestore();
    f.source.policyAuthority = testPromiseRuntimePolicy(
      {
        id: "bounded-fence-fixture",
        envelopeId: "test-only",
        publishStepMs: 100000,
        settleStepMs: 100000,
        closeStepMs: 100000,
        discoveryAndClockMarginMs: 0,
        supportedRecoveryMs: 0,
      },
      {
        deploymentFingerprint: f.config.deploymentFingerprint,
        contractManifestId: String(f.config.contractDeploymentInfo.manifestId),
      },
    ).authority;
    const admitted = await check();
    expect(admitted.decision.status).toBe("admitted");
    f.source.policyAuthority.breach("fixture_stage_overrun");
    await expect(admitted.assertCurrent!()).rejects.toThrow(
      "runtime policy changed",
    );
    expect((await check()).decision.status).toBe("incomplete_evidence");
  });
});
