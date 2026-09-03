import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION,
  type E2EStateCorrectionAcceptance,
  parseE2EStateCorrectionAcceptance,
  REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS,
  stateCorrectionAcceptanceEvidence,
  stateCorrectionLocalReadinessEvidence,
} from "../src/commands/e2e-state-correction-acceptance.js";

const hash = (index: number): string => index.toString(16).padStart(64, "0");

const acceptanceFixture = (): E2EStateCorrectionAcceptance => {
  let nextHash = 1;
  const takeHash = (): string => hash(nextHash++);
  return {
    schemaVersion: E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION,
    runId: "fresh-final-release",
    network: "Preprod",
    deployment: {
      manifestId: takeHash(),
      blueprintSha256: takeHash(),
      catalogueRoot: takeHash(),
      parametersSha256: takeHash(),
      releaseEvidenceSha256: takeHash(),
    },
    families: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((familyId) => ({
      familyId,
      violationId: `violation-${familyId}`,
      headerHash: takeHash().slice(0, 56),
      routeId: `route-${familyId}`,
      detectionSource: "public-l1-da",
      watcherDriven: true,
      initTxHash: takeHash(),
      proofStepTxHashes: [takeHash(), takeHash()],
      proofTokenTxHash: takeHash(),
      removalTxHash: takeHash(),
      correctionTxHash: takeHash(),
      permanentProofTokenRetained: true,
      stateQueueNodeRemoved: true,
      correctedQueueObserved: true,
      expectedSlashLovelace: "5000000",
      observedSlashLovelace: "5000000",
      expectedProverRewardLovelace: "1000000",
      observedProverRewardLovelace: "1000000",
      chainPoint: { slot: "123456", blockHash: takeHash() },
      finalStateRoot: takeHash(),
    })),
    withdrawalReservePayout: {
      withdrawalOrderTxHash: takeHash(),
      reserveTxHash: takeHash(),
      payoutInitTxHash: takeHash(),
      payoutAddTxHashes: [takeHash()],
      payoutConcludeTxHash: takeHash(),
      expectedDestination: "addr_test1qpayout",
      observedDestination: "addr_test1qpayout",
      expectedPayoutValueSha256: takeHash(),
      observedPayoutValueSha256: "",
      expectedReserveValueSha256: takeHash(),
      observedReserveValueSha256: "",
      reserveAccountingExact: true,
      finalStatus: "paid",
      chainPoint: { slot: "123457", blockHash: takeHash() },
    },
    forcedClassifications: [
      {
        direction: "valid-marked-invalid",
        operatorClassification: "invalid",
        canonicalClassification: "valid",
        finalClassification: "valid",
        detectionSource: "public-l1-da",
        watcherDriven: true,
        routeId: "forced-valid-route",
        evidenceTxHash: takeHash(),
        correctionTxHash: takeHash(),
        corrected: true,
        chainPoint: { slot: "123458", blockHash: takeHash() },
      },
      {
        direction: "invalid-marked-valid",
        operatorClassification: "valid",
        canonicalClassification: "invalid",
        finalClassification: "invalid",
        detectionSource: "public-l1-da",
        watcherDriven: true,
        routeId: "forced-invalid-route",
        evidenceTxHash: takeHash(),
        correctionTxHash: takeHash(),
        corrected: true,
        chainPoint: { slot: "123459", blockHash: takeHash() },
      },
    ],
    recoveryDrills: REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS.map((id) => ({
      id,
      status: "recovered",
      failClosed: true,
      duplicateSubmissions: 0,
      lostEvidence: 0,
      falseVerifiedStates: 0,
      unrecoverableWorkflows: 0,
      manualRepair: false,
      watcherReadyAfterRecovery: true,
      evidenceSha256: takeHash(),
    })),
    finalState: {
      stateQueueDepth: 0,
      unfinishedMutationJobs: 0,
      pendingFinalizations: 0,
      watcherReady: true,
      watcherVerificationResumed: true,
      exactEconomicReconciliation: true,
      finalStateSha256: takeHash(),
    },
  };
};

const finalizedFixture = (): E2EStateCorrectionAcceptance => {
  const fixture = acceptanceFixture();
  return {
    ...fixture,
    withdrawalReservePayout: {
      ...fixture.withdrawalReservePayout,
      observedPayoutValueSha256:
        fixture.withdrawalReservePayout.expectedPayoutValueSha256,
      observedReserveValueSha256:
        fixture.withdrawalReservePayout.expectedReserveValueSha256,
    },
  };
};

describe("state-correction acceptance evidence", () => {
  it("keeps Q56 and Q58 finalizer readiness explicitly fail closed", () => {
    const gates = stateCorrectionLocalReadinessEvidence({
      availabilityChallengeCapability: "missing",
    });
    expect(gates).toEqual([
      expect.objectContaining({
        label: "state_correction_local_workflow_readiness",
        status: "blocked",
        source: "compiled-production-workflow-registry-v1",
      }),
      expect.objectContaining({
        label: "availability_challenge_readiness",
        status: "blocked",
        source: "finalized-deployment-manifest-v1",
        details: expect.objectContaining({ capability: "missing" }),
      }),
    ]);
    expect(gates[0]!.details.missingCategoryCount).not.toBe("0");
  });

  it("keeps a complete aggregate blocked until independent provenance is reconciled", () => {
    const fixture = finalizedFixture();
    const parsed = parseE2EStateCorrectionAcceptance(fixture);
    const evidence = stateCorrectionAcceptanceEvidence({
      expectedRunId: fixture.runId,
      evidence: parsed,
      evidencePath: "logs/fresh-final-release/state-correction.json",
    });

    expect(parsed.families.map((family) => family.familyId)).toEqual(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    expect(parsed.recoveryDrills.map((drill) => drill.id)).toEqual(
      REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS,
    );
    expect(evidence.db).toHaveLength(6);
    expect(evidence.db.every((gate) => gate.status === "blocked")).toBe(true);
    expect(evidence.transactions).toEqual([]);
    expect(evidence.rawEvidence).toEqual([
      {
        label: "state-correction-acceptance",
        path: "logs/fresh-final-release/state-correction.json",
      },
    ]);
    expect(evidence.notes.join("\n")).toMatch(
      /blocked pending independent provenance/u,
    );
  });

  it("fails the finalizer gate when the artifact is absent or bound to another run", () => {
    expect(
      stateCorrectionAcceptanceEvidence({
        expectedRunId: "fresh-final-release",
      }).db,
    ).toContainEqual(
      expect.objectContaining({
        label: "state_correction_acceptance",
        status: "failed",
      }),
    );

    const fixture = finalizedFixture();
    const mismatch = stateCorrectionAcceptanceEvidence({
      expectedRunId: "different-run",
      evidence: fixture,
      evidencePath: "state-correction.json",
    });
    expect(mismatch.db.every((gate) => gate.status === "failed")).toBe(true);
    expect(mismatch.transactions).toEqual([]);
  });

  it("rejects an omitted family instead of accepting a partial sweep", () => {
    const fixture = finalizedFixture();
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        families: fixture.families.slice(0, -1),
      }),
    ).toThrow(/cover the launch-scope catalogue exactly/u);
  });

  it("rejects inexact slash or prover economics", () => {
    const fixture = finalizedFixture();
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        families: fixture.families.map((family, index) =>
          index === 0
            ? { ...family, observedSlashLovelace: "4999999" }
            : family,
        ),
      }),
    ).toThrow(/observed slash does not equal expected slash/u);
  });

  it("rejects manual, lossy, duplicate, or incomplete recovery claims", () => {
    const fixture = finalizedFixture();
    const first = fixture.recoveryDrills[0]!;
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        recoveryDrills: [
          { ...first, lostEvidence: 1 },
          ...fixture.recoveryDrills.slice(1),
        ],
      }),
    ).toThrow(/lostEvidence must be 0/u);
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        recoveryDrills: fixture.recoveryDrills.slice(0, -1),
      }),
    ).toThrow(/recoveryDrills must contain exactly/u);
  });

  it("rejects a forced direction without public autonomous correction", () => {
    const fixture = finalizedFixture();
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        forcedClassifications: fixture.forcedClassifications.map(
          (drill, index) =>
            index === 0 ? { ...drill, watcherDriven: false } : drill,
        ),
      }),
    ).toThrow(/watcherDriven must be true/u);
  });

  it("rejects payout destination, payout value, and reserve value mismatches", () => {
    const fixture = finalizedFixture();
    expect(() =>
      parseE2EStateCorrectionAcceptance({
        ...fixture,
        withdrawalReservePayout: {
          ...fixture.withdrawalReservePayout,
          observedDestination: "addr_test1qwrong",
        },
      }),
    ).toThrow(/payout destination mismatch/u);
  });
});
