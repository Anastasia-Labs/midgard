import { createHash } from "node:crypto";
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import { describe, expect, it, vi } from "vitest";

vi.mock("@al-ft/midgard-core/deployment-manifest-identity", async (load) => ({
  ...(await load<object>()),
  verifyFinalizedDeploymentManifest: (value: unknown) => value,
}));
import type { E2EStateCorrectionAcceptance } from "../src/commands/e2e-state-correction-acceptance.js";
import {
  E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION,
  E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
  E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION,
  reconcileStateCorrectionIndependentEvidence,
  type StateCorrectionIndependentAuthority,
  type StateCorrectionIndependentSourcePaths,
} from "../src/commands/e2e-state-correction-reconciliation.js";

const sha256 = (value: string | Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

type JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly JsonValue[]
  | { readonly [key: string]: JsonValue };

const stableJson = (value: JsonValue): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, child]) => `${JSON.stringify(key)}:${stableJson(child)}`)
    .join(",")}}`;
};

const hash = (index: number): string => index.toString(16).padStart(64, "0");

const writeJson = async (path: string, value: unknown): Promise<string> => {
  const content = JSON.stringify(value);
  await writeFile(path, content, "utf8");
  return sha256(content);
};

const makeFixture = async ({
  omitL1Observation = false,
  rejectAtIndependentAuthority = false,
  economicsChange = {},
}: {
  readonly omitL1Observation?: boolean;
  readonly rejectAtIndependentAuthority?: boolean;
  /** Fields of the family's final economics entry to replace. */
  readonly economicsChange?: Readonly<Record<string, string>>;
} = {}): Promise<{
  readonly expectedRunId: string;
  readonly claim: E2EStateCorrectionAcceptance;
  readonly paths: StateCorrectionIndependentSourcePaths;
  readonly authority: StateCorrectionIndependentAuthority;
}> => {
  const root = await mkdtemp(join(tmpdir(), "midgard-q57-independent-"));
  const runId = "q57-independent-rehearsal";
  const hubOraclePolicyId = hash(1).slice(-56);
  const catalogue = { root: hash(2), categories: {} };
  const parameters = { maxTxSize: 16_384, network: "Preprod" };
  const blueprintBytes = Buffer.from("independent-blueprint");
  const blueprintSha256 = sha256(blueprintBytes);
  const parametersSha256 = computeDeploymentManifestJsonDigest(parameters);
  const headerHash = "ab".repeat(28);
  const initTxHash = hash(10);
  const stepTxHash = hash(11);
  const proofTokenTxHash = hash(12);
  const removalTxHash = hash(13);
  const chainPoint = { slot: "123456", blockHash: hash(14) };
  const family = {
    familyId: "doubleSpend",
    violationId: "double-spend-q57",
    headerHash,
    routeId: "double-spend-production-route",
    detectionSource: "public-l1-da" as const,
    watcherDriven: true as const,
    initTxHash,
    proofStepTxHashes: [stepTxHash],
    proofTokenTxHash,
    removalTxHash,
    correctionTxHash: removalTxHash,
    permanentProofTokenRetained: true as const,
    stateQueueNodeRemoved: true as const,
    correctedQueueObserved: true as const,
    expectedSlashLovelace: "5000000",
    observedSlashLovelace: "5000000",
    expectedProverRewardLovelace: "1000000",
    observedProverRewardLovelace: "1000000",
    chainPoint,
    finalStateRoot: hash(15),
  };
  const withdrawalReservePayout = {
    withdrawalOrderTxHash: hash(20),
    reserveTxHash: hash(21),
    payoutInitTxHash: hash(22),
    payoutAddTxHashes: [hash(23)],
    payoutConcludeTxHash: hash(24),
    expectedDestination: "addr_test1qindependent",
    observedDestination: "addr_test1qindependent",
    expectedPayoutValueSha256: hash(25),
    observedPayoutValueSha256: hash(25),
    expectedReserveValueSha256: hash(26),
    observedReserveValueSha256: hash(26),
    reserveAccountingExact: true as const,
    finalStatus: "paid" as const,
    chainPoint: { slot: "123457", blockHash: hash(27) },
  };
  const forcedClassifications = [
    {
      direction: "valid-marked-invalid" as const,
      operatorClassification: "invalid" as const,
      canonicalClassification: "valid" as const,
      finalClassification: "valid" as const,
      detectionSource: "public-l1-da" as const,
      watcherDriven: true as const,
      routeId: "restore-valid",
      evidenceTxHash: hash(30),
      correctionTxHash: hash(31),
      corrected: true as const,
      chainPoint: { slot: "123458", blockHash: hash(32) },
    },
    {
      direction: "invalid-marked-valid" as const,
      operatorClassification: "valid" as const,
      canonicalClassification: "invalid" as const,
      finalClassification: "invalid" as const,
      detectionSource: "public-l1-da" as const,
      watcherDriven: true as const,
      routeId: "correct-invalid",
      evidenceTxHash: hash(33),
      correctionTxHash: hash(34),
      corrected: true as const,
      chainPoint: { slot: "123459", blockHash: hash(35) },
    },
  ];
  const stateQueue = { depth: 0, fraudulentHeaderHashes: [] };
  const jobs = { pendingFinalizations: 0 };
  const watcher = {
    readiness: "ready",
    verification: "resumed_after_reconciliation",
  };
  const economics = [
    {
      familyId: family.familyId,
      removalTxHash,
      proofTokenUnit: "aa".repeat(28),
      proofTokenOutRef: `${proofTokenTxHash}#0`,
      removalReferencedProofTokenOutRef: `${proofTokenTxHash}#0`,
      proofTokenFinalState: "retained",
      operatorCredential: "11".repeat(28),
      proverCredential: "22".repeat(28),
      operatorBondInputOutRef: `${hash(92)}#0`,
      operatorBondInputLovelace: "5000000",
      proverRewardOutputOutRef: `${removalTxHash}#0`,
      removalFeeLovelace: "200000",
      slashedLovelace: family.expectedSlashLovelace,
      proverRewardLovelace: family.expectedProverRewardLovelace,
      duplicateRewardCount: 0,
      ...economicsChange,
    },
  ];
  const finalWithdrawalReservePayout = {
    withdrawalOrderTxHash: withdrawalReservePayout.withdrawalOrderTxHash,
    reserveTxHash: withdrawalReservePayout.reserveTxHash,
    payoutInitTxHash: withdrawalReservePayout.payoutInitTxHash,
    payoutAddTxHashes: withdrawalReservePayout.payoutAddTxHashes,
    payoutConcludeTxHash: withdrawalReservePayout.payoutConcludeTxHash,
    destination: withdrawalReservePayout.expectedDestination,
    payoutValueSha256: withdrawalReservePayout.expectedPayoutValueSha256,
    reserveValueSha256: withdrawalReservePayout.expectedReserveValueSha256,
    status: "paid",
  };
  const finalForcedClassifications = forcedClassifications.map((drill) => ({
    direction: drill.direction,
    evidenceTxHash: drill.evidenceTxHash,
    correctionTxHash: drill.correctionTxHash,
    canonicalClassification: drill.canonicalClassification,
    finalClassification: drill.finalClassification,
  }));
  const kupoStateQueueResponsePath = join(
    root,
    "raw-final-kupo-state-queue.json",
  );
  const kupoStateQueueResponseSha256 = await writeJson(
    kupoStateQueueResponsePath,
    [],
  );
  const kupoProofTokenResponsePath = join(
    root,
    "raw-final-kupo-proof-token.json",
  );
  const kupoProofTokenResponseSha256 = await writeJson(
    kupoProofTokenResponsePath,
    [
      {
        transaction_id: proofTokenTxHash,
        output_index: 0,
        created_at: { slot_no: 123_450, header_hash: hash(62) },
        spent_at: null,
        value: { coins: "2000000", assets: { ["aa".repeat(28)]: "1" } },
      },
    ],
  );
  const finalOgmiosTipPath = join(root, "raw-final-ogmios-tip.json");
  const finalOgmiosTipSha256 = await writeJson(finalOgmiosTipPath, {
    id: hash(40),
    slot: 123_500,
    height: 102,
  });
  const nodeDatabaseExportPath = join(root, "raw-final-node-db.json");
  const nodeDatabaseExportSha256 = await writeJson(nodeDatabaseExportPath, {
    schemaVersion: "midgard-e2e-state-correction-node-db-export-v1",
    runId,
    hubOraclePolicyId,
    stateQueue,
    jobs,
    watcher,
    economics,
    withdrawalReservePayout: finalWithdrawalReservePayout,
    forcedClassifications: finalForcedClassifications,
  });
  const finalSnapshot = {
    schemaVersion: E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
    runId,
    network: "Preprod",
    hubOraclePolicyId,
    observedAt: {
      slot: "123500",
      blockHash: hash(40),
      confirmationDepth: 3,
    },
    authentication: {
      source: "local-kupmios-ogmios-and-node-db",
      kupoStateQueueResponsePath,
      kupoStateQueueResponseSha256,
      kupoProofTokenResponses: [
        {
          unit: "aa".repeat(28),
          outRef: `${proofTokenTxHash}#0`,
          responsePath: kupoProofTokenResponsePath,
          responseSha256: kupoProofTokenResponseSha256,
        },
      ],
      ogmiosTipResponsePath: finalOgmiosTipPath,
      ogmiosTipResponseSha256: finalOgmiosTipSha256,
      nodeDatabaseExportPath,
      nodeDatabaseExportSha256,
    },
    stateQueue,
    jobs,
    watcher,
    economics,
    withdrawalReservePayout: finalWithdrawalReservePayout,
    forcedClassifications: finalForcedClassifications,
  };

  const recoveryPath = join(root, "recovery.json");
  const recovery = {
    schemaVersion: E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION,
    runId,
    hubOraclePolicyId,
    id: "crash-before-detect",
    beforeJournalSha256: hash(50),
    afterJournalSha256: hash(51),
    duplicateSubmissionCount: 0,
    lostEvidenceCount: 0,
    verifiedBeforeReconciliationCount: 0,
    unrecoverableWorkflowCount: 0,
    manualRepairCount: 0,
    terminalState: "recovered",
    watcherState: "ready_after_reconciliation",
  };
  const recoveryDigest = await writeJson(recoveryPath, recovery);
  const claim: E2EStateCorrectionAcceptance = {
    schemaVersion: "midgard-e2e-state-correction-acceptance-v1",
    runId,
    network: "Preprod",
    deployment: {
      hubOraclePolicyId,
      blueprintSha256,
      catalogueRoot: catalogue.root,
      parametersSha256,
    },
    families: [family],
    withdrawalReservePayout,
    forcedClassifications,
    recoveryDrills: [
      {
        id: recovery.id,
        status: "recovered",
        failClosed: true,
        duplicateSubmissions: 0,
        lostEvidence: 0,
        falseVerifiedStates: 0,
        unrecoverableWorkflows: 0,
        manualRepair: false,
        watcherReadyAfterRecovery: true,
        evidenceSha256: recoveryDigest,
      },
    ],
    finalState: {
      stateQueueDepth: 0,
      pendingFinalizations: 0,
      watcherReady: true,
      watcherVerificationResumed: true,
      exactEconomicReconciliation: true,
      finalStateSha256: sha256(stableJson(finalSnapshot as JsonValue)),
    },
  };

  const manifestPath = join(root, "manifest.json");
  await writeJson(manifestPath, {
    network: "Preprod",
    contracts: {
      hubOracleMint: { scriptHash: hubOraclePolicyId },
      fraudProofCatalogueMint: { fraudProofCatalogue: catalogue },
    },
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: parametersSha256,
    },
    artifacts: {
      blueprintHash: blueprintSha256,
    },
  });
  const blueprintPath = join(root, "plutus.json");
  await writeFile(blueprintPath, blueprintBytes);
  const cataloguePath = join(root, "catalogue.json");
  await writeJson(cataloguePath, catalogue);
  const parametersPath = join(root, "parameters.json");
  await writeJson(parametersPath, parameters);
  const finalSnapshotPath = join(root, "final-snapshot.json");
  await writeJson(finalSnapshotPath, finalSnapshot);

  const requiredTxHashes = new Set([
    initTxHash,
    stepTxHash,
    proofTokenTxHash,
    removalTxHash,
    withdrawalReservePayout.withdrawalOrderTxHash,
    withdrawalReservePayout.reserveTxHash,
    withdrawalReservePayout.payoutInitTxHash,
    ...withdrawalReservePayout.payoutAddTxHashes,
    withdrawalReservePayout.payoutConcludeTxHash,
    ...forcedClassifications.flatMap((drill) => [
      drill.evidenceTxHash,
      drill.correctionTxHash,
    ]),
  ]);
  const l1ObservationPaths: string[] = [];
  for (const [index, txHash] of [...requiredTxHashes].entries()) {
    if (omitL1Observation && index === 0) continue;
    const terminalChainPoint =
      txHash === removalTxHash
        ? chainPoint
        : txHash === withdrawalReservePayout.payoutConcludeTxHash
          ? withdrawalReservePayout.chainPoint
          : forcedClassifications.find(
              (drill) => drill.correctionTxHash === txHash,
            )?.chainPoint;
    const inclusionBlockHash = hash(60 + index);
    const rawKupoPath = join(root, `raw-kupo-${index.toString()}.json`);
    const rawKupoSha256 = await writeJson(rawKupoPath, [
      {
        transaction_id: txHash,
        output_index: 0,
        created_at: {
          slot_no: 123_450,
          header_hash: inclusionBlockHash,
        },
        spent_at: null,
        value: { coins: "2000000", assets: {} },
      },
    ]);
    const rawOgmiosBlockPath = join(
      root,
      `raw-ogmios-block-${index.toString()}.json`,
    );
    const rawOgmiosBlockSha256 = await writeJson(rawOgmiosBlockPath, {
      result: {
        direction: "forward",
        block: {
          id: inclusionBlockHash,
          slot: 123_450,
          height: 100,
          transactions: [{ id: txHash }],
        },
      },
    });
    const rawOgmiosTipPath = join(
      root,
      `raw-ogmios-tip-${index.toString()}.json`,
    );
    const rawOgmiosTipSha256 = await writeJson(rawOgmiosTipPath, {
      result: {
        id: terminalChainPoint?.blockHash ?? hash(40),
        slot:
          terminalChainPoint === undefined
            ? 123_500
            : Number(terminalChainPoint.slot),
        height: terminalChainPoint === undefined ? 102 : 101,
      },
    });
    const observationPath = join(root, `l1-${index.toString()}.json`);
    await writeJson(observationPath, {
      schemaVersion: E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION,
      runId,
      network: "Preprod",
      hubOraclePolicyId,
      txHash,
      includedAt: { slot: "123450", blockHash: inclusionBlockHash },
      observedAtTip:
        terminalChainPoint === undefined
          ? { slot: "123500", blockHash: hash(40), confirmationDepth: 3 }
          : { ...terminalChainPoint, confirmationDepth: 2 },
      authentication: {
        source: "local-kupmios-ogmios",
        kupoResponsePath: rawKupoPath,
        kupoResponseSha256: rawKupoSha256,
        ogmiosBlockResponsePath: rawOgmiosBlockPath,
        ogmiosBlockResponseSha256: rawOgmiosBlockSha256,
        ogmiosTipResponsePath: rawOgmiosTipPath,
        ogmiosTipResponseSha256: rawOgmiosTipSha256,
      },
    });
    l1ObservationPaths.push(observationPath);
  }

  const authority = {
    authenticateTransaction: vi.fn(async ({ txHash }) => {
      if (rejectAtIndependentAuthority || !requiredTxHashes.has(txHash)) {
        throw new Error(
          `independent live authority rejected transaction ${txHash}`,
        );
      }
    }),
    authenticateFinalState: vi.fn(async (observed) => {
      if (
        rejectAtIndependentAuthority ||
        observed.hubOraclePolicyId !== hubOraclePolicyId ||
        observed.stateQueueDepth !== 0 ||
        observed.pendingFinalizations !== 0
      ) {
        throw new Error("independent live authority rejected final state");
      }
    }),
  } satisfies StateCorrectionIndependentAuthority;

  return {
    expectedRunId: runId,
    claim,
    authority,
    paths: {
      deploymentManifestPath: manifestPath,
      blueprintPath,
      cataloguePath,
      parametersPath,
      l1ObservationPaths,
      recoveryObservationPaths: [recoveryPath],
      finalSnapshotPath,
    },
  };
};

describe("independent state-correction reconciliation", () => {
  it("derives all gates and confirmations from independent sources", async () => {
    const fixture = await makeFixture();
    const evidence = await reconcileStateCorrectionIndependentEvidence(fixture);
    expect(evidence.db).toHaveLength(6);
    expect(evidence.db.every((gate) => gate.status === "satisfied")).toBe(true);
    expect(
      evidence.transactions.every((transaction) =>
        transaction.source.startsWith("authenticated-l1:"),
      ),
    ).toBe(true);
    expect(evidence.transactions).toContainEqual(
      expect.objectContaining({
        label: "fault-proof:doubleSpend:removal",
        status: "confirmed",
      }),
    );
  });

  it("rejects a required transaction without an authenticated L1 observation", async () => {
    const fixture = await makeFixture({ omitL1Observation: true });
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(/has no authenticated L1 observation/u);
  });

  it("rejects independently consistent sources bound to another run", async () => {
    const fixture = await makeFixture();
    await expect(
      reconcileStateCorrectionIndependentEvidence({
        ...fixture,
        expectedRunId: "different-finalizer-run",
      }),
    ).rejects.toThrow(/state-correction acceptance run mismatch/u);
  });

  it("rejects an acceptance claim for another deployment", async () => {
    const fixture = await makeFixture();
    await expect(
      reconcileStateCorrectionIndependentEvidence({
        ...fixture,
        claim: {
          ...fixture.claim,
          deployment: {
            ...fixture.claim.deployment,
            hubOraclePolicyId: "ee".repeat(28),
          },
        },
      }),
    ).rejects.toThrow(/deployment hub-oracle policy id mismatch/u);
  });

  it("rejects a retained proof token that is not an output of the family's proof-token transaction", async () => {
    const fixture = await makeFixture({
      economicsChange: {
        proofTokenOutRef: `${hash(77)}#0`,
        removalReferencedProofTokenOutRef: `${hash(77)}#0`,
      },
    });
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(/doubleSpend proof-token creation mismatch/u);
  });

  it("rejects a removal that references another proof token", async () => {
    const fixture = await makeFixture({
      economicsChange: {
        removalReferencedProofTokenOutRef: `${hash(12)}#1`,
      },
    });
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(/doubleSpend removal proof-token reference mismatch/u);
  });

  it("rejects fully self-consistent forged files when the independent live authority disagrees", async () => {
    const fixture = await makeFixture({ rejectAtIndependentAuthority: true });
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(/independent live authority rejected transaction/u);
  });

  it("rejects digest-consistent Kupo and Ogmios captures that disagree", async () => {
    const fixture = await makeFixture();
    const observationPath = fixture.paths.l1ObservationPaths[0]!;
    const observation = JSON.parse(await readFile(observationPath, "utf8")) as {
      authentication: {
        ogmiosBlockResponsePath: string;
        ogmiosBlockResponseSha256: string;
      };
    };
    const contradictoryBlock = {
      result: {
        direction: "forward",
        block: {
          id: hash(999),
          slot: 123_450,
          height: 100,
          transactions: [{ id: fixture.claim.families[0]!.initTxHash }],
        },
      },
    };
    observation.authentication.ogmiosBlockResponseSha256 = await writeJson(
      observation.authentication.ogmiosBlockResponsePath,
      contradictoryBlock,
    );
    await writeJson(observationPath, observation);
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(/Kupo\/Ogmios inclusion point mismatch/u);
  });
});
