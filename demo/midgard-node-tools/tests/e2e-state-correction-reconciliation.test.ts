import { createHash } from "node:crypto";
import { mkdir, mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

vi.mock("midgard-node/deployment-manifest-v1", () => ({
  parseDeploymentManifestValue: (value: unknown) => value,
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

const makeWorkflowEntries = ({
  manifestId,
  headerHash,
  txHashes,
  removalTxHash,
  chainPoint,
  omitActionHistory,
}: {
  readonly manifestId: string;
  readonly headerHash: string;
  readonly txHashes: readonly string[];
  readonly removalTxHash: string;
  readonly chainPoint: { readonly slot: string; readonly blockHash: string };
  readonly omitActionHistory: boolean;
}): readonly FraudProofWorkflowJournalEntry[] => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    category: "doubleSpend" as const,
    target: { kind: "state_queue_header" as const, headerHash },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const terminal: FraudProofWorkflowTerminal = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category: "doubleSpend",
    headerHash,
    proofToken: {
      unit: "aa".repeat(28),
      outRef: `${txHashes[2]!}#0`,
      createdByTxHash: txHashes[2]!,
      retainedAtFinalState: true,
    },
    correction: {
      removalTxHash,
      removedStateQueueOutRef: `${hash(91)}#0`,
      fraudulentHeaderAbsent: true,
      referencedProofTokenOutRef: `${txHashes[2]!}#0`,
    },
    economics: {
      operatorCredential: "11".repeat(28),
      proverCredential: "22".repeat(28),
      operatorBondInputOutRef: `${hash(92)}#0`,
      operatorBondInputLovelace: "5000000",
      proverRewardOutputOutRef: `${removalTxHash}#0`,
      removalFeeLovelace: "200000",
      slashedLovelace: "5000000",
      proverRewardLovelace: "1000000",
      duplicateRewardAbsent: true,
    },
    observedAt: { ...chainPoint, confirmationDepth: 2 },
  };
  const events: FraudProofWorkflowJournalEntry["event"][] = [
    { kind: "started" },
    {
      kind: "prepared",
      artifact: {},
      artifactDigest: journalJsonDigest({}),
    },
  ];
  txHashes.forEach((txHash, index) => {
    const actionId = `action-${index.toString()}`;
    if (!omitActionHistory) {
      events.push(
        {
          kind: "preflight_passed",
          actionId,
          txHash,
          localEvaluator: "uplc-v1",
          referenceScripts: [
            {
              role: "family-step",
              outRef: `${hash(92)}#0`,
              scriptHash: "33".repeat(28),
            },
          ],
        },
        {
          kind: "submission_intent",
          actionId,
          actionInput: {},
          attempt: 1,
          txHash,
        },
        { kind: "submitted", actionId, attempt: 1, txHash },
        {
          kind: "reconciled",
          actionId,
          outcome: "confirmed",
          txHash,
        },
      );
    }
    events.push({ kind: "confirmed", actionId, txHash });
  });
  events.push({
    kind: "completed",
    terminal,
    terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
  });
  return events.map((event, sequence) => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: new Date(Date.UTC(2026, 7, 29, 0, 0, sequence)).toISOString(),
    event,
  }));
};

const makeFixture = async ({
  omitL1Observation = false,
  omitActionHistory = false,
  rejectAtIndependentAuthority = false,
}: {
  readonly omitL1Observation?: boolean;
  readonly omitActionHistory?: boolean;
  readonly rejectAtIndependentAuthority?: boolean;
} = {}): Promise<{
  readonly expectedRunId: string;
  readonly claim: E2EStateCorrectionAcceptance;
  readonly paths: StateCorrectionIndependentSourcePaths;
  readonly authority: StateCorrectionIndependentAuthority;
}> => {
  const root = await mkdtemp(join(tmpdir(), "midgard-q57-independent-"));
  const runId = "q57-independent-rehearsal";
  const manifestId = hash(1);
  const catalogue = { root: hash(2), categories: {} };
  const parameters = { maxTxSize: 16_384, network: "Preprod" };
  const blueprintBytes = Buffer.from("independent-blueprint");
  const releaseEvidenceBytes = Buffer.from("independent-release-evidence");
  const blueprintSha256 = sha256(blueprintBytes);
  const parametersSha256 = computeDeploymentManifestJsonDigest(parameters);
  const releaseEvidenceSha256 = sha256(releaseEvidenceBytes);
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
  const jobs = { unfinishedMutationJobs: 0, pendingFinalizations: 0 };
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
    manifestId,
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
    manifestId,
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
    manifestId,
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
      manifestId,
      blueprintSha256,
      catalogueRoot: catalogue.root,
      parametersSha256,
      releaseEvidenceSha256,
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
      unfinishedMutationJobs: 0,
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
    manifestId,
    contracts: {
      fraudProofCatalogueMint: { fraudProofCatalogue: catalogue },
    },
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: parametersSha256,
    },
    proofEvidence: {
      blueprintHash: blueprintSha256,
      digest: releaseEvidenceSha256,
    },
  });
  const blueprintPath = join(root, "plutus.json");
  await writeFile(blueprintPath, blueprintBytes);
  const cataloguePath = join(root, "catalogue.json");
  await writeJson(cataloguePath, catalogue);
  const parametersPath = join(root, "parameters.json");
  await writeJson(parametersPath, parameters);
  const releaseEvidencePath = join(root, "release.json");
  await writeFile(releaseEvidencePath, releaseEvidenceBytes);
  const finalSnapshotPath = join(root, "final-snapshot.json");
  await writeJson(finalSnapshotPath, finalSnapshot);

  const workflowDirectory = join(root, "workflow");
  await mkdir(workflowDirectory);
  const workflowEntries = makeWorkflowEntries({
    manifestId,
    headerHash,
    txHashes: [initTxHash, stepTxHash, proofTokenTxHash, removalTxHash],
    removalTxHash,
    chainPoint,
    omitActionHistory,
  });
  await Promise.all(
    workflowEntries.map((entry, index) =>
      writeJson(
        join(workflowDirectory, `${index.toString().padStart(8, "0")}.json`),
        entry,
      ),
    ),
  );

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
      manifestId,
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
        observed.manifestId !== manifestId ||
        observed.stateQueueDepth !== 0 ||
        observed.unfinishedMutationJobs !== 0 ||
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
      releaseEvidencePath,
      workflowJournalDirectories: [workflowDirectory],
      l1ObservationPaths,
      recoveryObservationPaths: [recoveryPath],
      finalSnapshotPath,
    },
  };
};

describe("Q57 independent state-correction reconciliation", () => {
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

  it("rejects a terminal journal assembled without authenticated action history", async () => {
    const fixture = await makeFixture({ omitActionHistory: true });
    await expect(
      reconcileStateCorrectionIndependentEvidence(fixture),
    ).rejects.toThrow(
      /(?:lacks matching authenticated reconciliation|confirmation must follow matching confirmed reconciliation)/u,
    );
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
