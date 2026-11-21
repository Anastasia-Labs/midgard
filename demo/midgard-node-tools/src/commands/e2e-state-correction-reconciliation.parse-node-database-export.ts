import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { E2EStateCorrectionAcceptance } from "./e2e-state-correction-acceptance.js";
import {
  E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
  exactKeys,
  exactString,
  type NodeDatabaseExport,
  record,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";
import { parseFinalSnapshot } from "./e2e-state-correction-reconciliation.parse-final-snapshot.js";

export const parseNodeDatabaseExport = (value: unknown): NodeDatabaseExport => {
  const field = "raw node database export";
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "manifestId",
      "stateQueue",
      "jobs",
      "watcher",
      "economics",
      "withdrawalReservePayout",
      "forcedClassifications",
    ],
    field,
  );
  exactString(
    candidate.schemaVersion,
    "midgard-e2e-state-correction-node-db-export-v1",
    `${field}.schemaVersion`,
  );
  const parsed = parseFinalSnapshot({
    schemaVersion: E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
    runId: candidate.runId,
    network: "Preprod",
    manifestId: candidate.manifestId,
    observedAt: {
      slot: "0",
      blockHash: "00".repeat(32),
      confirmationDepth: 1,
    },
    authentication: {
      source: "local-kupmios-ogmios-and-node-db",
      kupoStateQueueResponsePath: "not-used",
      kupoStateQueueResponseSha256: "00".repeat(32),
      kupoProofTokenResponses: [],
      ogmiosTipResponsePath: "not-used",
      ogmiosTipResponseSha256: "00".repeat(32),
      nodeDatabaseExportPath: "not-used",
      nodeDatabaseExportSha256: "00".repeat(32),
    },
    stateQueue: candidate.stateQueue,
    jobs: candidate.jobs,
    watcher: candidate.watcher,
    economics: candidate.economics,
    withdrawalReservePayout: candidate.withdrawalReservePayout,
    forcedClassifications: candidate.forcedClassifications,
  });
  return {
    schemaVersion: "midgard-e2e-state-correction-node-db-export-v1",
    runId: parsed.runId,
    manifestId: parsed.manifestId,
    stateQueue: parsed.stateQueue,
    jobs: parsed.jobs,
    watcher: parsed.watcher,
    economics: parsed.economics,
    withdrawalReservePayout: parsed.withdrawalReservePayout,
    forcedClassifications: parsed.forcedClassifications,
  };
};

export const manifestCatalogue = (
  manifest: DeploymentManifest,
): NonNullable<
  DeploymentManifest["contracts"][string]["fraudProofCatalogue"]
> => {
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error("deployment manifest has no fraud-proof catalogue");
  }
  return catalogue;
};

type RequiredTransaction = {
  readonly label: string;
  readonly txHash: string;
};

export const requiredTransactions = (
  claim: E2EStateCorrectionAcceptance,
): readonly RequiredTransaction[] => [
  ...claim.families.flatMap((family) => [
    { label: `fault-proof:${family.familyId}:init`, txHash: family.initTxHash },
    ...family.proofStepTxHashes.map((txHash, index) => ({
      label: `fault-proof:${family.familyId}:step-${(index + 1).toString()}`,
      txHash,
    })),
    {
      label: `fault-proof:${family.familyId}:proof-token`,
      txHash: family.proofTokenTxHash,
    },
    {
      label: `fault-proof:${family.familyId}:removal`,
      txHash: family.removalTxHash,
    },
    {
      label: `fault-proof:${family.familyId}:correction`,
      txHash: family.correctionTxHash,
    },
  ]),
  {
    label: "withdrawal-order",
    txHash: claim.withdrawalReservePayout.withdrawalOrderTxHash,
  },
  {
    label: "withdrawal-reserve",
    txHash: claim.withdrawalReservePayout.reserveTxHash,
  },
  {
    label: "payout-init",
    txHash: claim.withdrawalReservePayout.payoutInitTxHash,
  },
  ...claim.withdrawalReservePayout.payoutAddTxHashes.map((txHash, index) => ({
    label: `payout-add-${(index + 1).toString()}`,
    txHash,
  })),
  {
    label: "payout-conclude",
    txHash: claim.withdrawalReservePayout.payoutConcludeTxHash,
  },
  ...claim.forcedClassifications.flatMap((drill) => [
    {
      label: `forced-classification:${drill.direction}:evidence`,
      txHash: drill.evidenceTxHash,
    },
    {
      label: `forced-classification:${drill.direction}:correction`,
      txHash: drill.correctionTxHash,
    },
  ]),
];
