import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "@al-ft/midgard-fault-proofs";

import type { E2EStateCorrectionAcceptance } from "./e2e-state-correction-acceptance.js";
import {
  E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
  exactKeys,
  exactString,
  type LoadedWorkflow,
  type NodeDatabaseExport,
  record,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";
import { parseFinalSnapshot } from "./e2e-state-correction-reconciliation.parse-final-snapshot.js";
import { sha256 } from "./e2e-state-correction-reconciliation.parse-kupo-matches.js";

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

const workflowDigest = (
  names: readonly string[],
  contents: readonly string[],
): string =>
  sha256(
    names
      .map((name, index) => `${name}:${sha256(contents[index] ?? "")}`)
      .join("\n"),
  );

export const loadWorkflow = async (
  directory: string,
): Promise<LoadedWorkflow> => {
  const names = (await readdir(directory))
    .filter((name) => /^\d{8}\.json$/u.test(name))
    .sort();
  if (names.length === 0) {
    throw new Error(`workflow journal ${directory} has no immutable entries`);
  }
  names.forEach((name, index) => {
    const expected = `${index.toString().padStart(8, "0")}.json`;
    if (name !== expected) {
      throw new Error(
        `workflow journal ${directory} entry gap: expected ${expected}, found ${name}`,
      );
    }
  });
  const entryPaths = names.map((name) => join(directory, name));
  const contents = await Promise.all(
    entryPaths.map((path) => readFile(path, "utf8")),
  );
  const entries = contents.map(
    (content) => JSON.parse(content) as FraudProofWorkflowJournalEntry,
  );
  const workflowId = entries[0]?.workflowId;
  if (workflowId === undefined)
    throw new Error(`workflow journal ${directory} is empty`);
  validateFraudProofWorkflowJournal({ workflowId, entries });
  const completed = entries.filter((entry) => entry.event.kind === "completed");
  const last = entries.at(-1);
  if (
    completed.length !== 1 ||
    last?.event.kind !== "completed" ||
    completed[0]?.event.kind !== "completed"
  ) {
    throw new Error(
      `workflow journal ${directory} must have exactly one terminal completed event and it must be last`,
    );
  }
  const terminalEvent = completed[0].event;
  const terminalDigest = journalJsonDigest(
    normalizeJournalJson(
      terminalEvent.terminal,
      "acceptance workflow terminal",
    ),
  );
  if (terminalDigest !== terminalEvent.terminalDigest) {
    throw new Error(`workflow journal ${directory} terminal digest mismatch`);
  }
  for (const entry of entries) {
    if (
      entry.event.kind === "prepared" &&
      journalJsonDigest(entry.event.artifact) !== entry.event.artifactDigest
    ) {
      throw new Error(`workflow journal ${directory} prepared digest mismatch`);
    }
  }
  const preflightTxByAction = new Map<string, string>();
  const intentTxByAction = new Map<string, string>();
  const observedTxByAction = new Map<string, string>();
  const confirmedActions = new Set<string>();
  for (const entry of entries) {
    const event = entry.event;
    if (event.kind === "preflight_passed") {
      preflightTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "submission_intent") {
      if (preflightTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} intent ${event.actionId} is not bound to its passed preflight body`,
        );
      }
      intentTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "submitted") {
      if (intentTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} submitted ${event.actionId} without a matching durable intent`,
        );
      }
      observedTxByAction.set(event.actionId, event.txHash);
    } else if (
      event.kind === "reconciled" &&
      event.outcome === "confirmed" &&
      event.txHash !== undefined
    ) {
      if (intentTxByAction.get(event.actionId) !== event.txHash) {
        throw new Error(
          `workflow journal ${directory} reconciled ${event.actionId} without a matching durable intent`,
        );
      }
      observedTxByAction.set(event.actionId, event.txHash);
    } else if (event.kind === "confirmed") {
      if (
        confirmedActions.has(event.actionId) ||
        observedTxByAction.get(event.actionId) !== event.txHash
      ) {
        throw new Error(
          `workflow journal ${directory} confirmed ${event.actionId} without one matching submitted/reconciled transaction`,
        );
      }
      confirmedActions.add(event.actionId);
    }
  }
  const confirmedTxHashes = new Set(
    entries.flatMap((entry) =>
      entry.event.kind === "confirmed" ? [entry.event.txHash] : [],
    ),
  );
  return {
    directory,
    digest: workflowDigest(names, contents),
    entries,
    terminal: terminalEvent.terminal,
    confirmedTxHashes,
    entryPaths,
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
