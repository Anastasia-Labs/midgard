import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdir, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "midgard-watcher";
import { openWatcherWorkflowStoreForTest } from "midgard-watcher/tests/support/watcher-workflow-store";

import {
  createWatcherEvidenceReader,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

/** The watcher package as its image installs it: `dist/index.js` in the working directory. */
export const watcherImage = fileURLToPath(
  new URL("../../../midgard-watcher/", import.meta.url),
);

/** Runs the reader with this machine's Node in the watcher package, as the container would. */
export const localExec =
  (cwd = watcherImage): WatcherEvidenceExec =>
  ({ script, request }) =>
    new Promise((resolve, reject) => {
      execFile(
        process.execPath,
        ["--input-type=module", "-e", script, request],
        { cwd, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
        (error, stdout, stderr) =>
          error === null ? resolve(stdout) : reject(new Error(stderr)),
      );
    });

/** Temporary roots `recordedWatcher` made; each test file removes them. */
export const directories: string[] = [];

export const FINGERPRINT = "ab".repeat(32);

/** A watcher's state as its container sees it: its process configuration and journals. */
export const recordedWatcher = async () => {
  const root = await mkdtemp(join(tmpdir(), "watcher-evidence-"));
  directories.push(root);
  const journalDirectory = join(root, "workflows");
  await mkdir(journalDirectory);
  const storePath = join(root, "watcher.sqlite");
  const processConfigPath = join(root, "watcher-process.json");
  await writeFile(
    processConfigPath,
    JSON.stringify({
      workflowJournalDirectory: journalDirectory,
      watcherConfig: { storage: { path: storePath } },
    }),
  );
  const reader = (exec: WatcherEvidenceExec = localExec()) =>
    createWatcherEvidenceReader({
      exec,
      hostProcessConfigPath: processConfigPath,
      containerProcessConfigPath: processConfigPath,
      deploymentFingerprint: FINGERPRINT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
  return { root, journalDirectory, storePath, processConfigPath, reader };
};

export const appendWorkflow = async (
  storePath: string,
  category: FraudProofCatalogueCategoryName,
  headerHash: string,
  count: number,
  fingerprint = FINGERPRINT,
) => {
  const workflows = openWatcherWorkflowStoreForTest(storePath);
  try {
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: fingerprint,
      category,
      target: { kind: "state_queue_header" as const, headerHash },
    };
    const store = workflows.forObjective({ category, headerHash });
    for (let sequence = 0; sequence < count; sequence += 1) {
      const entry: FraudProofWorkflowJournalEntry = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: computeFraudProofWorkflowId(identity),
        identity,
        sequence,
        recordedAt: "2026-10-29T00:00:00.000Z",
        event:
          sequence === 0
            ? { kind: "started" }
            : {
                kind: "prepared",
                artifact: {},
                artifactDigest: createHash("sha256").update("{}").digest("hex"),
              },
      };
      await store.append(entry, sequence);
    }
  } finally {
    workflows.close();
  }
};
