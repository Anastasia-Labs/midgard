import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdir, readdir, readFile } from "node:fs/promises";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { createSqliteHistoricalNativeScriptCheckpointStore } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import {
  createWatcherFaultProofReadinessApplication,
  decodeWatcherAuthenticationKey32,
  loadWatcherSecretText,
  loadWatcherVerifiedDeploymentAuthority,
  loadWatcherWorkflowFundingProfileOverlay,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofStartupReadiness,
  type WatcherProcessConfig,
} from "midgard-watcher";

import { writeJourneyArtifact } from "./artifacts.js";

const sha256 = (bytes: Uint8Array | string) =>
  createHash("sha256").update(bytes).digest("hex");
const demoRoot = fileURLToPath(new URL("../../../", import.meta.url));

const sourceIdentity = async () => {
  const files: string[] = [fileURLToPath(import.meta.url)];
  const visit = async (directory: string): Promise<void> => {
    for (const entry of await readdir(directory, { withFileTypes: true })) {
      const path = join(directory, entry.name);
      if (entry.isDirectory()) await visit(path);
      else if (entry.isFile() && entry.name.endsWith(".ts")) files.push(path);
    }
  };
  await visit(join(demoRoot, "midgard-watcher/src"));
  await visit(join(demoRoot, "midgard-fault-proofs/src"));
  const hashes = await Promise.all(
    files.sort().map(async (path) => ({
      path: relative(demoRoot, path),
      sha256: sha256(await readFile(path)),
    })),
  );
  return { files: hashes, digest: sha256(JSON.stringify(hashes)) };
};

/** Bind every installed runner using the same production loaders as watcher startup.
 * Reference UTxOs and ledger parameters are read from the configured provider;
 * no classification, workflow execution, signing, or native replay is started. */
export const verifyJourneyWorkflowBindings = async (input: {
  directory: string;
  config: WatcherProcessConfig;
  onProgress?: (event: {
    category: string;
    outcome: "started" | "passed" | "failed";
    durationMs: number;
    error?: string;
  }) => void;
}) => {
  await mkdir(input.directory, { recursive: true });
  const startedAt = new Date().toISOString();
  const started = performance.now();
  const sources = await sourceIdentity();
  const configurationFiles = await Promise.all(
    [
      input.config.deploymentAuthorityPath,
      input.config.ruleBundlePath,
      input.config.fundingProfileBundlePath,
      input.config.watcherRuntimeConfigPath,
      input.config.faultProofInfrastructure.manifestPath,
      input.config.faultProofInfrastructure.blueprintPath,
      input.config.faultProofInfrastructure.deploymentInfoPath,
    ].map(async (path) => ({ path, sha256: sha256(await readFile(path)) })),
  );
  const authority = await loadWatcherVerifiedDeploymentAuthority({
    path: input.config.deploymentAuthorityPath,
    ruleBundlePath: input.config.ruleBundlePath,
  });
  const fundingProfileOverlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: input.config.fundingProfileBundlePath,
    deploymentIdentity: authority.deploymentIdentity,
  });
  // This production store is lazy: binding never loads or advances history.
  const checkpointStore = createSqliteHistoricalNativeScriptCheckpointStore({
    path: input.config.watcherConfig.storage.path,
    rollbackAuthenticationKey: decodeWatcherAuthenticationKey32(
      await loadWatcherSecretText(
        input.config.watcherConfig.storage.rollbackAuthorityKeySource,
      ),
    ),
  });
  const application = createWatcherFaultProofReadinessApplication({
    deploymentAuthority: authority,
    infrastructure: input.config.faultProofInfrastructure,
    historicalNativeScriptCheckpointStore: checkpointStore,
    fundingProfileOverlay,
  });
  const receipts: {
    category: string;
    outcome: "passed" | "failed";
    durationMs: number;
    readiness?: WatcherFaultProofStartupReadiness;
    error?: string;
  }[] = [];
  const persist = async (
    outcome: "running" | "passed" | "failed",
    error?: string,
  ) => {
    const record = {
      schemaVersion: "midgard-workflow-binding-preflight-v1",
      startedAt,
      finishedAt: new Date().toISOString(),
      durationMs: performance.now() - started,
      deploymentFingerprint: authority.deploymentIdentity.manifestId,
      configurationFiles,
      sourceIdentity: sources,
      installedCategories: application.installedCategories,
      outcome,
      receipts,
      ...(error === undefined ? {} : { error }),
    };
    await writeJourneyArtifact(
      join(input.directory, "workflow-binding-preflight.json"),
      record,
    );
    return record;
  };
  try {
    assert.deepEqual(Object.keys(application).sort(), [
      "assertStartupReady",
      "close",
      "installedCategories",
    ]);
    assert.deepEqual(
      application.installedCategories,
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
      "Readiness application does not cover the complete installed catalogue",
    );
    await persist("running");
    for (const category of application.installedCategories) {
      const categoryStarted = performance.now();
      input.onProgress?.({ category, outcome: "started", durationMs: 0 });
      try {
        const journalDirectory = join(input.directory, "bindings", category);
        await mkdir(journalDirectory, { recursive: true });
        const readiness = await application.assertStartupReady({
          mode: "resume",
          category,
          deploymentFingerprint: authority.deploymentIdentity.manifestId,
          headerHash: WATCHER_STARTUP_READINESS_HEADER_HASH,
          journalDirectory,
          runtimeConfigPath: input.config.watcherRuntimeConfigPath,
        });
        assert.equal(readiness.category, category);
        assert.equal(
          readiness.deploymentFingerprint,
          authority.deploymentIdentity.manifestId,
        );
        receipts.push({
          category,
          outcome: "passed",
          durationMs: performance.now() - categoryStarted,
          readiness,
        });
      } catch (cause) {
        receipts.push({
          category,
          outcome: "failed",
          durationMs: performance.now() - categoryStarted,
          error: cause instanceof Error ? cause.message : String(cause),
        });
      }
      const latest = receipts.at(-1)!;
      input.onProgress?.({
        category: latest.category,
        outcome: latest.outcome,
        durationMs: latest.durationMs,
        ...(latest.error === undefined ? {} : { error: latest.error }),
      });
      await persist("running");
    }
    const failed = receipts.filter((row) => row.outcome === "failed");
    assert.equal(
      (await sourceIdentity()).digest,
      sources.digest,
      "Workflow source changed during binding verification",
    );
    if (failed.length !== 0)
      throw new Error(
        failed.map((row) => `${row.category}: ${row.error}`).join("\n"),
      );
    return await persist("passed");
  } catch (cause) {
    await persist(
      "failed",
      cause instanceof Error ? cause.message : String(cause),
    );
    throw cause;
  } finally {
    await application.close();
  }
};
