import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import type { StackConfig } from "../src/full-stack/config.js";
import {
  readJsonIfPresent,
  writeDurableJson,
} from "../src/full-stack/journal.js";
import { StackProcesses } from "../src/full-stack/process.js";
import {
  assertPreservedStorage,
  establishStorageIdentity,
  storageIdentityStep,
} from "../src/full-stack/storage.js";

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});
async function fixture() {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-storage-"));
  directories.push(directory);
  const marker = {
    runId: "de615069-9923-4006-9f83-8945969206d3",
    manifestId: "a".repeat(64),
  };
  // Only paths and the SQL transport are used by storage reconciliation.
  const config = {
    nodeRoot: directory,
    runDirectory: join(directory, "run"),
  } as StackConfig;
  const processes = new StackProcesses(config, {
    POSTGRES_USER: "test",
    POSTGRES_DB: "test",
  });
  await writeDurableJson(join(config.runDirectory, "stack-journal.json"), {
    schemaVersion: "midgard-full-stack-v1",
    runId: marker.runId,
    intentDigest: "b".repeat(64),
    steps: {},
  });
  await writeDurableJson(
    join(directory, "deploymentInfo/contract-deployment-info.json"),
    { manifestId: marker.manifestId },
  );
  const markerPath = join(directory, "db/full-stack-identity.json");
  return { marker, markerPath, processes };
}
it("recovers a storage marker committed before the file checkpoint without changing the deployment", async () => {
  const value = await fixture();
  value.processes.compose = async (_id, args) =>
    args.at(-1)!.includes("to_regclass")
      ? { exists: true }
      : { run_id: value.marker.runId, manifest_id: value.marker.manifestId };
  const recovered = await storageIdentityStep(value.processes).reconcile({
    status: "running",
    attempts: 1,
    data: null,
  });
  expect(recovered.status).toBe("complete");
  expect(await readJsonIfPresent(value.markerPath)).toEqual(value.marker);
});
it("refuses an existing Postgres identity without writing a misleading local marker", async () => {
  const value = await fixture();
  value.processes.compose = async () => ({
    run_id: "another-run",
    manifest_id: "c".repeat(64),
  });
  await expect(establishStorageIdentity(value.processes)).rejects.toThrow(
    "Storage identity differs",
  );
  expect(await readJsonIfPresent(value.markerPath)).toBeUndefined();
});
it("refuses lost durable storage on attachment instead of creating a new store", async () => {
  const value = await fixture();
  await writeDurableJson(value.markerPath, value.marker);
  value.processes.compose = async () => undefined;
  await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
    "Postgres deployment marker is missing",
  );
  expect(await readJsonIfPresent(value.markerPath)).toEqual(value.marker);
});
