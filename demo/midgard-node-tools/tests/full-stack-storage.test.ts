import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import type { StackConfig } from "../src/full-stack/config.js";
import {
  readJsonIfPresent,
  writeDurableJson,
} from "../src/full-stack/journal.js";
import { StackProcesses } from "../src/full-stack/process.js";
import {
  assertPreservedStorage,
  ATTACHMENT_QUERY,
  createIdentityQuery,
  establishStorageIdentity,
  FRESH_STORAGE_QUERY,
  IDENTITY_ROW_QUERY,
  IDENTITY_TABLE_QUERY,
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
  const queries: string[] = [];
  /** Answers exactly the listed SQL, as psql's JSON would; any other query fails the test. */
  const answer = (responses: Record<string, unknown>) => {
    processes.compose = async (_id, args) => {
      const query = args.at(-1)!;
      queries.push(query);
      if (!(query in responses)) throw new Error(`Unexpected SQL: ${query}`);
      const response = responses[query];
      if (response instanceof Error) throw response;
      return response;
    };
  };
  return { answer, directory, marker, markerPath, processes, queries };
}
const identityRow = (marker: { runId: string; manifestId: string }) => ({
  run_id: marker.runId,
  manifest_id: marker.manifestId,
});
it("recovers a storage marker committed before the file checkpoint without changing the deployment", async () => {
  const value = await fixture();
  value.answer({
    [IDENTITY_TABLE_QUERY]: { exists: true },
    [IDENTITY_ROW_QUERY]: identityRow(value.marker),
  });
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
  value.answer({
    [createIdentityQuery(value.marker)]: {
      run_id: "another-run",
      manifest_id: "c".repeat(64),
    },
  });
  await expect(establishStorageIdentity(value.processes)).rejects.toThrow(
    "Storage identity differs",
  );
  expect(await readJsonIfPresent(value.markerPath)).toBeUndefined();
});
it("refuses lost durable storage on attachment instead of creating a new store", async () => {
  const value = await fixture();
  await writeDurableJson(value.markerPath, value.marker);
  value.answer({ [IDENTITY_TABLE_QUERY]: { exists: false } });
  await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
    "Postgres deployment marker is missing",
  );
  expect(value.queries).toEqual([IDENTITY_TABLE_QUERY]);
  expect(await readJsonIfPresent(value.markerPath)).toEqual(value.marker);
});
it("refuses an emptied identity table", async () => {
  const value = await fixture();
  await writeDurableJson(value.markerPath, value.marker);
  value.answer({
    [IDENTITY_TABLE_QUERY]: { exists: true },
    [IDENTITY_ROW_QUERY]: null,
  });
  await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
    "Postgres deployment marker is missing",
  );
});
it("reports a failed storage query rather than a missing store", async () => {
  const value = await fixture();
  await writeDurableJson(value.markerPath, value.marker);
  value.answer({
    [IDENTITY_TABLE_QUERY]: new Error("storage-identity failed; inspect log"),
  });
  await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
    "storage-identity failed",
  );
});
const INIT_TX = "ab".repeat(32);

describe("fresh storage", () => {
  const journal = (value: Awaited<ReturnType<typeof fixture>>) =>
    writeDurableJson(join(value.directory, "run/stack-journal.json"), {
      schemaVersion: "midgard-full-stack-v1",
      runId: value.marker.runId,
      intentDigest: "b".repeat(64),
      steps: {},
    });
  async function fresh() {
    const value = await fixture();
    await rm(join(value.directory, "deploymentInfo"), { recursive: true });
    await journal(value);
    return value;
  }
  it("checks the database and the node db directory before a fresh deployment", async () => {
    const value = await fresh();
    value.answer({ [FRESH_STORAGE_QUERY]: { empty: true } });
    await expect(
      assertPreservedStorage(value.processes),
    ).resolves.toBeUndefined();
    expect(value.queries).toEqual([FRESH_STORAGE_QUERY]);
    await mkdir(join(value.directory, "db"));
    await writeFile(join(value.directory, "db/ledger"), "");
    await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
      "cannot reuse populated node db directory",
    );
  });
  it("refuses a populated database", async () => {
    const value = await fresh();
    value.answer({
      [FRESH_STORAGE_QUERY]: new Error("storage-identity failed; inspect log"),
    });
    await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
      "storage-identity failed",
    );
  });
  it("attaches an initialized deployment only to a store whose follower saw its initialization", async () => {
    const value = await fixture();
    await writeDurableJson(
      join(value.directory, "deploymentInfo/contract-deployment-info.json"),
      {
        manifestId: value.marker.manifestId,
        steps: { initProtocol: { status: "complete", txHash: INIT_TX } },
      },
    );
    value.answer({ [ATTACHMENT_QUERY]: null });
    await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
      "missing or mismatched local event history",
    );
    value.answer({ [ATTACHMENT_QUERY]: { initTxHashes: ["cd".repeat(32)] } });
    await expect(assertPreservedStorage(value.processes)).rejects.toThrow(
      "missing or mismatched local event history",
    );
    value.answer({ [ATTACHMENT_QUERY]: { initTxHashes: [INIT_TX] } });
    await expect(
      assertPreservedStorage(value.processes),
    ).resolves.toBeUndefined();
    expect(value.queries).toEqual([
      ATTACHMENT_QUERY,
      ATTACHMENT_QUERY,
      ATTACHMENT_QUERY,
    ]);
  });
});
