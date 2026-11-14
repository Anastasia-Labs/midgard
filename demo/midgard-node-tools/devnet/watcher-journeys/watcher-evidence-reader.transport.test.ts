import { chmod, mkdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { isTransientTransportError } from "./transport-failure.js";
import { composeWatcherEvidenceExec } from "./watcher-evidence-reader.js";
import {
  appendWorkflow,
  directories,
  recordedWatcher,
} from "./watcher-evidence-reader.test-support.js";

afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(() => {
  vi.unstubAllEnvs();
});

describe("a watcher evidence read that fails", () => {
  it("is a read that got no answer when the journal it reads is busy", async () => {
    const watcher = await recordedWatcher();
    await appendWorkflow(watcher.storePath, "doubleSpend", "12".repeat(28), 1);
    // The watcher holds its store locked while it writes.
    const writer = new DatabaseSync(watcher.storePath);
    try {
      writer.exec("PRAGMA locking_mode=EXCLUSIVE");
      writer.exec("BEGIN EXCLUSIVE");
      const failure = await watcher
        .reader()
        .readWorkflowRows("doubleSpend")
        .catch((error: unknown) => error);
      expect(failure).toBeInstanceOf(Error);
      expect((failure as Error).message).toMatch(/database is locked/u);
      expect(isTransientTransportError(failure)).toBe(true);
    } finally {
      writer.exec("ROLLBACK");
      writer.close();
    }
  });

  it("is a named failure, never one to read through, when the reader answers what went wrong", async () => {
    const watcher = await recordedWatcher();
    await writeFile(watcher.storePath, "not a database");
    const failure = await watcher
      .reader()
      .readWorkflowRows("doubleSpend")
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect((failure as Error).message).toMatch(/file is not a database/u);
    expect(isTransientTransportError(failure)).toBe(false);
  });

  it("is a read that got no answer when the Docker CLI exits without one", async () => {
    const watcher = await recordedWatcher();
    const bin = join(watcher.root, "bin");
    await mkdir(bin);
    // A Docker daemon that cannot exec into the container for a moment.
    await writeFile(
      join(bin, "docker"),
      `#!${process.execPath}
console.error("Error response from daemon: container is restarting, wait until the container is running");
process.exit(1);
`,
    );
    await chmod(join(bin, "docker"), 0o755);
    vi.stubEnv("PATH", `${bin}:${process.env.PATH}`);
    const failure = await watcher
      .reader(composeWatcherEvidenceExec("midgard-node-drill"))
      .readDecisions()
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect((failure as Error).message).toMatch(
      /midgard-node-drill: Error response from daemon: container is restarting/u,
    );
    expect(isTransientTransportError(failure)).toBe(true);
  });
});
