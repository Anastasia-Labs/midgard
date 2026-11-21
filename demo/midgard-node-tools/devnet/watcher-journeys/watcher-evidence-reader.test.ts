import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { chmod, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { COMPLETE_CANONICAL_REPLAY } from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
} from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "midgard-watcher";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  composeWatcherEvidenceExec,
  createWatcherEvidenceReader,
} from "./watcher-evidence-reader.js";
import {
  directories,
  FINGERPRINT,
  localExec,
  recordedWatcher,
  watcherImage,
} from "./watcher-evidence-reader.test-support.js";

afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(() => {
  vi.unstubAllEnvs();
});

/** The watcher's canonical JSON for records of strings and string lists. */
const canonical = (value: unknown): string =>
  Array.isArray(value)
    ? `[${value.map(canonical).join(",")}]`
    : typeof value === "object" && value !== null
      ? `{${Object.keys(value)
          .sort()
          .map(
            (key) =>
              `${JSON.stringify(key)}:${canonical((value as Record<string, unknown>)[key])}`,
          )
          .join(",")}}`
      : JSON.stringify(value);
const digest = (value: unknown) =>
  createHash("sha256").update(canonical(value)).digest("hex");

/** A persisted fault decision, built field by field as the watcher journals one. */
const rawFaultDecision = (headerHash: string, fingerprint = FINGERPRINT) => {
  const launchScope = [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES];
  const decision = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint: fingerprint,
    headerHash,
    authenticatedObservationDigest: "01".repeat(32),
    payloadEnvelopeSha256: "02".repeat(32),
    payloadSha256: "03".repeat(32),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: "04".repeat(32),
    launchScope,
    launchScopeDigest: digest(launchScope),
    classificationDigest: "05".repeat(32),
    decision: "fault_detected",
    category: launchScope[0],
    violationId: "violation:recorded",
    detectionId: "detection:recorded",
    position: "0",
  };
  return { ...decision, decisionDigest: digest(decision) };
};

const writeDecisions = (
  journalDirectory: string,
  decisions: readonly unknown[],
) => {
  const database = new DatabaseSync(
    join(journalDirectory, "watcher-journals.sqlite"),
  );
  try {
    database.exec(
      "CREATE TABLE IF NOT EXISTS watcher_fault_decisions (revision INTEGER, row_key TEXT, body TEXT)",
    );
    decisions.forEach((decision, index) =>
      database
        .prepare("INSERT INTO watcher_fault_decisions VALUES (?, ?, ?)")
        .run(index + 1, `row-${index}`, JSON.stringify(decision)),
    );
  } finally {
    database.close();
  }
};

describe("watcher evidence reader", () => {
  it("reads an absent journal as empty without creating it", async () => {
    const watcher = await recordedWatcher();
    const reader = watcher.reader();
    expect(await reader.readDecisions()).toEqual([]);
    expect(
      existsSync(join(watcher.journalDirectory, "watcher-journals.sqlite")),
    ).toBe(false);
  });

  it("reads decisions through the image's exported decision reader", async () => {
    const watcher = await recordedWatcher();
    const decision = rawFaultDecision("56".repeat(28));
    writeDecisions(watcher.journalDirectory, [decision]);
    expect(await watcher.reader().readDecisions()).toEqual([decision]);
  });

  it("fails on a journaled decision of another deployment", async () => {
    const watcher = await recordedWatcher();
    writeDecisions(watcher.journalDirectory, [
      rawFaultDecision("56".repeat(28), "cd".repeat(32)),
    ]);
    await expect(watcher.reader().readDecisions()).rejects.toThrow(
      /decision identity is invalid/u,
    );
  });

  it("refuses an answer from a watcher running another process configuration", async () => {
    const watcher = await recordedWatcher();
    const other = join(watcher.root, "other-watcher-process.json");
    await writeFile(
      other,
      JSON.stringify({
        workflowJournalDirectory: watcher.journalDirectory,
        watcherConfig: { storage: { path: watcher.storePath } },
        operationsEndpoint: "http://127.0.0.1:7402",
      }),
    );
    const reader = createWatcherEvidenceReader({
      exec: localExec(),
      hostProcessConfigPath: watcher.processConfigPath,
      containerProcessConfigPath: other,
      deploymentFingerprint: FINGERPRINT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    await expect(reader.readDecisions()).rejects.toThrow(
      /another process configuration/u,
    );
  });

  it("refuses malformed or foreign evidence in the reader's output", async () => {
    const watcher = await recordedWatcher();
    const configSha256 = createHash("sha256")
      .update(await readFile(watcher.processConfigPath))
      .digest("hex");
    const answering = (output: string) => watcher.reader(async () => output);
    const envelope = (evidence: unknown) =>
      JSON.stringify({ processConfigSha256: configSha256, evidence });
    for (const output of [
      "",
      "not json",
      JSON.stringify({ evidence: [] }),
      JSON.stringify({ processConfigSha256: configSha256, evidence: {} }),
    ]) {
      await expect(answering(output).readDecisions()).rejects.toThrow(
        /evidence/u,
      );
    }
    for (const decision of [
      rawFaultDecision("56".repeat(28), "cd".repeat(32)),
      { ...rawFaultDecision("56".repeat(28)), decision: "forged" },
      { ...rawFaultDecision("56".repeat(28)), headerHash: 56 },
      "fault_detected",
    ])
      await expect(
        answering(envelope([decision])).readDecisions(),
      ).rejects.toThrow(/outside this deployment/u);
  });

  it("execs into the project's watcher service by project name alone", async () => {
    const watcher = await recordedWatcher();
    const bin = join(watcher.root, "bin");
    await mkdir(bin);
    // A Docker CLI that serves `compose -p <project> exec -T watcher <cmd>`
    // for one project by running the command in the watcher package.
    await writeFile(
      join(bin, "docker"),
      `#!${process.execPath}
const { spawnSync } = require("node:child_process");
const args = process.argv.slice(2);
const [compose, p, project, exec, t, service, command, ...rest] = args;
if (compose !== "compose" || p !== "-p" || exec !== "exec" || t !== "-T") {
  console.error("unsupported docker invocation " + JSON.stringify(args));
  process.exit(125);
}
if (Object.keys(process.env).some((name) => name.startsWith("COMPOSE_")) || process.cwd() !== "/") {
  console.error("compose would load another project's files");
  process.exit(125);
}
if (project !== "midgard-node-drill" || service !== "watcher") {
  console.error("service " + service + " is not running");
  process.exit(1);
}
const run = spawnSync(command === "node" ? process.execPath : command, rest, {
  cwd: ${JSON.stringify(watcherImage)},
  stdio: "inherit",
});
process.exit(run.status ?? 1);
`,
    );
    await chmod(join(bin, "docker"), 0o755);
    vi.stubEnv("PATH", `${bin}:${process.env.PATH}`);
    vi.stubEnv("COMPOSE_FILE", join(watcher.root, "compose.yaml"));
    const decision = rawFaultDecision("9a".repeat(28));
    writeDecisions(watcher.journalDirectory, [decision]);
    expect(
      await watcher
        .reader(composeWatcherEvidenceExec("midgard-node-drill"))
        .readDecisions(),
    ).toEqual([decision]);
    await expect(
      watcher
        .reader(composeWatcherEvidenceExec("another-stack"))
        .readDecisions(),
    ).rejects.toThrow(/another-stack: service watcher is not running/u);
  });
});
