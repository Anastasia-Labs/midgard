import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import {
  chmod,
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { fileURLToPath } from "node:url";

import {
  COMPLETE_CANONICAL_REPLAY,
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "midgard-watcher";
import { openWatcherWorkflowStoreForTest } from "midgard-watcher/tests/support/watcher-workflow-store";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { captureJourneyWorkflowBaseline } from "./journey-session.js";
import {
  composeWatcherEvidenceExec,
  createWatcherEvidenceReader,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

/** The watcher package as its image installs it: `dist/index.js` in the working directory. */
const watcherImage = fileURLToPath(
  new URL("../../../midgard-watcher/", import.meta.url),
);

/** Runs the reader with this machine's Node in the watcher package, as the container would. */
const localExec =
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

const directories: string[] = [];
afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(() => {
  vi.unstubAllEnvs();
});

const FINGERPRINT = "ab".repeat(32);

/** A watcher's state as its container sees it: its process configuration and journals. */
const recordedWatcher = async () => {
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

const appendWorkflow = async (
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
  it("reads the same workflow rows and baseline as the journey watcher's own store", async () => {
    const watcher = await recordedWatcher();
    const reader = watcher.reader();
    expect(await reader.readWorkflowRows("doubleSpend")).toEqual(new Map());
    const first = "12".repeat(28);
    const second = "34".repeat(28);
    await appendWorkflow(watcher.storePath, "doubleSpend", first, 2);
    await appendWorkflow(watcher.storePath, "doubleSpend", second, 1);
    await appendWorkflow(watcher.storePath, "mintAuthorization", first, 1);
    for (const category of ["doubleSpend", "mintAuthorization"] as const)
      expect(await captureJourneyWorkflowBaseline(reader, category)).toEqual(
        await captureJourneyWorkflowBaseline(watcher.storePath, category),
      );
    const one = await reader.readWorkflowRows("doubleSpend", second);
    expect([...one.keys()]).toEqual([second]);
    expect(one.get(second)).toHaveLength(1);
  });

  it("fails on a journaled workflow of another deployment", async () => {
    const watcher = await recordedWatcher();
    await appendWorkflow(watcher.storePath, "doubleSpend", "12".repeat(28), 1);
    await appendWorkflow(
      watcher.storePath,
      "doubleSpend",
      "34".repeat(28),
      1,
      "cd".repeat(32),
    );
    await expect(
      watcher.reader().readWorkflowRows("doubleSpend"),
    ).rejects.toThrow(/workflow row outside this deployment/u);
  });

  it("opens a live WAL store read-only, leaving its files as they were", async () => {
    const watcher = await recordedWatcher();
    const live = join(watcher.root, "live.sqlite");
    const first = "12".repeat(28);
    const second = "34".repeat(28);
    await appendWorkflow(live, "doubleSpend", first, 1);
    // The watcher holds its store open: the next workflow stays in the WAL.
    const writer = new DatabaseSync(live);
    try {
      writer.prepare("SELECT count(*) FROM sqlite_master").get();
      await appendWorkflow(live, "doubleSpend", second, 1);
      await copyFile(live, watcher.storePath);
      await copyFile(`${live}-wal`, `${watcher.storePath}-wal`);
    } finally {
      writer.close();
    }
    const files = [watcher.storePath, `${watcher.storePath}-wal`];
    const before = await Promise.all(files.map((path) => readFile(path)));
    expect(before[1].length).toBeGreaterThan(0);
    const rows = await watcher.reader().readWorkflowRows("doubleSpend");
    expect([...rows.keys()]).toEqual([first, second]);
    expect(await Promise.all(files.map((path) => readFile(path)))).toEqual(
      before,
    );
  });

  it("reads an absent journal or store as empty without creating it", async () => {
    const watcher = await recordedWatcher();
    const reader = watcher.reader();
    expect(await reader.readDecisions()).toEqual([]);
    expect(await reader.readWorkflowRows("doubleSpend")).toEqual(new Map());
    expect(existsSync(watcher.storePath)).toBe(false);
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
    await expect(reader.readWorkflowRows("doubleSpend")).rejects.toThrow(
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
      await expect(
        answering(output).readWorkflowRows("doubleSpend"),
      ).rejects.toThrow(/evidence/u);
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
    const header = "78".repeat(28);
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: header },
    };
    const row = (
      workflowId: string,
      body: unknown = { workflowId, identity },
    ) => ({
      header_hash: header,
      workflow_id: workflowId,
      body: typeof body === "string" ? body : JSON.stringify(body),
    });
    await expect(
      answering(
        envelope([
          row("a", {
            workflowId: "a",
            identity: { ...identity, deploymentFingerprint: "cd".repeat(32) },
          }),
        ]),
      ).readWorkflowRows("doubleSpend"),
    ).rejects.toThrow(/workflow row outside this deployment/u);
    for (const body of [
      { workflowId: "a" },
      { workflowId: "b", identity },
      {
        workflowId: "a",
        identity: { ...identity, category: "mintAuthorization" },
      },
      {
        workflowId: "a",
        identity: {
          ...identity,
          target: { kind: "state_queue_header", headerHash: "90".repeat(28) },
        },
      },
      { workflowId: "a", identity: { ...identity, target: null } },
      "not json",
      "[]",
    ])
      await expect(
        answering(envelope([row("a", body)])).readWorkflowRows("doubleSpend"),
      ).rejects.toThrow(/malformed workflow row/u);
    await expect(
      answering(envelope([row("a"), row("b")])).readWorkflowRows("doubleSpend"),
    ).rejects.toThrow(/more than one doubleSpend workflow/u);
    await expect(
      answering(envelope([row("a")])).readWorkflowRows(
        "doubleSpend",
        "90".repeat(28),
      ),
    ).rejects.toThrow(/malformed workflow row/u);
    await expect(
      answering(envelope([{ ...row("a"), body: 1 }])).readWorkflowRows(
        "doubleSpend",
      ),
    ).rejects.toThrow(/malformed workflow row/u);
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
