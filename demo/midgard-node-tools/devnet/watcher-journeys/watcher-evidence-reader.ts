import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import type {
  FraudProofWorkflowJournalEntry,
  HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

/** Where the stack's watcher Compose service mounts its process configuration. */
export const WATCHER_CONTAINER_PROCESS_CONFIG_PATH =
  "/etc/midgard/watcher-process.json";

/**
 * The read-only evidence reader, run by the watcher image's own Node in its
 * working directory. It reads the paths the watcher actually runs with from
 * its mounted process configuration, opens the decision journal through the
 * image's exported `readWatcherFaultDecisionEvidence` and the workflow
 * journal as a read-only SQLite connection, and prints one JSON envelope
 * carrying the configuration's digest. Its one argument is the request.
 */
export const WATCHER_EVIDENCE_READER_SCRIPT = `
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

const request = JSON.parse(process.argv[process.argv.length - 1]);
const configBytes = readFileSync(request.processConfigPath);
const config = JSON.parse(configBytes.toString("utf8"));
const read = async () => {
  if (request.read === "decisions") {
    const directory = config.workflowJournalDirectory;
    if (!existsSync(join(directory, "watcher-journals.sqlite"))) return [];
    const { readWatcherFaultDecisionEvidence } = await import("./dist/index.js");
    return readWatcherFaultDecisionEvidence({
      directory,
      deploymentFingerprint: request.deploymentFingerprint,
      launchScope: request.launchScope,
    });
  }
  if (request.read === "workflow-rows") {
    const path = config.watcherConfig.storage.path;
    if (!existsSync(path)) return [];
    const store = new DatabaseSync(path, { readOnly: true });
    try {
      const table = store
        .prepare("SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'watcher_workflow_journal_v1'")
        .get();
      if (table === undefined) return [];
      const byHeader = request.headerHash === undefined ? "" : " AND header_hash = ?";
      return store
        .prepare("SELECT header_hash, workflow_id, body FROM watcher_workflow_journal_v1 WHERE category = ?" + byHeader + " ORDER BY header_hash, workflow_id, sequence")
        .all(...(request.headerHash === undefined ? [request.category] : [request.category, request.headerHash]));
    } finally {
      store.close();
    }
  }
  throw new Error("Unknown watcher evidence read");
};
const evidence = await read();
process.stdout.write(JSON.stringify({
  processConfigSha256: createHash("sha256").update(configBytes).digest("hex"),
  evidence,
}));
`;

/** One evidence read, as the reader script receives it. */
export type WatcherEvidenceRequest =
  | Readonly<{
      read: "decisions";
      processConfigPath: string;
      deploymentFingerprint: string;
      launchScope: readonly string[];
    }>
  | Readonly<{
      read: "workflow-rows";
      processConfigPath: string;
      category: FraudProofCatalogueCategoryName;
      headerHash?: string;
    }>;

/**
 * Runs the reader script with Node inside the watcher's own container and
 * returns its standard output.
 */
export type WatcherEvidenceExec = (input: {
  script: string;
  request: string;
}) => Promise<string>;

/** A Compose environment that addresses services by project name alone. */
const projectOnlyEnvironment = (): NodeJS.ProcessEnv =>
  Object.fromEntries(
    Object.entries(process.env).filter(
      ([name]) => !name.startsWith("COMPOSE_"),
    ),
  );

/**
 * The stack's watcher, by `docker compose -p <project> exec -T watcher`. It
 * runs from `/` with no `COMPOSE_*` variables, so no Compose file on the
 * caller's path can redefine the project's services.
 */
export const composeWatcherEvidenceExec =
  (composeProject: string): WatcherEvidenceExec =>
  ({ script, request }) =>
    new Promise((resolve, reject) => {
      execFile(
        "docker",
        [
          "compose",
          "-p",
          composeProject,
          "exec",
          "-T",
          "watcher",
          "node",
          "--input-type=module",
          "-e",
          script,
          request,
        ],
        {
          cwd: "/",
          env: projectOnlyEnvironment(),
          encoding: "utf8",
          maxBuffer: 64 * 1024 * 1024,
          timeout: 120_000,
        },
        (error, stdout, stderr) => {
          if (error === null) resolve(stdout);
          else
            reject(
              new Error(
                `Could not read the stack watcher's evidence in Compose project ${composeProject}: ${stderr.trim().split("\n").slice(-5).join(" | ") || error.message}`,
                { cause: error },
              ),
            );
        },
      );
    });

type WorkflowRow = { header_hash: string; workflow_id: string; body: string };

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const DECISIONS = new Set(["healthy", "unprovable", "fault_detected"]);

/** What a journey reads from its watcher's journals, wherever they live. */
export type JourneyWatcherEvidence = Readonly<{
  readDecisions(): Promise<readonly HeaderDecision[]>;
  /** The workflow rows for each header of `category`, or of one header. */
  readWorkflowRows(
    category: FraudProofCatalogueCategoryName,
    headerHash?: string,
  ): Promise<ReadonlyMap<string, readonly FraudProofWorkflowJournalEntry[]>>;
}>;

/**
 * Reads a running watcher's decision and workflow journals through `exec`.
 * Every answer must come from the watcher whose mounted process configuration
 * is byte-for-byte the host's `hostProcessConfigPath`, and every decision must
 * belong to `deploymentFingerprint`.
 */
export const createWatcherEvidenceReader = (input: {
  exec: WatcherEvidenceExec;
  hostProcessConfigPath: string;
  containerProcessConfigPath?: string;
  deploymentFingerprint: string;
  launchScope: readonly string[];
}): JourneyWatcherEvidence => {
  const processConfigPath =
    input.containerProcessConfigPath ?? WATCHER_CONTAINER_PROCESS_CONFIG_PATH;
  const read = async (request: WatcherEvidenceRequest): Promise<unknown[]> => {
    const expectedSha256 = createHash("sha256")
      .update(await readFile(input.hostProcessConfigPath))
      .digest("hex");
    const output = await input.exec({
      script: WATCHER_EVIDENCE_READER_SCRIPT,
      request: JSON.stringify(request),
    });
    let envelope: unknown;
    try {
      envelope = JSON.parse(output);
    } catch {
      throw new Error("The stack watcher's evidence reader printed no JSON");
    }
    if (
      !isRecord(envelope) ||
      typeof envelope.processConfigSha256 !== "string" ||
      !Array.isArray(envelope.evidence)
    )
      throw new Error("The stack watcher's evidence envelope is malformed");
    if (envelope.processConfigSha256 !== expectedSha256)
      throw new Error(
        "The watcher that answered runs another process configuration than this stack's",
      );
    return envelope.evidence as unknown[];
  };
  return Object.freeze({
    readDecisions: async () => {
      const decisions = await read({
        read: "decisions",
        processConfigPath,
        deploymentFingerprint: input.deploymentFingerprint,
        launchScope: input.launchScope,
      });
      for (const decision of decisions)
        if (
          !isRecord(decision) ||
          decision.deploymentFingerprint !== input.deploymentFingerprint ||
          typeof decision.headerHash !== "string" ||
          !DECISIONS.has(decision.decision as string)
        )
          throw new Error(
            "The stack watcher returned a decision outside this deployment",
          );
      return Object.freeze(decisions as HeaderDecision[]);
    },
    readWorkflowRows: async (category, headerHash) => {
      const rows = await read({
        read: "workflow-rows",
        processConfigPath,
        category,
        ...(headerHash === undefined ? {} : { headerHash }),
      });
      const byHeader = new Map<string, FraudProofWorkflowJournalEntry[]>();
      const workflows = new Map<string, string>();
      for (const row of rows) {
        if (
          !isRecord(row) ||
          typeof row.header_hash !== "string" ||
          typeof row.workflow_id !== "string" ||
          typeof row.body !== "string" ||
          (headerHash !== undefined && row.header_hash !== headerHash)
        )
          throw new Error(
            "The stack watcher returned a malformed workflow row",
          );
        const {
          header_hash: header,
          workflow_id: workflowId,
          body,
        } = row as WorkflowRow;
        const known = workflows.get(header);
        if (known !== undefined && known !== workflowId)
          throw new Error(
            `Header ${header} has more than one ${category} workflow`,
          );
        workflows.set(header, workflowId);
        const entries = byHeader.get(header) ?? [];
        entries.push(JSON.parse(body) as FraudProofWorkflowJournalEntry);
        byHeader.set(header, entries);
      }
      return byHeader;
    },
  });
};
