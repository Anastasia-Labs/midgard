import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import type { HeaderDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";

import { JourneyTransportError } from "./transport-failure.js";

/** Where the stack's watcher Compose service mounts its process configuration. */
export const WATCHER_CONTAINER_PROCESS_CONFIG_PATH =
  "/etc/midgard/watcher-process.json";

/**
 * The read-only evidence reader, run by the watcher image's own Node in its
 * working directory. It reads the paths the watcher actually runs with from
 * its mounted process configuration, opens the decision journal through the
 * image's exported `readWatcherFaultDecisionEvidence`, and prints one JSON
 * envelope carrying the configuration's digest. A read that fails prints the failure
 * as its envelope instead, with SQLite's result code when it has one, so the
 * host tells a named failure from a busy journal (`errcode` 5). Its one
 * argument is the request.
 */
export const WATCHER_EVIDENCE_READER_SCRIPT = `
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

const request = JSON.parse(process.argv[process.argv.length - 1]);
const read = async (config) => {
  if (request.read === "decisions") {
    const directory = config.decisionJournalDirectory;
    if (!existsSync(join(directory, "watcher-journals.sqlite"))) return [];
    const { readWatcherFaultDecisionEvidence } = await import("./dist/index.js");
    return readWatcherFaultDecisionEvidence({
      directory,
      hubOraclePolicyId: request.hubOraclePolicyId,
      launchScope: request.launchScope,
    });
  }
  throw new Error("Unknown watcher evidence read");
};
try {
  const configBytes = readFileSync(request.processConfigPath);
  const evidence = await read(JSON.parse(configBytes.toString("utf8")));
  process.stdout.write(JSON.stringify({
    processConfigSha256: createHash("sha256").update(configBytes).digest("hex"),
    evidence,
  }));
} catch (cause) {
  process.stdout.write(JSON.stringify({
    error: {
      message: String(cause?.message ?? cause),
      ...(typeof cause?.code === "string" ? { code: cause.code } : {}),
      ...(typeof cause?.errcode === "number" ? { errcode: cause.errcode } : {}),
    },
  }));
}
`;

/** One evidence read, as the reader script receives it. */
export type WatcherEvidenceRequest = Readonly<{
  read: "decisions";
  processConfigPath: string;
  hubOraclePolicyId: string;
  launchScope: readonly string[];
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
export const projectOnlyEnvironment = (): NodeJS.ProcessEnv =>
  Object.fromEntries(
    Object.entries(process.env).filter(
      ([name]) => !name.startsWith("COMPOSE_"),
    ),
  );

/**
 * The stack's watcher, by `docker compose -p <project> exec -T watcher`. It
 * runs from `/` with no `COMPOSE_*` variables, so no Compose file on the
 * caller's path can redefine the project's services. An exec that fails
 * (the daemon, the container or its Node did not answer; the reader prints
 * its own failures as an answer) is a `JourneyTransportError`.
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
              new JourneyTransportError(
                `Could not read the stack watcher's evidence in Compose project ${composeProject}: ${stderr.trim().split("\n").slice(-5).join(" | ") || error.message}`,
                { cause: error },
              ),
            );
        },
      );
    });

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const DECISIONS = new Set(["healthy", "unprovable", "fault_detected"]);

/** What a journey reads from its watcher's decision journal, wherever it lives. */
export type JourneyWatcherEvidence = Readonly<{
  readDecisions(): Promise<readonly HeaderDecision[]>;
}>;

/**
 * Reads a running watcher's decision journal through `exec`. Every answer
 * must come from the watcher whose mounted process configuration is
 * byte-for-byte the host's `hostProcessConfigPath`, and every decision must
 * belong to `hubOraclePolicyId`.
 */
export const createWatcherEvidenceReader = (input: {
  exec: WatcherEvidenceExec;
  hostProcessConfigPath: string;
  containerProcessConfigPath?: string;
  hubOraclePolicyId: string;
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
    if (isRecord(envelope) && isRecord(envelope.error)) {
      const { message, code, errcode } = envelope.error;
      throw Object.assign(
        new Error(
          `The stack watcher's evidence reader failed: ${String(message)}`,
        ),
        typeof code === "string" ? { code } : {},
        typeof errcode === "number" ? { errcode } : {},
      );
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
        hubOraclePolicyId: input.hubOraclePolicyId,
        launchScope: input.launchScope,
      });
      for (const decision of decisions)
        if (
          !isRecord(decision) ||
          decision.hubOraclePolicyId !== input.hubOraclePolicyId ||
          typeof decision.headerHash !== "string" ||
          !DECISIONS.has(decision.decision as string)
        )
          throw new Error(
            "The stack watcher returned a decision outside this deployment",
          );
      return Object.freeze(decisions as HeaderDecision[]);
    },
  });
};
