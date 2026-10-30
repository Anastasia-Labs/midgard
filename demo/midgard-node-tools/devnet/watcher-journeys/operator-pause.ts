import { execFile } from "node:child_process";
import { setTimeout as pause } from "node:timers/promises";

import {
  isJourneyOperationsConnectionRefused,
  JOURNEY_RESTART_TIMEOUT_MS,
} from "./operations.js";
import { projectOnlyEnvironment } from "./watcher-evidence-reader.js";

/** The Compose service of an operator's node in its stack. */
export const OPERATOR_NODE_SERVICE = "midgard-node";

/**
 * The node's own block disagrees with its journal (`landed-blocks/holds.ts`).
 * A resumed operator-2 node that names it took a drill block signed with its
 * key for its own, instead of replaying it as foreign.
 */
const OWN_JOURNAL_MISMATCH = "landed_block_own_journal_mismatch";

/** One `/readyz` read: refused, or the node's answer with any HTTP status. */
export type OperatorNodeReadiness =
  | Readonly<{ reachable: false }>
  | Readonly<{ reachable: true; httpStatus: number; body: unknown }>;

/**
 * What the drill does to operator-2's node: stop and start its Compose
 * service (never remove it or its volumes), list the project's running
 * services and read its `/readyz`.
 */
export type OperatorNodeControl = Readonly<{
  /** The Compose project and service, for reports. */
  name: string;
  stop(): Promise<void>;
  start(): Promise<void>;
  runningServices(): Promise<readonly string[]>;
  /** Refused connections are `reachable: false`; any other failure throws. */
  readReadiness(): Promise<OperatorNodeReadiness>;
}>;

const compose = (project: string, args: readonly string[]) =>
  new Promise<string>((resolve, reject) => {
    execFile(
      "docker",
      ["compose", "-p", project, ...args],
      {
        cwd: "/",
        env: projectOnlyEnvironment(),
        encoding: "utf8",
        timeout: 120_000,
      },
      (error, stdout, stderr) => {
        if (error === null) resolve(stdout);
        else
          reject(
            new Error(
              `docker compose -p ${project} ${args.join(" ")} failed: ${stderr.trim().split("\n").slice(-5).join(" | ") || error.message}`,
              { cause: error },
            ),
          );
      },
    );
  });

/**
 * Operator-2's node as `docker compose -p <project>` runs it, and its
 * `/readyz` at `endpoint`. Compose runs from `/` with no `COMPOSE_*`
 * variables, so no Compose file on the caller's path can redefine the
 * project. It only stops and starts the node's service.
 */
export const composeOperatorNodeControl = ({
  composeProject,
  endpoint,
}: {
  composeProject: string;
  /** The node's HTTP base URL, as its Compose host port publishes it. */
  endpoint: string;
}): OperatorNodeControl =>
  Object.freeze({
    name: `${composeProject}/${OPERATOR_NODE_SERVICE}`,
    stop: async () => {
      await compose(composeProject, ["stop", OPERATOR_NODE_SERVICE]);
    },
    start: async () => {
      await compose(composeProject, ["start", OPERATOR_NODE_SERVICE]);
    },
    runningServices: async () =>
      (
        await compose(composeProject, [
          "ps",
          "--status",
          "running",
          "--services",
        ])
      )
        .split("\n")
        .map((line) => line.trim())
        .filter((line) => line !== ""),
    readReadiness: async (): Promise<OperatorNodeReadiness> => {
      let response: Response;
      try {
        response = await fetch(new URL("/readyz", endpoint), {
          signal: AbortSignal.timeout(5000),
        });
      } catch (cause) {
        if (isJourneyOperationsConnectionRefused(cause))
          return { reachable: false };
        throw cause;
      }
      const text = await response.text();
      let body: unknown = text;
      try {
        body = JSON.parse(text);
      } catch {
        // A non-JSON answer still proves something is serving the port.
      }
      return { reachable: true, httpStatus: response.status, body };
    },
  });

/** Proof that operator-2's node could not sign when it was checked. */
export type OperatorStoppedEvidence = Readonly<{
  node: string;
  checkedAt: string;
  runningServices: readonly string[];
  readyz: "connection_refused";
}>;

/** Proof that operator-2's node came back ready after the drill. */
export type OperatorResumedEvidence = Readonly<{
  node: string;
  startedAt: string;
  readyAt: string;
  /** Every distinct set of `/readyz` reasons seen while it caught up. */
  observedReasons: readonly (readonly string[])[];
}>;

const readinessReasons = (body: unknown): readonly string[] | undefined => {
  const reasons = (body as { reasons?: unknown } | null | undefined)?.reasons;
  return Array.isArray(reasons) &&
    reasons.every((reason) => typeof reason === "string")
    ? reasons
    : undefined;
};

/**
 * Proves operator-2's node cannot sign: Compose lists its service as not
 * running, and its `/readyz` refuses connections. Either failing refuses.
 */
export const proveOperatorNodeStopped = async (
  control: OperatorNodeControl,
): Promise<OperatorStoppedEvidence> => {
  const runningServices = await control.runningServices();
  if (runningServices.includes(OPERATOR_NODE_SERVICE))
    throw new Error(
      `Operator-2's node ${control.name} is still running; refusing to sign as operator-2`,
    );
  const readiness = await control.readReadiness();
  if (readiness.reachable)
    throw new Error(
      `Operator-2's node ${control.name} is not running in Compose, yet its /readyz still answers (HTTP ${readiness.httpStatus}); refusing to sign as operator-2`,
    );
  return Object.freeze({
    node: control.name,
    checkedAt: new Date().toISOString(),
    runningServices: Object.freeze([...runningServices]),
    readyz: "connection_refused",
  });
};

/**
 * The drill's hold on operator-2's node, for the whole run. `hold` stops the
 * node (stop only: its containers, volumes and journal stay) and proves it
 * stopped; `assertStopped` proves it again, before every signature made with
 * its key; `resume` starts it only if this hold stopped it, then waits until
 * its `/readyz` is ready, refusing a node that took a drill block for its own.
 */
export const createOperatorPause = (
  control: OperatorNodeControl,
  options: Readonly<{
    /** How long a resumed node may take to report ready; a restarted service's allowance by default. */
    resumeTimeoutMs?: number;
    pollIntervalMs?: number;
  }> = {},
) => {
  const resumeTimeoutMs = options.resumeTimeoutMs ?? JOURNEY_RESTART_TIMEOUT_MS;
  if (!Number.isFinite(resumeTimeoutMs) || resumeTimeoutMs <= 0)
    throw new Error("Operator resume timeout must be finite and positive");
  const pollIntervalMs = options.pollIntervalMs ?? 5000;
  let stopped = false;
  let resuming: Promise<OperatorResumedEvidence | undefined> | undefined;
  const hold = async () => {
    if (resuming !== undefined)
      throw new Error("Operator-2's node was already resumed by this drill");
    stopped = true;
    await control.stop();
    return await proveOperatorNodeStopped(control);
  };
  const assertStopped = async () => {
    if (!stopped || resuming !== undefined)
      throw new Error(
        "Operator-2's node is not held stopped by this drill; refusing to sign as operator-2",
      );
    return await proveOperatorNodeStopped(control);
  };
  const resume = () =>
    (resuming ??= (async () => {
      if (!stopped) return undefined;
      const startedAt = new Date().toISOString();
      await control.start();
      const observedReasons: string[][] = [];
      const deadline = performance.now() + resumeTimeoutMs;
      for (;;) {
        const readiness = await control.readReadiness();
        if (readiness.reachable) {
          const reasons = readinessReasons(readiness.body);
          if (reasons === undefined)
            throw new Error(
              `Operator-2's node ${control.name} answered /readyz without its reasons`,
            );
          if (
            !observedReasons.some(
              (seen) => JSON.stringify(seen) === JSON.stringify(reasons),
            )
          )
            observedReasons.push([...reasons]);
          if (reasons.includes(OWN_JOURNAL_MISMATCH))
            throw new Error(
              `Resumed operator-2 node ${control.name} holds ${OWN_JOURNAL_MISMATCH}: it did not replay the drill's blocks as foreign`,
            );
          if (
            readiness.httpStatus === 200 &&
            (readiness.body as { ready?: unknown }).ready === true &&
            reasons.length === 0
          )
            return Object.freeze({
              node: control.name,
              startedAt,
              readyAt: new Date().toISOString(),
              observedReasons: Object.freeze(observedReasons),
            });
        }
        if (performance.now() >= deadline)
          throw new Error(
            `Resumed operator-2 node ${control.name} was not ready within ${resumeTimeoutMs.toString()} ms (reasons seen: ${JSON.stringify(observedReasons)})`,
          );
        await pause(pollIntervalMs);
      }
    })());
  /**
   * Starts a node this hold stopped without waiting for it, for a run that
   * ends early: its liveness is then unproven, and the failure that ended the
   * run is the report.
   */
  const release = async () => {
    if (!stopped || resuming !== undefined) return;
    resuming = control.start().then(() => undefined);
    await resuming;
  };
  return Object.freeze({
    hold,
    assertStopped,
    resume,
    release,
    held: () => stopped && resuming === undefined,
  });
};

export type OperatorPause = ReturnType<typeof createOperatorPause>;
