import { execFile } from "node:child_process";
import { setTimeout as pause } from "node:timers/promises";

import type { LucidEvolution } from "@lucid-evolution/lucid";

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

/**
 * How a resumed operator-2 node ended: ready, or live but out of the active
 * set because the drill's convictions slashed its operator
 * (`operator_removed`) or it is registered again and not yet active
 * (`operator_not_yet_active`).
 */
export type OperatorResumedTerminal =
  | "ready"
  | "operator_removed"
  | "awaiting_activation";

/** Proof that operator-2's node came back after the drill. */
export type OperatorResumedEvidence = Readonly<{
  node: string;
  startedAt: string;
  readyAt: string;
  terminal: OperatorResumedTerminal;
  /** Every distinct set of `/readyz` reasons seen while it caught up. */
  observedReasons: readonly (readonly string[])[];
}>;

/** Our node's own replay hold on a landed block it refuses. */
const LANDED_BLOCK_INVALID = "landed_block_invalid";

const readinessReasons = (body: unknown): readonly string[] | undefined => {
  const reasons = (body as { reasons?: unknown } | null | undefined)?.reasons;
  return Array.isArray(reasons) &&
    reasons.every((reason) => typeof reason === "string")
    ? reasons
    : undefined;
};

/**
 * Whether a resumed node's `/readyz` is a terminal state for the drill: ready
 * with no reason, or out of the active set (removed, or awaiting activation)
 * with its L1 follower at the node's tip and no replay hold on any block.
 * Removal holds the node's duties for good, so it never reports ready.
 */
export const resumedOperatorTerminal = (
  readiness: Extract<OperatorNodeReadiness, { reachable: true }>,
  reasons: readonly string[],
): OperatorResumedTerminal | undefined => {
  const body = readiness.body as {
    ready?: unknown;
    l1Follower?: { atTip?: unknown } | null;
  };
  if (
    readiness.httpStatus === 200 &&
    body.ready === true &&
    reasons.length === 0
  )
    return "ready";
  const membership = reasons.includes("operator_removed")
    ? "operator_removed"
    : reasons.includes("operator_not_yet_active")
      ? "awaiting_activation"
      : undefined;
  return membership !== undefined &&
    !reasons.includes(LANDED_BLOCK_INVALID) &&
    !reasons.includes(OWN_JOURNAL_MISMATCH) &&
    !reasons.includes("l1_follower_catching_up") &&
    body.l1Follower?.atTip === true
    ? membership
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
 * its `/readyz` reaches a terminal state (`resumedOperatorTerminal`),
 * refusing a node that took a drill block for its own.
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
        // A node restarting resets or times out reads before it serves;
        // inside this wait any transport failure is "not yet".
        const readiness = await control
          .readReadiness()
          .catch((): OperatorNodeReadiness => ({ reachable: false }));
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
          const terminal = resumedOperatorTerminal(readiness, reasons);
          if (terminal !== undefined)
            return Object.freeze({
              node: control.name,
              startedAt,
              readyAt: new Date().toISOString(),
              terminal,
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

/**
 * Makes every signature of `lucid`'s wallet, operator-2's, first prove its
 * node stopped (`assertStopped`): a signature while the drill does not hold
 * the node stopped, or while it runs, is refused before any key is used.
 */
export const guardOperatorSignatures = (
  lucid: LucidEvolution,
  hold: Pick<OperatorPause, "assertStopped">,
) => {
  const wallet = lucid.wallet();
  const { signTx, signTxs, signMessage } = wallet;
  wallet.signTx = async (tx) => {
    await hold.assertStopped();
    return signTx.call(wallet, tx);
  };
  if (signTxs !== undefined)
    wallet.signTxs = async (txs) => {
      await hold.assertStopped();
      return signTxs.call(wallet, txs);
    };
  wallet.signMessage = async (address, payload) => {
    await hold.assertStopped();
    return signMessage.call(wallet, address, payload);
  };
};
