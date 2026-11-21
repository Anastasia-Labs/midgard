import { unlinkSync } from "node:fs";
import { setTimeout as delay } from "node:timers/promises";

import { compose } from "./chain.js";
import { readJsonIfPresent, writeDurableJson } from "./durable.js";
import type { ExecResult } from "./exec.js";
import type { Layout, RunEnv } from "./layout.js";

/** L1 containers of this run's compose project a drill may touch. */
export type L1Service = "cardano-node" | "ogmios" | "kupo" | "postgres";

/** The compose calls a drill, and the restore it may owe, make. */
export type ComposeDeps = {
  /** Runs `docker compose <args>` against this run's own project only. */
  readonly compose: (
    args: readonly string[],
    label: string,
  ) => Promise<Pick<ExecResult, "code" | "stderr"> & { readonly log?: string }>;
  /** Resolves after `ms`, or early once `signal` aborts. */
  readonly sleep: (ms: number, signal?: AbortSignal) => Promise<void>;
};

export const abortableSleep = (ms: number, signal?: AbortSignal) =>
  delay(ms, undefined, { signal }).catch(() => undefined);

export const composeDeps = (layout: Layout, run: RunEnv): ComposeDeps => ({
  compose: (args, label) => compose(layout, run, args, label),
  sleep: abortableSleep,
});

const COMPOSE_VERBS = new Set(["stop", "start", "pause", "unpause", "restart"]);
const L1_SERVICES = new Set<string>([
  "cardano-node",
  "ogmios",
  "kupo",
  "postgres",
]);

/**
 * The only compose calls a drill makes: one lifecycle verb on one of this
 * project's L1 services. Never `down`, never a volume flag.
 */
export const assertComposeAction = (verb: string, service: string) => {
  if (!COMPOSE_VERBS.has(verb) || !L1_SERVICES.has(service))
    throw new Error(`refusing docker compose ${verb} ${service}`);
};

export const composeAction = async (
  deps: ComposeDeps,
  verb: string,
  service: string,
) => {
  assertComposeAction(verb, service);
  const result = await deps.compose(
    [verb, service],
    `drill-${verb}-${service}`,
  );
  if (result.code !== 0)
    throw new Error(
      `docker compose ${verb} ${service} exited ${result.code}: ${result.stderr.trim().slice(-300)}${result.log === undefined ? "" : ` (${result.log})`}`,
    );
};

/** What brings a dependency a drill took down back. */
export type OwedRestore = {
  readonly drill: string;
  readonly service: L1Service;
  readonly undo: "start" | "unpause";
};

/** Written before a drill takes a dependency down, removed once it is back. */
export const owedRestorePath = (drillsLog: string) =>
  `${drillsLog}.restore.json`;

export const recordOwedRestore = (drillsLog: string, owed: OwedRestore) =>
  writeDurableJson(owedRestorePath(drillsLog), owed);

/** Undoes `owed`, retrying; the record is removed only once it succeeded. */
export const restore = async (
  deps: ComposeDeps,
  drillsLog: string,
  owed: OwedRestore,
  attempts = 5,
) => {
  for (let attempt = 1; ; attempt += 1) {
    try {
      await composeAction(deps, owed.undo, owed.service);
      break;
    } catch (error) {
      // An unpause of a container that is no longer paused has nothing to do.
      if (owed.undo === "unpause" && /not paused/iu.test(String(error))) break;
      if (attempt >= attempts) throw error;
      // Not abortable: an interrupted run still restores the dependency.
      await deps.sleep(5_000);
    }
  }
  unlinkSync(owedRestorePath(drillsLog));
};

/**
 * Finishes the restore a drill run left owed when it was killed mid-outage;
 * returns it, or undefined when nothing was owed. `up` and every drill run
 * call this first, so no stopped or paused dependency outlives its drill.
 */
export const finishOwedRestore = async (
  deps: ComposeDeps,
  drillsLog: string,
) => {
  const owed = readJsonIfPresent<OwedRestore>(owedRestorePath(drillsLog));
  if (owed !== undefined) await restore(deps, drillsLog, owed);
  return owed;
};
