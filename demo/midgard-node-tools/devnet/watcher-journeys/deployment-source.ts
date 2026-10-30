import { isAbsolute } from "node:path";

import type { JourneyContext } from "./fixture.js";
import { loadJourneyContext } from "./live-context.js";
import {
  openStackJourneyContext,
  type StackJourneyContext,
} from "./stack-context.js";

/** The environment variables that choose where a journey's deployment comes from. */
export const JOURNEY_DEPLOYMENT_ENV = Object.freeze({
  /** `devnet` (the default) or `stack`. */
  mode: "MIDGARD_WATCHER_JOURNEY_DEPLOYMENT",
  /** Devnet mode: the journey devnet's run directory. */
  runDirectory: "MIDGARD_WATCHER_JOURNEY_RUN_DIR",
  /** Stack mode: the e2e-stack configuration file. */
  stackConfig: "MIDGARD_WATCHER_JOURNEY_STACK_CONFIG",
  /** Stack mode: the name of this drill run, kept across restarts to resume it. */
  stackRunId: "MIDGARD_WATCHER_JOURNEY_STACK_RUN_ID",
} as const);

/**
 * Where a journey's deployment comes from.
 * - `devnet`: the journey devnet run directory's own deployment, which the
 *   journey runs its own watcher and committee against.
 * - `stack`: a running e2e-stack's deployment, whose services play every role
 *   but the adversary.
 */
export type JourneyDeploymentSource =
  | Readonly<{
      mode: "devnet";
      runDirectory: string;
      open: () => Promise<JourneyContext>;
    }>
  | Readonly<{
      mode: "stack";
      configPath: string;
      runId: string;
      open: () => Promise<StackJourneyContext>;
    }>;

const set = (value: string | undefined): value is string =>
  value !== undefined && value !== "";

/**
 * The deployment source the environment selects, or `undefined` when nothing
 * selects one (devnet mode without a run directory), so the journey skips.
 * Refuses an unknown mode and a variable of the other mode.
 */
export const selectJourneyDeploymentSource = (
  env: Readonly<Record<string, string | undefined>>,
): JourneyDeploymentSource | undefined => {
  const mode = set(env[JOURNEY_DEPLOYMENT_ENV.mode])
    ? env[JOURNEY_DEPLOYMENT_ENV.mode]
    : "devnet";
  const runDirectory = env[JOURNEY_DEPLOYMENT_ENV.runDirectory];
  const configPath = env[JOURNEY_DEPLOYMENT_ENV.stackConfig];
  const runId = env[JOURNEY_DEPLOYMENT_ENV.stackRunId];
  if (mode === "devnet") {
    for (const name of [
      JOURNEY_DEPLOYMENT_ENV.stackConfig,
      JOURNEY_DEPLOYMENT_ENV.stackRunId,
    ])
      if (set(env[name]))
        throw new Error(`${name} applies only to the stack deployment mode`);
    if (runDirectory === undefined) return undefined;
    return Object.freeze({
      mode,
      runDirectory,
      open: () => loadJourneyContext(runDirectory),
    });
  }
  if (mode === "stack") {
    if (set(runDirectory))
      throw new Error(
        `${JOURNEY_DEPLOYMENT_ENV.runDirectory} applies only to the devnet deployment mode`,
      );
    if (!set(configPath) || !isAbsolute(configPath))
      throw new Error(
        `Stack mode requires an absolute ${JOURNEY_DEPLOYMENT_ENV.stackConfig}`,
      );
    if (!set(runId))
      throw new Error(
        `Stack mode requires ${JOURNEY_DEPLOYMENT_ENV.stackRunId}`,
      );
    return Object.freeze({
      mode,
      configPath,
      runId,
      open: () => openStackJourneyContext(configPath, runId),
    });
  }
  throw new Error(
    `${JOURNEY_DEPLOYMENT_ENV.mode} must be devnet or stack, not ${mode}`,
  );
};
