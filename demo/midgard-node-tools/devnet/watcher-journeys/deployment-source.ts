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
  /** Stack mode: the Compose project of operator-2's node, which the drill stops. */
  operator2ComposeProject: "MIDGARD_WATCHER_JOURNEY_OPERATOR2_COMPOSE_PROJECT",
  /** Stack mode: operator-2's node HTTP endpoint, whose `/readyz` proves it stopped. */
  operator2Endpoint: "MIDGARD_WATCHER_JOURNEY_OPERATOR2_ENDPOINT",
} as const);

/** Stack mode's own variables, which devnet mode refuses. */
const STACK_ONLY_ENV = [
  JOURNEY_DEPLOYMENT_ENV.stackConfig,
  JOURNEY_DEPLOYMENT_ENV.stackRunId,
  JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject,
  JOURNEY_DEPLOYMENT_ENV.operator2Endpoint,
] as const;

/** Operator-2's node, a second stack instance the drill pauses while it signs as operator-2. */
export type JourneyOperator2Node = Readonly<{
  composeProject: string;
  endpoint: string;
}>;

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
      operator2: JourneyOperator2Node;
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
    for (const name of STACK_ONLY_ENV)
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
    const composeProject = env[JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject];
    if (!set(composeProject) || !/^[a-z0-9][a-z0-9_-]*$/u.test(composeProject))
      throw new Error(
        `Stack mode requires ${JOURNEY_DEPLOYMENT_ENV.operator2ComposeProject}, a Compose project name`,
      );
    const endpoint = env[JOURNEY_DEPLOYMENT_ENV.operator2Endpoint];
    if (!set(endpoint) || !/^https?:\/\/[^/]+\/?$/u.test(endpoint))
      throw new Error(
        `Stack mode requires ${JOURNEY_DEPLOYMENT_ENV.operator2Endpoint}, an http(s) origin`,
      );
    return Object.freeze({
      mode,
      configPath,
      runId,
      operator2: Object.freeze({ composeProject, endpoint }),
      open: () => openStackJourneyContext(configPath, runId),
    });
  }
  throw new Error(
    `${JOURNEY_DEPLOYMENT_ENV.mode} must be devnet or stack, not ${mode}`,
  );
};
