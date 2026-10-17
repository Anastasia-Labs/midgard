import { createHash, randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import { type FileHandle, mkdir, open, readFile, rm } from "node:fs/promises";
import { dirname, resolve as resolvePath } from "node:path";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { writeJsonFileAtomic } from "../files/atomic-write.js";
import {
  assertIsoString,
  assertLowerHex,
  assertRecord,
  assertString,
  DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
  type DeploymentRunIdentity,
  type DeploymentRunMode,
  type DeploymentRunState,
  type DeploymentStepState,
  type DeploymentStepStatus,
  exactRunStateRecord,
  parseMode,
  RunStateError,
} from "./run-state.deployment-run-identity.js";
import {
  parseDeploymentRunIdentity,
  parseDeploymentStepState,
  parseEvents,
} from "./run-state.parse-deployment-run-identity.js";

export const bindDeploymentRunStateToMarker = (
  state: DeploymentRunState,
  {
    marker,
    manifestPath,
    manifestSha256,
    now = new Date(),
  }: {
    readonly marker: DeploymentMarker;
    readonly manifestPath: string;
    readonly manifestSha256: string;
    readonly now?: Date;
  },
): DeploymentRunState => {
  const canonicalMarker = (() => {
    try {
      return parseDeploymentMarker(marker);
    } catch (cause) {
      throw new RunStateError(
        `Cannot bind invalid deployment marker: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
        { cause },
      );
    }
  })();
  const canonicalManifestPath = assertString(
    manifestPath,
    "identity.manifestPath",
  );
  const canonicalManifestSha256 = assertLowerHex(
    manifestSha256,
    "identity.manifestSha256",
    32,
  );
  if (
    state.identity.deploymentMarker !== undefined &&
    state.identity.deploymentMarker.manifestId !== canonicalMarker.manifestId
  ) {
    throw new RunStateError(
      `Deployment run state marker mismatch: existing=${state.identity.deploymentMarker.manifestId}, current=${canonicalMarker.manifestId}. A different final deployment requires an explicit fresh run state.`,
    );
  }
  const timestamp = now.toISOString();
  return parseDeploymentRunState({
    ...state,
    updatedAt: timestamp,
    identity: {
      ...state.identity,
      manifestPath: canonicalManifestPath,
      manifestSha256: canonicalManifestSha256,
      deploymentMarker: canonicalMarker,
    },
    steps: {
      ...state.steps,
      deploymentMarker: {
        status: "complete",
        updatedAt: timestamp,
        evidence: [
          `manifestId=${canonicalMarker.manifestId}`,
          `manifestSha256=${canonicalManifestSha256}`,
        ],
      },
    },
    events: [
      ...state.events,
      {
        at: timestamp,
        kind: "step_transition",
        stepId: "deploymentMarker",
        message: "deploymentMarker -> complete",
      },
    ],
  });
};

export const parseDeploymentRunState = (value: unknown): DeploymentRunState => {
  const input = exactRunStateRecord(value, "run state", [
    "schemaVersion",
    "runId",
    "createdAt",
    "updatedAt",
    "mode",
    "identity",
    "steps",
    "events",
  ]);
  if (input.schemaVersion !== DEPLOYMENT_RUN_STATE_SCHEMA_VERSION) {
    throw new RunStateError(
      `Unsupported run-state schemaVersion: ${String(input.schemaVersion)}`,
    );
  }
  const stepsInput = assertRecord(input.steps, "steps");
  const steps = Object.fromEntries(
    Object.entries(stepsInput).map(([stepId, step]) => [
      stepId,
      parseDeploymentStepState(step, `steps.${stepId}`),
    ]),
  );
  const parsed: DeploymentRunState = {
    schemaVersion: DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
    runId: assertString(input.runId, "runId"),
    createdAt: assertIsoString(input.createdAt, "createdAt"),
    updatedAt: assertIsoString(input.updatedAt, "updatedAt"),
    mode: parseMode(input.mode),
    identity: parseDeploymentRunIdentity(input.identity),
    steps,
    events: parseEvents(input.events),
  };
  const createdAtMs = Date.parse(parsed.createdAt);
  const updatedAtMs = Date.parse(parsed.updatedAt);
  const identityPolicyId = parsed.identity.referenceScriptAuthPolicy?.policyId;
  if (
    createdAtMs > updatedAtMs ||
    (parsed.identity.referenceScriptAuthPolicyId !== undefined &&
      identityPolicyId !== undefined &&
      parsed.identity.referenceScriptAuthPolicyId !== identityPolicyId) ||
    parsed.events.length === 0 ||
    parsed.events[0]?.kind !== "created" ||
    parsed.events[0]?.at !== parsed.createdAt ||
    parsed.events[0]?.stepId !== undefined ||
    parsed.events[0]?.message !==
      `Created ${parsed.mode} deployment run state.` ||
    parsed.events.at(-1)?.at !== parsed.updatedAt
  ) {
    throw new RunStateError(
      "run state identity, timestamps, or creation event are inconsistent.",
    );
  }
  let previousEventAtMs = createdAtMs;
  for (const [index, event] of parsed.events.entries()) {
    const eventAtMs = Date.parse(event.at);
    if (
      eventAtMs < previousEventAtMs ||
      eventAtMs > updatedAtMs ||
      (index > 0 && event.kind !== "step_transition") ||
      (event.kind === "step_transition" &&
        (event.stepId === undefined ||
          parsed.steps[event.stepId] === undefined))
    ) {
      throw new RunStateError(
        "run state event chronology or step identity is inconsistent.",
      );
    }
    previousEventAtMs = eventAtMs;
  }
  for (const [stepId, step] of Object.entries(parsed.steps)) {
    assertString(stepId, `steps key ${JSON.stringify(stepId)}`);
    const lastTransition = [...parsed.events]
      .reverse()
      .find(
        (event) => event.kind === "step_transition" && event.stepId === stepId,
      );
    if (
      Date.parse(step.updatedAt) < createdAtMs ||
      Date.parse(step.updatedAt) > updatedAtMs ||
      lastTransition?.at !== step.updatedAt ||
      lastTransition.message !== `${stepId} -> ${step.status}`
    ) {
      throw new RunStateError(
        `run state step ${stepId} is not bound to its latest transition event.`,
      );
    }
  }
  return parsed;
};

export const createDeploymentRunState = ({
  mode,
  runId = `deployment-run-${randomUUID()}`,
  now = new Date(),
  identity = {},
}: {
  readonly mode: DeploymentRunMode;
  readonly runId?: string;
  readonly now?: Date;
  readonly identity?: DeploymentRunIdentity;
}): DeploymentRunState => {
  const timestamp = now.toISOString();
  return parseDeploymentRunState({
    schemaVersion: DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
    runId,
    createdAt: timestamp,
    updatedAt: timestamp,
    mode,
    identity,
    steps: {},
    events: [
      {
        at: timestamp,
        kind: "created",
        message: `Created ${mode} deployment run state.`,
      },
    ],
  });
};

export const transitionDeploymentStep = (
  state: DeploymentRunState,
  stepId: string,
  status: DeploymentStepStatus,
  patch: Omit<Partial<DeploymentStepState>, "status" | "updatedAt"> = {},
  now = new Date(),
): DeploymentRunState => {
  if (stepId.trim().length === 0) {
    throw new RunStateError("stepId must be non-empty.");
  }
  const timestamp = now.toISOString();
  return parseDeploymentRunState({
    ...state,
    updatedAt: timestamp,
    steps: {
      ...state.steps,
      [stepId]: {
        ...state.steps[stepId],
        ...patch,
        status,
        updatedAt: timestamp,
      },
    },
    events: [
      ...state.events,
      {
        at: timestamp,
        kind: "step_transition",
        stepId,
        message: `${stepId} -> ${status}`,
      },
    ],
  });
};

export const defaultDeploymentRunStatePath = (
  env: NodeJS.ProcessEnv = process.env,
): string =>
  resolvePath(
    env.MIDGARD_RUN_STATE_PATH?.trim() ||
      "deploymentInfo/midgard-run-state.json",
  );

export const sha256File = async (path: string): Promise<string> => {
  const data = await readFile(path);
  return createHash("sha256").update(data).digest("hex");
};

export const loadDeploymentRunState = async (
  path: string,
): Promise<DeploymentRunState | null> => {
  if (!existsSync(path)) {
    return null;
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(await readFile(path, "utf8"));
  } catch (cause) {
    throw new RunStateError(`Failed to read run state at ${path}.`, { cause });
  }
  return parseDeploymentRunState(parsed);
};

export const writeDeploymentRunStateAtomic = async (
  path: string,
  state: DeploymentRunState,
): Promise<void> => {
  const normalized = parseDeploymentRunState(state);
  await writeJsonFileAtomic(path, normalized);
};

export const withDeploymentRunStateLock = async <A>(
  path: string,
  action: () => Promise<A>,
): Promise<A> => {
  await mkdir(dirname(path), { recursive: true });
  const lockPath = `${path}.lock`;
  let handle: FileHandle;
  try {
    handle = await open(lockPath, "wx");
  } catch (cause) {
    throw new RunStateError(`Run state is locked: ${lockPath}`, { cause });
  }
  try {
    await handle.writeFile(
      JSON.stringify({
        pid: process.pid,
        acquiredAt: new Date().toISOString(),
      }),
      "utf8",
    );
    return await action();
  } finally {
    await handle.close();
    await rm(lockPath, { force: true });
  }
};
