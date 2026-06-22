import { createHash, randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import {
  type FileHandle,
  mkdir,
  open,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { dirname, resolve as resolvePath } from "node:path";

export const DEPLOYMENT_RUN_STATE_SCHEMA_VERSION =
  "midgard-deployment-run-state-v1";

export type DeploymentRunMode = "attach" | "resume" | "fresh";

export type DeploymentRunIdentity = {
  readonly network?: string;
  readonly hubOracleOneShot?: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  readonly referenceScriptAuthPolicyId?: string;
  readonly referenceScriptAuthPolicy?: {
    readonly policyId: string;
    readonly nativeScript: {
      readonly type: "Native";
      readonly cborHex: string;
      readonly expiresAtSlot: number;
      readonly expiresAtUnixTime: number;
      readonly timelockDurationMs: number;
    };
  };
  readonly manifestPath?: string;
  readonly manifestSha256?: string;
};

export type DeploymentStepStatus =
  | "not_started"
  | "submitted"
  | "confirmed"
  | "complete"
  | "blocked"
  | "failed";

export type DeploymentStepState = {
  readonly status: DeploymentStepStatus;
  readonly updatedAt: string;
  readonly txHashes?: readonly string[];
  readonly outRefs?: readonly string[];
  readonly message?: string;
  readonly evidence?: readonly string[];
};

export type DeploymentRunEvent = {
  readonly at: string;
  readonly kind: string;
  readonly message: string;
  readonly stepId?: string;
};

export type DeploymentRunState = {
  readonly schemaVersion: typeof DEPLOYMENT_RUN_STATE_SCHEMA_VERSION;
  readonly runId: string;
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly mode: DeploymentRunMode;
  readonly identity: DeploymentRunIdentity;
  readonly steps: Readonly<Record<string, DeploymentStepState>>;
  readonly events: readonly DeploymentRunEvent[];
};

export class RunStateError extends Error {
  constructor(message: string, options?: { readonly cause?: unknown }) {
    super(message, options);
    this.name = "RunStateError";
  }
}

const assertRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new RunStateError(`${label} must be an object.`);
  }
  return value as Record<string, unknown>;
};

const assertString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new RunStateError(`${label} must be a non-empty string.`);
  }
  return value;
};

const assertIsoString = (value: unknown, label: string): string => {
  const text = assertString(value, label);
  if (Number.isNaN(Date.parse(text))) {
    throw new RunStateError(`${label} must be an ISO timestamp.`);
  }
  return text;
};

const assertStringArray = (
  value: unknown,
  label: string,
): readonly string[] | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (
    !Array.isArray(value) ||
    value.some((entry) => typeof entry !== "string")
  ) {
    throw new RunStateError(`${label} must be an array of strings.`);
  }
  return value as readonly string[];
};

const parseMode = (value: unknown): DeploymentRunMode => {
  if (value === "attach" || value === "resume" || value === "fresh") {
    return value;
  }
  throw new RunStateError("mode must be attach, resume, or fresh.");
};

const parseStepStatus = (value: unknown): DeploymentStepStatus => {
  if (
    value === "not_started" ||
    value === "submitted" ||
    value === "confirmed" ||
    value === "complete" ||
    value === "blocked" ||
    value === "failed"
  ) {
    return value;
  }
  throw new RunStateError("step.status is invalid.");
};

const parseIdentity = (value: unknown): DeploymentRunIdentity => {
  const input = assertRecord(value, "identity");
  const hubOracleOneShot =
    input.hubOracleOneShot === undefined
      ? undefined
      : assertRecord(input.hubOracleOneShot, "identity.hubOracleOneShot");
  const referenceScriptAuthPolicy =
    input.referenceScriptAuthPolicy === undefined
      ? undefined
      : assertRecord(
          input.referenceScriptAuthPolicy,
          "identity.referenceScriptAuthPolicy",
        );
  const referenceScriptAuthPolicyNativeScript =
    referenceScriptAuthPolicy?.nativeScript === undefined
      ? undefined
      : assertRecord(
          referenceScriptAuthPolicy.nativeScript,
          "identity.referenceScriptAuthPolicy.nativeScript",
        );
  return {
    ...(input.network === undefined
      ? {}
      : { network: assertString(input.network, "identity.network") }),
    ...(hubOracleOneShot === undefined
      ? {}
      : {
          hubOracleOneShot: {
            txHash: assertString(
              hubOracleOneShot.txHash,
              "identity.hubOracleOneShot.txHash",
            ),
            outputIndex:
              typeof hubOracleOneShot.outputIndex === "number" &&
              Number.isInteger(hubOracleOneShot.outputIndex) &&
              hubOracleOneShot.outputIndex >= 0
                ? hubOracleOneShot.outputIndex
                : (() => {
                    throw new RunStateError(
                      "identity.hubOracleOneShot.outputIndex must be a non-negative integer.",
                    );
                  })(),
          },
        }),
    ...(input.referenceScriptAuthPolicyId === undefined
      ? {}
      : {
          referenceScriptAuthPolicyId: assertString(
            input.referenceScriptAuthPolicyId,
            "identity.referenceScriptAuthPolicyId",
          ),
        }),
    ...(referenceScriptAuthPolicy === undefined
      ? {}
      : {
          referenceScriptAuthPolicy: {
            policyId: assertString(
              referenceScriptAuthPolicy.policyId,
              "identity.referenceScriptAuthPolicy.policyId",
            ),
            nativeScript: {
              type:
                referenceScriptAuthPolicyNativeScript?.type === "Native"
                  ? "Native"
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.type must be Native.",
                      );
                    })(),
              cborHex: assertString(
                referenceScriptAuthPolicyNativeScript.cborHex,
                "identity.referenceScriptAuthPolicy.nativeScript.cborHex",
              ),
              expiresAtSlot:
                typeof referenceScriptAuthPolicyNativeScript.expiresAtSlot ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.expiresAtSlot,
                ) &&
                referenceScriptAuthPolicyNativeScript.expiresAtSlot >= 0
                  ? referenceScriptAuthPolicyNativeScript.expiresAtSlot
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.expiresAtSlot must be a non-negative safe integer.",
                      );
                    })(),
              expiresAtUnixTime:
                typeof referenceScriptAuthPolicyNativeScript.expiresAtUnixTime ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.expiresAtUnixTime,
                ) &&
                referenceScriptAuthPolicyNativeScript.expiresAtUnixTime > 0
                  ? referenceScriptAuthPolicyNativeScript.expiresAtUnixTime
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime must be a positive safe integer.",
                      );
                    })(),
              timelockDurationMs:
                typeof referenceScriptAuthPolicyNativeScript.timelockDurationMs ===
                  "number" &&
                Number.isSafeInteger(
                  referenceScriptAuthPolicyNativeScript.timelockDurationMs,
                ) &&
                referenceScriptAuthPolicyNativeScript.timelockDurationMs > 0
                  ? referenceScriptAuthPolicyNativeScript.timelockDurationMs
                  : (() => {
                      throw new RunStateError(
                        "identity.referenceScriptAuthPolicy.nativeScript.timelockDurationMs must be a positive safe integer.",
                      );
                    })(),
            },
          },
        }),
    ...(input.manifestPath === undefined
      ? {}
      : {
          manifestPath: assertString(
            input.manifestPath,
            "identity.manifestPath",
          ),
        }),
    ...(input.manifestSha256 === undefined
      ? {}
      : {
          manifestSha256: assertString(
            input.manifestSha256,
            "identity.manifestSha256",
          ),
        }),
  };
};

const parseStep = (value: unknown): DeploymentStepState => {
  const input = assertRecord(value, "step");
  return {
    status: parseStepStatus(input.status),
    updatedAt: assertIsoString(input.updatedAt, "step.updatedAt"),
    ...(input.txHashes === undefined
      ? {}
      : { txHashes: assertStringArray(input.txHashes, "step.txHashes") }),
    ...(input.outRefs === undefined
      ? {}
      : { outRefs: assertStringArray(input.outRefs, "step.outRefs") }),
    ...(input.message === undefined
      ? {}
      : { message: assertString(input.message, "step.message") }),
    ...(input.evidence === undefined
      ? {}
      : { evidence: assertStringArray(input.evidence, "step.evidence") }),
  };
};

const parseEvents = (value: unknown): readonly DeploymentRunEvent[] => {
  if (!Array.isArray(value)) {
    throw new RunStateError("events must be an array.");
  }
  return value.map((entry, index) => {
    const input = assertRecord(entry, `events[${index.toString()}]`);
    return {
      at: assertIsoString(input.at, `events[${index.toString()}].at`),
      kind: assertString(input.kind, `events[${index.toString()}].kind`),
      message: assertString(
        input.message,
        `events[${index.toString()}].message`,
      ),
      ...(input.stepId === undefined
        ? {}
        : {
            stepId: assertString(
              input.stepId,
              `events[${index.toString()}].stepId`,
            ),
          }),
    };
  });
};

export const parseDeploymentRunState = (value: unknown): DeploymentRunState => {
  const input = assertRecord(value, "run state");
  if (input.schemaVersion !== DEPLOYMENT_RUN_STATE_SCHEMA_VERSION) {
    throw new RunStateError(
      `Unsupported run-state schemaVersion: ${String(input.schemaVersion)}`,
    );
  }
  const stepsInput = assertRecord(input.steps, "steps");
  const steps = Object.fromEntries(
    Object.entries(stepsInput).map(([stepId, step]) => [
      stepId,
      parseStep(step),
    ]),
  );
  return {
    schemaVersion: DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
    runId: assertString(input.runId, "runId"),
    createdAt: assertIsoString(input.createdAt, "createdAt"),
    updatedAt: assertIsoString(input.updatedAt, "updatedAt"),
    mode: parseMode(input.mode),
    identity: parseIdentity(input.identity),
    steps,
    events: parseEvents(input.events),
  };
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
  return {
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
  };
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
  return {
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
  };
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
  await mkdir(dirname(path), { recursive: true });
  const tmpPath = `${path}.tmp-${process.pid.toString()}-${Date.now().toString()}`;
  await writeFile(tmpPath, `${JSON.stringify(normalized, null, 2)}\n`, "utf8");
  await rename(tmpPath, path);
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

export const mutateDeploymentRunState = async (
  path: string,
  createInitial: () => DeploymentRunState,
  mutate: (
    state: DeploymentRunState,
  ) => DeploymentRunState | Promise<DeploymentRunState>,
): Promise<DeploymentRunState> =>
  withDeploymentRunStateLock(path, async () => {
    const current = (await loadDeploymentRunState(path)) ?? createInitial();
    const next = await mutate(current);
    await writeDeploymentRunStateAtomic(path, next);
    return next;
  });
