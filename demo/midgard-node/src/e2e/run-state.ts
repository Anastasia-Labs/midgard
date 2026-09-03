import { createHash, randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import { type FileHandle, mkdir, open, readFile, rm } from "node:fs/promises";
import { dirname, resolve as resolvePath } from "node:path";

import {
  type DeploymentMarkerV1,
  parseDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";

import { exactRecord } from "../artifact-schema.js";
import { writeJsonFileAtomic } from "../files/atomic-write.js";

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
  readonly deploymentMarker?: DeploymentMarkerV1;
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
  readonly details?: Readonly<Record<string, string>>;
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

const exactRunStateRecord = (
  value: unknown,
  label: string,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[] = [],
): Record<string, unknown> => {
  try {
    return exactRecord(value, label, requiredKeys, optionalKeys);
  } catch (cause) {
    throw new RunStateError(
      cause instanceof Error ? cause.message : String(cause),
      { cause },
    );
  }
};

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
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value !== value.trim()
  ) {
    throw new RunStateError(
      `${label} must be a canonical non-empty string without surrounding whitespace.`,
    );
  }
  return value;
};

const assertLowerHex = (
  value: unknown,
  label: string,
  byteLength: number,
): string => {
  const parsed = assertString(value, label);
  if (parsed.length !== byteLength * 2 || !/^[0-9a-f]+$/u.test(parsed)) {
    throw new RunStateError(
      `${label} must be ${byteLength.toString()} bytes of lowercase hexadecimal.`,
    );
  }
  return parsed;
};

const assertIsoString = (value: unknown, label: string): string => {
  const text = assertString(value, label);
  if (Number.isNaN(Date.parse(text)) || new Date(text).toISOString() !== text) {
    throw new RunStateError(`${label} must be a canonical ISO timestamp.`);
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
  const parsed = value.map((entry, index) =>
    assertString(entry, `${label}[${index.toString()}]`),
  );
  if (new Set(parsed).size !== parsed.length) {
    throw new RunStateError(`${label} must not contain duplicates.`);
  }
  return parsed;
};

const assertStringRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, string>> | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const input = assertRecord(value, label);
  for (const [key, entry] of Object.entries(input)) {
    if (typeof entry !== "string") {
      throw new RunStateError(`${label}.${key} must be a string.`);
    }
  }
  return input as Readonly<Record<string, string>>;
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

export const parseDeploymentRunIdentityV1 = (
  value: unknown,
): DeploymentRunIdentity => {
  const input = exactRunStateRecord(
    value,
    "identity",
    [],
    [
      "network",
      "hubOracleOneShot",
      "referenceScriptAuthPolicyId",
      "referenceScriptAuthPolicy",
      "manifestPath",
      "manifestSha256",
      "deploymentMarker",
    ],
  );
  const hubOracleOneShot =
    input.hubOracleOneShot === undefined
      ? undefined
      : exactRunStateRecord(
          input.hubOracleOneShot,
          "identity.hubOracleOneShot",
          ["txHash", "outputIndex"],
        );
  const referenceScriptAuthPolicy =
    input.referenceScriptAuthPolicy === undefined
      ? undefined
      : exactRunStateRecord(
          input.referenceScriptAuthPolicy,
          "identity.referenceScriptAuthPolicy",
          ["policyId", "nativeScript"],
        );
  const referenceScriptAuthPolicyNativeScript =
    referenceScriptAuthPolicy === undefined
      ? undefined
      : exactRunStateRecord(
          referenceScriptAuthPolicy.nativeScript,
          "identity.referenceScriptAuthPolicy.nativeScript",
          [
            "type",
            "cborHex",
            "expiresAtSlot",
            "expiresAtUnixTime",
            "timelockDurationMs",
          ],
        );
  const parsed: DeploymentRunIdentity = {
    ...(input.network === undefined
      ? {}
      : { network: assertString(input.network, "identity.network") }),
    ...(hubOracleOneShot === undefined
      ? {}
      : {
          hubOracleOneShot: {
            txHash: assertLowerHex(
              hubOracleOneShot.txHash,
              "identity.hubOracleOneShot.txHash",
              32,
            ),
            outputIndex:
              typeof hubOracleOneShot.outputIndex === "number" &&
              Number.isSafeInteger(hubOracleOneShot.outputIndex) &&
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
          referenceScriptAuthPolicyId: assertLowerHex(
            input.referenceScriptAuthPolicyId,
            "identity.referenceScriptAuthPolicyId",
            28,
          ),
        }),
    ...(referenceScriptAuthPolicy === undefined
      ? {}
      : {
          referenceScriptAuthPolicy: {
            policyId: assertLowerHex(
              referenceScriptAuthPolicy.policyId,
              "identity.referenceScriptAuthPolicy.policyId",
              28,
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
              cborHex: (() => {
                const cborHex = assertString(
                  referenceScriptAuthPolicyNativeScript.cborHex,
                  "identity.referenceScriptAuthPolicy.nativeScript.cborHex",
                );
                if (cborHex.length % 2 !== 0 || !/^[0-9a-f]+$/u.test(cborHex)) {
                  throw new RunStateError(
                    "identity.referenceScriptAuthPolicy.nativeScript.cborHex must be non-empty even-length lowercase hexadecimal.",
                  );
                }
                return cborHex;
              })(),
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
          manifestSha256: assertLowerHex(
            input.manifestSha256,
            "identity.manifestSha256",
            32,
          ),
        }),
    ...(input.deploymentMarker === undefined
      ? {}
      : {
          deploymentMarker: (() => {
            try {
              return parseDeploymentMarkerV1(input.deploymentMarker);
            } catch (cause) {
              throw new RunStateError(
                `identity.deploymentMarker is invalid: ${
                  cause instanceof Error ? cause.message : String(cause)
                }`,
                { cause },
              );
            }
          })(),
        }),
  };
  if (
    parsed.referenceScriptAuthPolicyId !== undefined &&
    parsed.referenceScriptAuthPolicy !== undefined &&
    parsed.referenceScriptAuthPolicyId !==
      parsed.referenceScriptAuthPolicy.policyId
  ) {
    throw new RunStateError(
      "identity reference-script policy identifiers are inconsistent.",
    );
  }
  return parsed;
};

export const bindDeploymentRunStateToMarkerV1 = (
  state: DeploymentRunState,
  {
    marker,
    manifestPath,
    manifestSha256,
    now = new Date(),
  }: {
    readonly marker: DeploymentMarkerV1;
    readonly manifestPath: string;
    readonly manifestSha256: string;
    readonly now?: Date;
  },
): DeploymentRunState => {
  const canonicalMarker = (() => {
    try {
      return parseDeploymentMarkerV1(marker);
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

export const parseDeploymentStepStateV1 = (
  value: unknown,
  label = "step",
): DeploymentStepState => {
  const input = exactRunStateRecord(
    value,
    label,
    ["status", "updatedAt"],
    ["txHashes", "outRefs", "message", "evidence", "details"],
  );
  const parsed: DeploymentStepState = {
    status: parseStepStatus(input.status),
    updatedAt: assertIsoString(input.updatedAt, `${label}.updatedAt`),
    ...(input.txHashes === undefined
      ? {}
      : {
          txHashes: assertStringArray(input.txHashes, `${label}.txHashes`),
        }),
    ...(input.outRefs === undefined
      ? {}
      : { outRefs: assertStringArray(input.outRefs, `${label}.outRefs`) }),
    ...(input.message === undefined
      ? {}
      : { message: assertString(input.message, `${label}.message`) }),
    ...(input.evidence === undefined
      ? {}
      : {
          evidence: assertStringArray(input.evidence, `${label}.evidence`),
        }),
    ...(input.details === undefined
      ? {}
      : { details: assertStringRecord(input.details, `${label}.details`) }),
  };
  for (const [index, txHash] of (parsed.txHashes ?? []).entries()) {
    assertLowerHex(txHash, `${label}.txHashes[${index.toString()}]`, 32);
  }
  for (const [index, outRef] of (parsed.outRefs ?? []).entries()) {
    if (!/^[0-9a-f]{64}#(0|[1-9]\d*)$/u.test(outRef)) {
      throw new RunStateError(
        `${label}.outRefs[${index.toString()}] must be a canonical transaction output reference.`,
      );
    }
  }
  return parsed;
};

export const parseDeploymentRunEventV1 = (
  value: unknown,
  label = "event",
): DeploymentRunEvent => {
  const input = exactRunStateRecord(
    value,
    label,
    ["at", "kind", "message"],
    ["stepId"],
  );
  return {
    at: assertIsoString(input.at, `${label}.at`),
    kind:
      input.kind === "created" || input.kind === "step_transition"
        ? input.kind
        : (() => {
            throw new RunStateError(
              `${label}.kind must be created or step_transition.`,
            );
          })(),
    message: assertString(input.message, `${label}.message`),
    ...(input.stepId === undefined
      ? {}
      : {
          stepId: assertString(input.stepId, `${label}.stepId`),
        }),
  };
};

const parseEvents = (value: unknown): readonly DeploymentRunEvent[] => {
  if (!Array.isArray(value)) {
    throw new RunStateError("events must be an array.");
  }
  return value.map((entry, index) =>
    parseDeploymentRunEventV1(entry, `events[${index.toString()}]`),
  );
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
      parseDeploymentStepStateV1(step, `steps.${stepId}`),
    ]),
  );
  const parsed: DeploymentRunState = {
    schemaVersion: DEPLOYMENT_RUN_STATE_SCHEMA_VERSION,
    runId: assertString(input.runId, "runId"),
    createdAt: assertIsoString(input.createdAt, "createdAt"),
    updatedAt: assertIsoString(input.updatedAt, "updatedAt"),
    mode: parseMode(input.mode),
    identity: parseDeploymentRunIdentityV1(input.identity),
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
