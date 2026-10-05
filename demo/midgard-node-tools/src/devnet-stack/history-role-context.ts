import { parseWatcherStrictJsonValue } from "midgard-watcher";

import { CHILD_STATUS_ATTEMPT_ENV } from "./child-status-channel.js";
import {
  HISTORY_CHILD_ROLES,
  type HistoryChildRole,
  parseHistoryChildActor,
} from "./history-child-evidence.js";
import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import type { RunEnv } from "./layout.js";
import type { ServiceRecoveryScope } from "./service-recovery-scope.js";

export const HISTORY_ROLE_CONTEXT_ENV = "MIDGARD_DEVNET_HISTORY_PROOF_CONTEXT";
export type HistoryReadinessSpecification = Readonly<{
  role: HistoryChildRole;
  runId: string;
  deploymentFingerprint: string;
  publicBindingDigest: string;
  expectedNetwork: "Custom" | "Preprod";
}>;
const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const exact = (value: Record<string, unknown>, fields: readonly string[]) =>
  Object.keys(value).sort().join() === [...fields].sort().join();
const hex = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);
export const parseHistoryReadinessSpecification = (
  value: unknown,
): HistoryReadinessSpecification | null => {
  if (
    !object(value) ||
    !exact(value, [
      "role",
      "runId",
      "deploymentFingerprint",
      "publicBindingDigest",
      "expectedNetwork",
    ]) ||
    typeof value.runId !== "string" ||
    value.runId.length === 0 ||
    value.runId.length > 200 ||
    !hex(value.deploymentFingerprint) ||
    !hex(value.publicBindingDigest) ||
    (value.expectedNetwork !== "Custom" && value.expectedNetwork !== "Preprod")
  )
    return null;
  const role = HISTORY_CHILD_ROLES.find(
    (candidate) => candidate === value.role,
  );
  return role === undefined
    ? null
    : {
        role,
        runId: value.runId,
        deploymentFingerprint: value.deploymentFingerprint,
        publicBindingDigest: value.publicBindingDigest,
        expectedNetwork: value.expectedNetwork,
      };
};

/** Declarative scope only; the actual child still freshly verifies recorded authority. */
export const historyChildEnvironment = (
  specification: HistoryReadinessSpecification,
  scope: ServiceRecoveryScope,
  attemptId: string,
) => ({
  [CHILD_STATUS_ATTEMPT_ENV]: attemptId,
  [HISTORY_ROLE_CONTEXT_ENV]: JSON.stringify({ specification, ...scope }),
});

/** Never substitutes a response/endpoint identity for the inherited actor. */
export const readHistoryChildContext = (
  role: HistoryChildRole,
  run: RunEnv,
  env: NodeJS.ProcessEnv = process.env,
) => {
  const refuse = (): never => {
    throw new HistoryConfigurationRefusal(
      "history child recorded scope is absent or invalid",
    );
  };
  const text = env[HISTORY_ROLE_CONTEXT_ENV];
  if (text === undefined || text.length > 4096) return refuse();
  let value: unknown;
  try {
    value = parseWatcherStrictJsonValue(text);
  } catch {
    return refuse();
  }
  if (
    !object(value) ||
    !exact(value, ["specification", "codeStamp", "serviceSpecsDigest"])
  )
    return refuse();
  const specification = parseHistoryReadinessSpecification(value.specification);
  if (
    specification === null ||
    specification.role !== role ||
    specification.runId !== run.runId
  )
    return refuse();
  const actor = parseHistoryChildActor({
    role,
    runId: run.runId,
    deploymentFingerprint: specification.deploymentFingerprint,
    codeStamp: value.codeStamp,
    serviceSpecsDigest: value.serviceSpecsDigest,
    attemptId: env[CHILD_STATUS_ATTEMPT_ENV],
    childPid: process.pid,
  });
  if (actor === null) return refuse();
  return { specification, actor };
};
