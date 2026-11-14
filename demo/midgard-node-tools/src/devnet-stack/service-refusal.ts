import { randomUUID } from "node:crypto";
import {
  closeSync,
  existsSync,
  fsyncSync,
  openSync,
  unlinkSync,
} from "node:fs";
import { dirname, join } from "node:path";

import { readJsonIfPresent, writeDurableJson } from "./durable.js";
import {
  recoveryScope,
  recoveryScopeMatches,
  type ServiceRecoveryScope,
} from "./service-recovery-scope.js";
import type { SupervisorPaths } from "./supervisor.js";

/** EX_CONFIG: normal process retries cannot correct a configuration refusal. */
export const CONFIG_REFUSAL_EXIT_CODE = 78;
export type ServiceRefusal = Readonly<{
  runDir: string;
  service: string;
  refusalId: string;
  refusedAt: string;
  exitCode: 78;
  logPath: string;
  /** Allowlisted public deployment-manifest pin, never a secret fingerprint. */
  deploymentBinding?: string;
}>;
export type RecoveryRequest = ServiceRecoveryScope &
  Readonly<{
    runDir: string;
    service: string;
    refusalId: string;
    requestedAt: string;
    deploymentBinding?: string;
    /** Operator explanation only, never verified chain/configuration evidence. */
    note: string;
  }>;
const removeDurably = (path: string) => {
  unlinkSync(path);
  const fd = openSync(dirname(path), "r");
  try {
    fsyncSync(fd);
  } finally {
    closeSync(fd);
  }
};
const files = (paths: SupervisorPaths, service: string) => {
  if (!/^[a-z0-9][a-z0-9-]*$/u.test(service))
    throw new Error("invalid service name for refusal record");
  const directory = join(paths.pidDir, "refused");
  return {
    refusal: join(directory, `${service}.json`),
    request: join(directory, `${service}.retry.json`),
  };
};
export const serviceRefusal = (
  paths: SupervisorPaths,
  service: string,
): ServiceRefusal | undefined => {
  const record = readJsonIfPresent<ServiceRefusal>(
    files(paths, service).refusal,
  );
  if (
    record !== undefined &&
    (record.runDir !== paths.runDir ||
      record.service !== service ||
      record.exitCode !== 78 ||
      typeof record.refusalId !== "string" ||
      record.refusalId.length === 0)
  )
    throw new Error(
      `invalid refusal record for ${service}; repair its recorded metadata explicitly`,
    );
  return record;
};
export const refuseService = (
  paths: SupervisorPaths,
  service: string,
): ServiceRefusal => {
  const record: ServiceRefusal = {
    runDir: paths.runDir,
    service,
    refusalId: randomUUID(),
    refusedAt: new Date().toISOString(),
    exitCode: CONFIG_REFUSAL_EXIT_CODE,
    logPath: paths.serviceLog(service),
    deploymentBinding: paths.deploymentBinding,
  };
  writeDurableJson(files(paths, service).refusal, record);
  return record;
};
/** A single scoped reattempt, not permission to rewrite/clear integrity state. */
export const requestServiceRecovery = (
  paths: SupervisorPaths,
  service: string,
  refusalId: string,
  note: string,
): void => {
  const record = serviceRefusal(paths, service);
  if (record === undefined || record.refusalId !== refusalId)
    throw new Error(`${service}: refusal token does not match current refusal`);
  if (record.deploymentBinding !== paths.deploymentBinding)
    throw new Error(
      "deployment manifest changed; scoped recovery cannot adopt another deployment",
    );
  if (note.trim().length === 0 || note.length > 1_000)
    throw new Error("recovery note must contain 1 to 1000 characters");
  const scope = recoveryScope(paths);
  if (scope === undefined)
    throw new Error(
      "recovery requires current runtime code and exact service set",
    );
  writeDurableJson(files(paths, service).request, {
    ...scope,
    runDir: paths.runDir,
    service,
    refusalId,
    requestedAt: new Date().toISOString(),
    deploymentBinding: paths.deploymentBinding,
    note,
  } satisfies RecoveryRequest);
};
/** Consume permission before spawning; the refusal survives until readiness. */
export const consumeServiceRecovery = (
  paths: SupervisorPaths,
  record: ServiceRefusal,
): RecoveryRequest | undefined => {
  const path = files(paths, record.service).request;
  const request = readJsonIfPresent<RecoveryRequest>(path);
  if (request === undefined) return undefined;
  // A stale/mismatched request never retries a newer refusal.
  removeDurably(path);
  return request.runDir === paths.runDir &&
    request.service === record.service &&
    request.refusalId === record.refusalId &&
    request.deploymentBinding === record.deploymentBinding &&
    record.deploymentBinding === paths.deploymentBinding &&
    recoveryScopeMatches(request, recoveryScope(paths)) &&
    typeof request.note === "string" &&
    request.note.trim().length > 0 &&
    request.note.length <= 1_000
    ? request
    : undefined;
};
export const clearServiceRefusal = (
  paths: SupervisorPaths,
  record: ServiceRefusal,
): boolean => {
  if (serviceRefusal(paths, record.service)?.refusalId !== record.refusalId)
    return false;
  const pathsFor = files(paths, record.service);
  removeDurably(pathsFor.refusal);
  if (existsSync(pathsFor.request)) removeDurably(pathsFor.request);
  return true;
};
export const readinessAnswered = (body: string): boolean => {
  try {
    const parsed = JSON.parse(body) as { ready?: unknown; readiness?: unknown };
    return parsed.ready === true || parsed.readiness === "ready";
  } catch {
    return false;
  }
};
