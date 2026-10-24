import { asObject, assertExactKeys } from "./artifact-fields.js";
import { type StressWalletConsolidationReadinessResponse } from "./types.js";

export type ConsolidationReadinessSnapshot = {
  readonly httpStatus: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly durableAdmissionBacklog: number;
  readonly mempoolTxCount: number;
  readonly unfinishedLocalMutationJobs: number;
  readonly unresolvedBlockSubmissionAgeMs: number;
  readonly providerQueryHealthy: boolean;
  readonly leaseStatus: string;
  readonly pendingFinalizationCount: number;
  readonly commitWorkerActive: boolean;
  readonly commitPipelinePhase: string;
};

const requiredReadinessCount = (value: unknown, fieldName: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string" && /^\d+$/.test(value)
        ? Number(value)
        : Number.NaN;
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(
      "Malformed consolidation readiness response: " +
        fieldName +
        " must be a non-negative integer.",
    );
  }
  return parsed;
};

export const parseConsolidationReadiness = (
  response: StressWalletConsolidationReadinessResponse,
): ConsolidationReadinessSnapshot => {
  if (!Number.isSafeInteger(response.httpStatus)) {
    throw new Error(
      "Malformed consolidation readiness response: httpStatus must be an integer.",
    );
  }
  const body = asObject(response.body, "consolidation readiness body");
  try {
    assertExactKeys(body, "consolidation readiness body", [
      "ready",
      "reasons",
      "durableAdmissionBacklog",
      "mempoolTxCount",
      "unfinishedLocalMutationJobs",
      "unresolvedBlockSubmissionAgeMs",
      "providerQueryHealthy",
      "stateQueueMutationLease",
      "blockCommitmentCoordination",
    ]);
  } catch (cause) {
    throw new Error(
      `Malformed consolidation readiness response: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
  }
  if (typeof body.ready !== "boolean") {
    throw new Error(
      "Malformed consolidation readiness response: ready must be boolean.",
    );
  }
  if (
    !Array.isArray(body.reasons) ||
    !body.reasons.every((reason) => typeof reason === "string")
  ) {
    throw new Error(
      "Malformed consolidation readiness response: reasons must be a string array.",
    );
  }
  if (typeof body.providerQueryHealthy !== "boolean") {
    throw new Error(
      "Malformed consolidation readiness response: providerQueryHealthy must be boolean.",
    );
  }
  const lease = asObject(
    body.stateQueueMutationLease,
    "stateQueueMutationLease",
  );
  const coordination = asObject(
    body.blockCommitmentCoordination,
    "blockCommitmentCoordination",
  );
  assertExactKeys(lease, "stateQueueMutationLease", [
    "status",
    "pendingFinalizations",
  ]);
  assertExactKeys(coordination, "blockCommitmentCoordination", [
    "commitWorkerActive",
    "commitPipelinePhase",
  ]);
  if (
    typeof lease.status !== "string" ||
    !Array.isArray(lease.pendingFinalizations)
  ) {
    throw new Error(
      "Malformed consolidation readiness response: lease status/pendingFinalizations are invalid.",
    );
  }
  if (
    typeof coordination.commitWorkerActive !== "boolean" ||
    typeof coordination.commitPipelinePhase !== "string"
  ) {
    throw new Error(
      "Malformed consolidation readiness response: commit coordination is invalid.",
    );
  }
  return {
    httpStatus: response.httpStatus,
    ready: body.ready,
    reasons: body.reasons,
    durableAdmissionBacklog: requiredReadinessCount(
      body.durableAdmissionBacklog,
      "durableAdmissionBacklog",
    ),
    mempoolTxCount: requiredReadinessCount(
      body.mempoolTxCount,
      "mempoolTxCount",
    ),
    unfinishedLocalMutationJobs: requiredReadinessCount(
      body.unfinishedLocalMutationJobs,
      "unfinishedLocalMutationJobs",
    ),
    unresolvedBlockSubmissionAgeMs: requiredReadinessCount(
      body.unresolvedBlockSubmissionAgeMs,
      "unresolvedBlockSubmissionAgeMs",
    ),
    providerQueryHealthy: body.providerQueryHealthy,
    leaseStatus: lease.status,
    pendingFinalizationCount: lease.pendingFinalizations.length,
    commitWorkerActive: coordination.commitWorkerActive,
    commitPipelinePhase: coordination.commitPipelinePhase,
  };
};

export const defaultFetchConsolidationReadiness = async (
  nodeEndpoint: string,
  requestTimeoutMs: number,
): Promise<StressWalletConsolidationReadinessResponse> => {
  const response = await fetch(nodeEndpoint + "/readyz", {
    signal: AbortSignal.timeout(requestTimeoutMs),
  });
  const responseText = await response.text();
  let body: unknown;
  try {
    body = JSON.parse(responseText) as unknown;
  } catch {
    body = responseText;
  }
  return { httpStatus: response.status, body };
};

export const isFullConsolidationReadiness = (
  snapshot: ConsolidationReadinessSnapshot,
): boolean =>
  snapshot.httpStatus === 200 &&
  snapshot.ready &&
  snapshot.reasons.length === 0 &&
  snapshot.durableAdmissionBacklog === 0 &&
  snapshot.mempoolTxCount === 0 &&
  snapshot.unfinishedLocalMutationJobs === 0 &&
  snapshot.unresolvedBlockSubmissionAgeMs === 0 &&
  snapshot.providerQueryHealthy &&
  snapshot.leaseStatus === "idle" &&
  snapshot.pendingFinalizationCount === 0 &&
  !snapshot.commitWorkerActive &&
  snapshot.commitPipelinePhase === "idle";
