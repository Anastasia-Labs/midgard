import type {
  FraudProofWorkflowNamedState,
  FraudProofWorkflowRunResult,
} from "./orchestrator.fraud-proof-workflow-run-result.js";

/**
 * A family could not build or check its prepared artifact for a reason that
 * says nothing about the fault, such as its evaluator failing without a
 * result. The run journals nothing for it and hands its turn back pending,
 * to build the artifact again on the next observation; `namedState`, when
 * given, is the readiness state it is held under meanwhile.
 */
export class WorkflowArtifactPendingError extends Error {
  readonly _tag = "WorkflowArtifactPendingError";

  constructor(
    message: string,
    readonly namedState?: FraudProofWorkflowNamedState,
    options?: { readonly cause?: unknown },
  ) {
    super(message, options);
    this.name = "WorkflowArtifactPendingError";
  }
}

/** Matched by tag: the package ships more than one entry bundle. */
export const isWorkflowArtifactPendingError = (
  error: unknown,
): error is WorkflowArtifactPendingError =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag ===
    "WorkflowArtifactPendingError";

/**
 * The pending result of a run whose artifact build met `cause`, a
 * `WorkflowArtifactPendingError`; any other cause is rethrown.
 */
export const artifactPendingResult = (
  cause: unknown,
  resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult,
): FraudProofWorkflowRunResult => {
  if (!isWorkflowArtifactPendingError(cause)) throw cause;
  const result = resumeOnObservation(cause.message);
  return cause.namedState === undefined || result.kind !== "pending"
    ? result
    : Object.freeze({ ...result, namedState: cause.namedState });
};
