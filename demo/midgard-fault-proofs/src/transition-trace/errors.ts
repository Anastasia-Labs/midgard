export type TransitionTraceChallengerErrorCode =
  | "malformedPayload"
  | "nonCanonicalPayload"
  | "wrongPayloadVersion"
  | "invalidPayloadEntries"
  | "headerMismatch"
  | "rootMismatch"
  | "countMismatch"
  | "missingWitnessData"
  | "unsupportedWitness"
  | "proofConstructionFailed"
  | "fetchFailed"
  | "submissionRejected";

export class TransitionTraceChallengerError extends Error {
  readonly code: TransitionTraceChallengerErrorCode;
  readonly cause?: unknown;

  constructor(
    code: TransitionTraceChallengerErrorCode,
    message: string,
    options?: { readonly cause?: unknown },
  ) {
    super(message);
    this.name = "TransitionTraceChallengerError";
    this.code = code;
    this.cause = options?.cause;
  }
}

export const transitionTraceError = (
  code: TransitionTraceChallengerErrorCode,
  message: string,
  cause?: unknown,
): TransitionTraceChallengerError =>
  new TransitionTraceChallengerError(
    code,
    message,
    cause === undefined ? undefined : { cause },
  );

export const formatCause = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);
