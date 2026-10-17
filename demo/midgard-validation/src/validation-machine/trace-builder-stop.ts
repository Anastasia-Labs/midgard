import type { RejectCode } from "../types.js";

/** A process-local trace alarm carries the existing classification unchanged. */
export class ValidationTraceStopped extends Error {
  readonly _tag = "ValidationTraceStopped";
  readonly retryable = false;
  readonly cause: unknown;
  readonly committedVerdict: "accepted" | "rejected";
  readonly committedRejectionCode: RejectCode | null;
  constructor(
    readonly reason: "disagreement" | "unavailable",
    classification: {
      readonly expectedVerdict: "accepted" | "rejected";
      readonly expectedRejectionCode: RejectCode | null;
    },
    message: string,
    options?: { readonly cause?: unknown },
  ) {
    super(message);
    this.cause = options?.cause;
    this.committedVerdict = classification.expectedVerdict;
    this.committedRejectionCode = classification.expectedRejectionCode;
    this.name = "ValidationTraceStopped";
  }
}
