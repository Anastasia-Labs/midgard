import { join } from "node:path";

/**
 * Whether a staged fault checkpoint can still be resumed.
 *
 * A checkpoint that never reached the chain (no commit hash, no signed commit,
 * no header on the queue) binds the predecessor it was built on. When that
 * predecessor is no longer the state queue tail, another journey's correction
 * appended a healthy successor in between, so the build can never be
 * committed: the journey must archive it and stage a fresh fault on the
 * current head. Every other checkpoint resumes, and the resume path verifies
 * the chain itself.
 */
export type StagedCheckpointDisposition = "resume" | "superseded";

export const classifyStagedCheckpoint = (input: {
  readonly checkpoint: {
    readonly commitTxHash?: string;
    readonly signedCommit?: unknown;
  };
  readonly faultHeaderOnQueue: boolean;
  readonly predecessorIsTail: boolean;
}): StagedCheckpointDisposition =>
  input.checkpoint.commitTxHash === undefined &&
  input.checkpoint.signedCommit === undefined &&
  !input.faultHeaderOnQueue &&
  !input.predecessorIsTail
    ? "superseded"
    : "resume";

/** Where a superseded checkpoint is kept; journey state is never discarded. */
export const supersededCheckpointArchivePath = (
  directory: string,
  now: Date,
): string =>
  join(
    directory,
    "archive",
    `staged-superseded-${now.toISOString().replace(/[:.]/g, "-")}.json`,
  );
