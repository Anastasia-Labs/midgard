import { type Effect } from "effect";

export type CommitSubmissionHooks = {
  /** Reports exact DA admission before journal preparation or submission. */
  readonly afterDaFrameAccepted?: Effect.Effect<void>;
};
