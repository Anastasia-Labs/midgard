/** The foreign replayer's verdicts (`ReplayOutcome` kinds other than `replayed`). */
import type { ReplayOutcome } from "./replay.js";

/** A block's verdict, carried through the replay's error channel. */
export class Verdict extends Error {
  constructor(
    readonly kind: Exclude<ReplayOutcome["kind"], "replayed">,
    readonly detail: string,
  ) {
    super(detail);
  }
}

export const missing = (detail: string) => new Verdict("missing", detail);
export const unknownEvent = (detail: string) =>
  new Verdict("event_unknown", detail);
export const invalid = (detail: string) => new Verdict("invalid", detail);
