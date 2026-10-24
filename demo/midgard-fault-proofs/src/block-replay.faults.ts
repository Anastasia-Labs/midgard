/**
 * How a block replay's failures are attributed.
 *
 * - `BlockReplayFault`: the block's payload disagrees with what its replay
 *   reproduces. The block is at fault.
 * - `ReplayLocalFault`: the replaying process itself failed (a runtime
 *   fault of a library it calls, or its own bookkeeping), on input already
 *   known to be within what that code handles. Never the block's fault: the
 *   replay is retried. One marked `held` (the event replay's own throw) is
 *   held on its block and retried for as long as it recurs, never ending
 *   the replaying process.
 *
 * Every other throw while replaying stays attributed as before: it may come
 * from parsing bytes the block or its L1 events supply, so it is never
 * taken for a local fault. A local fault is named only where it arises.
 */
import { Data } from "effect";

export class BlockReplayFault extends Data.TaggedError("BlockReplayFault")<{
  readonly message: string;
}> {}

export class ReplayLocalFault extends Data.TaggedError("ReplayLocalFault")<{
  readonly message: string;
  readonly cause: unknown;
  /** Held on its block and retried on a backoff for as long as it recurs;
   * never bounded by an exit of the replaying process. */
  readonly held?: true;
}> {}

/** The block's fault `message`, to throw. */
export const blockFault = (message: string): BlockReplayFault =>
  new BlockReplayFault({ message });

const hasTag = (error: unknown, tag: string): boolean =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag === tag;

/** Whether `error` is a local fault (`ReplayLocalFault`). Matched by tag:
 * the package ships more than one entry bundle. */
export const isReplayLocalFault = (error: unknown): error is ReplayLocalFault =>
  hasTag(error, "ReplayLocalFault");

/** Whether `error` is the block's fault (`BlockReplayFault`). */
export const isBlockReplayFault = (error: unknown): error is BlockReplayFault =>
  hasTag(error, "BlockReplayFault");

/** `run()`, with any throw named a local fault of `what`. */
export const locally = <A>(what: string, run: () => A): A => {
  try {
    return run();
  } catch (cause) {
    if (isReplayLocalFault(cause)) throw cause;
    throw new ReplayLocalFault({
      message: `${what} failed locally: ${String(cause)}`,
      cause,
    });
  }
};
