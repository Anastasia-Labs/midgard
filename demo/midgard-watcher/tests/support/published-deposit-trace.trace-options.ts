import type { DaLocalSignerConfig } from "midgard-node/da/local-signers";

import type { PublishedWatcherBlock } from "./published-block-actor.js";
import { sharedShiftWork } from "./published-block-actor.operator-shift.js";
import type { PublishedDepositTraceCheckpoint } from "./published-deposit-trace.published-deposit-trace-checkpoint.js";
import type { PublishedDepositTraceLiveTail } from "./published-deposit-trace.queue-tail.js";

/** The DA path of a trace staged on an attached stack. */
export type PublishedDepositTraceAttachedStack = Readonly<{
  /** Follow a committed block's payload to the stack committee's answer. */
  attest(block: PublishedWatcherBlock): Promise<{ readonly kind: string }>;
  /**
   * The trace's work in operator-2's shift, from its shift wait to the end
   * of its fault's commit; derived from the stack's timing.
   */
  shiftWorkMs: number;
  /** Before each attempt to commit the fault: its late-shift wait. */
  beforeFaultCommit(operatorVkey: string): Promise<void>;
}>;

/** An attached trace's work in operator-2's shift, from its shift wait. */
export const attachedTraceShiftWork = (
  stack: PublishedDepositTraceAttachedStack,
) => sharedShiftWork(stack.shiftWorkMs);

export type PublishedDepositTraceOptions = {
  /** The journey's own committee signers; absent on an attached stack. */
  daSignerConfig?: DaLocalSignerConfig;
  /**
   * An attached stack, whose own committee attests the trace's blocks and
   * whose scheduler the operator shares with the stack's node. The trace
   * then extends its live tail within one shift of the operator's.
   */
  attachedStack?: PublishedDepositTraceAttachedStack;
  honest?: boolean;
  onStage?: (name: string) => void;
  resume?: PublishedDepositTraceCheckpoint;
  onCheckpoint?: (checkpoint: PublishedDepositTraceCheckpoint) => Promise<void>;
  /** Durable journal of abandoned-header removals; in memory when absent. */
  timeoutCorrectionJournalPath?: string;
  /** Blocks a removal must be buried under before the watcher may start. */
  finalityDepth?: number;
  /** Absent, abandoned headers are removed and the trace starts at the confirmed state. */
  liveTail?: PublishedDepositTraceLiveTail;
};

/**
 * A trace is attested by exactly one of its own committee or an attached
 * stack's, and an attached stack's trace extends its live tail.
 */
export const assertPublishedDepositTraceDaPath = ({
  daSignerConfig,
  attachedStack,
  liveTail,
}: Pick<
  PublishedDepositTraceOptions,
  "daSignerConfig" | "attachedStack" | "liveTail"
>) => {
  if ((daSignerConfig === undefined) === (attachedStack === undefined))
    throw new Error(
      "A deposit trace is attested by its own committee or an attached stack's",
    );
  if (attachedStack !== undefined && liveTail === undefined)
    throw new Error("An attached stack's deposit trace extends its live tail");
};
