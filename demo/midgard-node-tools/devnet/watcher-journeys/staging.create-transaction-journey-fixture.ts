import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixture,
} from "./fixture.js";
import { stackStagingShiftCover } from "./journey-timing.js";
import {
  type FaultPreparation,
  type JourneyFaultBuildInput,
  type JourneyFaultPreparationInput,
  type JourneyPreparedFault,
} from "./staging.decode-journey-retained-block.js";
import { stageJourney } from "./staging.stage-journey.js";

/**
 * Deposit-backed transaction fixtures share this real operator staging path.
 * A completed journey hands off its retained healthy tail to the next family.
 * Only the fault constructor varies; it cannot launch or direct the watcher.
 */
export const createTransactionJourneyFixture = (
  category: JourneyCategory,
  buildFault: (input: JourneyFaultBuildInput) => Promise<JourneyBlock>,
): JourneyFixture =>
  createPreparedJourneyFixture(category, async () => ({ buildFault }));

/** Prepare required L1 actions before choosing the next commitment interval. */
export const createPreparedJourneyFixture = (
  category: JourneyCategory,
  prepare: (
    input: JourneyFaultPreparationInput,
  ) => Promise<JourneyPreparedFault>,
  options: Pick<FaultPreparation, "historyBlocks" | "userEvents"> = {},
): JourneyFixture => {
  // On an attached stack, its staging asks operator-2's shift for this cover
  // and its time limit is derived from it. A user event's protection is the
  // longer of the two histories'.
  const stackShiftCoverMs: FaultPreparation["stackShiftCoverMs"] = (input) =>
    stackStagingShiftCover({
      ...input,
      eventHistoryProtectionMs: Math.max(
        input.eventHistoryProtectionMs.deposit,
        input.eventHistoryProtectionMs.withdrawal,
      ),
      category,
      historyBlocks: options.historyBlocks ?? 0,
      userEvents: options.userEvents ?? 0,
    });
  return {
    category,
    stackShiftCoverMs,
    stage: (input) =>
      stageJourney(input, { ...options, category, prepare, stackShiftCoverMs }),
  };
};
