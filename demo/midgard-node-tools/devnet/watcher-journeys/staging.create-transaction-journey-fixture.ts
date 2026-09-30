import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixture,
  JourneyFixtureStage,
  JourneySuccessor,
} from "./fixture.js";
import {
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
): JourneyFixture => ({
  category,
  stage: (input) => stageJourney(input, { mode: "fault", prepare }),
});

/** Stage genuine long-lived prerequisites without constructing a fault. */
export const prepareJourneyHistory = (
  input: JourneyFixtureStage,
  prepare: (input: JourneyFaultPreparationInput) => Promise<void>,
): Promise<JourneySuccessor> =>
  stageJourney(input, { mode: "history", prepare });
