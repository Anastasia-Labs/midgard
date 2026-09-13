import { it } from "vitest";

import { selectJourneyFixtures } from "./catalogue.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import { readJourneyExecutionTiming } from "./journey-timing.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const fixtures = selectJourneyFixtures(
  process.env.MIDGARD_WATCHER_JOURNEY_CATEGORIES,
);

// All families share wallets and chain state. The package config admits one fork,
// and this sequential suite leaves deployment and service ownership with the runner.
for (const fixture of fixtures) {
  // Every family is budgeted from the actual cadence and finality depth: the
  // audited transitionTrace plan or the finite generic plan. A fixed wall-clock
  // timeout cannot fit a multi-step proof chain at release finality.
  const timing =
    runDirectory !== undefined
      ? await readJourneyExecutionTiming(runDirectory, fixture.category)
      : undefined;
  const timeoutMs = timing?.journeyTimeoutMs ?? 7_200_000;
  it.skipIf(runDirectory === undefined)(
    `automatically proves '${fixture.category}', corrects exact economics, and processes an honest successor`,
    async () => {
      await runAutonomousWatcherJourney(runDirectory!, fixture);
    },
    timeoutMs,
  );
}
