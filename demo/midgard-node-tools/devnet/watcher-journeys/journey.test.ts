import { it } from "vitest";

import { selectJourneyFixtures } from "./catalogue.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const fixtures = selectJourneyFixtures(
  process.env.MIDGARD_WATCHER_JOURNEY_CATEGORIES,
);

// All families share wallets and chain state. The package config admits one fork,
// and this sequential suite leaves deployment and service ownership with the runner.
it.skipIf(runDirectory === undefined).each(fixtures)(
  "automatically proves $category, corrects exact economics, and processes an honest successor",
  async (fixture) => {
    await runAutonomousWatcherJourney(runDirectory!, fixture);
  },
  7_200_000,
);
