import { it } from "vitest";

import { prepareAutonomousWatcherHistory } from "./journey-runner.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const selected = process.env.MIDGARD_WATCHER_JOURNEY_PREPARE_HISTORY === "1";

// Explicit opt-in keeps the shared live service lifecycle out of local suites.
it.skipIf(runDirectory === undefined || !selected)(
  "retains the verified trace successor as the real duplicate-event source",
  async () => {
    await prepareAutonomousWatcherHistory(runDirectory!);
  },
  7_200_000,
);
