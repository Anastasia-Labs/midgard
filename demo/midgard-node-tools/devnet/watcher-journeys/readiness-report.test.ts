import { expect, it } from "vitest";

import { readJourneyReadiness } from "./readiness.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

// Read-only entrypoint through the package's existing source-aware runtime.
it.skipIf(runDirectory === undefined)(
  "reports all 54 journey families without launching services",
  async () => {
    const report = await readJourneyReadiness(runDirectory!);
    expect(report.counts.families).toBe(54);
    console.info(JSON.stringify(report, null, 2));
  },
);
