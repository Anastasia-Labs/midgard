import { join } from "node:path";

import { loadWatcherProcessConfigFile } from "midgard-watcher";
import { expect, it } from "vitest";

import { verifyJourneyWorkflowBindings } from "./workflow-binding-preflight.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
it.skipIf(runDirectory === undefined)(
  "binds every installed production workflow to the actual finalized deployment without launching the watcher",
  async () => {
    const config = await loadWatcherProcessConfigFile(
      join(
        runDirectory!,
        "work/journeys/transition-trace/watcher-process.json",
      ),
    );
    const result = await verifyJourneyWorkflowBindings({
      config,
      directory: join(runDirectory!, "work/workflow-binding-probe"),
      onProgress: (event) =>
        console.info(JSON.stringify({ stage: "workflow_binding", ...event })),
    });
    expect(result.outcome).toBe("passed");
    expect(result.receipts.map((row) => row.category)).toEqual(
      result.installedCategories,
    );
    expect(result.receipts.every((row) => row.outcome === "passed")).toBe(true);
  },
  180_000,
);
