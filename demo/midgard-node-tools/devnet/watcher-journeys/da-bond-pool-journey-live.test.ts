import { execFileSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { dirname, join } from "node:path";

import { expect, it } from "vitest";

import { writeJourneyArtifact, writeJourneyFile } from "./artifacts.js";
import {
  DA_BOND_POOL_JOURNEY_CHRONOLOGY,
  DaBondPoolJourneyFailure,
  type DaBondPoolJourneyRecord,
  renderDaBondPoolJourneyReport,
  runDaBondPoolJourney,
} from "./da-bond-pool-journey.js";
import {
  createLiveDaBondPoolJourneyPort,
  type LiveDaBondPoolJourneyPort,
} from "./da-bond-pool-live-port.js";
import { loadJourneyContext } from "./live-context.js";
import { measureJourneyStage } from "./stage-timing.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const reportPath = process.env.MIDGARD_DA_BOND_JOURNEY_REPORT_PATH;
/**
 * `5` resumes a run directory whose journey passed steps 1, 3, 4 and 5: a
 * smoke of steps 2 and 6 on a kept devnet, never journey evidence (the live
 * port's `resume`). No other value is accepted.
 */
const resumeAfter = process.env.MIDGARD_DA_BOND_JOURNEY_RESUME_AFTER;
if (resumeAfter !== undefined && resumeAfter !== "" && resumeAfter !== "5")
  throw new Error(
    `MIDGARD_DA_BOND_JOURNEY_RESUME_AFTER must be 5 or unset, not ${JSON.stringify(resumeAfter)}`,
  );
const resume = resumeAfter === "5" ? ({ afterStep: 5 } as const) : undefined;

const gitHead = (): string | undefined => {
  try {
    return execFileSync("git", ["rev-parse", "HEAD"], {
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
  } catch {
    return undefined;
  }
};

/** The ledger lands on disk before the test passes or fails. */
const persist = async (
  port: LiveDaBondPoolJourneyPort,
  record: DaBondPoolJourneyRecord,
): Promise<void> => {
  await writeJourneyArtifact(
    join(port.artifactDirectory, "da-bond-pool-journey.json"),
    record,
  );
  if (reportPath !== undefined && reportPath.length > 0) {
    const head = gitHead();
    mkdirSync(dirname(reportPath), { recursive: true });
    await writeJourneyFile(
      reportPath,
      renderDaBondPoolJourneyReport(record, {
        adapter: "live-devnet",
        runDir: runDirectory!,
        deploymentManifestId: port.manifestId,
        networkMagic: port.networkMagic,
        ...(head === undefined ? {} : { gitHead: head }),
      }),
    );
  }
};

it.skipIf(runDirectory === undefined)(
  "walks the pooled DA bond journey on the process devnet",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const port = await createLiveDaBondPoolJourneyPort(
      context,
      resume === undefined ? {} : { resume },
    );
    try {
      let record: DaBondPoolJourneyRecord;
      try {
        record = await runDaBondPoolJourney(port, {
          stageTimer: (name, action) =>
            measureJourneyStage(port.artifactDirectory, name, action),
          // P16/P18: the devnet evidence counts only with the real
          // da-committee-node process and the real da-bond CLI.
          requireProcessEvidence: true,
          ...(port.resume === undefined ? {} : { resume: port.resume }),
        });
      } catch (error) {
        if (error instanceof DaBondPoolJourneyFailure)
          await persist(port, error.record);
        throw error;
      }
      await persist(port, record);
      expect(record.status).toBe("passed");
      expect(record.stages.map((stage) => stage.step)).toEqual(
        port.resume === undefined
          ? [...DA_BOND_POOL_JOURNEY_CHRONOLOGY]
          : [2, 6],
      );
      // P27(3): the checked stop at the end of step 6 ran; dispose's
      // teardown would stop the node without its exit and UTxO checks.
      expect(port.committeeRunning()).toBe(false);
    } finally {
      await port.dispose();
    }
  },
  3 * 60 * 60_000,
);
