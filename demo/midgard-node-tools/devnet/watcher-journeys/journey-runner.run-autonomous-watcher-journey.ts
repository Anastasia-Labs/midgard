import { join } from "node:path";

import type { JourneyFixture } from "./fixture.js";
import { runJourney } from "./journey-runner.run-journey.js";
import { type JourneySession, openJourneySession } from "./journey-session.js";
import { loadJourneyContext } from "./live-context.js";
import { verifyJourneyResultEvidence } from "./readiness-evidence.js";

/** One process-driven acceptance path for every non-interactive family. */
export const runAutonomousWatcherJourney = async (
  runDirectory: string,
  fixture: JourneyFixture,
  options: { waitForAnchors?: boolean; session?: JourneySession } = {},
) => {
  const session = options.session ?? (await openJourneySession(runDirectory));
  try {
    await runJourney(
      runDirectory,
      {
        kind: "journey",
        fixture,
        waitForAnchors: options.waitForAnchors ?? true,
      },
      session,
    );
  } catch (cause) {
    await session.close();
    throw cause;
  } finally {
    if (options.session === undefined) await session.close();
  }
};

/** Start the real maturity clock after the verified trace baseline has completed. */
export const prepareAutonomousWatcherHistory = async (runDirectory: string) => {
  const context = await loadJourneyContext(runDirectory);
  await verifyJourneyResultEvidence(
    context.runDirectory,
    join(context.runDirectory, "work/journeys/transition-trace"),
    "transitionTrace",
    context.deployment,
  );
  const session = await openJourneySession(runDirectory);
  try {
    await runJourney(
      runDirectory,
      { kind: "prepare_duplicate_event_history" },
      session,
    );
  } finally {
    await session.close();
  }
};
