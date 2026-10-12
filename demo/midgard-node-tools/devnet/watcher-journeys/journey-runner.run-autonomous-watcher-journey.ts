import type { JourneyFixture } from "./fixture.js";
import { runJourney } from "./journey-runner.run-journey.js";
import { type JourneySession, openJourneySession } from "./journey-session.js";

/** One process-driven acceptance path for every non-interactive family. */
export const runAutonomousWatcherJourney = async (
  runDirectory: string,
  fixture: JourneyFixture,
  options: {
    waitForAnchors?: boolean;
    session?: JourneySession;
  } = {},
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
