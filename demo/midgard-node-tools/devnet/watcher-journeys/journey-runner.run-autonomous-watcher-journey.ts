import type { JourneyFixture } from "./fixture.js";
import { runJourney } from "./journey-runner.run-journey.js";
import {
  type AttachedJourneySession,
  type JourneySession,
  openJourneySession,
} from "./journey-session.js";

/**
 * One process-driven acceptance path for every non-interactive family, run
 * as one family of its session (`runFamily`): a failure, or the end of the
 * family's time limit `limit`, stops the family's work and closes the
 * session before this returns. `afterJourney` runs inside the family's run,
 * once its journey has passed.
 */
export const runAutonomousWatcherJourney = async (
  runDirectory: string,
  fixture: JourneyFixture,
  options: {
    waitForAnchors?: boolean;
    session?: JourneySession | AttachedJourneySession;
    limit?: AbortSignal;
    afterJourney?(): Promise<unknown>;
  } = {},
) => {
  const session = options.session ?? (await openJourneySession(runDirectory));
  try {
    await session.runFamily(async (signal) => {
      await runJourney(
        runDirectory,
        {
          kind: "journey",
          fixture,
          waitForAnchors: options.waitForAnchors ?? true,
        },
        session,
        signal,
      );
      await options.afterJourney?.();
    }, options.limit);
  } finally {
    if (options.session === undefined) await session.close();
  }
};
