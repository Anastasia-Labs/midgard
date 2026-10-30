import { afterAll, it } from "vitest";

import { selectJourneyFixtures } from "./catalogue.js";
import { selectJourneyDeploymentSource } from "./deployment-source.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import {
  type AttachedJourneySession,
  type JourneySession,
  openAttachedJourneySession,
  openJourneySession,
} from "./journey-session.js";
import { readJourneyExecutionTiming } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";

// Devnet mode (the default) runs from MIDGARD_WATCHER_JOURNEY_RUN_DIR and
// skips without it; stack mode attaches to a running e2e-stack.
const source = selectJourneyDeploymentSource(process.env);
const runDirectory =
  source?.mode === "devnet" ? source.runDirectory : undefined;
const fixtures = selectJourneyFixtures(
  process.env.MIDGARD_WATCHER_JOURNEY_CATEGORIES,
);

let session: Promise<JourneySession | AttachedJourneySession> | undefined;
afterAll(async () => {
  const opened = await session?.catch(() => undefined);
  await opened?.close();
}, 120_000);

// All families share wallets and chain state. The package config admits one fork,
// and this sequential suite leaves deployment and service ownership with the runner.
for (const fixture of fixtures) {
  // Every family is budgeted from the actual cadence and the depth its actions
  // wait for: the audited transitionTrace plan or the finite generic plan, plus
  // the single release-finality window of the finalized evidence stamp.
  const timing =
    runDirectory !== undefined
      ? await readJourneyExecutionTiming(runDirectory, fixture.category, {
          actionDepth: JOURNEY_ACTION_DEPTH,
        })
      : undefined;
  const timeoutMs = timing?.journeyTimeoutMs ?? 7_200_000;
  it.skipIf(source === undefined)(
    `automatically proves '${fixture.category}', corrects exact economics, and processes an honest successor`,
    async () => {
      const activeSession = await (session ??=
        source!.mode === "devnet"
          ? openJourneySession(source!.runDirectory)
          : source!.open().then((context) =>
              openAttachedJourneySession(context, {
                operator2: source!.operator2,
              }),
            ));
      if (activeSession.mode === "attached")
        throw new Error(
          "The journey runner does not drive an attached stack watcher yet",
        );
      await runAutonomousWatcherJourney(runDirectory!, fixture, {
        session: activeSession,
        waitForAnchors: fixture === fixtures.at(-1),
      });
    },
    timeoutMs,
  );
}
