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
import {
  readJourneyExecutionTiming,
  readStackJourneyTimeoutMs,
  stackRunTimeoutsMs,
} from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { readStackJourneyTimingPaths } from "./stack-context.js";

// Devnet mode (the default) runs from MIDGARD_WATCHER_JOURNEY_RUN_DIR and
// skips without it; stack mode attaches to a running e2e-stack.
const source = selectJourneyDeploymentSource(process.env);
const runDirectory =
  source?.mode === "devnet" ? source.runDirectory : undefined;
const fixtures = selectJourneyFixtures(
  process.env.MIDGARD_WATCHER_JOURNEY_CATEGORIES,
);
const stackTimingPaths =
  source?.mode === "stack"
    ? await readStackJourneyTimingPaths(source.configPath)
    : undefined;

let session: Promise<JourneySession | AttachedJourneySession> | undefined;
afterAll(async () => {
  const opened = await session?.catch(() => undefined);
  await opened?.close();
}, 120_000);

// Every family is budgeted from the actual cadence and the depth its actions
// wait for: the audited transitionTrace plan or the finite generic plan, plus
// the single release-finality window of the finalized evidence stamp. On a
// stack it also waits for operator-2's shift to hold the family's staging
// shift cover C: at worst the rest of one of its shifts, our node's whole
// shift, and C of the next. The run's last family also resumes operator-2's
// node and waits for it to report ready.
const options = { actionDepth: JOURNEY_ACTION_DEPTH };
const stackTimeoutsMs =
  stackTimingPaths !== undefined
    ? stackRunTimeoutsMs(
        await Promise.all(
          fixtures.map((fixture) =>
            readStackJourneyTimeoutMs(stackTimingPaths, fixture, options),
          ),
        ),
      )
    : undefined;

// All families share wallets and chain state. The package config admits one fork,
// and this sequential suite leaves deployment and service ownership with the runner.
for (const [index, fixture] of fixtures.entries()) {
  const timeoutMs =
    runDirectory !== undefined
      ? (
          await readJourneyExecutionTiming(
            runDirectory,
            fixture.category,
            options,
          )
        ).journeyTimeoutMs
      : // A stack's limit; with no deployment selected the case is skipped.
        stackTimeoutsMs?.[index];
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
      const last = fixture === fixtures.at(-1);
      await runAutonomousWatcherJourney(
        activeSession.mode === "devnet"
          ? runDirectory!
          : activeSession.context.runDirectory,
        fixture,
        {
          session: activeSession,
          waitForAnchors: last,
        },
      );
      // Operator-2's node stays held through every family's fault phase; the
      // completed run starts it again and proves it live.
      if (activeSession.mode === "attached" && last)
        await activeSession.operator2.resume();
    },
    timeoutMs,
  );
}
