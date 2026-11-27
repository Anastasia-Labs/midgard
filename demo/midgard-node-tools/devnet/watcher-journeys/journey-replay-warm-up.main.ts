/**
 * The journey replay warm-up command (`pnpm run journey-replay:warm-up` in
 * `demo/midgard-node-tools`). Run once before a long stack drill, with the
 * drill's own environment (`MIDGARD_WATCHER_JOURNEY_DEPLOYMENT=stack`, its
 * stack configuration and its one run id), it fills the run's replay store,
 * `<stack runDirectory>/fault-journeys/<runId>/work/journeys/replay`, so no
 * family of the drill walks L1 from the stack's origin. It sends no
 * transaction and prints no secret: only the store's path and block heights.
 */
import { selectJourneyDeploymentSource } from "./deployment-source.js";
import { describeJourneyReplayWarmth } from "./journey-replay-warm-up.js";
import { warmStackJourneyReplay } from "./stack-replayed-ledger.js";

const warmUp = async () => {
  const source = selectJourneyDeploymentSource(process.env);
  if (source?.mode !== "stack")
    throw new Error(
      "The journey replay warm-up fills a stack drill's replay store: set MIDGARD_WATCHER_JOURNEY_DEPLOYMENT=stack and the drill's stack variables",
    );
  const warmed = await warmStackJourneyReplay(await source.open());
  process.stdout.write(`${describeJourneyReplayWarmth(warmed)}\n`);
};

// The stack context keeps L1 provider connections open; the command ends
// with its result.
warmUp().then(
  () => process.exit(0),
  (error: unknown) => {
    process.stderr.write(
      `Journey replay warm-up failed: ${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  },
);
