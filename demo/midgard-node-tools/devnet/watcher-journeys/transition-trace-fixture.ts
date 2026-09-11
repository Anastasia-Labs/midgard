import { existsSync } from "node:fs";
import { join } from "node:path";

import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyFixture } from "./fixture.js";

export const transitionTraceJourneyFixture: JourneyFixture = {
  category: "transitionTrace",
  async stage({ context, directory, retain, onStage }) {
    const checkpointPath = join(directory, "staged.json");
    const resume = existsSync(checkpointPath)
      ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
          checkpointPath,
        )
      : undefined;
    const { deployment, accounts } = context;
    const staged = await stagePublishedDepositTrace(deployment, {
      daSignerConfig: {
        NETWORK: "Custom",
        L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
        DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
      },
      onStage,
      resume,
      onCheckpoint: (checkpoint) =>
        writeJourneyArtifact(checkpointPath, checkpoint),
    });
    await writeJourneyArtifact(checkpointPath, staged.checkpoint);
    await retain(staged.predecessor, staged.commits[0]!);
    await retain(staged.current, staged.commits[1]!);
    return staged;
  },
};
