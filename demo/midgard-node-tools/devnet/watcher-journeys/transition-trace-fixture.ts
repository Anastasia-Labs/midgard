import { existsSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyFixture } from "./fixture.js";
import { verifyTransitionTraceJourneyOutputPlan } from "./journey-timing.js";

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
    const { depositEvent, depositMetadata } = staged.checkpoint;
    const datum = Data.from(depositEvent.datum!, SDK.DepositDatum);
    const projected = deriveCanonicalDepositTransitionEffect({
      configuredNetwork: "Custom",
      eventId: datum.event.id,
      l2Address: datum.event.info.l2_address,
      l2NetworkId: datum.event.info.l2_network_id,
      l2DatumCbor:
        datum.event.info.l2_datum === null
          ? null
          : Buffer.from(Data.to(datum.event.info.l2_datum), "hex"),
      l1Assets: depositEvent.assets,
      depositPolicyId: deployment.contracts.deposit.policyId,
      depositAssetNameHex: depositMetadata.depositAssetName,
    });
    const insertion = projected.operations[0];
    if (
      projected.operations.length !== 1 ||
      insertion?.type !== "insert" ||
      staged.current.header.depositCount !== 1n
    )
      throw new Error(
        "Transition-trace fixture differs from its single-output timing plan",
      );
    verifyTransitionTraceJourneyOutputPlan(insertion.outputCbor);
    await writeJourneyArtifact(checkpointPath, staged.checkpoint);
    await retain(staged.predecessor, staged.commits[0]!);
    await retain(staged.current, staged.commits[1]!);
    return staged;
  },
};
