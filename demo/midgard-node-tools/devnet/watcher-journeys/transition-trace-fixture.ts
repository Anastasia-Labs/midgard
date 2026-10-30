import { existsSync } from "node:fs";
import { join } from "node:path";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { readPublishedDepositHistory } from "midgard-watcher/tests/support/published-deposit-history";
import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyFixture } from "./fixture.js";
import { journeyDaSignerConfig, journeyNetwork } from "./journey-network.js";
import { verifyTransitionTraceJourneyOutputPlan } from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { isStackJourneyContext } from "./stack-da-publication.js";

/**
 * The deposit-trace journey. By default it starts from the confirmed state,
 * removing abandoned headers first; with `liveTail` it appends after the
 * queue's current tail, whose ledger the journey's replay supplies.
 */
export const createTransitionTraceJourneyFixture = ({
  liveTail,
}: {
  liveTail: boolean;
}): JourneyFixture => ({
  category: "transitionTrace",
  async stage({ context, ledgerAt, directory, retain, onStage }) {
    if (isStackJourneyContext(context))
      throw new Error(
        "The transition-trace journey does not stage on an attached stack yet",
      );
    const checkpointPath = join(directory, "staged.json");
    const resume = existsSync(checkpointPath)
      ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
          checkpointPath,
        )
      : undefined;
    const { deployment, accounts } = context;
    const staged = await stagePublishedDepositTrace(deployment, {
      daSignerConfig: journeyDaSignerConfig(accounts),
      onStage,
      resume,
      onCheckpoint: (checkpoint) =>
        writeJourneyArtifact(checkpointPath, checkpoint),
      timeoutCorrectionJournalPath: join(directory, "timeout-correction.json"),
      // The watcher observes the queue at its action depth, so the abandoned
      // header removal only has to be that deep before the watcher starts.
      finalityDepth: JOURNEY_ACTION_DEPTH,
      ...(liveTail ? { liveTail: { ledgerAt } } : {}),
    });
    const deposit = readPublishedDepositHistory(
      staged.checkpoint.depositHistory,
      staged.checkpoint.depositMetadata,
    );
    const projected = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: journeyNetwork().network,
      eventId: deposit.event.id,
      l2Address: deposit.event.info.l2_address,
      l2NetworkId: deposit.event.info.l2_network_id,
      l2DatumCbor:
        deposit.event.info.l2_datum === null
          ? null
          : Buffer.from(
              aikenSerialisedPlutusDataCborPreservingMapOrder(
                Data.to(deposit.event.info.l2_datum),
              ),
              "hex",
            ),
      originalAssets: deposit.originalAssets,
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
    // The trace attests inline and fails on any other outcome, so the target
    // is attested whenever staging returns; its commit identifies the header.
    return {
      ...staged,
      target: { kind: "attested", txHash: staged.commits[1]! },
    };
  },
});

export const transitionTraceJourneyFixture =
  createTransitionTraceJourneyFixture({ liveTail: false });
