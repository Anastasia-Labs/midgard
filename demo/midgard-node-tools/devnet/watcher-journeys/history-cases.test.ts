import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  admitCompleteCanonicalReplayPredecessor,
  canonicalBlockEvidenceFromVerifiedPayload,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  prepareDoubleSpendFromCanonicalEvidence,
  prepareInputNoIdxFromCanonicalEvidence,
  prepareNonExistentInputFromCanonicalEvidence,
  prepareNoReferenceInputFromCanonicalEvidence,
  prepareReferenceInputNoIdxFromCanonicalEvidence,
  REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import { readPublishedDepositHistory } from "midgard-watcher/tests/support/published-deposit-history";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { verifyJourneyFixture } from "./fixture-verification.js";
import { classifyFullCatalogueTransactionFixture } from "./full-catalogue-verification.js";
import {
  buildJourneyHistoryTransaction,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
} from "./history-cases.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");
const replayers = {
  doubleSpend: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  nonExistentInput: NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  nonExistentInputNoIndex: INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  noReferenceInput: NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  referenceInputNoIdx: REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
};
const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "history-journey/local-verification",
  grade: "security",
} as const;

it
  .skipIf(stagedPath === undefined || !existsSync(stagedPath))
  .each(JOURNEY_HISTORY_TRANSACTION_CATEGORIES)(
  "%s isolates its intended fault against a real deposited predecessor and accepts its control",
  async (category) => {
    const staged = await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
      stagedPath!,
    );
    const accounts = JSON.parse(
      await readFile(
        join(runDirectory!, "secrets/journey-accounts.json"),
        "utf8",
      ),
    );
    const deposit = readPublishedDepositHistory(
      staged.depositHistory,
      staged.depositMetadata,
    );
    const predecessor = await depositEventsRetainedBlock({
      operatorVkey: staged.current.header.operatorVkey,
      startTime: staged.predecessor.header.endTime,
      endTime: staged.current.header.endTime,
      blockSlot: staged.current.header.blockSlot,
      prevHeaderHash: staged.predecessor.headerHash,
      prevUtxosRoot: staged.predecessor.header.utxosRoot,
      priorLedger: staged.predecessor.payload.block_body.utxos,
      events: [
        {
          event: deposit.event,
          originalAssets: deposit.originalAssets,
          honest: true,
        },
      ],
    });
    for (const honest of [false, true]) {
      const block = await buildJourneyHistoryTransaction({
        category,
        predecessor,
        honest,
        ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
        operatorVkey: staged.current.header.operatorVkey,
        endTime: predecessor.header.endTime + 40_000n,
        blockSlot: predecessor.header.blockSlot + 40n,
      });
      expect(block.header.prevUtxosRoot).toBe(predecessor.header.utxosRoot);
      if (honest)
        expect(block.replays.map((replay) => replay.trace.verdict)).toEqual(
          block.transactions.map(() => "accepted"),
        );
      else expect(block.replays.at(-1)?.trace.verdict).toBe("rejected");
      const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance,
      });
      const context = {
        predecessor: await admitCompleteCanonicalReplayPredecessor({
          value: {
            observation: authenticatedHeaderObservation(predecessor),
            payloadEnvelopeCborHex:
              predecessor.payloadEnvelopeCbor.toString("hex"),
            daProvenance,
          },
          currentEvidence: evidence,
          minimumConfirmationDepth: 1,
        }),
      };
      const result = await replayers[category].replay(evidence, context);
      expect(result.detections).toHaveLength(honest ? 0 : 1);
      const full = await classifyFullCatalogueTransactionFixture({
        block,
        predecessor,
        history: [staged.predecessor],
      });
      expect(full.decision.decision).toBe(
        honest ? "healthy" : "fault_detected",
      );
      if (full.decision.decision === "fault_detected")
        expect(full.decision.category).toBe(category);
      const verification = await verifyJourneyFixture({
        category,
        replayer: replayers[category],
        block,
        predecessor,
        expected: honest ? "healthy" : "fault",
      });
      if (!honest) {
        const previousBlockEvidence =
          await canonicalBlockEvidenceFromVerifiedPayload({
            observation: authenticatedHeaderObservation(predecessor),
            payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
            daProvenance,
          });
        const preparers = {
          doubleSpend: prepareDoubleSpendFromCanonicalEvidence,
          nonExistentInput: prepareNonExistentInputFromCanonicalEvidence,
          nonExistentInputNoIndex: prepareInputNoIdxFromCanonicalEvidence,
          noReferenceInput: prepareNoReferenceInputFromCanonicalEvidence,
          referenceInputNoIdx: prepareReferenceInputNoIdxFromCanonicalEvidence,
        };
        if (verification.evidence === undefined)
          throw new Error("Fault decision omitted canonical evidence");
        const prepared = await preparers[category]({
          evidence: verification.evidence,
          previousBlockEvidence,
        });
        expect(prepared.headerHash).toBe(block.headerHash);
      }
    }
  },
);
