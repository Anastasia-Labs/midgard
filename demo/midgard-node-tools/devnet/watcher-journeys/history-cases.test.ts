import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
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
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  checkedLedgerAfter,
  payloadLedgerAt,
} from "@al-ft/midgard-fault-proofs/test-support/predecessor-ledger";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { readPublishedDepositHistory } from "midgard-watcher/tests/support/published-deposit-history";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { beforeAll, describe, expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import {
  buildJourneyHistoryTransaction,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
} from "./history-cases.js";
import { readCheckpointJourneyLedgerBlock } from "./journey-checkpoint-ledger.test-support.js";
import { verifyJourneyFixture } from "./journey-classification.test-support.js";
import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import {
  checkedFixtureBlock,
  depositEventsRetainedBlock,
} from "./journey-fixture-ledger.test-support.js";
import {
  journeyLedgerPairs,
  type JourneyLedgerPredecessor,
} from "./journey-ledger.js";
import { stackNodeFeeSchedule } from "./stack-fee-schedule.test-support.js";

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
      priorLedger: journeyLedgerPairs(
        (
          await readCheckpointJourneyLedgerBlock(
            runDirectory!,
            staged.predecessor,
          )
        ).ledger,
      ),
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
          ledgerAt: payloadLedgerAt(predecessor.payloadEnvelopeCbor),
          currentEvidence: evidence,
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
        const prevLedger = await checkedLedgerAfter(
          await canonicalBlockEvidenceFromVerifiedPayload({
            observation: authenticatedHeaderObservation(predecessor),
            payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
            daProvenance,
          }),
        );
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
          prevLedger,
        });
        expect(prepared.headerHash).toBe(block.headerHash);
      }
    }
  },
);

describe("history transaction fixtures on a predecessor with a Preprod stack's fee schedule", () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const owner = CML.PrivateKey.from_bech32(
    walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_raw_bytes();
  let predecessor: JourneyLedgerPredecessor;
  beforeAll(async () => {
    predecessor = await checkedFixtureBlock(
      await buildCanonicalBlockFixture({
        transactions: [],
        ...(await stackNodeFeeSchedule()),
        utxos: [
          {
            key: outRefCbor(0x81, 0n),
            value: encodeMidgardTxOutput({
              address: Buffer.concat([Buffer.from([0x60]), owner]),
              value: { lovelace: 100_000_000n, assets: new Map() },
            }),
          },
        ],
      }),
    );
  });

  it.each(JOURNEY_HISTORY_TRANSACTION_CATEGORIES)(
    "%s: its control replays accepted and its fault is rejected and convicted by its own family",
    async (category) => {
      for (const honest of [false, true]) {
        const block = await buildJourneyHistoryTransaction({
          category,
          predecessor,
          honest,
          ledgerOwnerSeedPhrase: account.seedPhrase,
          operatorVkey: "82".repeat(28),
          endTime: predecessor.header.endTime + 40_000n,
          blockSlot: predecessor.header.blockSlot + 40n,
        });
        expect(block.replays.map((replay) => replay.trace.verdict)).toEqual(
          block.transactions.map((_, index) =>
            honest || index < block.transactions.length - 1
              ? "accepted"
              : "rejected",
          ),
        );
        const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation(block),
          payloadEnvelopeCbor: block.payloadEnvelopeCbor,
          daProvenance,
        });
        const context = {
          predecessor: await admitCompleteCanonicalReplayPredecessor({
            ledgerAt: payloadLedgerAt(predecessor.payloadEnvelopeCbor),
            currentEvidence: evidence,
          }),
        };
        const own = await replayers[category].replay(evidence, context);
        expect(own.detections).toHaveLength(honest ? 0 : 1);
        const full = await classifyFullCatalogueTransactionFixture({
          block,
          predecessor,
          history: [],
        });
        expect(full.decision.decision).toBe(
          honest ? "healthy" : "fault_detected",
        );
      }
    },
    120_000,
  );
});
