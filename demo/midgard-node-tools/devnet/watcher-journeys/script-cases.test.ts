import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeMidgardTxOutput,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import * as FP from "@al-ft/midgard-fault-proofs";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { readPublishedDepositHistory } from "midgard-watcher/tests/support/published-deposit-history";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { readCheckpointJourneyLedgerBlock } from "./journey-checkpoint-ledger.test-support.js";
import { verifyJourneyFixture } from "./journey-classification.test-support.js";
import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import {
  checkedFixtureBlock,
  depositEventsRetainedBlock,
} from "./journey-fixture-ledger.test-support.js";
import { journeyLedgerPairs } from "./journey-ledger.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
} from "./script-cases.js";
const replayers = {
  nativeScriptDecoding: FP.NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  nativeScriptInvalid: FP.NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  witnessScriptDecoding: FP.WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMissing:
    FP.SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  outputReferenceScriptDecoding:
    FP.OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  redeemerCanonicity: FP.REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  missingRedeemer: FP.MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMismatch:
    FP.SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
};
const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");
const loadRetainedDeposit = async () => {
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

  return { staged, predecessor, accounts };
};
it
  .skipIf(stagedPath === undefined || !existsSync(stagedPath))
  .each(JOURNEY_SCRIPT_FORCED_CATEGORIES)(
  "retains exact executable script material for %s wrongful-rejection fixture",
  async (category) => {
    const { staged, predecessor, accounts } = await loadRetainedDeposit();

    const block = await buildJourneyScriptForcedFault({
      category,
      predecessor,
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      operatorVkey: predecessor.header.operatorVkey,
      endTime: predecessor.header.endTime + 40000n,
      blockSlot: predecessor.header.blockSlot + 40n,
      orderKey: { transactionId: "a1".repeat(32), outputIndex: 0n },
    });
    expect(block.control.replay.trace.verdict).toBe("accepted");
    expect(block.replay.trace.verdict).toBe("accepted");
    expect(block.material.transaction.canonicalCbor.length).toBeGreaterThan(0);
    const forcedOrigin = {
      transaction: block.material.transaction,
      orderKey: { transactionId: "a1".repeat(32), outputIndex: 0n },
    };
    const full = await classifyFullCatalogueTransactionFixture({
      block,
      predecessor,
      history: [staged.predecessor],
      forced: forcedOrigin,
    });
    expect(full.decision).toMatchObject({
      decision: "fault_detected",
      category,
    });
    const honestForced = await buildJourneyScriptForcedSuccessor({
      category,
      predecessor,
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      operatorVkey: predecessor.header.operatorVkey,
      endTime: block.header.endTime,
      blockSlot: block.header.blockSlot,
      orderKey: forcedOrigin.orderKey,
    });
    const healthy = await classifyFullCatalogueTransactionFixture({
      block: honestForced,
      predecessor,
      history: [staged.predecessor],
      forced: forcedOrigin,
    });
    expect(healthy.decision).toMatchObject({ decision: "healthy" });

    const classification = await verifyJourneyFixture({
      category,
      replayer: replayers[category],
      block,
      predecessor,
      history: [staged.predecessor],
    });
    const evidence = classification.evidence;
    if (evidence === undefined)
      throw new Error("Script fixture has no canonical evidence");
    const forced = evidence.reconstruction.forcedTransactions[0];
    if (forced === undefined || forced.value.verdict === "ForcedTxValid")
      throw new Error("Missing retained forced rejection");
    const subject = SDK.forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
    });
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (category === "redeemerCanonicity")
      expect(
        FP.redeemerCanonicityEvidenceCloses(
          FP.prepareRedeemerCanonicityEvidence({
            finding: { subject, redeemerIndex: 0 },
            fieldPreimage: material.fieldPreimages[8]!,
            committedFieldHashHex: midgardFieldCommitment(
              material.fieldPreimages[8]!,
            ).toString("hex"),
          }),
        ),
      ).toBe(true);
    if (category === "witnessScriptDecoding")
      expect(
        FP.witnessScriptDecodingEvidenceCloses(
          FP.prepareWitnessScriptDecodingEvidence({
            finding: {
              subject,
              scriptIndex: 0,
              witnessSetHash: Buffer.from(
                material.compact.transactionWitnessSetHash,
              ).toString("hex"),
            },
            fieldPreimage: material.fieldPreimages[6]!,
            committedFieldHashHex: midgardFieldCommitment(
              material.fieldPreimages[6]!,
            ).toString("hex"),
          }),
        ),
      ).toBe(true);
    if (category === "outputReferenceScriptDecoding")
      expect(
        FP.outputReferenceScriptEvidenceCloses(
          FP.prepareOutputReferenceScriptDecodingEvidence({
            subject,
            outputIndex: 0,
            canonicalTransactionCbor: forced.fullTransactionCbor,
          }),
        ),
      ).toBe(true);
    if (category === "nativeScriptInvalid")
      expect(
        (await FP.prepareNativeScriptInvalidForcedArtifact({ block: evidence }))
          .headerHash,
      ).toBe(block.headerHash);
    if (
      category === "nativeScriptDecoding" ||
      category === "scriptIntegrityHashMissing" ||
      category === "missingRedeemer" ||
      category === "scriptIntegrityHashMismatch"
    ) {
      const context = FP.headerDecisionReplayContext(classification.decision);
      const replayer = replayers[category];
      const detections = FP.requireCompleteCanonicalReplayDecision({
        evidence,
        replayer,
        decision: await replayer.replay(evidence, context),
        context,
      });
      const selected = await FP.classifyCanonicalBlockViolations({
        evidence,
        detections,
        minimumConfirmationDepth: 30,
      });
      if (selected.decision !== "fault_detected")
        throw new Error("Closing classification disappeared");
      const artifact =
        category === "nativeScriptDecoding"
          ? await FP.prepareNativeScriptDecodingWorkflowArtifact({
              evidence,
              classification: selected,
              replayContext: context,
            })
          : category === "missingRedeemer"
            ? await FP.prepareMissingRedeemerWorkflowArtifact({
                evidence,
                classification: selected,
                replayContext: context,
              })
            : category === "scriptIntegrityHashMismatch"
              ? await FP.prepareScriptIntegrityHashMismatchWorkflowArtifact({
                  evidence,
                  classification: selected,
                  replayContext: context,
                })
              : await FP.prepareScriptIntegrityHashMissingArtifact({
                  evidence,
                  classification: selected,
                });
      expect(artifact.headerHash).toBe(block.headerHash);
    }
    await verifyJourneyFixture({
      category,
      replayer: replayers[category],
      block: block.control,
      predecessor,
      history: [staged.predecessor],
      expected: "healthy",
    });
  },
  60000,
);

it.each(["missingRedeemer", "scriptIntegrityHashMismatch"] as const)(
  "classifies the %s forced-claim fixture through the full catalogue and its honest successor as healthy",
  async (category) => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const wallet = walletFromSeed(account.seedPhrase, { network: "Custom" });
    const owner = CML.PrivateKey.from_bech32(wallet.paymentKey)
      .to_public()
      .hash()
      .to_raw_bytes();
    const predecessor = await checkedFixtureBlock(
      await buildCanonicalBlockFixture({
        transactions: [],
        utxos: [
          {
            key: outRefCbor(0x81, 0n),
            value: encodeMidgardTxOutput({
              address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner)]),
              value: { lovelace: 100_000_000n, assets: new Map() },
            }),
          },
        ],
      }),
    );
    const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };
    const input = {
      category,
      predecessor,
      ledgerOwnerSeedPhrase: account.seedPhrase,
      operatorVkey: "82".repeat(28),
      endTime: predecessor.header.endTime + 40_000n,
      blockSlot: predecessor.header.blockSlot + 40n,
      orderKey,
    };
    const block = await buildJourneyScriptForcedFault(input);
    const forced = { transaction: block.material.transaction, orderKey };
    const full = await classifyFullCatalogueTransactionFixture({
      block,
      predecessor,
      history: [],
      forced,
    });
    expect(full.decision).toMatchObject({
      decision: "fault_detected",
      category,
    });
    const healthy = await classifyFullCatalogueTransactionFixture({
      block: await buildJourneyScriptForcedSuccessor(input),
      predecessor,
      history: [],
      forced,
    });
    expect(healthy.decision).toMatchObject({ decision: "healthy" });
  },
  60000,
);
