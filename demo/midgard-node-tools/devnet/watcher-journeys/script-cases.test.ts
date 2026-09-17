import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import * as FP from "@al-ft/midgard-fault-proofs";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { verifyJourneyFixture } from "./fixture-verification.js";
import { classifyFullCatalogueTransactionFixture } from "./full-catalogue-verification.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
} from "./script-cases.js";
import { prepareJourneyNativeExecutionEvidence } from "./script-execution-evidence.js";
import {
  buildJourneyScriptHistoryFault,
  JOURNEY_SCRIPT_HISTORY_CATEGORIES,
} from "./script-history-cases.js";
import { prepareJourneyMissingNativeScriptTxEvidence } from "./script-history-evidence.js";
const replayers = {
  nativeScriptDecoding: FP.NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  nativeScriptInvalid: FP.NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  witnessScriptDecoding: FP.WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMissing:
    FP.SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  outputReferenceScriptDecoding:
    FP.OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  executionSourceScriptDecoding:
    FP.EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  receivePurposeLanguage: FP.RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  unusedScriptWitness: FP.UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  missingScriptSource: FP.MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  missingRedeemer: FP.MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  unusedRedeemer: FP.UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  executionNativeScriptInvalid:
    FP.EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMismatch:
    FP.SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  redeemerCanonicity: FP.REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
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
        event: staged.depositEvent,
        depositPolicyId: staged.depositMetadata.depositAuthUnit.slice(0, 56),
        assetName: staged.depositMetadata.depositAssetName,
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
      category === "scriptIntegrityHashMissing"
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
          : await FP.prepareScriptIntegrityHashMissingArtifact({
              evidence,
              classification: selected,
            });
      expect(artifact.headerHash).toBe(block.headerHash);
    }
    const recoveryMaterial = async <T>(material: Promise<T>): Promise<T> => {
      const fresh = await material;
      const encoded = FP.encodeWorkflowArtifact(fresh);
      expect(FP.requireWorkflowArtifactMatches(encoded, fresh)).toBe(fresh);
      return fresh;
    };
    if (category === "executionNativeScriptInvalid") {
      expect(
        (await prepareJourneyNativeExecutionEvidence(evidence, predecessor))
          .evidence.contradiction,
      ).toBe(true);
      if (
        full.evidence === undefined ||
        full.historicalNativeScriptCorpus === undefined
      )
        throw new Error(
          "Native recovery fixture omitted authenticated history",
        );
      const detections =
        FP.executionNativeScriptInvalid.detectExecutionNativeScriptInvalidCanonicalViolations(
          { block: full.evidence, corpus: full.historicalNativeScriptCorpus },
        );
      expect(detections).toHaveLength(1);
      await recoveryMaterial(
        Promise.resolve({
          header: full.evidence.header,
          headerHash: full.evidence.headerHash,
          detection: detections[0]!,
          corpus: full.historicalNativeScriptCorpus,
        }),
      );
    }
    if (category === "missingRedeemer") {
      const candidates = await FP.replayMissingRedeemer(evidence);
      expect(candidates).toHaveLength(1);
      const artifact = await recoveryMaterial(
        Promise.resolve(candidates[0]!.artifact),
      );
      expect(artifact.headerHash).toHaveLength(56);
      expect(FP.admitMissingRedeemerArtifact(artifact)).toBe(artifact);
      expect(() =>
        FP.admitMissingRedeemerArtifact({
          ...artifact,
          headerHash: "11".repeat(32),
        }),
      ).toThrow("not admitted");
      expect(
        FP.missingRedeemerEvidenceCloses(candidates[0]!.artifact.evidence),
      ).toBe(true);
    }
    if (category === "unusedRedeemer")
      expect(
        FP.unusedRedeemerEvidenceCloses(
          (await recoveryMaterial(FP.prepareUnusedRedeemerArtifact(evidence)))
            .evidence,
        ),
      ).toBe(true);
    if (category === "unusedScriptWitness")
      expect(
        FP.unusedScriptWitnessEvidenceCloses(
          (
            await recoveryMaterial(
              FP.prepareUnusedScriptWitnessArtifact(evidence),
            )
          ).evidence,
        ),
      ).toBe(true);
    if (category === "receivePurposeLanguage")
      expect(
        FP.receivePurposeLanguageEvidenceCloses(
          (
            await recoveryMaterial(
              FP.prepareReceivePurposeLanguageArtifact(evidence),
            )
          ).evidence,
        ),
      ).toBe(true);
    if (category === "executionSourceScriptDecoding")
      expect(
        FP.executionSourceScriptDecodingEvidenceCloses(
          (
            await recoveryMaterial(
              FP.prepareExecutionSourceScriptDecodingArtifact(evidence),
            )
          ).evidence,
        ),
      ).toBe(true);
    if (category === "missingScriptSource") {
      const artifact =
        await FP.missingScriptSourceV1.prepareMissingScriptSourceArtifact(
          evidence,
        );
      expect(
        FP.missingScriptSourceV1.missingScriptSourceEvidenceCloses(
          artifact.evidence,
        ),
      ).toBe(true);
      await recoveryMaterial(
        Promise.resolve(
          FP.missingScriptSourceV1.missingScriptSourceRecoveryMaterial(
            artifact,
          ),
        ),
      );
    }
    if (category === "scriptIntegrityHashMismatch")
      expect(
        FP.scriptIntegrityHashMismatchEvidenceCloses(
          (
            await recoveryMaterial(
              FP.prepareScriptIntegrityHashMismatchArtifact(evidence),
            )
          ).evidence,
        ),
      ).toBe(true);

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

it
  .skipIf(stagedPath === undefined || !existsSync(stagedPath))
  .each(JOURNEY_SCRIPT_HISTORY_CATEGORIES)(
  "retains authenticated native script producer history for %s",
  async (category) => {
    const { staged, predecessor, accounts } = await loadRetainedDeposit();
    const { fault, control, preparedPredecessor } =
      await buildJourneyScriptHistoryFault({
        category,
        predecessor,
        ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
        operatorVkey: predecessor.header.operatorVkey,
        endTime: predecessor.header.endTime + 40000n,
        blockSlot: predecessor.header.blockSlot + 40n,
      });
    expect(
      control.replays.every((replay) => replay.trace.verdict === "accepted"),
    ).toBe(true);
    expect(fault.replays.at(-1)?.trace.verdict).toBe("rejected");
    const replayer =
      category === "missingNativeScriptTx"
        ? FP.MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY
        : FP.MISSING_NATIVE_SCRIPT_UTXO_COMPLETE_CANONICAL_REPLAY;
    const classification = await verifyJourneyFixture({
      category,
      replayer,
      block: fault,
      predecessor: preparedPredecessor ?? predecessor,
      history: [predecessor, staged.predecessor],
    });
    if (classification.evidence === undefined)
      throw new Error("Missing canonical history fixture evidence");
    if (category === "missingNativeScriptTx")
      expect(
        (
          await prepareJourneyMissingNativeScriptTxEvidence(
            classification.evidence,
          )
        ).missingNativeScriptBytes,
      ).toEqual(Buffer.from("820180", "hex"));
    const full = await classifyFullCatalogueTransactionFixture({
      block: fault,
      predecessor: preparedPredecessor ?? predecessor,
      history: [predecessor, staged.predecessor],
    });
    expect(full.decision).toMatchObject({
      decision: "fault_detected",
      category,
    });
    if (category === "missingNativeScriptUtxo") {
      if (
        full.evidence === undefined ||
        full.historicalNativeScriptCorpus === undefined
      )
        throw new Error("Missing authenticated history material");
      const prepared =
        await FP.prepareMissingNativeScriptUtxoFromCanonicalEvidence({
          evidence: full.evidence,
          historicalNativeScriptCorpus: full.historicalNativeScriptCorpus,
        });
      expect(prepared.headerHash).toBe(fault.headerHash);
      expect(prepared.missingNativeScriptBytes).toBe("820180");
    }

    const healthy = await classifyFullCatalogueTransactionFixture({
      block: control,
      predecessor: preparedPredecessor ?? predecessor,
      history: [predecessor, staged.predecessor],
    });
    expect(healthy.decision).toMatchObject({ decision: "healthy" });

    await verifyJourneyFixture({
      category,
      replayer,
      block: control,
      predecessor: preparedPredecessor ?? predecessor,
      history: [predecessor, staged.predecessor],
      expected: "healthy",
    });
  },
  60000,
);

it.skipIf(stagedPath === undefined || !existsSync(stagedPath))(
  "keeps owner funding usable after a native receive successor",
  async () => {
    const { predecessor, accounts } = await loadRetainedDeposit();
    let current: Parameters<
      typeof buildJourneyScriptForcedSuccessor
    >[0]["predecessor"] = predecessor;
    for (const [index, category] of (
      [
        "receivePurposeLanguage",
        "nativeScriptInvalid",
        "scriptIntegrityHashMismatch",
      ] as const
    ).entries()) {
      const successor = await buildJourneyScriptForcedSuccessor({
        category,
        predecessor: current,
        ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
        operatorVkey: current.header.operatorVkey,
        endTime: current.header.endTime + 40_000n,
        blockSlot: current.header.blockSlot + 40n,
        orderKey: {
          transactionId: (index + 1).toString(16).padStart(64, "0"),
          outputIndex: 0n,
        },
      });
      expect(successor.replay.trace.verdict).toBe("accepted");
      current = successor;
    }
  },
  60_000,
);
