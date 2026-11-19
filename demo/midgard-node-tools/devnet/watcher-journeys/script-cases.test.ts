import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeMidgardTxOutput,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  type DirectPurposeArtifactAdmission,
  type DirectPurposeReplay,
  prepareDirectPurposeArtifact,
} from "@al-ft/midgard-fault-proofs/direct-purpose-family";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/distinct-asset-accumulation-limit";
import { EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/execution-native-script-invalid";
import { EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/execution-source-script-decoding";
import { MISSING_REDEEMER_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/missing-redeemer";
import { MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/missing-script-source";
import { prepareNativeScriptDecodingWorkflowArtifact } from "@al-ft/midgard-fault-proofs/native-script-decoding/artifact";
import { prepareNativeScriptInvalidForcedArtifact } from "@al-ft/midgard-fault-proofs/native-script-invalid";
import {
  outputReferenceScriptEvidenceCloses,
  prepareOutputReferenceScriptDecodingEvidence,
} from "@al-ft/midgard-fault-proofs/output-reference-script-decoding";
import { RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/receive-purpose-language";
import {
  prepareRedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "@al-ft/midgard-fault-proofs/redeemer-canonicity";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/script-integrity-hash-mismatch";
import { prepareScriptIntegrityHashMissingArtifact } from "@al-ft/midgard-fault-proofs/script-integrity-hash-missing/artifact";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { UNUSED_REDEEMER_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/unused-redeemer";
import { UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY } from "@al-ft/midgard-fault-proofs/unused-script-witness";
import {
  prepareWitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceCloses,
} from "@al-ft/midgard-fault-proofs/witness-script-decoding";
import { classifyCanonicalBlockViolations } from "@al-ft/midgard-fault-proofs/workflow/classification";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import { headerDecisionReplayContext } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
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
  nativeScriptDecoding: NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  nativeScriptInvalid: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  witnessScriptDecoding: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMissing:
    SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  outputReferenceScriptDecoding:
    OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  redeemerCanonicity: REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  missingRedeemer: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  missingScriptSource: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  unusedRedeemer: UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  receivePurposeLanguage: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMismatch:
    SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  executionSourceScriptDecoding:
    EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  executionNativeScriptInvalid:
    EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  distinctAssetAccumulationLimit:
    DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  unusedScriptWitness: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
};
const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");
/** The direct purpose families, by category, whose artifact the kit prepares. */
const DIRECT_PURPOSE_ARTIFACT_FAMILIES: Partial<
  Record<string, DirectPurposeArtifactAdmission<DirectPurposeReplay>>
> = {
  missingRedeemer: MISSING_REDEEMER_ARTIFACT_FAMILY,
  missingScriptSource: MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY,
  unusedRedeemer: UNUSED_REDEEMER_ARTIFACT_FAMILY,
  receivePurposeLanguage: RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY,
  scriptIntegrityHashMismatch: SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY,
  executionSourceScriptDecoding:
    EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY,
  executionNativeScriptInvalid: EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY,
  distinctAssetAccumulationLimit:
    DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY,
  unusedScriptWitness: UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY,
};

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
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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
        redeemerCanonicityEvidenceCloses(
          prepareRedeemerCanonicityEvidence({
            finding: { subject, redeemerIndex: 0n },
            fieldPreimage: material.fieldPreimages[8]!,
            committedFieldHashHex: midgardFieldCommitment(
              material.fieldPreimages[8]!,
            ).toString("hex"),
          }),
        ),
      ).toBe(true);
    if (category === "witnessScriptDecoding")
      expect(
        witnessScriptDecodingEvidenceCloses(
          prepareWitnessScriptDecodingEvidence({
            finding: {
              subject,
              scriptIndex: 0n,
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
        outputReferenceScriptEvidenceCloses(
          prepareOutputReferenceScriptDecodingEvidence({
            subject,
            outputIndex: 0n,
            canonicalTransactionCbor: forced.fullTransactionCbor,
          }),
        ),
      ).toBe(true);
    if (category === "nativeScriptInvalid")
      expect(
        (await prepareNativeScriptInvalidForcedArtifact({ block: evidence }))
          .headerHash,
      ).toBe(block.headerHash);
    if (
      category === "nativeScriptDecoding" ||
      category === "scriptIntegrityHashMissing" ||
      category === "missingRedeemer" ||
      category === "missingScriptSource" ||
      category === "unusedRedeemer" ||
      category === "receivePurposeLanguage" ||
      category === "scriptIntegrityHashMismatch" ||
      category === "executionSourceScriptDecoding" ||
      category === "executionNativeScriptInvalid" ||
      category === "distinctAssetAccumulationLimit" ||
      category === "unusedScriptWitness"
    ) {
      const context = headerDecisionReplayContext(classification.decision);
      const replayer = replayers[category];
      const detections = requireCompleteCanonicalReplayDecision({
        evidence,
        replayer,
        decision: await replayer.replay(evidence, context),
        context,
      });
      const selected = await classifyCanonicalBlockViolations({
        evidence,
        detections,
        minimumConfirmationDepth: 30,
      });
      if (selected.decision !== "fault_detected")
        throw new Error("Closing classification disappeared");
      const directPurpose = DIRECT_PURPOSE_ARTIFACT_FAMILIES[category];
      const artifact =
        category === "nativeScriptDecoding"
          ? await prepareNativeScriptDecodingWorkflowArtifact({
              evidence,
              classification: selected,
              replayContext: context,
            })
          : directPurpose !== undefined
            ? await prepareDirectPurposeArtifact(directPurpose, {
                evidence,
                classification: selected,
                replayContext: context,
              })
            : await prepareScriptIntegrityHashMissingArtifact({
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

it.each([
  "missingRedeemer",
  "missingScriptSource",
  "unusedRedeemer",
  "receivePurposeLanguage",
  "scriptIntegrityHashMismatch",
  "executionSourceScriptDecoding",
  "executionNativeScriptInvalid",
  "distinctAssetAccumulationLimit",
  "unusedScriptWitness",
] as const)(
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
