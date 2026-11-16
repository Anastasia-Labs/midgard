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
import { beforeAll, describe, expect, it } from "vitest";

import { journeyForcedOrderSubmission } from "./forced-order.js";
import { prepareJourneyForcedTransaction } from "./forced-transaction.js";
import {
  classifyFullCatalogueTransactionFixture,
  verifyJourneyFixture,
} from "./journey-classification.test-support.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
  type JourneyScriptForcedCategory,
} from "./script-cases.js";
import { stackNodeFeeSchedule } from "./stack-fee-schedule.test-support.js";
import {
  buildJourneyForcedTransaction,
  JOURNEY_FORCED_TRANSACTION_CATEGORIES,
  type JourneyForcedTransactionCategory,
} from "./transaction-forced-cases.js";
import { prepareJourneyTransactionProof } from "./transaction-proof-material.js";

const account = generateEmulatorAccount({ lovelace: 100_000_000n });
const owner = Buffer.from(
  CML.PrivateKey.from_bech32(
    walletFromSeed(account.seedPhrase, { network: "Custom" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_raw_bytes(),
);
const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };

const replayers: Record<
  JourneyForcedTransactionCategory | JourneyScriptForcedCategory,
  FP.CompleteCanonicalReplay
> = {
  minAda: FP.MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  valueNotPreserved: FP.VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
  missingSignature: FP.MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  transactionOutputNonCanonical:
    FP.TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  resolvedOutputNonCanonical:
    FP.RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  fieldPreimageLengthMismatch:
    FP.FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  fieldItemWidthIllegal: FP.FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  mintDeclaredAssetLimit:
    FP.MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  nativeScriptDecoding: FP.NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  nativeScriptInvalid: FP.NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  witnessScriptDecoding: FP.WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMissing:
    FP.SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  outputReferenceScriptDecoding:
    FP.OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  redeemerCanonicity: FP.REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  missingRedeemer: FP.MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  missingScriptSource: FP.MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  unusedRedeemer: FP.UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  receivePurposeLanguage: FP.RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  scriptIntegrityHashMismatch:
    FP.SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  executionSourceScriptDecoding:
    FP.EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  executionNativeScriptInvalid:
    FP.EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  distinctAssetAccumulationLimit:
    FP.DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  unusedScriptWitness: FP.UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
};

/** The direct purpose families, by category, whose artifact the kit prepares. */
const DIRECT_PURPOSE_ARTIFACT_FAMILIES: Partial<
  Record<string, FP.DirectPurposeArtifactAdmission<FP.DirectPurposeReplay>>
> = {
  missingRedeemer: FP.MISSING_REDEEMER_ARTIFACT_FAMILY,
  missingScriptSource: FP.MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY,
  unusedRedeemer: FP.UNUSED_REDEEMER_ARTIFACT_FAMILY,
  receivePurposeLanguage: FP.RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY,
  scriptIntegrityHashMismatch:
    FP.SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY,
  executionSourceScriptDecoding:
    FP.EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY,
  executionNativeScriptInvalid:
    FP.EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY,
  distinctAssetAccumulationLimit:
    FP.DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY,
  unusedScriptWitness: FP.UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY,
};

type Predecessor = Parameters<
  typeof prepareJourneyForcedTransaction
>[0]["predecessor"];
let predecessor: Predecessor;
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

const fixtureInput = () => ({
  predecessor,
  ledgerOwnerSeedPhrase: account.seedPhrase,
  operatorVkey: "82".repeat(28),
  endTime: predecessor.header.endTime + 40_000n,
  blockSlot: predecessor.header.blockSlot + 40n,
  orderKey,
});

it("places a transaction the tx-order mint admits in every forced order", () => {
  expect(() =>
    SDK.deriveTxOrderMaterial({
      submittedTxCbor: journeyForcedOrderSubmission(
        prepareJourneyForcedTransaction({
          predecessor,
          ledgerOwnerSeedPhrase: account.seedPhrase,
        }).transaction,
      ),
      owner,
    }),
  ).not.toThrow();
});

/** The prepared proof for a classified wrongful forced rejection. */
const prepareScriptProof = async (
  category: JourneyScriptForcedCategory,
  verified: Awaited<ReturnType<typeof verifyJourneyFixture>>,
  headerHash: string,
) => {
  const evidence = verified.evidence;
  if (evidence === undefined) throw new Error("No canonical evidence");
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
    return FP.redeemerCanonicityEvidenceCloses(
      FP.prepareRedeemerCanonicityEvidence({
        finding: { subject, redeemerIndex: 0n },
        fieldPreimage: material.fieldPreimages[8]!,
        committedFieldHashHex: midgardFieldCommitment(
          material.fieldPreimages[8]!,
        ).toString("hex"),
      }),
    );
  if (category === "witnessScriptDecoding")
    return FP.witnessScriptDecodingEvidenceCloses(
      FP.prepareWitnessScriptDecodingEvidence({
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
    );
  if (category === "outputReferenceScriptDecoding")
    return FP.outputReferenceScriptEvidenceCloses(
      FP.prepareOutputReferenceScriptDecodingEvidence({
        subject,
        outputIndex: 0n,
        canonicalTransactionCbor: forced.fullTransactionCbor,
      }),
    );
  if (category === "nativeScriptInvalid")
    return (
      (await FP.prepareNativeScriptInvalidForcedArtifact({ block: evidence }))
        .headerHash === headerHash
    );
  const context = FP.headerDecisionReplayContext(verified.decision);
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
  const directPurpose = DIRECT_PURPOSE_ARTIFACT_FAMILIES[category];
  const artifact =
    category === "nativeScriptDecoding"
      ? await FP.prepareNativeScriptDecodingWorkflowArtifact({
          evidence,
          classification: selected,
          replayContext: context,
        })
      : directPurpose !== undefined
        ? await FP.prepareDirectPurposeArtifact(directPurpose, {
            evidence,
            classification: selected,
            replayContext: context,
          })
        : await FP.prepareScriptIntegrityHashMissingArtifact({
            evidence,
            classification: selected,
          });
  return artifact.headerHash === headerHash;
};

/** Held out of this check until the owner rules on their journey fixtures. */
const pendingRuling = new Set<string>([
  "outputReferenceScriptDecoding",
  "redeemerCanonicity",
]);

describe("wrongful forced rejections of the native-only order transaction", () => {
  it.each(
    [
      ...JOURNEY_FORCED_TRANSACTION_CATEGORIES,
      ...JOURNEY_SCRIPT_FORCED_CATEGORIES,
    ].filter((category) => !pendingRuling.has(category)),
  )(
    "%s classifies, prepares its proof, and its honest successor is healthy",
    async (category) => {
      const input = { ...fixtureInput(), category };
      const forced = {
        transaction: prepareJourneyForcedTransaction(input).transaction,
        orderKey,
      };
      const isScript = (
        JOURNEY_SCRIPT_FORCED_CATEGORIES as readonly string[]
      ).includes(category);
      const block = isScript
        ? await buildJourneyScriptForcedFault({
            ...input,
            category: category as JourneyScriptForcedCategory,
          })
        : await buildJourneyForcedTransaction({
            ...input,
            category: category as JourneyForcedTransactionCategory,
          });
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
      const verified = await verifyJourneyFixture({
        category,
        replayer: replayers[category],
        block,
        predecessor,
        history: [],
      });
      if (isScript)
        expect(
          await prepareScriptProof(
            category as JourneyScriptForcedCategory,
            verified,
            block.headerHash,
          ),
        ).toBe(true);
      else
        expect(
          await prepareJourneyTransactionProof({
            category: category as JourneyForcedTransactionCategory,
            block,
            verified,
            predecessor,
          }),
        ).toBeDefined();
      const successor = isScript
        ? await buildJourneyScriptForcedSuccessor({
            ...input,
            category: category as JourneyScriptForcedCategory,
          })
        : await buildJourneyForcedTransaction({
            ...input,
            category: category as JourneyForcedTransactionCategory,
            honest: true,
          });
      const healthy = await classifyFullCatalogueTransactionFixture({
        block: successor,
        predecessor,
        history: [],
        forced,
      });
      expect(healthy.decision).toMatchObject({ decision: "healthy" });
    },
    120_000,
  );
});
