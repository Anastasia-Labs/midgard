import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeMidgardTxOutput,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
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
  type CompleteCanonicalReplay,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import { headerDecisionReplayContext } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import * as SDK from "@al-ft/midgard-sdk/users";
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
  CompleteCanonicalReplay
> = {
  minAda: MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  valueNotPreserved: VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
  missingSignature: MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  transactionOutputNonCanonical:
    TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  resolvedOutputNonCanonical:
    RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  fieldPreimageLengthMismatch:
    FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  fieldItemWidthIllegal: FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  mintDeclaredAssetLimit: MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
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
  const subject = SDKFraudProof.forcedVerdictSubject({
    transactionId: forced.value.tx_id,
    sourceKey: forced.key,
    rejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
  });
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (category === "redeemerCanonicity")
    return redeemerCanonicityEvidenceCloses(
      prepareRedeemerCanonicityEvidence({
        finding: { subject, redeemerIndex: 0n },
        fieldPreimage: material.fieldPreimages[8]!,
        committedFieldHashHex: midgardFieldCommitment(
          material.fieldPreimages[8]!,
        ).toString("hex"),
      }),
    );
  if (category === "witnessScriptDecoding")
    return witnessScriptDecodingEvidenceCloses(
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
    );
  if (category === "outputReferenceScriptDecoding")
    return outputReferenceScriptEvidenceCloses(
      prepareOutputReferenceScriptDecodingEvidence({
        subject,
        outputIndex: 0n,
        canonicalTransactionCbor: forced.fullTransactionCbor,
      }),
    );
  if (category === "nativeScriptInvalid")
    return (
      (await prepareNativeScriptInvalidForcedArtifact({ block: evidence }))
        .headerHash === headerHash
    );
  const context = headerDecisionReplayContext(verified.decision);
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
