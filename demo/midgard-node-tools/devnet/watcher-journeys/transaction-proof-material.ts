import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import { prepareCanonicalDecodability } from "@al-ft/midgard-fault-proofs/canonical-decodability";
import { prepareCommittedFieldShapeFromCanonicalTx } from "@al-ft/midgard-fault-proofs/committed-field-shape";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs/evidence";
import { deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock } from "@al-ft/midgard-fault-proofs/field-item-width-illegal";
import { prepareFieldPreimageLengthWorkflow } from "@al-ft/midgard-fault-proofs/field-preimage-length-mismatch";
import { prepareL2TxMistagFromTransactions } from "@al-ft/midgard-fault-proofs/l2-tx-mistag";
import { prepareMinAdaForcedPlan } from "@al-ft/midgard-fault-proofs/min-ada";
import { prepareMintAuthorizationReplay } from "@al-ft/midgard-fault-proofs/mint-authorization";
import { prepareMintDeclaredAssetLimitForcedArtifact } from "@al-ft/midgard-fault-proofs/mint-declared-asset-limit";
import { deriveMintItemNonCanonicalEvidenceFromCanonicalBlock } from "@al-ft/midgard-fault-proofs/mint-item-non-canonical";
import { prepareMissingSignatureWrongfulRejection } from "@al-ft/midgard-fault-proofs/missing-signature";
import { prepareNetworkIdFromCanonicalEvidence } from "@al-ft/midgard-fault-proofs/network-id";
import {
  observerOrderInvalidRawBlockEvidenceFromVerifiedPayload,
  prepareObserverOrderInvalidAcceptedArtifact,
} from "@al-ft/midgard-fault-proofs/observer-order-invalid";
import {
  observersForbiddenRawBlockEvidenceFromVerifiedPayload,
  prepareObserversForbiddenAcceptedArtifact,
} from "@al-ft/midgard-fault-proofs/observers-forbidden-on-untagged-network";
import { prepareDaHashPreimageFromRetainedDa } from "@al-ft/midgard-fault-proofs/prepare-da-hash-preimage";
import { prepareInvalidRangeFromTransactions } from "@al-ft/midgard-fault-proofs/prepare-invalid-range";
import { prepareInvalidSignatureFromTransactions } from "@al-ft/midgard-fault-proofs/prepare-invalid-signature";
import { prepareMinFeeFromTransactions } from "@al-ft/midgard-fault-proofs/prepare-min-fee";
import { prepareZeroInputFromTransactions } from "@al-ft/midgard-fault-proofs/prepare-zero-input";
import { prepareProtectedOutputSignerMissingEvidence } from "@al-ft/midgard-fault-proofs/protected-output-signer-missing";
import {
  deriveResolvedOutputPriorLedgerReplay,
  detectResolvedOutputNonCanonicalCompleteReplay,
  resolvedOutputEvidenceCloses,
} from "@al-ft/midgard-fault-proofs/resolved-output-non-canonical";
import { signerWitnessVerdictCloses } from "@al-ft/midgard-fault-proofs/signer-witness-scan";
import { deriveSpendInputSignerMissingEvidenceFromCompleteReplay } from "@al-ft/midgard-fault-proofs/spend-input-signer-missing";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  checkedLedgerAfter,
  ledgerAfter,
} from "@al-ft/midgard-fault-proofs/test-support/predecessor-ledger";
import { deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock } from "@al-ft/midgard-fault-proofs/transaction-output-non-canonical";
import { prepareValueConservationArtifact } from "@al-ft/midgard-fault-proofs/value-not-preserved";
import { classifyCanonicalBlockViolations } from "@al-ft/midgard-fault-proofs/workflow/classification";
import { INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY } from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import { prepareInputSetUniquenessArtifact } from "@al-ft/midgard-fault-proofs/workflow/input-set-uniqueness";
import type * as SDKCore from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";

import type { verifyJourneyFixture } from "./fixture-verification.js";
import type { JourneyTransactionCategory } from "./transaction-cases.js";
import type { JourneyForcedTransactionCategory } from "./transaction-forced-cases.js";
import type { JourneyTransactionSourceCategory } from "./transaction-source-cases.js";

type Verification = Awaited<ReturnType<typeof verifyJourneyFixture>>;

/** Exercise the production proof preparer against the exact classified bytes. */
export const prepareJourneyTransactionProof = async (input: {
  category:
    | JourneyTransactionCategory
    | JourneyForcedTransactionCategory
    | JourneyTransactionSourceCategory;
  block: {
    header: SDKCore.Header;
    headerHash: string;
    payloadEnvelopeCbor: Uint8Array;
    payload: SDKDa.DaPayload;
  };
  verified: Verification;
  predecessor: {
    header: SDKCore.Header;
    headerHash: string;
    payloadEnvelopeCbor: Uint8Array;
  };
}): Promise<unknown> => {
  const { category, block, verified } = input;
  const raw = {
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da" as const,
      sourceId: "retained-journey",
      grade: "security" as const,
    },
  };
  if (category === "daHashPreimage")
    return prepareDaHashPreimageFromRetainedDa({
      observation: raw.observation,
      sources: verified.sources,
    });
  if (category === "observersForbiddenOnUntaggedNetwork")
    return prepareObserversForbiddenAcceptedArtifact(
      await observersForbiddenRawBlockEvidenceFromVerifiedPayload(raw),
    );
  if (category === "observerOrderInvalid")
    return prepareObserverOrderInvalidAcceptedArtifact(
      await observerOrderInvalidRawBlockEvidenceFromVerifiedPayload(raw),
    );
  if (
    category === "canonicalDecodability" ||
    category === "committedFieldShape"
  ) {
    const entry = block.payload.block_body.transaction_preimages[0];
    if (entry === undefined) throw new Error("Source proof preimage missing");
    const tx = decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence(
      Buffer.from(entry[1], "hex"),
    );
    return category === "committedFieldShape"
      ? prepareCommittedFieldShapeFromCanonicalTx({ tx, fieldIndex: 4 })
      : prepareCanonicalDecodability({
          badTxId: entry[0],
          nativeTxCompactCbor: encodeMidgardNativeTxCompact(
            deriveMidgardNativeTxCompact(tx.body, tx.witnessSet, tx.validity),
          ).toString("hex"),
          fieldIndex: 4,
          committedPreimage: tx.body.requiredSignersPreimageCbor,
        });
  }
  const evidence = verified.evidence;
  if (evidence === undefined)
    throw new Error(`Missing canonical proof evidence for ${category}`);
  const common = {
    headerHash: block.headerHash,
    transactions: evidence.transactions,
    expectedTransactionsRoot: block.header.transactionsRoot,
  };
  if (
    category === "spendInputSignerMissing" ||
    category === "resolvedOutputNonCanonical"
  ) {
    // The prior ledger is the named predecessor's; the helper refuses an
    // absent or substituted one for a non-genesis header.
    const priorLedger = await deriveResolvedOutputPriorLedgerReplay({
      block: evidence,
      predecessor: await checkedLedgerAfter(
        await canonicalBlockEvidenceFromVerifiedPayload({
          ...raw,
          observation: authenticatedHeaderObservation(input.predecessor),
          payloadEnvelopeCbor: input.predecessor.payloadEnvelopeCbor,
        }),
      ),
    });
    if (category === "spendInputSignerMissing") {
      const prepared = deriveSpendInputSignerMissingEvidenceFromCompleteReplay({
        block: evidence,
        priorLedger,
      });
      if (!signerWitnessVerdictCloses(prepared))
        throw new Error("Spend-input signer proof does not close");
      return prepared;
    }
    const prepared = detectResolvedOutputNonCanonicalCompleteReplay({
      block: evidence,
      priorLedger,
    });
    if (prepared.length !== 1 || !resolvedOutputEvidenceCloses(prepared[0]!))
      throw new Error("Resolved output proof does not close uniquely");
    return prepared[0];
  }
  if (category === "missingSignature")
    return prepareMissingSignatureWrongfulRejection({ block: evidence });
  if (category === "inputSetUniqueness") {
    const replay =
      await INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== category
    )
      throw new Error("Proof preparation changed classification");
    return prepareInputSetUniquenessArtifact({
      evidence,
      classification,
    });
  }
  switch (category) {
    case "zeroInput":
      return prepareZeroInputFromTransactions(common);
    case "invalidRange":
      return prepareInvalidRangeFromTransactions({
        ...common,
        blockSlot: block.header.blockSlot,
      });
    case "invalidSignature":
      return prepareInvalidSignatureFromTransactions(common);
    case "minFee":
      return prepareMinFeeFromTransactions({
        ...common,
        minFeeA: block.header.minFeeA,
        minFeeB: block.header.minFeeB,
      });
    case "minAda":
      return prepareMinAdaForcedPlan({ block: evidence });
    case "networkId": {
      const expectedNetworkId = block.header.expectedNetworkId;
      if (expectedNetworkId !== 0n && expectedNetworkId !== 1n)
        throw new Error("Network proof needs tagged predecessor network");
      return prepareNetworkIdFromCanonicalEvidence({
        evidence,
        expectedNetworkId,
      });
    }
    case "l2TxMistag":
      return prepareL2TxMistagFromTransactions(common);
    case "transactionOutputNonCanonical":
      return deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        evidence,
      );
    case "fieldItemWidthIllegal":
      return deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(evidence);
    case "mintDeclaredAssetLimit":
      return prepareMintDeclaredAssetLimitForcedArtifact(evidence);
    case "mintItemNonCanonical":
      return deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(evidence);
    case "mintAuthorization": {
      const predecessor = await ledgerAfter(
        await canonicalBlockEvidenceFromVerifiedPayload({
          ...raw,
          observation: authenticatedHeaderObservation(input.predecessor),
          payloadEnvelopeCbor: input.predecessor.payloadEnvelopeCbor,
        }),
      );
      const prepared = await prepareMintAuthorizationReplay({
        current: evidence.reconstruction,
        predecessor,
        sourceIndex: 0,
      });
      if (prepared.length !== 1)
        throw new Error(
          "Mint authority fixture must have exactly one usable proof",
        );
      return prepared[0];
    }
    case "valueNotPreserved": {
      const predecessor = await checkedLedgerAfter(
        await canonicalBlockEvidenceFromVerifiedPayload({
          ...raw,
          observation: authenticatedHeaderObservation(input.predecessor),
          payloadEnvelopeCbor: input.predecessor.payloadEnvelopeCbor,
        }),
      );
      const prepared = await prepareValueConservationArtifact({
        block: evidence,
        predecessor,
        sourceIndex: 0,
        forced: true,
      });
      if (prepared === null)
        throw new Error("Value fixture has no usable conservation proof");
      return prepared;
    }
    case "protectedOutputSignerMissing": {
      const transaction = evidence.transactions[0];
      if (transaction === undefined)
        throw new Error("Protected-output transaction absent");
      const prepared = prepareProtectedOutputSignerMissingEvidence({
        subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
        outputIndex: 0n,
        canonicalTransactionCbor: Buffer.from(transaction.txCbor, "hex"),
      });
      if (!signerWitnessVerdictCloses(prepared))
        throw new Error("Protected-output proof does not close");
      return prepared;
    }
    case "fieldPreimageLengthMismatch": {
      const forced = evidence.reconstruction.forcedTransactions[0];
      if (forced === undefined || forced.value.verdict === "ForcedTxValid")
        throw new Error("Forced length claim absent");
      const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
        forced.fullTransactionCbor,
      );
      return prepareFieldPreimageLengthWorkflow({
        headerHash: block.headerHash,
        transactionId: forced.value.tx_id,
        direction: "wrongfulRejection",
        sourceKind: "forced",
        fieldIndex: 0,
        fieldPreimageLengthsCbor: Buffer.from(
          forced.value.submitted_source.field_preimage_lengths_cbor,
          "hex",
        ),
        fieldPreimage: tx.body.spendInputsPreimageCbor,
        forcedRejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
      });
    }
  }
};
