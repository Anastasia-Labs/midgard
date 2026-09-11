import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import * as Proofs from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";

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
    header: SDK.Header;
    headerHash: string;
    payloadEnvelopeCbor: Uint8Array;
    payload: SDK.DaPayload;
  };
  verified: Verification;
  predecessor: {
    header: SDK.Header;
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
    return Proofs.prepareDaHashPreimageFromRetainedDa({
      observation: raw.observation,
      sources: verified.sources,
    });
  if (category === "observersForbiddenOnUntaggedNetwork")
    return Proofs.prepareObserversForbiddenAcceptedArtifact(
      await Proofs.observersForbiddenRawBlockEvidenceFromVerifiedPayload(raw),
    );
  if (category === "observerOrderInvalid")
    return Proofs.prepareObserverOrderInvalidAcceptedArtifact(
      await Proofs.observerOrderInvalidRawBlockEvidenceFromVerifiedPayload(raw),
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
      ? Proofs.prepareCommittedFieldShapeFromCanonicalTx({ tx, fieldIndex: 4 })
      : Proofs.prepareCanonicalDecodability({
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
    const directory = await mkdtemp("/var/tmp/midgard-transaction-proof-");
    try {
      const deploymentFingerprint = "d1".repeat(32);
      const corpus = await Proofs.resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint,
        currentEvidence: evidence,
        sources: verified.sources,
        checkpointStore:
          Proofs.createSqliteHistoricalNativeScriptCheckpointStore({
            path: join(directory, "history.sqlite"),
            rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
          }),
        historySource: Proofs.createHistoricalNativeScriptHistorySource({
          providerRoster: Proofs.createHistoricalNativeScriptProviderRoster({
            deploymentFingerprint,
            providers: [
              {
                sourceId: "archive-a",
                authorityEndpoint: "https://archive-a.example.test",
                operatorIdentitySha256: "aa".repeat(32),
              },
              {
                sourceId: "archive-b",
                authorityEndpoint: "https://archive-b.example.test",
                operatorIdentitySha256: "bb".repeat(32),
              },
            ],
          }),
        }),
      });
      const priorLedger =
        await Proofs.deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
          block: evidence,
          corpus,
        });
      if (category === "spendInputSignerMissing") {
        const prepared =
          Proofs.deriveSpendInputSignerMissingEvidenceFromCompleteReplay({
            block: evidence,
            priorLedger,
          });
        if (!Proofs.spendInputSignerMissingEvidenceCloses(prepared))
          throw new Error("Spend-input signer proof does not close");
        return prepared;
      }
      const prepared = Proofs.detectResolvedOutputNonCanonicalCompleteReplay({
        block: evidence,
        priorLedger,
      });
      if (
        prepared.length !== 1 ||
        !Proofs.resolvedOutputEvidenceCloses(prepared[0]!)
      )
        throw new Error("Resolved output proof does not close uniquely");
      return prepared[0];
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  }
  if (category === "missingSignature")
    return Proofs.prepareMissingSignatureWrongfulRejection({ block: evidence });
  if (category === "inputSetUniqueness") {
    const replay =
      await Proofs.INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
      );
    const classification = await Proofs.classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== category
    )
      throw new Error("Proof preparation changed classification");
    return Proofs.prepareInputSetUniquenessArtifact({
      evidence,
      classification,
    });
  }
  switch (category) {
    case "zeroInput":
      return Proofs.prepareZeroInputFromTransactions(common);
    case "invalidRange":
      return Proofs.prepareInvalidRangeFromTransactions({
        ...common,
        blockSlot: block.header.blockSlot,
      });
    case "invalidSignature":
      return Proofs.prepareInvalidSignatureFromTransactions(common);
    case "minFee":
      return Proofs.prepareMinFeeFromTransactions({
        ...common,
        minFeeA: block.header.minFeeA,
        minFeeB: block.header.minFeeB,
      });
    case "minAda":
      return Proofs.prepareMinAdaForcedPlan({ block: evidence });
    case "networkId": {
      const expectedNetworkId = block.header.expectedNetworkId;
      if (expectedNetworkId !== 0n && expectedNetworkId !== 1n)
        throw new Error("Network proof needs tagged predecessor network");
      return Proofs.prepareNetworkIdFromCanonicalEvidence({
        evidence,
        expectedNetworkId,
      });
    }
    case "l2TxMistag":
      return Proofs.prepareL2TxMistagFromTransactions(common);
    case "transactionOutputNonCanonical":
      return Proofs.deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        evidence,
      );
    case "fieldItemWidthIllegal":
      return Proofs.deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(
        evidence,
      );
    case "mintDeclaredAssetLimit":
      return Proofs.prepareMintDeclaredAssetLimitForcedArtifact(evidence);
    case "distinctAssetAccumulationLimit":
      return Proofs.prepareDistinctAssetAccumulationArtifact(evidence);
    case "mintAuthorization": {
      const predecessor =
        await Proofs.canonicalBlockEvidenceFromVerifiedPayload({
          ...raw,
          observation: authenticatedHeaderObservation(input.predecessor),
          payloadEnvelopeCbor: input.predecessor.payloadEnvelopeCbor,
        });
      const prepared = await Proofs.prepareMintAuthorizationReplay({
        current: evidence.reconstruction,
        predecessor: predecessor.reconstruction,
        sourceIndex: 0,
      });
      if (prepared.length !== 1)
        throw new Error(
          "Mint authority fixture must have exactly one usable proof",
        );
      return prepared[0];
    }
    case "valueNotPreserved": {
      const predecessor =
        await Proofs.canonicalBlockEvidenceFromVerifiedPayload({
          ...raw,
          observation: authenticatedHeaderObservation(input.predecessor),
          payloadEnvelopeCbor: input.predecessor.payloadEnvelopeCbor,
        });
      const prepared = await Proofs.prepareValueConservationArtifact({
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
      const prepared = Proofs.prepareProtectedOutputSignerMissingEvidence({
        subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
        outputIndex: 0,
        canonicalTransactionCbor: Buffer.from(transaction.txCbor, "hex"),
      });
      if (!Proofs.protectedOutputSignerMissingEvidenceCloses(prepared))
        throw new Error("Protected-output proof does not close");
      return prepared;
    }
    case "fieldPreimageLengthMismatch": {
      const forced = evidence.reconstruction.forcedTransactions[0];
      if (forced === undefined || forced.value.verdict === "ForcedTxValid")
        throw new Error("Forced length claim absent");
      const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
        forced.fullTransactionCbor,
      );
      return Proofs.prepareFieldPreimageLengthWorkflow({
        headerHash: block.headerHash,
        transactionId: forced.value.tx_id,
        direction: "wrongfulRejection",
        fieldIndex: 0,
        fieldPreimageLengthsCbor: Buffer.from(
          forced.value.source.field_preimage_lengths_cbor,
          "hex",
        ),
        fieldPreimage: tx.body.spendInputsPreimageCbor,
        forcedRejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
      });
    }
  }
};
