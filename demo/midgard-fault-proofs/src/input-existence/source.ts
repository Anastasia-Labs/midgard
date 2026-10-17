/** One source-opening producer for both verdict directions. It consumes
 * authenticated L1/public-DA evidence and emits source material, never resolved
 * L1 redeemer indices or an unchecked onchain VerifiedTxSourceView. */
import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/native";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { commitCountedRoot } from "../transition-trace/phas.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import {
  buildForcedTransactionLeafMembershipProof,
  buildRawL2TransactionSourceMembershipProof,
} from "../transition-trace/witnesses.js";
import type { NativeInclusionArtifact } from "../workflow/native-index-artifact.js";

type CommonSource = Readonly<{
  headerHash: string;
  header: SDK.Header;
  eventKey: SDK.EventKey;
  subject: SDK.VerdictSubject;
  compactCbor: string;
  witnessSetCompactCbor: string;
  fieldPreimageLengthsCbor: string;
  fullTransactionCbor: string;
  fieldPreimages: readonly Buffer[];
}>;

export type OpenedProofThreadSource = CommonSource &
  (
    | Readonly<{ kind: "accepted"; inclusion: NativeInclusionArtifact }>
    | Readonly<{
        kind: "forced";
        membership: SDK.ForcedTransactionSourceMembershipProof;
      }>
  );

const requireSourceBytes = (
  material: Readonly<{
    transactionId: Buffer;
    proofSource: Readonly<{
      compactCbor: Buffer;
      witnessSetCompactCbor: Buffer;
      fieldPreimageLengthsCbor: Buffer;
    }>;
  }>,
  transactionId: string,
  source: SDK.ForcedInclusionTxV1["submitted_source"],
): void => {
  if (
    material.transactionId.toString("hex") !== transactionId ||
    material.proofSource.compactCbor.toString("hex") !== source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      source.field_preimage_lengths_cbor
  )
    throw new Error(
      "input-existence: transaction preimage differs from committed source",
    );
};

export const openProofThreadSubjectFromCanonicalEvidence = async ({
  block,
  eventKey,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
}): Promise<OpenedProofThreadSource> => {
  blockTransactionsFromCanonicalEvidence(block);
  SDK.assertTransactionSourceInclusionRootAuthenticated(
    block.inclusionRootAuthentication,
  );
  const event = block.reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(eventKey),
  );
  if (
    event === undefined ||
    Data.to(event.eventKey, SDK.EventKey) !== Data.to(eventKey, SDK.EventKey)
  ) {
    throw new Error(
      "input-existence: selected source event is absent or substituted",
    );
  }
  const common = {
    headerHash: block.headerHash,
    header: block.header,
    eventKey,
  };
  if (event.phase === "L2Transaction") {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      event.entry.fullTransactionCbor,
    );
    if (
      event.entry.validity !== "TxIsValid" ||
      material.canonical.validity !== "TxIsValid"
    ) {
      throw new Error("input-existence: normal source is not an acceptance");
    }
    requireSourceBytes(material, event.entry.txId, event.entry.value.source);
    const membership = await buildRawL2TransactionSourceMembershipProof({
      reconstruction: block.reconstruction,
      txId: event.entry.txId,
    });
    if (
      membership.domain !== SDK.ROOT_DOMAINS.transactionsV1 ||
      membership.root !== block.header.transactionsRoot ||
      membership.count !== block.header.l2TransactionCount ||
      membership.count <= 0n ||
      (await commitCountedRoot({
        domain: membership.domain,
        phasRoot: membership.phas_root,
        count: membership.count,
      })) !== membership.root
    ) {
      throw new Error("input-existence: normal counted source root changed");
    }
    return {
      ...common,
      kind: "accepted",
      subject: SDK.acceptedVerdictSubject(event.entry.txId),
      compactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      fieldPreimageLengthsCbor:
        material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      fullTransactionCbor: event.entry.fullTransactionCbor.toString("hex"),
      fieldPreimages: material.fieldPreimages,
      inclusion: {
        nativeTxId: event.entry.txId,
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
        l2TransactionSourceCbor: membership.value,
        transactionsPhasRoot: membership.phas_root,
        txMembershipProofCbor: Data.to(membership.proof, SDK.Proof),
      },
    };
  }
  if (event.phase !== "ForcedTransaction") {
    throw new Error(
      "input-existence: selected source is not a transaction event",
    );
  }
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    event.entry.fullTransactionCbor,
  );
  requireSourceBytes(
    material,
    event.entry.value.tx_id,
    event.entry.value.submitted_source,
  );
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  if (
    membership.domain !== SDK.ROOT_DOMAINS.forcedTransactionsV1 ||
    membership.root !== block.header.forcedTransactionsRoot ||
    membership.count !== block.header.forcedTransactionCount ||
    membership.count <= 0n ||
    (await commitCountedRoot({
      domain: membership.domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    })) !== membership.root
  ) {
    throw new Error("input-existence: forced counted source root changed");
  }
  return {
    ...common,
    kind: "forced",
    membership,
    subject: SDK.forcedVerdictSubject({
      transactionId: event.entry.value.tx_id,
      sourceKey: event.entry.key,
      rejectionReason:
        event.entry.value.verdict === "ForcedTxValid"
          ? null
          : event.entry.value.verdict.ForcedTxInvalid.reason,
    }),
    compactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    fieldPreimageLengthsCbor:
      material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
    fullTransactionCbor: event.entry.fullTransactionCbor.toString("hex"),
    fieldPreimages: material.fieldPreimages,
  };
};
