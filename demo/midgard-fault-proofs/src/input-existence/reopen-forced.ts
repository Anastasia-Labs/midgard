/** Reopen retained forced-source evidence through the current common admission.
 * Derived subject/selection fields are reconstructed, never trusted from a caller. */
import { computeHash32 } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  admitInputExistenceArtifact,
  INPUT_EXISTENCE_ARTIFACT,
} from "./artifact.js";
import type { PreparedInputExistence } from "./prepare.js";
import { deriveInputExistenceSelection } from "./selection.js";
import type { OpenedProofThreadSource } from "./source.js";

export type ForcedInputExistenceEvidence = Readonly<{
  headerHash: string;
  forcedSource: Readonly<{
    header: SDK.Header;
    membership: SDK.ForcedTransactionSourceMembershipProof;
  }>;
  fullTransactionCbor: string;
  inputIndex: bigint;
  eventMembership: SDK.EventToStepMembershipProof;
  transitionMembership: SDK.IndexedTraceProof;
  ledgerMembership: Readonly<{ value: string; proof: SDK.Proof }> | null;
}>;

export const reopenForcedInputExistence = (
  evidence: ForcedInputExistenceEvidence,
  field: 0 | 1,
  owner: string,
) =>
  admitInputExistenceArtifact(
    {
      schemaVersion: INPUT_EXISTENCE_ARTIFACT,
      field,
      headerHash: evidence.headerHash,
      header: Data.to(evidence.forcedSource.header, SDK.Header),
      source: {
        kind: "forced",
        membership: Data.to(
          evidence.forcedSource.membership,
          SDK.ForcedTransactionSourceMembershipProof,
        ),
      },
      transaction: evidence.fullTransactionCbor,
      inputIndex: evidence.inputIndex.toString(),
      event: Data.to(evidence.eventMembership, SDK.EventToStepMembershipProof),
      transition: Data.to(evidence.transitionMembership, SDK.IndexedTraceProof),
      ledger:
        evidence.ledgerMembership === null
          ? "OutOfDomain"
          : {
              kind: "present",
              value: evidence.ledgerMembership.value,
              proof: Data.to(evidence.ledgerMembership.proof, SDK.Proof),
            },
    },
    owner,
  );

/** Deterministic material construction only, also used by raw adversarial test
 * builders. This does not authenticate roots or the supplied ledger witness;
 * production submission must use reopenForcedInputExistence instead. */
export const forcedInputExistenceMaterial = (
  evidence: ForcedInputExistenceEvidence,
  field: 0 | 1,
  owner: string,
): PreparedInputExistence => {
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    Buffer.from(evidence.fullTransactionCbor, "hex"),
  );
  const leaf = evidence.forcedSource.membership.value;
  const source: OpenedProofThreadSource = {
    kind: "forced",
    headerHash: evidence.headerHash,
    header: evidence.forcedSource.header,
    membership: evidence.forcedSource.membership,
    eventKey: {
      ForcedTransactionEventKey: {
        tx_order_id: evidence.forcedSource.membership.key,
      },
    },
    subject: SDK.forcedVerdictSubject({
      transactionId: leaf.tx_id,
      sourceKey: evidence.forcedSource.membership.key,
      rejectionReason:
        leaf.verdict === "ForcedTxValid"
          ? null
          : leaf.verdict.ForcedTxInvalid.reason,
    }),
    compactCbor: leaf.submitted_source.compact_cbor,
    witnessSetCompactCbor: leaf.submitted_source.witness_set_compact_cbor,
    fieldPreimageLengthsCbor: leaf.submitted_source.field_preimage_lengths_cbor,
    fullTransactionCbor: evidence.fullTransactionCbor,
    fieldPreimages: material.fieldPreimages,
  };
  return {
    field,
    source,
    ...deriveInputExistenceSelection({
      source,
      field,
      inputIndex: evidence.inputIndex,
      owner,
    }),
    eventMembership: evidence.eventMembership,
    transitionMembership: evidence.transitionMembership,
    ledgerEvidence:
      evidence.ledgerMembership === null
        ? "OutOfDomain"
        : {
            Present: {
              ...evidence.ledgerMembership,
              value_hash: computeHash32(
                Buffer.from(evidence.ledgerMembership.value, "hex"),
              ).toString("hex"),
            },
          },
  };
};
