import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";

export const planProtectedOutputSignerOutputOpening = ({
  evidence,
  nativeTxCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan =>
  planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 2,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      (evidence.subject.source_kind === 1n
        ? deriveMidgardForcedTxFaultEvidenceMaterial
        : deriveMidgardNativeTxFaultEvidenceMaterial)(
        Buffer.from(evidence.canonicalTransactionCborHex, "hex"),
      ).fieldPreimages[2]!,
    ),
    owner,
    ...(publish === undefined ? {} : { publish }),
    label: "protected-output-signer-missing outputs",
  });

export const planProtectedOutputSignerWitnessOpening = ({
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan => {
  const compact = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  return planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 7,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.addressWitnessFieldPreimageHex, "hex"),
    ),
    owner,
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    },
    anchorWitnessSetHash: evidence.witnessSetHashHex,
    ...(publish === undefined ? {} : { publish }),
    label: "protected-output-signer-missing address witnesses",
  });
};
