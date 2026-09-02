import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";

import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
} from "../field-opening-v1.js";
import type { ProtectedOutputSignerMissingEvidenceV1 } from "./protected-output-signer-missing-v1.js";

export const planProtectedOutputSignerOutputOpeningV1 = ({
  evidence,
  nativeTxCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlanV1 =>
  planFaultProofFieldOpeningV1({
    fieldIndex: 2,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimageV1(
      deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(evidence.canonicalTransactionCborHex, "hex"),
      ).fieldPreimages[2]!,
    ),
    owner,
    ...(publish === undefined ? {} : { publish }),
    label: "protected-output-signer-missing outputs",
  });

export const planProtectedOutputSignerWitnessOpeningV1 = ({
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlanV1 => {
  const compact = decodeMidgardNativeTxWitnessSetCompactV1(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  return planFaultProofFieldOpeningV1({
    fieldIndex: 7,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimageV1(
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
