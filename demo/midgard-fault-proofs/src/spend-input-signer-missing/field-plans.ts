import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import type { SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";

export const planSpendInputSignerInputOpening = ({
  evidence,
  nativeTxCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan =>
  planFaultProofFieldOpening({
    fieldIndex: 0,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.inputFieldPreimageHex, "hex"),
    ),
    owner,
    ...(publish === undefined ? {} : { publish }),
    label: "spend-input-signer-missing spend inputs",
  });

export const planSpendInputSignerWitnessOpening = ({
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  owner,
  publish,
}: {
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan => {
  const compact = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  return planFaultProofFieldOpening({
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
    label: "spend-input-signer-missing address witnesses",
  });
};
