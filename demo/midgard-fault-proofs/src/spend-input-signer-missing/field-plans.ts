import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.plan-fault-proof-field-opening.js";
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
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
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
