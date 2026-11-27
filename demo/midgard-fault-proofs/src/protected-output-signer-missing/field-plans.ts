import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.plan-fault-proof-field-opening.js";
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
