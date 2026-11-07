import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpening } from "../field-opening.js";
import type {
  DoubleSpendArtifact,
  DoubleSpendConstrainedWorkflowAdapterConfig,
} from "./double-spend-adapter.preflight-of.js";
export const createDoubleSpendFieldOpeningPlan = (
  config: DoubleSpendConstrainedWorkflowAdapterConfig,
) => {
  const fieldPlan = (
    proofStage: "step_03" | "step_04",
    artifact: DoubleSpendArtifact,
  ) => {
    const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
    return planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
      anchorTxId: tx.nativeTxId,
      nativeTxCompactCbor: tx.nativeTxCompactCbor,
      itemCbors: tx.spendInputCbors.map((cbor) => Buffer.from(cbor, "hex")),
      owner: config.signer.paymentKeyHash,
      label: `${proofStage} production preflight`,
    });
  };

  return fieldPlan;
};
