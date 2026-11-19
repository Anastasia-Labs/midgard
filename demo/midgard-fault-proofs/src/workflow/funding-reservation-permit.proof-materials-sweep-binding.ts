import { CML, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import { isPlainFundingOutput } from "../funding/plain-funding-output.js";
import { proofMaterialsAddressOfFundingAddress } from "../funding/proof-materials-address.js";
import { workflowActuationPermitIsReconciliationOnly } from "./actuation-permit.js";
import {
  exactResolvedOutputCbor,
  parseConfirmedActionOutput,
  resolveExactOutRefs,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  assertProofMaterialsSweepOutput,
  isProofMaterialsSweepAction,
  proofMaterialsSweepInputs,
} from "./proof-materials-sweep.action.js";

/**
 * The runtime bound of a proof-materials sweep: it spends exactly its page
 * of our materials (each reserved, or an output of a transaction this
 * reservation confirmed), runs no Plutus (its one script witness is the
 * materials' `sig(key)` lock, which the general bound pins), posts no
 * collateral, and pays one plain coin to the funding address. It adds a reserved funding coin only
 * when its materials alone would leave less than min-ADA after the fee,
 * and none under reconciliation-only authority. None for any other action.
 */
export const bindProofMaterialsSweep = async ({
  state,
  action,
  signed,
  bodyInputs,
  fundingOutRefs,
  coinsPerUtxoByte,
}: {
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  readonly signed: TxSigned;
  readonly bodyInputs: readonly string[];
  readonly fundingOutRefs: readonly string[];
  readonly coinsPerUtxoByte: bigint;
}): Promise<{ readonly admits: (outRef: string) => boolean } | null> => {
  if (!isProofMaterialsSweepAction(action)) return null;
  const page = proofMaterialsSweepInputs(action);
  const walletAddress = state.snapshot.walletAddress;
  const materialsAddress = proofMaterialsAddressOfFundingAddress(walletAddress);
  const transaction = signed.toTransaction();
  const body = transaction.body();
  if (
    body.mint() !== undefined ||
    body.certs() !== undefined ||
    body.withdrawals() !== undefined ||
    (body.reference_inputs()?.len() ?? 0) !== 0 ||
    body.collateral_inputs() !== undefined ||
    body.collateral_return() !== undefined ||
    body.total_collateral() !== undefined ||
    body.script_data_hash() !== undefined ||
    body.voting_procedures() !== undefined ||
    body.proposal_procedures() !== undefined ||
    transaction.witness_set().redeemers() !== undefined
  )
    throw new Error(
      "proof-materials sweep does more than spend its materials back",
    );
  assertProofMaterialsSweepOutput(signed, walletAddress);
  const inputs = await resolveExactOutRefs({
    port: state.port,
    outRefs: bodyInputs,
    label: "proof-materials sweep inputs",
  });
  if (page.some((outRef) => !inputs.has(outRef)))
    throw new Error("proof-materials sweep omits part of its page");
  const reconciling = workflowActuationPermitIsReconciliationOnly(
    state.actuationPermit,
  );
  let materials = 0n;
  let funded = false;
  const lineage = async (outRef: string, input: UTxO): Promise<boolean> => {
    const confirmed = await state.port.resolveConfirmedInput({ outRef });
    if (confirmed === null) return false;
    const parsed = parseConfirmedActionOutput(confirmed);
    return (
      parsed.outRef === outRef &&
      parsed.resolvedOutputCborHex === exactResolvedOutputCbor(input)
    );
  };
  for (const [outRef, input] of inputs) {
    if (page.includes(outRef)) {
      if (
        input.address !== materialsAddress ||
        (!fundingOutRefs.includes(outRef) && !(await lineage(outRef, input)))
      )
        throw new Error(
          `proof-materials sweep spends an output that is not a material of its workflow: ${outRef}`,
        );
      materials += input.assets.lovelace ?? 0n;
      continue;
    }
    if (
      reconciling ||
      input.address !== walletAddress ||
      !fundingOutRefs.includes(outRef) ||
      !isPlainFundingOutput(input)
    )
      throw new Error(
        `proof-materials sweep spends more than its materials: ${outRef}`,
      );
    funded = true;
  }
  if (
    funded &&
    materials - body.fee() >=
      CML.min_ada_required(body.outputs().get(0), coinsPerUtxoByte)
  )
    throw new Error(
      "proof-materials sweep adds funding its materials do not need",
    );
  return Object.freeze({ admits: (outRef: string) => page.includes(outRef) });
};
