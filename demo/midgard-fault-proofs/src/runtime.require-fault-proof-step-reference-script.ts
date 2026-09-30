import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type DaHashPreimageFaultProofContracts,
  type DoubleSpendFaultProofContracts,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type InputNoIdxFaultProofContracts,
  type InvalidRangeFaultProofContracts,
  type InvalidSignatureFaultProofContracts,
  type NonExistentInputFaultProofContracts,
  type NoReferenceInputFaultProofContracts,
  type ReferenceInputNoIdxFaultProofContracts,
  type TransitionTraceFaultProofContracts,
  type ValidationTraceDisputeFaultProofContracts,
  type ZeroInputFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type OutRef,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";
import {
  type ParsedOutRef,
  requireDeploymentReferenceScriptOutRef,
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
} from "./runtime.resolve-prover-signer.js";

export const requireDeploymentReferenceScript = async ({
  lucid,
  deploymentInfo,
  name,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly name: string;
}): Promise<UTxO> => {
  const expectedScriptHash = requireDeploymentScriptHash(deploymentInfo, name);
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: requireDeploymentReferenceScriptOutRef(deploymentInfo, name),
    label: `${name} reference-script UTxO`,
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `${name} reference-script UTxO ${outRefLabel(utxo)} does not carry a reference script.`,
    );
  }
  requireMatchingScriptHash({
    label: `${name} reference script`,
    deployed: expectedScriptHash,
    derived: validatorToScriptHash(utxo.scriptRef),
  });
  return utxo;
};

/**
 * Validates a published step reference-script UTxO fail-closed and returns it
 * for use as a transaction reference input.
 *
 * A registered fraud-proof family sources its step spending validators from
 * reference scripts (owner ruling: always reference scripts, never inline
 * attach), so every step submitter accepts an optional `referenceScriptUtxo`.
 * The UTxO must carry a reference script, and that script must hash to the
 * step's own deployed spending-script hash — a divergence would read a witness
 * that does not authorize the spend.
 */
export const requireFaultProofStepReferenceScript = ({
  utxo,
  expectedScriptHash,
  label,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly label: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(
      `${label} reference UTxO ${outRefLabel(utxo)} carries no reference script.`,
    );
  }
  requireMatchingScriptHash({
    label: `${label} reference script`,
    deployed: expectedScriptHash,
    derived: validatorToScriptHash(utxo.scriptRef),
  });
  return utxo;
};

export type ResolvedDoubleSpendDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly doubleSpendCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: DoubleSpendFaultProofContracts;
};

export type ResolvedInvalidRangeDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly invalidRangeCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InvalidRangeFaultProofContracts;
};

export type ResolvedNonExistentInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly nonExistentInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: NonExistentInputFaultProofContracts;
};

export type ResolvedTransitionTraceDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly transitionTraceCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: TransitionTraceFaultProofContracts;
};

export type ResolvedValidationTraceDisputeDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly referenceScriptAuthPolicyId: string;
  readonly cekProgramMaterialScriptHash: string;
  readonly cekProgramMaterialAddress: string;
  readonly validationTraceDisputeCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
};

export type ResolvedDaHashPreimageDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly daHashPreimageCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: DaHashPreimageFaultProofContracts;
};

export type ResolvedInputNoIdxDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly nonExistentInputNoIndexCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InputNoIdxFaultProofContracts;
};

export type ResolvedZeroInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly zeroInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ZeroInputFaultProofContracts;
};

export type ResolvedNoReferenceInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly noReferenceInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: NoReferenceInputFaultProofContracts;
};

export type ResolvedReferenceInputNoIdxDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly referenceInputNoIdxCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ReferenceInputNoIdxFaultProofContracts;
};

export type ResolvedInvalidSignatureDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly invalidSignatureCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InvalidSignatureFaultProofContracts;
};

export type SupportedFaultProofCategoryName = FraudProofCatalogueCategoryName;

/**
 * The network-id family's forced (wrongful-rejection) door.
 *
 * `buildNetworkIdChain` deliberately keeps it out of `steps` — it is a side
 * entrance into step 02, not a third link — so it cannot be named by the
 * step-indexed table below without binding step 02's script to the forced
 * role. It carries its own canonical deployment name and reference-script
 * role instead.
 */
export const NETWORK_ID_FORCED_STEP_DEPLOYMENT_ENTRY =
  "fraudProofNetworkIdForcedStep" as const;

/**
 * The network-id family's resumable forced output scan.
 *
 * The forced door hands off to it, and it hands off to step 02 once every
 * output of the rejected forced transaction has been folded. Like the forced
 * door it is deliberately outside `steps`, so it too carries its own canonical
 * deployment name and reference-script role.
 */
export const NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY =
  "fraudProofNetworkIdForcedScan" as const;

export const fetchUtxoByOutRef = async ({
  lucid,
  outRef,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly outRef: ParsedOutRef;
  readonly label: string;
}): Promise<UTxO> => {
  const outRefs: OutRef[] = [
    { txHash: outRef.txHash, outputIndex: outRef.outputIndex },
  ];
  const utxos = await lucid.utxosByOutRef(outRefs);
  if (utxos.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} at ${outRefLabel(outRef)}, found ${utxos.length.toString()}.`,
    );
  }
  return utxos[0]!;
};
