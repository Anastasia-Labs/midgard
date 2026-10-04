import { createHash } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core";
import { parseDeploymentManifestEconomics } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type EmulatorStateQueueRemoveSlashingParams,
  type OutputReference,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  CML,
  type Script,
  type TxSigned,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";

import { type SupportedFaultProofCategoryName } from "./runtime.js";

export const STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS = 300_000n;

export const STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS = 120_000n;

export type FraudSlashEconomicsPolicy = Readonly<{
  profile: "public-preprod-launch-v1" | "bounded-acceptance-v1";
  requiredBondLovelace: bigint;
  slashingPenaltyLovelace: bigint;
  inactivitySlashingPenaltyLovelace: bigint;
  fraudProverRewardLovelace: bigint;
  proverCollateralFloorLovelace: bigint;
}>;

export type FraudSlashFundingAuthority = Readonly<{
  deploymentFingerprint: string;
  economicsPolicyDigest: string;
  category: string;
  headerHash: string;
  fraudProofOutRef: string;
  fraudProofUnit: string;
  fraudProofAddress: string;
  fraudProofResolvedOutputCborHex: string;
  removedStateQueueOutRef: string;
  operatorOutRef: string;
  operatorBondLovelace: string;
  tranche: "full" | "partially-inactivity-slashed";
  exactFeeLovelace: string;
  rewardLovelace: string;
  rewardAddress: string;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
  inputs: readonly Readonly<{
    outRef: string;
    resolvedOutputCborHex: string;
  }>[];
}>;

export const fraudSlashFundingProofSource = (proof: UTxO, unit: string) => ({
  fraudProofOutRef: outRefLabel(proof),
  fraudProofUnit: unit,
  fraudProofAddress: proof.address,
  fraudProofResolvedOutputCborHex: utxoToCore(proof)
    .output()
    .to_canonical_cbor_hex(),
});

// Only the evaluated canonical-manifest removal path below can mint this
// authority. A declared action kind or fee never creates a slashing allowance.
export const slashFundingAuthorities = new WeakMap<
  TxSigned,
  FraudSlashFundingAuthority
>();

export const readFraudSlashFundingAuthority = (
  signed: TxSigned,
): FraudSlashFundingAuthority | null => {
  const authority = slashFundingAuthorities.get(signed);
  if (authority === undefined) return null;
  const transaction = signed.toTransaction();
  if (
    signed.toHash().toLowerCase() !== authority.transactionHash ||
    CML.hash_transaction(transaction.body()).to_hex() !==
      authority.transactionHash ||
    transaction.to_cbor_hex() !== authority.signedTransactionCborHex ||
    createHash("sha256")
      .update(Buffer.from(transaction.body().to_cbor_hex(), "hex"))
      .digest("hex") !== authority.transactionBodySha256
  )
    throw new Error("signed fraud slash changed after local evaluation");
  return authority;
};

export const fraudSlashEconomicsFromDeploymentManifest = (
  deploymentInfo: unknown,
): FraudSlashEconomicsPolicy => {
  if (
    typeof deploymentInfo !== "object" ||
    deploymentInfo === null ||
    Array.isArray(deploymentInfo) ||
    (Object.getPrototypeOf(deploymentInfo) !== Object.prototype &&
      Object.getPrototypeOf(deploymentInfo) !== null)
  ) {
    throw new Error("Deployment manifest must be an object.");
  }
  const manifest = deploymentInfo as { readonly economics?: unknown };
  const economics = parseDeploymentManifestEconomics(manifest.economics);
  return {
    profile: economics.profile,
    requiredBondLovelace: BigInt(economics.requiredBondLovelace),
    slashingPenaltyLovelace: BigInt(economics.slashingPenaltyLovelace),
    inactivitySlashingPenaltyLovelace: BigInt(
      economics.inactivitySlashingPenaltyLovelace,
    ),
    fraudProverRewardLovelace: BigInt(economics.fraudProverRewardLovelace),
    proverCollateralFloorLovelace: BigInt(
      economics.proverCollateralFloorLovelace,
    ),
  };
};

export const resolveFraudSlashEconomics = (
  economics: FraudSlashEconomicsPolicy,
  operatorNodeLovelace: bigint,
): Readonly<{
  requiredBondLovelace: bigint;
  fraudProverRewardLovelace: bigint;
  exactFeeLovelace: bigint;
  tranche: "full" | "partially-inactivity-slashed";
}> => {
  const partialBond =
    economics.requiredBondLovelace -
    economics.inactivitySlashingPenaltyLovelace;
  if (
    economics.requiredBondLovelace !==
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace ||
    economics.inactivitySlashingPenaltyLovelace <= 0n ||
    economics.inactivitySlashingPenaltyLovelace >=
      economics.slashingPenaltyLovelace
  ) {
    throw new Error("Deployment economics violate F04 slash relations.");
  }
  if (operatorNodeLovelace === economics.requiredBondLovelace) {
    return {
      requiredBondLovelace: economics.requiredBondLovelace,
      fraudProverRewardLovelace: economics.fraudProverRewardLovelace,
      exactFeeLovelace: economics.slashingPenaltyLovelace,
      tranche: "full",
    };
  }
  if (operatorNodeLovelace === partialBond) {
    return {
      requiredBondLovelace: economics.requiredBondLovelace,
      fraudProverRewardLovelace: economics.fraudProverRewardLovelace,
      exactFeeLovelace:
        economics.slashingPenaltyLovelace -
        economics.inactivitySlashingPenaltyLovelace,
      tranche: "partially-inactivity-slashed",
    };
  }
  throw new Error(
    `Operator bond must be exactly ${economics.requiredBondLovelace.toString()} or ${partialBond.toString()} lovelace; found ${operatorNodeLovelace.toString()}.`,
  );
};

export const fraudRemovalUsesWalletCoinSelection = (
  approach:
    | "SlashActiveOperator"
    | "SlashRetiredOperator"
    | "OperatorAlreadySlashed",
): boolean => approach === "OperatorAlreadySlashed";

const utxoLovelace = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

const sumUtxoLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + utxoLovelace(utxo), 0n);

export const assertExactFraudSlashLovelaceConservation = ({
  stateQueueAnchor,
  removedStateQueueNode,
  slashing,
  economics,
}: {
  readonly stateQueueAnchor: StateQueueUTxO;
  readonly removedStateQueueNode: StateQueueUTxO;
  readonly slashing: Exclude<
    EmulatorStateQueueRemoveSlashingParams,
    { readonly kind: "operatorAlreadySlashed" }
  >;
  readonly economics: ReturnType<typeof resolveFraudSlashEconomics>;
}): void => {
  const stateQueueInputLovelace =
    utxoLovelace(stateQueueAnchor.utxo) +
    utxoLovelace(removedStateQueueNode.utxo);
  const stateQueueOutputLovelace = stateQueueInputLovelace;
  const rewardLovelace = slashing.fraudProverReward?.lovelace;
  if (rewardLovelace !== economics.fraudProverRewardLovelace) {
    throw new Error(
      `Fraud slash reward must conserve exactly ${economics.fraudProverRewardLovelace.toString()} lovelace; found ${rewardLovelace?.toString() ?? "none"}.`,
    );
  }

  const operatorInputLovelace =
    slashing.kind === "slashActiveOperator"
      ? sumUtxoLovelace([
          ...slashing.activeOperatorInputs,
          ...(slashing.schedulerSpend === undefined
            ? []
            : [slashing.schedulerSpend.input]),
        ])
      : sumUtxoLovelace(slashing.retiredOperatorInputs);
  const operatorOutputLovelace =
    slashing.kind === "slashActiveOperator"
      ? (slashing.continuedActiveOperatorAnchorOutput?.assets.lovelace ?? 0n) +
        (slashing.schedulerSpend?.continuedOutput.assets.lovelace ?? 0n)
      : (slashing.continuedRetiredOperatorAnchorOutput?.assets.lovelace ?? 0n);
  const totalInputs = stateQueueInputLovelace + operatorInputLovelace;
  const totalOutputsAndFee =
    stateQueueOutputLovelace +
    operatorOutputLovelace +
    rewardLovelace +
    economics.exactFeeLovelace;
  if (totalInputs !== totalOutputsAndFee) {
    throw new Error(
      `Fraud slash lovelace is not exactly conserved: inputs=${totalInputs.toString()}, outputs_and_fee=${totalOutputsAndFee.toString()}, residual=${(totalInputs - totalOutputsAndFee).toString()}.`,
    );
  }
};

/**
 * The published reference scripts a state-queue removal spends by reference.
 * Exported so the cursor families that follow a proof token with a removal
 * declare this same set as their auxiliary reference scripts.
 */
export const REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES = [
  "correctionLockSpend",
  "stateQueueSpend",
  "stateQueueMint",
  "stateQueueFraudRemovalWithdraw",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "schedulerSpend",
] as const;

export const STATE_QUEUE_REMOVE_REFERENCE_SCRIPT_NAMES = [
  "correctionLockSpend",
  "stateQueueSpend",
  "stateQueueMint",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "schedulerSpend",
] as const;

export type RemoveFraudulentBlockReferenceScriptName =
  (typeof REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES)[number];

export type ReferenceScriptName = RemoveFraudulentBlockReferenceScriptName;

export type DeploymentScriptName =
  | ReferenceScriptName
  | "registeredOperatorsSpend";

export type RemoveFraudulentBlockContracts = {
  readonly correctionLockAddress: string;
  readonly correctionLockSpendingScript: Script;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly stateQueueFraudRemovalWithdrawalScript: Script;
  readonly activeOperatorsPolicyId: string;
  readonly activeOperatorsAddress: string;
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsMintingScript: Script;
  readonly retiredOperatorsPolicyId: string;
  readonly retiredOperatorsAddress: string;
  readonly retiredOperatorsSpendingScript: Script;
  readonly retiredOperatorsMintingScript: Script;
  readonly schedulerPolicyId: string;
  readonly schedulerAddress: string;
  readonly schedulerSpendingScript: Script;
  readonly hubOraclePolicyId: string;
  readonly registeredOperatorsPolicyId: string;
  readonly registeredOperatorsAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
};

export type RemoveFraudulentBlockLayout = {
  readonly fraudProofRefInputIndex: bigint;
  readonly stateQueueRedeemerTxInfoIndex: bigint;
  readonly activeOperatorsRedeemerTxInfoIndex?: bigint;
  readonly retiredOperatorsRedeemerTxInfoIndex?: bigint;
  readonly activeOperatorsElementRefInputIndex?: bigint;
  readonly retiredOperatorsElementRefInputIndex?: bigint;
  readonly anchorElementOutputIndex?: bigint;
  readonly fraudulentNodeOutputIndex?: bigint;
};

export type OperatorSlashingLayout = {
  readonly activeOperatorsRedeemerTxInfoIndex?: bigint;
  readonly retiredOperatorsRedeemerTxInfoIndex?: bigint;
  readonly stateQueueRedeemerTxInfoIndex: bigint;
  readonly operatorDirectoryAnchorInputOutRef: OutputReference;
  readonly operatorDirectoryAnchorOutputIndex: bigint;
  readonly schedulerRefInputIndex?: bigint;
  readonly schedulerInputIndex?: bigint;
  readonly schedulerOutputIndex?: bigint;
  readonly schedulerRedeemerTxInfoIndex?: bigint;
  readonly activeOperatorsLastNodeRefInputIndex?: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly registeredOperatorsElementRefInputIndex?: bigint;
};

export type RemoveFraudulentBlockFraudCategory =
  SupportedFaultProofCategoryName;

/**
 * A canonical removable category name, or the label of an explicit
 * pre-registration category. The `string & {}` half keeps the canonical
 * literals in editor completion without narrowing away explicit labels.
 */
export type RemoveFraudulentBlockCategoryLabel =
  | RemoveFraudulentBlockFraudCategory
  | (string & {});
