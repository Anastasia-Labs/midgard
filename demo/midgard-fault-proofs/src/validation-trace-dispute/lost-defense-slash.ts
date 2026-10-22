/** The slash of a defender who lost a thread of our case. A thread we win
 * closes into the case and mints the defender's lost-defense receipt at the
 * award (move-builders.ts); the receipt is then spent, and its token burned,
 * by the operator-directory slash `SlashOperatorForLostDefense`
 * (operator-directory.ak, step 8c; the award policy's `BurnReceipt` with
 * `SlashedForThisLoss`, award-v1.ak). The slash removes the defender's node
 * from the active or retired operator set, pays the slashing penalty as the
 * transaction fee and the prover reward to the receipt's bounty claimant,
 * and leaves the rest of the bond and the receipt's lovelace with the
 * directory anchor. It is due while the receipt of a thread we closed is
 * live and its defender is still in either set; a slash someone else landed
 * first, or a defender already slashed for another reason, leaves nothing to
 * do. It must land before the defender's bond unlocks: a retired operator
 * withdraws its bond once its validity range is past `bond_unlock_time`
 * (retired-operators.ak), and a defense holds that time at least
 * `started_at + D_case + P` (validation-case-v1 `defense_bond_unlock_time`). */
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseContractDeploymentInfo } from "../inspect-contracts.js";
import { buildRemovalContracts } from "../remove-fraudulent-block.assemble-removal-contracts.js";
import {
  fraudSlashEconomicsFromDeploymentManifest,
  resolveFraudSlashEconomics,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
} from "../remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { resolveOperatorSlashingPlan } from "../remove-fraudulent-block.build-active-slashing-inputs.js";
import { resolveReferenceScripts } from "../remove-fraudulent-block.load-state-queue-topology.js";
import { resolveRegisteredOperatorRemovalWitness } from "../remove-fraudulent-block.resolve-registered-operator-removal-witness.js";
import { buildSlashingInputs } from "../remove-fraudulent-block.resolve-state-queue-slashing-approach.js";
import { requireSingletonUtxo, type ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  grantLostDefenseSlashFundingAuthority,
  LOST_DEFENSE_SLASH_STAGE,
} from "../workflow/funding-reservation-permit.lost-defense-slash-authority.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "../workflow/release-economics-policy.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import { validationTraceRecovery } from "./actions.js";
import {
  type Directory,
  receiptOf,
  validationTraceLostDefenseSlashDeadlineMs,
  ValidationTraceLostDefenseSlashUnfundedError,
} from "./lost-defense-slash-due.js";
import type { ValidationTraceRemovalConfig } from "./removal.js";
import {
  captured,
  type CapturedValidationTraceStage,
  hubOracleUtxo,
  ours,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The deployment document the removal resolves its contracts from. */
const removalDocument = (removal: ValidationTraceRemovalConfig): unknown =>
  typeof removal.deploymentInfo === "object" &&
  removal.deploymentInfo !== null &&
  "manifestId" in removal.deploymentInfo
    ? verifyFinalizedDeploymentManifest(removal.deploymentInfo)
    : removal.deploymentInfo;

type SlashInput = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  deployment: ValidationTraceDisputeDeployment;
  removal: ValidationTraceRemovalConfig;
  /** The challenged block, which the funding authority names. */
  headerHash: string;
  receiptOutRef: string;
}>;

const buildSlash = async ({
  lucid,
  signer,
  deployment,
  removal,
  headerHash,
  receiptOutRef,
}: SlashInput): Promise<CapturedValidationTraceStage> => {
  const { chain, references, network } = deployment;
  const receipt = await ours(lucid, receiptOutRef, "lost-defense receipt");
  const datum = receiptOf(receipt);
  const receiptUnit = Object.keys(receipt.assets).find(
    (unit) =>
      unit.startsWith(chain.award.policyId) && receipt.assets[unit] === 1n,
  );
  if (receiptUnit === undefined || datum.outcome === "NeitherClaimValid")
    throw new Error("lost-defense receipt is not a receipt of our won thread");
  const document = removalDocument(removal);
  const contracts = await buildRemovalContracts({
    blueprint: deployment.blueprint,
    deploymentInfo: document,
    network,
    fraudCategory: "validationTraceDispute",
  });
  const referenceScripts = await resolveReferenceScripts({
    lucid,
    deploymentInfo: parseContractDeploymentInfo(document),
    requireReferenceScripts: true,
  });
  const economics = fraudSlashEconomicsFromDeploymentManifest(document);
  signer.selectWallet(lucid);
  const [schedulerUtxo, hubOracle] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: contracts.schedulerAddress,
      unit: toUnit(contracts.schedulerPolicyId, SDK.SCHEDULER_ASSET_NAME),
      label: "scheduler",
    }),
    hubOracleUtxo(lucid, deployment),
  ]);
  const plan = await resolveOperatorSlashingPlan({
    lucid,
    contracts,
    operator: datum.defender_vkh,
    schedulerUtxo,
    activeOperatorsRootUnit: toUnit(
      contracts.activeOperatorsPolicyId,
      SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    ),
    retiredOperatorsRootUnit: toUnit(
      contracts.retiredOperatorsPolicyId,
      SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
    ),
  });
  if (!("removalPlan" in plan))
    throw new WorkflowActionChangedError(
      `lost-defense receipt ${receiptOutRef} names a defender already slashed`,
    );
  const bond = plan.removalPlan.node.utxo;
  const slashEconomics = resolveFraudSlashEconomics(
    economics,
    bond.assets.lovelace ?? 0n,
  );
  const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
  const validTo = validFrom + Number(STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS);
  // The scheduler compares a new shift against the ledger's inclusive upper
  // bound, after Lucid converts validTo to an exclusive slot bound.
  const schedulerStartTime =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(validTo))) - 1n;
  const registeredOperatorsElementUtxo =
    plan.approach === "SlashActiveOperator" &&
    plan.schedulerPlan.kind === "rewind"
      ? await resolveRegisteredOperatorRemovalWitness({
          utxos: await lucid.utxosAt(contracts.registeredOperatorsAddress),
          address: contracts.registeredOperatorsAddress,
          policyId: contracts.registeredOperatorsPolicyId,
          inclusiveValidityUpperBound: schedulerStartTime,
        })
      : undefined;
  const rewardAddress = credentialToAddress(network, {
    type: "Key",
    hash: datum.bounty_claimant,
  });
  const slashingPlan = buildSlashingInputs({
    plan,
    operator: datum.defender_vkh,
    contracts,
    hubOracleUtxo: hubOracle,
    ...(registeredOperatorsElementUtxo === undefined
      ? {}
      : { registeredOperatorsElementUtxo }),
    fraudProverReward: {
      proverEnterpriseAddress: rewardAddress,
      lovelace: slashEconomics.fraudProverRewardLovelace,
    },
    // The anchor keeps the bond beyond fee and reward, and the receipt's
    // lovelace: the slash pays no change.
    bondExcessLovelace:
      slashEconomics.excessLovelace + (receipt.assets.lovelace ?? 0n),
    lostDefenseReceipt: receipt,
  });
  const directory: Directory =
    plan.approach === "SlashActiveOperator"
      ? "ActiveOperatorsDirectory"
      : "RetiredOperatorsDirectory";
  const active = directory === "ActiveOperatorsDirectory";
  const scriptReferences = active
    ? [
        referenceScripts.activeOperatorsSpend,
        referenceScripts.activeOperatorsMint,
        referenceScripts.schedulerSpend,
      ]
    : [
        referenceScripts.retiredOperatorsSpend,
        referenceScripts.retiredOperatorsMint,
      ];
  const read = new Map<string, UTxO>();
  for (const utxo of [
    references.award,
    ...slashingPlan.additionalRefInputs,
    ...scriptReferences,
  ])
    if (utxo !== undefined) read.set(outRefOf(utxo), utxo);
  const tx = SDK.collectRemoveSlashingInputs(
    lucid
      .newTx()
      .collectFrom(
        [receipt],
        Data.to("SpendReceipt", SDK.ValidationAwardSpendRedeemer),
      )
      .mintAssets({ [receiptUnit]: -1n }, (ctx) =>
        Data.to(
          {
            BurnReceipt: {
              receipt_input_index: SDK.requireInputIndex(
                ctx,
                receipt,
                "lost-defense receipt",
              ),
              hub_oracle_ref_input_index: SDK.requireReferenceInputIndex(
                ctx,
                hubOracle,
                "lost-defense slash hub oracle",
              ),
              permission: { SlashedForThisLoss: { directory } },
            },
          },
          SDK.ReceiptRedeemer,
        ),
      )
      .readFrom([...read.values()]),
    slashingPlan.buildSlashing(schedulerStartTime),
    referenceScripts,
  )
    .pay.ToAddress(rewardAddress, {
      lovelace: slashEconomics.fraudProverRewardLovelace,
    })
    .validFrom(validFrom)
    .validTo(validTo)
    .setMinFee(slashEconomics.exactFeeLovelace);
  // The slash is balanced exactly by its fee, reward and anchor: no wallet
  // input or change. The wallet lends collateral only.
  const unsigned = await tx.complete({
    coinSelection: false,
    localUPLCEval: true,
  });
  const signed = await unsigned.sign.withWallet().complete();
  if (signed.toTransaction().body().fee() !== slashEconomics.exactFeeLovelace)
    throw new Error("signed lost-defense slash fee differs from its economics");
  const inputOutRefs = [...workflowTransactionInputOutRefs(signed)];
  const inputs = await lucid.utxosByOutRef(
    inputOutRefs.map((outRef) => ({
      txHash: outRef.slice(0, 64),
      outputIndex: Number(outRef.slice(65)),
    })),
  );
  if (inputs.length !== inputOutRefs.length)
    throw new Error("signed lost-defense slash could not resolve its inputs");
  grantLostDefenseSlashFundingAuthority({
    signed,
    inputs,
    deploymentFingerprint: deployment.deploymentFingerprint,
    economicsPolicyDigest: computeFraudProofReleaseEconomicsPolicyDigest({
      profile: economics.profile,
      requiredBondLovelace: economics.requiredBondLovelace.toString(),
      slashingPenaltyLovelace: economics.slashingPenaltyLovelace.toString(),
      fraudProverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
      inactivitySlashingPenaltyLovelace:
        economics.inactivitySlashingPenaltyLovelace.toString(),
      proverCollateralFloorLovelace:
        economics.proverCollateralFloorLovelace.toString(),
    }),
    headerHash,
    receiptOutRef,
    receiptUnit,
    operatorOutRef: outRefOf(bond),
    operatorBondLovelace: (bond.assets.lovelace ?? 0n).toString(),
    exactFeeLovelace: slashEconomics.exactFeeLovelace.toString(),
    rewardLovelace: slashEconomics.fraudProverRewardLovelace.toString(),
    rewardAddress,
  });
  const transaction = captured(signed, [
    { role: "validation-trace-award", utxo: references.award },
    ...(active
      ? [
          {
            role: "active-operators-spend",
            utxo: referenceScripts.activeOperatorsSpend,
            expectedScript: contracts.activeOperatorsSpendingScript,
          },
          {
            role: "active-operators-mint",
            utxo: referenceScripts.activeOperatorsMint,
            expectedScript: contracts.activeOperatorsMintingScript,
          },
          {
            role: "scheduler-spend",
            utxo: referenceScripts.schedulerSpend,
            expectedScript: contracts.schedulerSpendingScript,
          },
        ]
      : [
          {
            role: "retired-operators-spend",
            utxo: referenceScripts.retiredOperatorsSpend,
            expectedScript: contracts.retiredOperatorsSpendingScript,
          },
          {
            role: "retired-operators-mint",
            utxo: referenceScripts.retiredOperatorsMint,
            expectedScript: contracts.retiredOperatorsMintingScript,
          },
        ]),
  ]);
  return {
    transaction,
    durableRecovery: validationTraceRecovery({
      stage: LOST_DEFENSE_SLASH_STAGE,
      transaction,
      slots: [],
    }),
  };
};

/**
 * The slash of the defender the receipt at `receiptOutRef` names, signed
 * and evaluated, with its funding authority. A wallet short of the
 * collateral is the operator's alarm, naming the slash's deadline.
 */
export const captureValidationTraceLostDefenseSlash = async (
  input: SlashInput,
): Promise<CapturedValidationTraceStage> => {
  try {
    return await buildSlash(input);
  } catch (cause) {
    if (!isWorkflowFundingShortfall(cause)) throw cause;
    const { lucid, deployment, receiptOutRef } = input;
    const receipt = await ours(lucid, receiptOutRef, "lost-defense receipt");
    throw new ValidationTraceLostDefenseSlashUnfundedError({
      receiptOutRef,
      defenderVkh: receiptOf(receipt).defender_vkh,
      deadlineMs: await validationTraceLostDefenseSlashDeadlineMs(
        lucid,
        deployment,
        receiptOutRef,
      ),
      cause,
    });
  }
};
