/**
 * The slash a funded transaction carries, bound to its exact action and the
 * live economics: a removal's fraud slash (its fraud-proof record names the
 * prover), or a validation-trace dispute's lost-defense slash (its receipt
 * names the claimant). Either consumes one operator bond, spends no wallet
 * funding and pays its exact fee and reward; the runtime bound walks the
 * rest of the transaction the same way for both.
 */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { CML, TxSigned, UTxO } from "@lucid-evolution/lucid";

import {
  fraudSlashFeeLovelace,
  readFraudSlashFundingAuthority,
} from "../remove-fraudulent-block.js";
import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import {
  assertAuthenticatedFraudSlashReward,
  isExactFraudSlashRewardOutput,
} from "./funding-reservation-permit.assert-authenticated-fraud-slash-reward.js";
import { readValidationDisputeFundingAuthority } from "./funding-reservation-permit.assert-validation-dispute-funding.js";
import { actionKind } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  assertLostDefenseSlashBinding,
  assertLostDefenseSlashReceipt,
  isLostDefenseSlashRewardOutput,
  readLostDefenseSlashFundingAuthority,
} from "./funding-reservation-permit.lost-defense-slash-authority.js";
import { readValidationDisputeCloseAuthority } from "./funding-reservation-permit.validation-dispute-close-authority.js";
import { readValidationDisputeMoveAuthority } from "./funding-reservation-permit.validation-dispute-move-authority.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { readWorkflowRuntimeFundingPolicy } from "./runtime-funding-policy.js";
import { workflowTransactionReferenceInputOutRefs } from "./transaction-boundary.js";

type Policy = ReturnType<typeof readWorkflowRuntimeFundingPolicy>;

export type SlashFundingBinding = Readonly<{
  /** Whether the slash is a dispute's lost-defense slash. */
  lostDefense: boolean;
  inputs: readonly Readonly<{
    outRef: string;
    resolvedOutputCborHex: string;
  }>[];
  operatorOutRef: string;
  operatorBondLovelace: string;
  rewardLovelace: string;
  /** Checks the source naming the reward's payee, among the references. */
  assertReward: (
    references: ReadonlyMap<string, UTxO>,
    contracts: ReadonlyMap<string, unknown>,
    walletAddress: string,
  ) => void;
  /**
   * Whether `input` at `outRef` is the lost-defense receipt the slash
   * spends, which must then be the deployment's receipt of our won defense.
   */
  admitsReceipt: (
    outRef: string,
    input: UTxO,
    contracts: ReadonlyMap<string, Readonly<{ scriptHash: string }>>,
  ) => boolean;
  isRewardOutput: (output: CML.TransactionOutput) => boolean;
}>;

type Input = Readonly<{
  state: PermitState;
  action: FraudProofWorkflowAction;
  signed: TxSigned;
  policy: Policy;
  bodyInputs: readonly string[];
  fundingOutRefs: readonly string[];
}>;

const fraudSlashBinding = (
  slash: NonNullable<ReturnType<typeof readFraudSlashFundingAuthority>>,
  { state, action, signed, policy, bodyInputs, fundingOutRefs }: Input,
): SlashFundingBinding => {
  const actuation = assertWorkflowActuationPermitIdentity({
    permit: state.actuationPermit,
    category: state.category,
    rollbackGeneration: state.snapshot.rollbackGeneration,
  });
  const economics = policy.economics.policy;
  const bond = BigInt(slash.operatorBondLovelace);
  const fee = fraudSlashFeeLovelace(economics, bond);
  const reward = BigInt(economics.fraudProverRewardLovelace);
  // Removal binds nextRemovalOutRef/fraudProofOutRef; actionKind also
  // accepts stage. Each refusal names the failed binding.
  const differing = (
    [
      ["current action kind", state.currentActionKind === "remove"],
      ["action kind", actionKind(action) === "remove"],
      [
        "action digest",
        state.currentActionDigest ===
          computeDeploymentManifestJsonDigest(action),
      ],
      [
        "nextRemovalOutRef",
        action.input.nextRemovalOutRef === slash.removedStateQueueOutRef,
      ],
      [
        "fraudProofOutRef",
        action.input.fraudProofOutRef === slash.fraudProofOutRef,
      ],
      ["category", slash.category === state.category],
      ["header hash", slash.headerHash === actuation.headerHash],
      [
        "deployment fingerprint",
        slash.deploymentFingerprint === policy.deploymentFingerprint,
      ],
      [
        "economics policy digest",
        slash.economicsPolicyDigest === policy.economicsPolicyDigest,
      ],
      ["operator bond", fee !== null],
      ["reward", BigInt(slash.rewardLovelace) === reward],
      ["exact fee", BigInt(slash.exactFeeLovelace) === fee],
      ["body fee", signed.toTransaction().body().fee() === fee],
      ["funding inputs", fundingOutRefs.length === 0],
      [
        "authority inputs",
        slash.inputs.length === bodyInputs.length &&
          slash.inputs.every(
            (input, index) => input.outRef === bodyInputs[index],
          ),
      ],
      ["operator input", bodyInputs.includes(slash.operatorOutRef)],
      [
        "removed state-queue input",
        bodyInputs.includes(slash.removedStateQueueOutRef),
      ],
      [
        "fraud proof reference input",
        workflowTransactionReferenceInputOutRefs(signed).includes(
          slash.fraudProofOutRef,
        ),
      ],
    ] as const
  )
    .filter(([, holds]) => !holds)
    .map(([name]) => name);
  if (differing.length > 0)
    throw new Error(
      `fraud slash funding authority differs from its exact removal action or economics (${differing.join(", ")})`,
    );
  return Object.freeze({
    lostDefense: false,
    inputs: slash.inputs,
    operatorOutRef: slash.operatorOutRef,
    operatorBondLovelace: slash.operatorBondLovelace,
    rewardLovelace: slash.rewardLovelace,
    assertReward: (references, contracts, walletAddress) =>
      assertAuthenticatedFraudSlashReward(
        slash,
        references,
        contracts,
        walletAddress,
      ),
    admitsReceipt: () => false,
    isRewardOutput: (output) => isExactFraudSlashRewardOutput(output, slash),
  });
};

const lostDefenseSlashBinding = (
  slash: NonNullable<ReturnType<typeof readLostDefenseSlashFundingAuthority>>,
  input: Input,
): SlashFundingBinding => {
  const { signed, policy } = input;
  if (
    readFraudSlashFundingAuthority(signed) !== null ||
    readValidationDisputeFundingAuthority(signed) !== null ||
    readValidationDisputeMoveAuthority(signed) !== null ||
    readValidationDisputeCloseAuthority(signed) !== null
  )
    throw new Error(
      "funding transaction carries a lost-defense slash authority beside another funding authority",
    );
  assertLostDefenseSlashBinding({ authority: slash, ...input });
  return Object.freeze({
    lostDefense: true,
    inputs: slash.inputs,
    operatorOutRef: slash.operatorOutRef,
    operatorBondLovelace: slash.operatorBondLovelace,
    rewardLovelace: slash.rewardLovelace,
    // The reward's payee is the receipt's claimant, checked as it is spent.
    assertReward: () => undefined,
    admitsReceipt: (outRef, receipt, contracts) => {
      if (outRef !== slash.receiptOutRef) return false;
      assertLostDefenseSlashReceipt({
        authority: slash,
        receipt,
        contracts,
        fundingPaymentKeyHash: policy.fundingPaymentKeyHash,
      });
      return true;
    },
    isRewardOutput: (output) => isLostDefenseSlashRewardOutput(output, slash),
  });
};

/** The slash `input.signed` carries, bound to its action; null for none. */
export const bindSlashFunding = (input: Input): SlashFundingBinding | null => {
  const lostDefense = readLostDefenseSlashFundingAuthority(input.signed);
  if (lostDefense !== null) return lostDefenseSlashBinding(lostDefense, input);
  const fraud = readFraudSlashFundingAuthority(input.signed);
  return fraud === null ? null : fraudSlashBinding(fraud, input);
};
