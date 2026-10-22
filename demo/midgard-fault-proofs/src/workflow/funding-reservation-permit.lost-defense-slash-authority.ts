/**
 * The validation-trace dispute's lost-defense slash funding authority.
 *
 * A thread we won closes with its defender's lost-defense receipt, and the
 * slash that spends the receipt (`SlashOperatorForLostDefense`) consumes the
 * defender's bond: the transaction fee is the bond's slashing penalty, the
 * reward goes to the receipt's bounty claimant, and the directory anchor
 * keeps the rest of the bond and the receipt's lovelace. Like the removal's
 * fraud slash it spends no wallet funding and only posts collateral. The
 * authority binds the exact signed bytes evaluated locally, with every input
 * at its exact output, and is minted only by the slash builder, for category
 * 00000006.
 */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import { fraudSlashFeeLovelace } from "../remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import {
  actionKind,
  exactResolvedOutputCbor,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { readWorkflowRuntimeFundingPolicy } from "./runtime-funding-policy.js";

const CATEGORY = "validationTraceDispute" as const;

/** The dispute stage, and the funding action kind, of the slash. */
export const LOST_DEFENSE_SLASH_STAGE = "slash_lost_defense" as const;

type Policy = ReturnType<typeof readWorkflowRuntimeFundingPolicy>;

export type LostDefenseSlashFundingAuthority = Readonly<{
  category: typeof CATEGORY;
  deploymentFingerprint: string;
  economicsPolicyDigest: string;
  headerHash: string;
  receiptOutRef: string;
  receiptUnit: string;
  operatorOutRef: string;
  operatorBondLovelace: string;
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

/** In this process only, keyed by the builder's own TxSigned, as VerifySource. */
const authorities = new WeakMap<TxSigned, LostDefenseSlashFundingAuthority>();

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * Called by the slash builder once it has evaluated and signed the slash;
 * `inputs` are the body's inputs as the builder spent them.
 */
export const grantLostDefenseSlashFundingAuthority = ({
  signed,
  inputs,
  ...terms
}: Omit<
  LostDefenseSlashFundingAuthority,
  | "category"
  | "transactionHash"
  | "transactionBodySha256"
  | "signedTransactionCborHex"
  | "inputs"
> &
  Readonly<{ signed: TxSigned; inputs: readonly UTxO[] }>): void => {
  const transaction = signed.toTransaction();
  authorities.set(
    signed,
    Object.freeze({
      ...terms,
      category: CATEGORY,
      transactionHash: CML.hash_transaction(transaction.body()).to_hex(),
      transactionBodySha256: bodySha256(signed),
      signedTransactionCborHex: transaction.to_cbor_hex(),
      inputs: Object.freeze(
        inputs
          .map((utxo) =>
            Object.freeze({
              outRef: outRefLabel(utxo),
              resolvedOutputCborHex: exactResolvedOutputCbor(utxo),
            }),
          )
          .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
      ),
    }),
  );
};

export const readLostDefenseSlashFundingAuthority = (
  signed: TxSigned,
): LostDefenseSlashFundingAuthority | null => {
  const authority = authorities.get(signed);
  if (authority === undefined) return null;
  const transaction = signed.toTransaction();
  if (
    signed.toHash().toLowerCase() !== authority.transactionHash ||
    CML.hash_transaction(transaction.body()).to_hex() !==
      authority.transactionHash ||
    transaction.to_cbor_hex() !== authority.signedTransactionCborHex ||
    bodySha256(signed) !== authority.transactionBodySha256
  )
    throw new Error("signed lost-defense slash changed after local evaluation");
  return authority;
};

/** The enterprise address of `keyHash` on the network of `walletAddress`. */
const enterpriseAddress = (walletAddress: string, keyHash: string): string =>
  CML.EnterpriseAddress.new(
    CML.Address.from_bech32(walletAddress).network_id(),
    CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex(keyHash)),
  )
    .to_address()
    .to_bech32();

/**
 * The authority's binding to this workflow, its action, the policy's
 * economics and the funding key, which the slash's reward pays. Each
 * refusal names the failed binding.
 */
export const assertLostDefenseSlashBinding = ({
  authority,
  state,
  action,
  signed,
  policy,
  bodyInputs,
  fundingOutRefs,
}: {
  readonly authority: LostDefenseSlashFundingAuthority;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  readonly signed: TxSigned;
  readonly policy: Policy;
  readonly bodyInputs: readonly string[];
  readonly fundingOutRefs: readonly string[];
}): void => {
  const actuation = assertWorkflowActuationPermitIdentity({
    permit: state.actuationPermit,
    category: state.category,
    rollbackGeneration: state.snapshot.rollbackGeneration,
  });
  const economics = policy.economics.policy;
  const fee = fraudSlashFeeLovelace(
    economics,
    BigInt(authority.operatorBondLovelace),
  );
  const differing = (
    [
      ["category", state.category === CATEGORY],
      [
        "current action kind",
        state.currentActionKind === LOST_DEFENSE_SLASH_STAGE,
      ],
      ["action kind", actionKind(action) === LOST_DEFENSE_SLASH_STAGE],
      [
        "action digest",
        state.currentActionDigest ===
          computeDeploymentManifestJsonDigest(action),
      ],
      ["receipt", action.input.consumes === authority.receiptOutRef],
      ["header hash", authority.headerHash === actuation.headerHash],
      [
        "deployment fingerprint",
        authority.deploymentFingerprint === policy.deploymentFingerprint,
      ],
      [
        "economics policy digest",
        authority.economicsPolicyDigest === policy.economicsPolicyDigest,
      ],
      ["operator bond", fee !== null],
      [
        "reward",
        BigInt(authority.rewardLovelace) ===
          BigInt(economics.fraudProverRewardLovelace),
      ],
      ["exact fee", BigInt(authority.exactFeeLovelace) === fee],
      ["body fee", signed.toTransaction().body().fee() === fee],
      ["funding inputs", fundingOutRefs.length === 0],
      [
        "authority inputs",
        authority.inputs.length === bodyInputs.length &&
          authority.inputs.every(
            (input, index) => input.outRef === bodyInputs[index],
          ),
      ],
      ["operator input", bodyInputs.includes(authority.operatorOutRef)],
      ["receipt input", bodyInputs.includes(authority.receiptOutRef)],
      [
        "reward address",
        authority.rewardAddress ===
          enterpriseAddress(
            state.snapshot.walletAddress,
            policy.fundingPaymentKeyHash,
          ),
      ],
    ] as const
  )
    .filter(([, holds]) => !holds)
    .map(([name]) => name);
  if (differing.length > 0)
    throw new Error(
      `lost-defense slash funding authority differs from its exact receipt action or economics (${differing.join(", ")})`,
    );
};

/**
 * The receipt the slash spends: the deployment's award receipt, its token
 * minted by the award contract whose address holds it, naming the funding
 * key as the claimant of a won defense.
 */
export const assertLostDefenseSlashReceipt = ({
  authority,
  receipt,
  contracts,
  fundingPaymentKeyHash,
}: {
  readonly authority: LostDefenseSlashFundingAuthority;
  readonly receipt: UTxO;
  readonly contracts: ReadonlyMap<string, Readonly<{ scriptHash: string }>>;
  readonly fundingPaymentKeyHash: string;
}): void => {
  const datum =
    typeof receipt.datum === "string"
      ? Data.from(receipt.datum, SDK.LostDefenseReceipt)
      : undefined;
  if (
    contracts.get(receipt.address)?.scriptHash !==
      authority.receiptUnit.slice(0, 56) ||
    receipt.assets[authority.receiptUnit] !== 1n ||
    datum === undefined ||
    datum.outcome === "NeitherClaimValid" ||
    datum.bounty_claimant !== fundingPaymentKeyHash
  )
    throw new Error(
      "lost-defense slash receipt is not the deployment's receipt of our won defense",
    );
};

/** Whether `output` pays the slash's reward, which must then be exact. */
export const isLostDefenseSlashRewardOutput = (
  output: CML.TransactionOutput,
  authority: LostDefenseSlashFundingAuthority,
): boolean => {
  if (output.address().to_bech32() !== authority.rewardAddress) return false;
  if (
    output.amount().coin().toString() !== authority.rewardLovelace ||
    output.amount().has_multiassets() ||
    output.datum() !== undefined ||
    output.script_ref() !== undefined
  )
    throw new Error(
      "lost-defense slash reward differs from exact release economics",
    );
  return true;
};
