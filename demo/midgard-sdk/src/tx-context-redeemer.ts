import {
  compareOutRefs,
  findOutRefIndex,
  outRefLabel,
} from "@al-ft/midgard-core/out-ref";
import type {
  PolicyId,
  RedeemerContext,
  RedeemerPurpose,
  RewardAddress,
  TxOutput,
  UTxO,
} from "@lucid-evolution/lucid";

const sameOutRef = (
  left: Pick<UTxO, "txHash" | "outputIndex">,
  right: Pick<UTxO, "txHash" | "outputIndex">,
): boolean =>
  left.txHash === right.txHash && left.outputIndex === right.outputIndex;

const requireRedeemerIndex = (
  ctx: RedeemerContext,
  purpose: RedeemerPurpose,
  label: string,
): bigint => {
  const index = ctx.redeemerIndex(purpose);
  if (index === undefined) {
    throw new Error(`${label} redeemer is missing a tx-info index`);
  }
  return index;
};

const requirePurpose = (
  ctx: RedeemerContext,
  predicate: (purpose: RedeemerPurpose) => boolean,
  label: string,
): RedeemerPurpose => {
  const matches = ctx.redeemers.filter(predicate);
  if (matches.length !== 1) {
    throw new Error(
      `${label} expected exactly one matching redeemer purpose, got ${matches.length.toString()}`,
    );
  }
  return matches[0]!;
};

const jsonBigInt = (_key: string, value: unknown): unknown =>
  typeof value === "bigint" ? value.toString() : value;

export const requireInputIndex = (
  ctx: RedeemerContext,
  input: UTxO,
  label: string,
): bigint => {
  const index = ctx.inputIndex(input);
  if (index === undefined) {
    throw new Error(
      `${label} input ${outRefLabel(input)} is missing from final tx inputs`,
    );
  }
  return index;
};

export const requireReferenceInputIndex = (
  ctx: RedeemerContext,
  input: UTxO,
  label: string,
): bigint => {
  const index = findOutRefIndex(
    [...ctx.referenceInputs].sort(compareOutRefs),
    input,
  );
  if (index === undefined) {
    throw new Error(
      `${label} reference input ${outRefLabel(input)} is missing from final tx reference inputs`,
    );
  }
  return BigInt(index);
};

export const requireUniqueOutputIndex = (
  outputs: readonly TxOutput[],
  predicate: (output: TxOutput) => boolean,
  label: string,
): bigint => {
  let foundIndex: bigint | undefined;
  for (let index = 0; index < outputs.length; index += 1) {
    if (!predicate(outputs[index])) {
      continue;
    }
    if (foundIndex !== undefined) {
      throw new Error(`${label} output selector matched multiple outputs`);
    }
    foundIndex = BigInt(index);
  }
  if (foundIndex === undefined) {
    throw new Error(
      `${label} output is missing from final tx outputs: ${JSON.stringify(
        outputs,
        jsonBigInt,
      )}`,
    );
  }
  return foundIndex;
};

export const requireOwnSpendPurpose = (
  ctx: RedeemerContext,
  input: UTxO,
  label: string,
): RedeemerPurpose & { readonly tag: "spend" } => {
  const purpose = ctx.ownPurpose;
  if (purpose.tag !== "spend" || !sameOutRef(purpose.input, input)) {
    throw new Error(
      `${label} expected own spend purpose for ${outRefLabel(input)}`,
    );
  }
  return purpose;
};

export const requireOwnMintPurpose = (
  ctx: RedeemerContext,
  policyId: PolicyId,
  label: string,
): RedeemerPurpose & { readonly tag: "mint" } => {
  const purpose = ctx.ownPurpose;
  if (purpose.tag !== "mint" || purpose.policyId !== policyId) {
    throw new Error(`${label} expected own mint purpose for ${policyId}`);
  }
  return purpose;
};

export const requireOwnRedeemerIndex = (
  ctx: RedeemerContext,
  label: string,
): bigint => requireRedeemerIndex(ctx, ctx.ownPurpose, label);

export const requireSpendRedeemerIndex = (
  ctx: RedeemerContext,
  input: UTxO,
  label: string,
): bigint =>
  requireRedeemerIndex(
    ctx,
    requirePurpose(
      ctx,
      (purpose) => purpose.tag === "spend" && sameOutRef(purpose.input, input),
      `${label} spend`,
    ),
    `${label} spend`,
  );

export const requireMintRedeemerIndex = (
  ctx: RedeemerContext,
  policyId: PolicyId,
  label: string,
): bigint =>
  requireRedeemerIndex(
    ctx,
    requirePurpose(
      ctx,
      (purpose) => purpose.tag === "mint" && purpose.policyId === policyId,
      `${label} mint`,
    ),
    `${label} mint`,
  );

export const requireWithdrawalRedeemerIndex = (
  ctx: RedeemerContext,
  rewardAddress: RewardAddress,
  label: string,
): bigint =>
  requireRedeemerIndex(
    ctx,
    requirePurpose(
      ctx,
      (purpose) =>
        purpose.tag === "withdraw" && purpose.rewardAddress === rewardAddress,
      `${label} withdrawal`,
    ),
    `${label} withdrawal`,
  );

export const requireSinglePublishRedeemerIndex = (
  ctx: RedeemerContext,
  label: string,
): bigint =>
  requireRedeemerIndex(
    ctx,
    requirePurpose(ctx, (purpose) => purpose.tag === "publish", label),
    label,
  );
