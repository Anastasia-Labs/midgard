import type { DeploymentScriptPurpose } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";

import type {
  MidgardValidators,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../common.js";

export type DeploymentScriptValue = Readonly<{
  script: Script;
  scriptHash: string;
}>;
export type DeploymentRecipeContext = Readonly<{
  phasMembership: () => Script;
}>;
export type DeploymentRecipe<
  Purpose extends DeploymentScriptPurpose = DeploymentScriptPurpose,
> = Readonly<{
  purpose: Purpose;
  publishable?: (contracts: MidgardValidators) => boolean;
  resolve: (
    contracts: MidgardValidators,
    context: DeploymentRecipeContext,
  ) => DeploymentScriptValue;
}>;

export const spend = (
  select: (contracts: MidgardValidators) => SpendingValidator,
  publishable?: (contracts: MidgardValidators) => boolean,
): DeploymentRecipe<"spend"> => ({
  purpose: "spend",
  ...(publishable === undefined ? {} : { publishable }),
  resolve: (contracts) => {
    const validator = select(contracts);
    return {
      script: validator.spendingScript,
      scriptHash: validator.spendingScriptHash,
    };
  },
});
export const mint = (
  select: (contracts: MidgardValidators) => MintingValidator,
): DeploymentRecipe<"mint"> => ({
  purpose: "mint",
  resolve: (contracts) => {
    const validator = select(contracts);
    return { script: validator.mintingScript, scriptHash: validator.policyId };
  },
});
export const withdraw = (
  select: (contracts: MidgardValidators) => WithdrawalValidator,
): DeploymentRecipe<"withdraw"> => ({
  purpose: "withdraw",
  resolve: (contracts) => {
    const validator = select(contracts);
    return {
      script: validator.withdrawalScript,
      scriptHash: validator.withdrawalScriptHash,
    };
  },
});
export const phasMembership: DeploymentRecipe<"withdraw"> = {
  purpose: "withdraw",
  resolve: (_contracts, context) => {
    const script = context.phasMembership();
    return { script, scriptHash: validatorToScriptHash(script) };
  },
};
