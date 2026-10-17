import {
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MEMBERS,
  type DeploymentContract,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import type {
  MidgardValidators,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../common.js";
import type { FaultProofContractChains } from "../fraud-proof/contracts/types.js";
import { protocolRecipes } from "./protocol.js";
import { type DeploymentRecipe, spend, withdraw } from "./recipes.types.js";
import { validationTraceControlRecipes } from "./validation-trace-control.js";

type Members = typeof DEPLOYMENT_MEMBERS;
type RecipeFor<Contract extends DeploymentContract> = DeploymentRecipe<
  Members[Contract]["purpose"]
>;
type ProtocolContract = {
  [Contract in DeploymentContract]: Members[Contract]["family"] extends "protocol"
    ? Contract
    : never;
}[DeploymentContract];

/**
 * Members whose recipe is not the selection of their first family path: every
 * protocol member, and the validation-trace control scripts, which publish
 * only when that family is installed.
 */
const EXPLICIT_RECIPES = Object.freeze({
  ...protocolRecipes,
  ...validationTraceControlRecipes,
}) satisfies {
  readonly [Contract in ProtocolContract]: RecipeFor<Contract>;
} & Partial<{ readonly [Contract in DeploymentContract]: RecipeFor<Contract> }>;
type ExplicitContract = keyof typeof EXPLICIT_RECIPES;

type FamilyContract = {
  [Contract in DeploymentContract]: Contract extends ExplicitContract
    ? never
    : Members[Contract]["family"] extends keyof FaultProofContractChains
      ? Contract
      : never;
}[DeploymentContract];

/** `unknown` (never a validator) when the path does not exist. */
type AtPath<Value, Path> = Path extends readonly [
  infer Key,
  ...infer Rest extends readonly string[],
]
  ? Key extends keyof Value
    ? AtPath<Value[Key], Rest>
    : Value extends readonly (infer Item)[]
      ? Key extends `${number}`
        ? AtPath<Item, Rest>
        : unknown
      : unknown
  : Value;

type ValidatorFor<Purpose> = Purpose extends "spend"
  ? SpendingValidator
  : Purpose extends "withdraw"
    ? WithdrawalValidator
    : MintingValidator;

/**
 * A family member's first path, read from its SDK family chain, must name a
 * validator of the member's purpose; any member that does not is excluded
 * here, so passing it to `familyRecipe` fails to compile.
 */
type SelectableFamilyContract = {
  [Contract in FamilyContract]: AtPath<
    FaultProofContractChains[Members[Contract]["family"] &
      keyof FaultProofContractChains],
    Members[Contract]["familyPaths"][0]
  > extends ValidatorFor<Members[Contract]["purpose"]>
    ? Contract
    : never;
}[FamilyContract];

const familyRecipe = (contract: SelectableFamilyContract): DeploymentRecipe => {
  const member = DEPLOYMENT_MEMBERS[contract];
  const family = member.family as keyof FaultProofContractChains;
  const [path] = member.familyPaths;
  const select = (contracts: MidgardValidators) => {
    let value: unknown = contracts.fraudProofContracts[family];
    for (const key of path ?? [])
      value =
        value === null || typeof value !== "object"
          ? undefined
          : (value as Record<string, unknown>)[key];
    if (value === null || typeof value !== "object")
      throw new Error(
        `Deployment member ${contract} is absent at fraudProofContracts.${family}.${(path ?? []).join(".")}`,
      );
    return value as never;
  };
  switch (member.purpose) {
    case "spend":
      return spend(select);
    case "withdraw":
      return withdraw(select);
  }
};

const isExplicit = (
  contract: DeploymentContract,
): contract is ExplicitContract =>
  Object.prototype.hasOwnProperty.call(EXPLICIT_RECIPES, contract);

/**
 * SDK owns the selection functions; identity, order and the selected family
 * path live in the core member registry. Every core member receives a recipe.
 */
export const DEPLOYMENT_RECIPES = Object.freeze(
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_MEMBERS.map((contract) => [
      contract,
      isExplicit(contract)
        ? EXPLICIT_RECIPES[contract]
        : familyRecipe(contract),
    ]),
  ),
) as { readonly [Contract in DeploymentContract]: RecipeFor<Contract> };
