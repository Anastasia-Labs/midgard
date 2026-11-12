import {
  type DeploymentContract,
  deploymentValidatorTitle,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type BlueprintMismatch,
  type BlueprintRef,
  parseBlueprintAbi,
} from "@al-ft/midgard-sdk/blueprint-abi";

import {
  type FamilyCategory,
  type FamilyDefinition,
  familyStepContractNames,
} from "../../src/workflow/family-definition.js";
import { readBlueprint, realBlueprintPath } from "./emulator/blueprints.js";
import { REGISTERED_FAMILY_DEFINITIONS } from "./registered-family-definitions.js";

const blueprintAbi = parseBlueprintAbi(readBlueprint(realBlueprintPath));

/** One step of a family, by the validator its step contract deploys. */
export type RegisteredStepValidator = Readonly<{
  category: FamilyCategory;
  step: number;
  title: string;
  /** The validator's datum and redeemer definitions; each throws when undeclared. */
  abi: Readonly<{ datum: () => BlueprintRef; redeemer: () => BlueprintRef }>;
}>;

const definitionOf =
  (title: string, slot: "datum" | "redeemer") => (): BlueprintRef => {
    const pointer = blueprintAbi.validator(title)[slot]?.$ref;
    if (pointer === undefined) {
      throw new Error(`${title} declares no ${slot} definition`);
    }
    return blueprintAbi.ref(pointer);
  };

/**
 * Step contract `i` of the family -> its validator title -> the validator's
 * blueprint ABI. Every lookup throws rather than skipping a step.
 */
export const stepValidators = (
  definition: FamilyDefinition<FamilyCategory>,
): readonly RegisteredStepValidator[] =>
  familyStepContractNames(definition).map((contract, step) => {
    const title: unknown = deploymentValidatorTitle(
      contract as DeploymentContract,
    );
    if (typeof title !== "string") {
      throw new Error(`${contract} deploys no compiled validator`);
    }
    return {
      category: definition.category,
      step,
      title,
      abi: {
        datum: definitionOf(title, "datum"),
        redeemer: definitionOf(title, "redeemer"),
      },
    };
  });

/** Every step of every registered family, in registry order. */
export const REGISTERED_STEP_VALIDATORS: readonly RegisteredStepValidator[] = [
  ...REGISTERED_FAMILY_DEFINITIONS.values(),
].flatMap(stepValidators);

/**
 * One allowed blueprint report, keyed by validator title elsewhere: the exact
 * report, and the twin it belongs to when a title has more than one.
 */
export type BlueprintAllowance = Readonly<{
  twin?: string;
  mismatches: readonly BlueprintMismatch[];
}>;

type Pair = Readonly<{ title: string; twin?: string }>;

const pairKey = ({ title, twin }: Pair): string => `${title} ${twin ?? ""}`;

/** The exact report allowed for this pair; none unless an entry names it. */
export const allowedMismatches = (
  allowances: ReadonlyMap<string, BlueprintAllowance>,
  pair: Pair,
): readonly BlueprintMismatch[] => {
  const allowance = allowances.get(pair.title);
  return allowance !== undefined && allowance.twin === pair.twin
    ? allowance.mismatches
    : [];
};

/** Allowance titles no checked pair reaches any more. */
export const unreachedAllowances = (
  allowances: ReadonlyMap<string, BlueprintAllowance>,
  pairs: readonly Pair[],
): readonly string[] => {
  const reached = new Set(pairs.map(pairKey));
  return [...allowances]
    .filter(([title, { twin }]) => !reached.has(pairKey({ title, twin })))
    .map(([title]) => title);
};
