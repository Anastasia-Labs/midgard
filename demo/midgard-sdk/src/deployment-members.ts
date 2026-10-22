import {
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MEMBERS,
  DEPLOYMENT_PUBLICATION_MEMBERS,
  type DeploymentContract,
  type DeploymentMember,
  deploymentMembersForFamily,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  applyDoubleCborEncoding,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type {
  MidgardValidators,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "./common.js";
import { DEPLOYMENT_RECIPES } from "./deployment-members/recipes.js";
import type {
  DeploymentRecipeContext,
  DeploymentScriptValue,
} from "./deployment-members/recipes.types.js";
import type { FaultProofBlueprint } from "./fraud-proof/contracts/blueprint.parse-fault-proof-blueprint.js";
import { blueprintApplicationRecords } from "./fraud-proof/contracts/blueprint-applications.js";
import { REDEEMER_ITEM_EXECUTOR_KEYS } from "./fraud-proof/contracts/families/shared-redeemer-item.js";
import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "./fraud-proof/contracts/families/validation-trace-dispute.validation-trace-dispute-fault-proof-titles.js";
import type { FaultProofContractChains } from "./fraud-proof/contracts/types.js";

export { DEPLOYMENT_RECIPES } from "./deployment-members/recipes.js";
export type {
  DeploymentRecipeContext,
  DeploymentScriptValue,
} from "./deployment-members/recipes.types.js";

export type DeploymentScript = DeploymentScriptValue &
  Readonly<{
    contract: DeploymentContract;
    role:
      | NonNullable<
          (typeof DEPLOYMENT_MEMBERS)[DeploymentContract]["referenceRole"]
        >
      | undefined;
    purpose: DeploymentMember["purpose"];
    commands: DeploymentMember["commands"];
  }>;

const resolveDeploymentMember = (
  contract: DeploymentContract,
  contracts: MidgardValidators,
  context: DeploymentRecipeContext,
): DeploymentScript => {
  const member = DEPLOYMENT_MEMBERS[contract];
  return {
    contract,
    role: member.referenceRole ?? undefined,
    purpose: member.purpose,
    commands: member.commands,
    ...DEPLOYMENT_RECIPES[contract].resolve(contracts, context),
  };
};

export const manifestDeploymentScripts = (
  contracts: MidgardValidators,
  context: DeploymentRecipeContext,
): readonly DeploymentScript[] =>
  DEPLOYMENT_MANIFEST_MEMBERS.map((contract) =>
    resolveDeploymentMember(contract, contracts, context),
  );

export type PublishedDeploymentScript = DeploymentScript &
  Readonly<{ role: NonNullable<DeploymentScript["role"]> }>;

export const publishedDeploymentScripts = (
  contracts: MidgardValidators,
  context: DeploymentRecipeContext,
): readonly PublishedDeploymentScript[] =>
  DEPLOYMENT_PUBLICATION_MEMBERS.filter(
    (contract) => DEPLOYMENT_RECIPES[contract].publishable?.(contracts) ?? true,
  ).map((contract) => {
    const resolved = resolveDeploymentMember(contract, contracts, context);
    if (resolved.role === undefined)
      throw new Error(`Published deployment member lacks a role: ${contract}`);
    return { ...resolved, role: resolved.role };
  });

/** Binds the complete protocol member universe to actual compiler/application evidence. */
export const verifyDeploymentScriptBindings = (
  blueprint: FaultProofBlueprint,
  scripts: readonly Readonly<{
    contract: string;
    purpose: string;
    script: DeploymentScriptValue["script"];
    scriptHash: string;
  }>[],
): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_MEMBERS);
  const titles = new Set(
    blueprint.validators.map((validator) => validator.title),
  );
  for (const member of Object.values(DEPLOYMENT_MEMBERS)) {
    if (member.validatorTitle !== null && !titles.has(member.validatorTitle))
      throw new Error(
        `Deployment validator absent from blueprint: ${member.validatorTitle}`,
      );
  }
  const applications = blueprintApplicationRecords(blueprint);
  for (const entry of scripts) {
    if (!expected.delete(entry.contract))
      throw new Error(
        `Unexpected or duplicated deployment binding: ${entry.contract}`,
      );
    const member = DEPLOYMENT_MEMBERS[entry.contract as DeploymentContract];
    if (
      entry.purpose !== member.purpose ||
      validatorToScriptHash(entry.script) !== entry.scriptHash
    )
      throw new Error(
        `Deployment purpose or script hash mismatch: ${entry.contract}`,
      );
    if (member.validatorTitle === null) {
      if (entry.script.type !== "Native")
        throw new Error(
          "Reference-script authorization must use its native policy recipe",
        );
      continue;
    }
    const bytes = applyDoubleCborEncoding(entry.script.script);
    if (
      !applications.some(
        (record) =>
          record.validatorTitle === member.validatorTitle &&
          record.appliedScriptCborHex === bytes,
      )
    )
      throw new Error(
        `Deployment script differs from its declared blueprint application: ${entry.contract}`,
      );
  }
  if (expected.size !== 0)
    throw new Error(`Missing deployment bindings: ${[...expected].join(", ")}`);
};

export type FamilyMemberValidator =
  | SpendingValidator
  | MintingValidator
  | WithdrawalValidator;

/**
 * The raw projection may contain sparse arrays and omits shared-family aliases.
 * Members of different purposes at one path form one validator (for example a
 * spending script and its minting policy).
 */
const restoreRecordedFamilyScriptPaths = (
  family: keyof FaultProofContractChains,
  resolve: (contract: DeploymentContract) => FamilyMemberValidator,
): Record<string, unknown> => {
  const restored: Record<string, unknown> = {};
  for (const contract of deploymentMembersForFamily(family)) {
    const member: DeploymentMember = DEPLOYMENT_MEMBERS[contract];
    const validator = resolve(contract);
    for (const path of member.familyPaths) {
      let target = restored;
      for (let index = 0; index < path.length - 1; index++) {
        const key = path[index]!;
        if (target[key] === undefined)
          target[key] = /^\d+$/u.test(path[index + 1]!) ? [] : {};
        target = target[key] as Record<string, unknown>;
      }
      const key = path[path.length - 1]!;
      target[key] =
        target[key] === undefined
          ? validator
          : { ...(target[key] as FamilyMemberValidator), ...validator };
    }
  }
  return restored;
};

/** Restore complete family scripts; domain history bounds remain with their owner. */
export const restoreFaultProofFamilyScripts = <
  Family extends Exclude<
    keyof FaultProofContractChains,
    "validationTraceDispute"
  >,
>(
  family: Family,
  resolve: (contract: DeploymentContract) => FamilyMemberValidator,
): Omit<FaultProofContractChains[Family], "history"> =>
  restoreRecordedFamilyScriptPaths(family, resolve) as Omit<
    FaultProofContractChains[Family],
    "history"
  >;

type ValidationTraceScripts =
  FaultProofContractChains["validationTraceDispute"];
type RecordedValidationTraceScripts = Omit<
  ValidationTraceScripts,
  "cekContextItemStages" | "steps"
> & {
  readonly cekContextItemStages: Pick<
    ValidationTraceScripts["cekContextItemStages"],
    "entry" | "settlement"
  >;
};

/**
 * Finalize the recorded validation-trace scripts before exposing the SDK chain.
 * Unpublished members explicitly refuse reads; shared CEK aliases use the
 * published redeemer-item programs.
 */
export const restoreValidationTraceDisputeScripts = ({
  resolve,
  missingMember,
}: {
  readonly resolve: (contract: DeploymentContract) => FamilyMemberValidator;
  readonly missingMember: (member: string) => never;
}): ValidationTraceScripts => {
  const projection = restoreRecordedFamilyScriptPaths(
    "validationTraceDispute",
    resolve,
  );
  // Each resolver list keeps its recorded slots; every other slot refuses.
  const refuseUnrecorded = (
    list: unknown,
    keys: readonly string[],
    label: string,
  ) => {
    if (!Array.isArray(list))
      throw new Error(
        `Validation-trace metadata lacks its ${label} projection`,
      );
    keys.forEach((key, index) => {
      if (!(index in list))
        Object.defineProperty(list, index, {
          enumerable: true,
          get: () => missingMember(`${label} ${key}`),
        });
    });
  };
  const titles = VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES;
  refuseUnrecorded(
    projection.semanticResolvers,
    Object.keys(titles.semantics),
    "semantic resolver",
  );
  refuseUnrecorded(
    projection.prepareResolvers,
    Object.keys(titles.prepares),
    "prepare resolver",
  );
  refuseUnrecorded(
    projection.resolvers,
    Object.keys(titles.prepares),
    "resolver",
  );
  // Every resolver slot now resolves or refuses. The step list, which
  // includes unrecorded members, is omitted from this private projection
  // type and replaced below.
  const recorded = projection as RecordedValidationTraceScripts;
  const redeemer = recorded.scriptSourcesStageOneRedeemerStages;
  return {
    ...recorded,
    cekContextItemStages: {
      ...recorded.cekContextItemStages,
      traversalNormalizer: redeemer.traversalNormalizer,
      outerNormalizer: redeemer.outerNormalizer,
      sourceAuthenticator: redeemer.sourceAuthenticator,
      executors: redeemer.executors.slice(
        0,
        REDEEMER_ITEM_EXECUTOR_KEYS.length,
      ),
    },
    get steps() {
      return missingMember("steps (the full list includes unrecorded members)");
    },
  };
};
