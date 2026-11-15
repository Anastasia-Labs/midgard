import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "./catalogue-roles.deployment-manifest-contract-names.additions.js";
import { DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "./catalogue-roles.protocol-categories.js";
import { DEPLOYMENT_MEMBERS } from "./members.data.js";
import {
  type DeploymentContract,
  type DeploymentFamily,
  type DeploymentMember,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "./members.types.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.additions.js";

export { DEPLOYMENT_MEMBERS } from "./members.data.js";
export {
  type DeploymentContract,
  type DeploymentFamily,
  type DeploymentMember,
  type DeploymentScriptCommandName,
  type DeploymentScriptPurpose,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  type ReferenceScriptCommandName,
} from "./members.types.js";

/** Admission compares the rows against the independently maintained contract set. */
export const verifyDeploymentMembers = (
  members: Readonly<Record<string, DeploymentMember>>,
): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_CONTRACT_NAMES);
  for (const contract of Object.keys(members)) {
    if (!expected.delete(contract))
      throw new Error(`Unexpected deployment member: ${contract}`);
  }
  if (expected.size !== 0)
    throw new Error(`Missing deployment members: ${[...expected].join(", ")}`);
  const families = new Set<string>([
    "protocol",
    ...DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  ]);
  const roles = new Set<string>();
  const paths = new Set<string>();
  for (const [contract, member] of Object.entries(members)) {
    if (!families.has(member.family))
      throw new Error(
        `Unexpected deployment family for ${contract}: ${member.family}`,
      );
    if (!["spend", "mint", "withdraw"].includes(member.purpose))
      throw new Error(`Invalid deployment purpose for ${contract}`);
    if (member.referenceRole !== null) {
      if (
        member.referenceRole.length === 0 ||
        !Object.prototype.hasOwnProperty.call(
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
          member.referenceRole,
        ) ||
        roles.has(member.referenceRole)
      )
        throw new Error(
          `Invalid or duplicated deployment role for ${contract}`,
        );
      roles.add(member.referenceRole);
    }
    if (
      (member.validatorTitle === null) !==
      (contract === "referenceScriptAuthMint")
    )
      throw new Error(`Missing deployment blueprint title for ${contract}`);
    if (
      new Set(member.commands).size !== member.commands.length ||
      member.commands.some(
        (command) => !REFERENCE_SCRIPT_COMMAND_NAMES.includes(command),
      )
    )
      throw new Error(`Invalid deployment commands for ${contract}`);
    for (const path of member.familyPaths) {
      // One path may carry one member of each purpose.
      const key = JSON.stringify([member.family, path, member.purpose]);
      if (path.length === 0 || paths.has(key))
        throw new Error(
          `Invalid or duplicated family member path for ${contract}`,
        );
      paths.add(key);
    }
  }
  for (const family of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const first = Object.values(members).filter(
      (member) =>
        member.family === family &&
        member.familyPaths.some(
          (path) => path.length === 1 && path[0] === "firstStep",
        ),
    );
    if (
      first.length !== 1 ||
      first[0]!.purpose !== "spend" ||
      !first[0]!.familyPaths.some(
        (path) => path.length === 2 && path[0] === "steps" && path[1] === "0",
      )
    )
      throw new Error(
        `Deployment catalogue family lacks one first spending step: ${family}`,
      );
  }
};

verifyDeploymentMembers(DEPLOYMENT_MEMBERS);

/**
 * The deployment order is the contract-name list's order. Publication, and so
 * the reference-role map, keeps the published members in that same order.
 */
export const DEPLOYMENT_MANIFEST_MEMBERS: readonly DeploymentContract[] =
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES;

export const DEPLOYMENT_PUBLICATION_MEMBERS = Object.freeze(
  DEPLOYMENT_MANIFEST_MEMBERS.filter(
    (contract) => DEPLOYMENT_MEMBERS[contract].referenceRole !== null,
  ),
);

export const deploymentMembersForFamily = (
  family: DeploymentFamily,
): readonly DeploymentContract[] =>
  DEPLOYMENT_MANIFEST_MEMBERS.filter(
    (contract) => DEPLOYMENT_MEMBERS[contract].family === family,
  );

/** Complete step names, including names absent from a damaged manifest. */
export const deploymentFamilyStepNames = (
  family: Exclude<DeploymentFamily, "protocol">,
): readonly DeploymentContract[] =>
  deploymentMembersForFamily(family)
    .flatMap((contract) => {
      const member: DeploymentMember = DEPLOYMENT_MEMBERS[contract];
      const path = member.familyPaths.find(
        (path) => path.length === 2 && path[0] === "steps",
      );
      return path === undefined ? [] : [{ contract, index: Number(path[1]) }];
    })
    .sort((a, b) => a.index - b.index)
    .map((entry) => entry.contract);

/** The compiler title used by a member's SDK construction recipe. */
export const deploymentValidatorTitle = <Contract extends DeploymentContract>(
  contract: Contract,
): (typeof DEPLOYMENT_MEMBERS)[Contract]["validatorTitle"] =>
  DEPLOYMENT_MEMBERS[contract].validatorTitle;
