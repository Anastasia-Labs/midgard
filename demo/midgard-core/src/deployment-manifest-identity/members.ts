import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "./catalogue-roles.deployment-manifest-contract-names.js";
import { DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "./catalogue-roles.protocol-categories.js";
import { DEPLOYMENT_MEMBERS } from "./members.data.js";
import {
  type DeploymentContract,
  type DeploymentFamily,
  type DeploymentMember,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "./members.types.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.js";

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

/** Admission compares against the independently maintained positional protocol ABI. */
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
  const positions = {
    manifest: new Set<number>(),
    publication: new Set<number>(),
    role: new Set<number>(),
  };
  const position = (
    kind: keyof typeof positions,
    value: number,
    contract: string,
  ) => {
    if (!Number.isSafeInteger(value) || value < 0 || positions[kind].has(value))
      throw new Error(`Invalid or duplicated ${kind} position for ${contract}`);
    positions[kind].add(value);
  };
  for (const [contract, member] of Object.entries(members)) {
    if (!families.has(member.family))
      throw new Error(
        `Unexpected deployment family for ${contract}: ${member.family}`,
      );
    if (!["spend", "mint", "withdraw"].includes(member.purpose))
      throw new Error(`Invalid deployment purpose for ${contract}`);
    position("manifest", member.manifestPosition, contract);
    if (member.referenceRole === null) {
      if (
        member.publicationPosition !== null ||
        member.referenceRolePosition !== null
      )
        throw new Error(
          `Unpublished deployment member has publication positions: ${contract}`,
        );
    } else {
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
      if (
        member.publicationPosition === null ||
        member.referenceRolePosition === null
      )
        throw new Error(
          `Published deployment member lacks positions: ${contract}`,
        );
      position("publication", member.publicationPosition, contract);
      position("role", member.referenceRolePosition, contract);
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
      const key = JSON.stringify([member.family, path]);
      if (path.length === 0 || paths.has(key))
        throw new Error(
          `Invalid or duplicated family member path for ${contract}`,
        );
      paths.add(key);
    }
  }
  for (const [kind, values] of Object.entries(positions)) {
    for (let i = 0; i < values.size; i++) {
      if (!values.has(i))
        throw new Error(`Non-contiguous deployment ${kind} positions`);
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

export const DEPLOYMENT_MANIFEST_MEMBERS = Object.freeze(
  (Object.keys(DEPLOYMENT_MEMBERS) as DeploymentContract[]).sort(
    (left, right) =>
      DEPLOYMENT_MEMBERS[left].manifestPosition -
      DEPLOYMENT_MEMBERS[right].manifestPosition,
  ),
);

export const DEPLOYMENT_PUBLICATION_MEMBERS = Object.freeze(
  DEPLOYMENT_MANIFEST_MEMBERS.filter(
    (contract) => DEPLOYMENT_MEMBERS[contract].publicationPosition !== null,
  ).sort(
    (left, right) =>
      DEPLOYMENT_MEMBERS[left].publicationPosition! -
      DEPLOYMENT_MEMBERS[right].publicationPosition!,
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
