import {
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MEMBERS,
  type DeploymentContract,
} from "./members.js";

type ReferenceRoleContracts = Readonly<{
  [Contract in DeploymentContract as NonNullable<
    (typeof DEPLOYMENT_MEMBERS)[Contract]["referenceRole"]
  >]: Contract;
}>;

/** Preserve the role map's historical enumeration independently of publication. */
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze(
    Object.fromEntries(
      DEPLOYMENT_MANIFEST_MEMBERS.filter(
        (contract) => DEPLOYMENT_MEMBERS[contract].referenceRole !== null,
      )
        .sort(
          (left, right) =>
            DEPLOYMENT_MEMBERS[left].referenceRolePosition! -
            DEPLOYMENT_MEMBERS[right].referenceRolePosition!,
        )
        .map((contract) => [
          DEPLOYMENT_MEMBERS[contract].referenceRole,
          contract,
        ]),
    ),
  ) as ReferenceRoleContracts;
