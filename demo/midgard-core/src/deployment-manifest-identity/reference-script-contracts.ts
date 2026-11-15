import {
  DEPLOYMENT_MEMBERS,
  DEPLOYMENT_PUBLICATION_MEMBERS,
  type DeploymentContract,
} from "./members.js";

type ReferenceRoleContracts = Readonly<{
  [Contract in DeploymentContract as NonNullable<
    (typeof DEPLOYMENT_MEMBERS)[Contract]["referenceRole"]
  >]: Contract;
}>;

/** Roles enumerate in publication order. */
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze(
    Object.fromEntries(
      DEPLOYMENT_PUBLICATION_MEMBERS.map((contract) => [
        DEPLOYMENT_MEMBERS[contract].referenceRole,
        contract,
      ]),
    ),
  ) as ReferenceRoleContracts;
