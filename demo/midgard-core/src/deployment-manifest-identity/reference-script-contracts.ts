import {
  DEPLOYMENT_MEMBERS,
  DEPLOYMENT_PUBLICATION_MEMBERS,
  type DeploymentContract,
} from "./members.js";

type PublishedReference<Contract extends DeploymentContract> = NonNullable<
  (typeof DEPLOYMENT_MEMBERS)[Contract]["reference"]
>;

type ReferenceRoleContracts = Readonly<{
  [Contract in DeploymentContract as PublishedReference<Contract>["role"]]: Contract;
}>;

type ReferenceRoleTokenNames = Readonly<{
  [Contract in DeploymentContract as PublishedReference<Contract>["role"]]: PublishedReference<Contract>["token"];
}>;

const publishedReferences = DEPLOYMENT_PUBLICATION_MEMBERS.map(
  (contract) => [contract, DEPLOYMENT_MEMBERS[contract].reference!] as const,
);

/** Roles enumerate in publication order. */
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze(
    Object.fromEntries(
      publishedReferences.map(([contract, { role }]) => [role, contract]),
    ),
  ) as ReferenceRoleContracts;

/** Each role's auth-token name, from the member rows, in publication order. */
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES = Object.freeze(
  Object.fromEntries(
    publishedReferences.map(([, { role, token }]) => [role, token]),
  ),
) as ReferenceRoleTokenNames;
