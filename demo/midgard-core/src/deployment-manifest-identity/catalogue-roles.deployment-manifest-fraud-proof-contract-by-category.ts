import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "./catalogue-roles.protocol-categories.js";
import { DEPLOYMENT_MANIFEST_MEMBERS, DEPLOYMENT_MEMBERS } from "./members.js";
import type { DeploymentContract, DeploymentMember } from "./members.types.js";

export * from "./catalogue-roles.protocol-categories.js";

/** The catalogue root is each admitted family's first step; retain key order. */
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY =
  Object.freeze(
    Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS).map(
        (family) => {
          const contract = DEPLOYMENT_MANIFEST_MEMBERS.find((contract) => {
            const member: DeploymentMember = DEPLOYMENT_MEMBERS[contract];
            return (
              member.family === family &&
              member.familyPaths.some(
                (path) => path.length === 1 && path[0] === "firstStep",
              )
            );
          });
          if (contract === undefined)
            throw new Error(
              `Missing deployment catalogue first step: ${family}`,
            );
          return [family, contract];
        },
      ),
    ),
  ) as Readonly<
    Record<DeploymentManifestFraudProofCatalogueCategory, DeploymentContract>
  >;
