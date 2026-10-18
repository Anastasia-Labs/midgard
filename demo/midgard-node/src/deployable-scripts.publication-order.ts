import * as SDK from "@al-ft/midgard-sdk";

import { publicDeploymentBlueprintRegistry } from "./deployment-blueprint-registry.js";
import { loadPhasMembershipWithdrawalScript } from "./phas-membership.js";

export {
  DEPLOYMENT_MANIFEST_MEMBERS as MANIFEST_ORDER,
  DEPLOYMENT_PUBLICATION_MEMBERS as PUBLICATION_ORDER,
} from "@al-ft/midgard-core/deployment-manifest-identity";

const recipeContext: SDK.DeploymentRecipeContext = {
  phasMembership: loadPhasMembershipWithdrawalScript,
};

/** Every deployment member, in the manifest's historical serialization order. */
export const manifestDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly SDK.DeploymentScript[] => {
  const scripts = SDK.manifestDeploymentScripts(contracts, recipeContext);
  publicDeploymentBlueprintRegistry(scripts);
  return scripts;
};

/** The same members projected into the historical reference publication order. */
export const publishedDeployableScripts = (
  contracts: SDK.MidgardValidators,
): readonly SDK.PublishedDeploymentScript[] => {
  manifestDeployableScripts(contracts);
  return SDK.publishedDeploymentScripts(contracts, recipeContext);
};
