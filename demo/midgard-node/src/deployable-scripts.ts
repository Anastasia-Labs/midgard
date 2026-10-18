/** Core owns member identity/order; SDK owns validator recipes and restoration. */
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { ReferenceScriptAuthTokenTarget } from "@al-ft/midgard-sdk";

export {
  MANIFEST_ORDER,
  manifestDeployableScripts,
  NOT_YET_PUBLISHABLE_CONTRACTS,
  PUBLICATION_ORDER,
  publishedDeployableScripts,
} from "./deployable-scripts.publication-order.js";
export {
  type DeploymentScriptPurpose as DeployableScriptPurpose,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  type ReferenceScriptCommandName,
} from "@al-ft/midgard-core/deployment-manifest-identity";
export {
  type DeploymentScript as DeployableScript,
  type PublishedDeploymentScript as PublishedDeployableScript,
} from "@al-ft/midgard-sdk";

const roleByContract: ReadonlyMap<string, ReferenceScriptAuthTokenTarget> =
  new Map(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contract]) => [contract, role as ReferenceScriptAuthTokenTarget],
    ),
  );
export const referenceScriptRoleForContract = (
  contract: string,
): ReferenceScriptAuthTokenTarget | undefined => roleByContract.get(contract);
