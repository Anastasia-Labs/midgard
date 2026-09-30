import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./deployment-manifest.required-transaction-order-contracts.js";
import "./deployment-manifest.deployment-manifest-reference-script-contract-by-role.js";
import "./deployment-manifest.require-out-ref-string.js";
import "./deployment-manifest.validate-fraud-proof-catalogue.js";
import "./deployment-manifest.validate-contracts.js";
import "./deployment-manifest.validate-da-identity.js";
import "./deployment-manifest.validate-deployment-manifest-common.js";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  type DeploymentManifestJsonValue,
  normalizeDeploymentManifestJsonValue,
} from "@al-ft/midgard-core/deployment-manifest-identity";
export { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./deployment-manifest.deployment-manifest-reference-script-contract-by-role.js";
export {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
} from "./deployment-manifest.require-out-ref-string.js";
export {
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  REQUIRED_TRANSACTION_ORDER_CONTRACTS,
} from "./deployment-manifest.required-transaction-order-contracts.js";
export { validationDisputeMaturityFitsProfile } from "./deployment-manifest.validate-da-identity.js";
export { parseDeploymentManifestValue } from "./deployment-manifest.validate-deployment-manifest-common.js";

export {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  normalizeDeploymentManifestJsonValue,
};

export type { DeploymentManifestJsonValue };
