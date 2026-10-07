/**
 * Builds and writes a deployment manifest for the currently configured Midgard
 * validator bundle.
 *
 * The manifest is keyed by explicit script names such as `depositMint` and
 * `depositSpend`, because many logical contracts compile to distinct scripts for
 * different purposes. Each entry records the compiled script bytes, its
 * corresponding script hash/policy id, and any matching reference-script UTxO
 * currently published in the dedicated reference-script wallet.
 */
import "node:crypto";
import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-core/retention-window";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../deployable-scripts.js";
import "../deployment-manifest.js";
import "../e2e/run-state.js";
import "../environment.js";
import "../files/atomic-write.js";
import "@al-ft/midgard-core/ogmios-slot";
import "../services/index.js";
import "../transactions/initialization.js";
import "../transactions/reference-scripts.js";
import "../transactions/script-reward-registration.js";
import "../tx-context.js";
import "./contract-deployment-info.build-reference-script-out-ref-map.js";
import "./contract-deployment-info.exact-protocol-parameter-snapshot.js";
import "./contract-deployment-info.build-deployment-manifest.js";
import "./contract-deployment-info.build-contract-deployment-info-from-contracts.js";
import "./contract-deployment-info.build-live-deployment-manifest-program.js";

import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "../deployment-manifest.js";
export {
  buildContractDeploymentInfoFromContracts,
  buildContractDeploymentInfoProgram,
  type LiveContractDeploymentInfoWriteOptions,
  writeContractDeploymentInfoFileProgram,
} from "./contract-deployment-info.build-contract-deployment-info-from-contracts.js";
export {
  buildDeploymentManifest,
  configuredContractDeploymentInfoPath,
  defaultContractDeploymentInfoOutputPath,
  parseDeploymentManifest,
  readDeploymentManifestFile,
  readFinalizedDeploymentIdentity,
  verifyConfiguredDeploymentManifestIfPresentProgram,
  verifyConfiguredDeploymentManifestProgram,
  verifyDeploymentManifestAgainstConfig,
} from "./contract-deployment-info.build-deployment-manifest.js";
export {
  type ReconcileInitializedDeploymentManifestOptions,
  reconcileInitializedDeploymentManifestProgram,
  type ReconcileInitializedDeploymentManifestSummary,
  writeLiveContractDeploymentInfoProgram,
} from "./contract-deployment-info.build-live-deployment-manifest-program.js";
export {
  buildReferenceScriptOutRefMap,
  collectScriptDescriptors,
  type ContractDeploymentInfo,
  type ContractDeploymentInfoEntry,
  type ContractDeploymentInfoRefScriptUTxO,
  type DeploymentManifestBuildContext,
  type DeploymentManifestIdentityContext,
  type DeploymentManifestVerificationReport,
  type FinalizedDeploymentIdentity,
} from "./contract-deployment-info.build-reference-script-out-ref-map.js";
export {
  buildDeploymentManifestIdentityContextProgram,
  cardanoProtocolParametersIdentityFromProvider,
  deploymentDaTransportProfile,
  queryLocalOgmiosProtocolParameters,
} from "./contract-deployment-info.exact-protocol-parameter-snapshot.js";

export { computeDeploymentManifestId, DEPLOYMENT_MANIFEST_SCHEMA_VERSION };

export type {
  DeploymentManifest,
  DeploymentManifestStepStatus,
} from "@al-ft/midgard-core/deployment-manifest-identity";
