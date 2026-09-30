/**
 * The one ordered catalogue of every script a Midgard deployment carries.
 *
 * Each entry states the manifest contract name, the purpose it is compiled for
 * (spend, mint or withdraw), how its validator is selected from the resolved
 * SDK bundle (or the blueprint), whether it is published as a reference script,
 * and which reference-script commands need it. Reference-script roles are never
 * restated here: they come from the manifest's role map, so an entry is
 * published under exactly the role the manifest declares for its contract.
 *
 * Two consumers read the catalogue, in two historically different orders that
 * are both observable and therefore both pinned:
 *
 * - the manifest order (`manifestDeployableScripts`) is the key order of
 *   `ContractDeploymentInfo.contracts`, so it is baked into the bytes of
 *   `contract-deployment-info.json` and into its digest. It is the order the
 *   sections are declared in below.
 * - the publication order (`publishedDeployableScripts`) drives reference-script
 *   publication batching. It is `PUBLICATION_ORDER`, an explicit permutation of
 *   the same sections.
 *
 * Fault-proof families and the validation-trace dispute's stage sets are
 * generated from the SDK's own arrays and reference tables instead of being
 * indexed by hand.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./deployment-manifest.js";
import "./phas-membership.js";
import "./deployable-scripts.fault-proof-step-contract-names.js";
import "./deployable-scripts.cek-core-stage-order.js";
import "./deployable-scripts.legacy-chain-steps.js";
import "./deployable-scripts.deployable-script-catalogue.js";
import "./deployable-scripts.publication-order.js";
export {
  isRecordedValidationTraceSemantic,
  referenceScriptRoleForContract,
  VALIDATION_TRACE_RECORDED_YIELD_KEYS,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX,
  validationTraceYieldContractName,
} from "./deployable-scripts.cek-core-stage-order.js";
export {
  type DeployableScript,
  type DeployableScriptPurpose,
  faultProofStepContractName,
  LEGACY_FAULT_PROOF_FAMILIES,
  type LegacyFaultProofFamily,
  type PublishedDeployableScript,
  recordedFaultProofStepContractNames,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  type ReferenceScriptCommandName,
  REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES,
  type RegisteredLinearFaultProofCategory,
  TRANSITION_TRACE_FINAL_CONTRACT_NAMES,
  VALIDATION_TRACE_SEMANTIC_KEYS,
  validationTraceSemanticContractName,
  type ValidationTraceSemanticKey,
  type ValidationTraceYieldKey,
} from "./deployable-scripts.fault-proof-step-contract-names.js";
export {
  type DeployableScriptSectionId,
  MANIFEST_ORDER,
  manifestDeployableScripts,
  PUBLICATION_ORDER,
  publishedDeployableScripts,
} from "./deployable-scripts.publication-order.js";
