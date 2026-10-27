import {
  createManifestBoundDistinctAssetAccumulationLimitWorkflow,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_DEFINITION,
  type ManifestBoundDistinctAssetAccumulationLimitWorkflow,
  runOrResumeManifestBoundDistinctAssetAccumulationLimitWorkflow,
} from "../distinct-asset-accumulation-limit/workflow.js";
import type { ManifestBoundDistinctAssetAccumulationLimitWorkflowConfig } from "../distinct-asset-accumulation-limit/workflow.transaction-port.js";
import {
  createManifestBoundExecutionNativeScriptInvalidWorkflow,
  EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
  type ManifestBoundExecutionNativeScriptInvalidWorkflow,
  runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
} from "../execution-native-script-invalid/workflow.js";
import type { ManifestBoundExecutionNativeScriptInvalidWorkflowConfig } from "../execution-native-script-invalid/workflow.transaction-port.js";
import {
  createManifestBoundExecutionSourceScriptDecodingWorkflow,
  EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION,
  type ManifestBoundExecutionSourceScriptDecodingWorkflow,
  runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow,
} from "../execution-source-script-decoding/workflow.js";
import type { ManifestBoundExecutionSourceScriptDecodingWorkflowConfig } from "../execution-source-script-decoding/workflow.transaction-port.js";
import {
  createManifestBoundMissingScriptSourceWorkflow,
  type ManifestBoundMissingScriptSourceWorkflow,
  MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION,
  runOrResumeManifestBoundMissingScriptSourceWorkflow,
} from "../missing-script-source/workflow.js";
import type { ManifestBoundMissingScriptSourceWorkflowConfig } from "../missing-script-source/workflow.transaction-port.js";
import {
  createManifestBoundReceivePurposeLanguageWorkflow,
  type ManifestBoundReceivePurposeLanguageWorkflow,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION,
  runOrResumeManifestBoundReceivePurposeLanguageWorkflow,
} from "../receive-purpose-language/workflow.js";
import type { ManifestBoundReceivePurposeLanguageWorkflowConfig } from "../receive-purpose-language/workflow.transaction-port.js";
import {
  createManifestBoundUnusedRedeemerWorkflow,
  type ManifestBoundUnusedRedeemerWorkflow,
  runOrResumeManifestBoundUnusedRedeemerWorkflow,
  UNUSED_REDEEMER_FAMILY_DEFINITION,
} from "../unused-redeemer/workflow.js";
import type { ManifestBoundUnusedRedeemerWorkflowConfig } from "../unused-redeemer/workflow.transaction-port.js";
import {
  createManifestBoundUnusedScriptWitnessWorkflow,
  type ManifestBoundUnusedScriptWitnessWorkflow,
  runOrResumeManifestBoundUnusedScriptWitnessWorkflow,
  UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION,
} from "../unused-script-witness/workflow.js";
import type { ManifestBoundUnusedScriptWitnessWorkflowConfig } from "../unused-script-witness/workflow.transaction-port.js";
import { bundleCursorFamilyApplicationRecord } from "./family-application-registry.bundle-cursor-family-application-record.js";

/**
 * The direct-walk cursor families: each binds its event and pre-state, then
 * walks the committed transaction directly against the replayed ledger. They
 * need the replay context and read their reference scripts from the shared
 * bundle.
 */
export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "executionSourceScriptDecoding",
    ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
    ManifestBoundExecutionSourceScriptDecodingWorkflow
  >(EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundExecutionSourceScriptDecodingWorkflow,
    execute: runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow,
  });

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "executionNativeScriptInvalid",
    ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
    ManifestBoundExecutionNativeScriptInvalidWorkflow
  >(EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundExecutionNativeScriptInvalidWorkflow,
    execute: runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow,
  });

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "distinctAssetAccumulationLimit",
    ManifestBoundDistinctAssetAccumulationLimitWorkflowConfig,
    ManifestBoundDistinctAssetAccumulationLimitWorkflow
  >(DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow:
      createManifestBoundDistinctAssetAccumulationLimitWorkflow,
    execute: runOrResumeManifestBoundDistinctAssetAccumulationLimitWorkflow,
  });

export const UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "unusedScriptWitness",
    ManifestBoundUnusedScriptWitnessWorkflowConfig,
    ManifestBoundUnusedScriptWitnessWorkflow
  >(UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundUnusedScriptWitnessWorkflow,
    execute: runOrResumeManifestBoundUnusedScriptWitnessWorkflow,
  });

export const MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "missingScriptSource",
    ManifestBoundMissingScriptSourceWorkflowConfig,
    ManifestBoundMissingScriptSourceWorkflow
  >(MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundMissingScriptSourceWorkflow,
    execute: runOrResumeManifestBoundMissingScriptSourceWorkflow,
  });

export const UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "unusedRedeemer",
    ManifestBoundUnusedRedeemerWorkflowConfig,
    ManifestBoundUnusedRedeemerWorkflow
  >(UNUSED_REDEEMER_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundUnusedRedeemerWorkflow,
    execute: runOrResumeManifestBoundUnusedRedeemerWorkflow,
  });

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD =
  bundleCursorFamilyApplicationRecord<
    "receivePurposeLanguage",
    ManifestBoundReceivePurposeLanguageWorkflowConfig,
    ManifestBoundReceivePurposeLanguageWorkflow
  >(RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION, {
    requires: ["replayContext"],
    constructWorkflow: createManifestBoundReceivePurposeLanguageWorkflow,
    execute: runOrResumeManifestBoundReceivePurposeLanguageWorkflow,
  });
