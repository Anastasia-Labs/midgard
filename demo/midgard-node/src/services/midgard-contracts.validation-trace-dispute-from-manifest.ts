import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseDeploymentManifestEventHistoryRecipe } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";

import {
  isRecordedValidationTraceSemantic,
  VALIDATION_TRACE_RECORDED_YIELD_KEYS,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX,
  VALIDATION_TRACE_SEMANTIC_KEYS,
  validationTraceSemanticContractName,
  validationTraceYieldContractName,
} from "../deployable-scripts.js";
import {
  authenticatedValidatorFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";

export const eventHistoryContractsFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.EventHistoryContractPair => {
  const load = (name: "deposit" | "withdrawal"): SDK.EventHistoryContracts => {
    const metadata = parseDeploymentManifestEventHistoryRecipe(
      manifest.contracts[`${name}Mint`]?.eventHistoryRecipe,
    );
    const kind = name === "deposit" ? "Deposit" : "Withdrawal";
    if (
      metadata.kind !== kind ||
      metadata.hubPolicyId !== manifest.contracts.hubOracleMint.scriptHash ||
      metadata.initializationNonce.txHash !==
        manifest.hubOracleOneShot.txHash ||
      metadata.initializationNonce.outputIndex !==
        manifest.hubOracleOneShot.outputIndex
    )
      throw new Error(
        `Manifest ${name} history recipe differs from its deployment`,
      );
    const list = authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      `${name}Spend`,
      `${name}Mint`,
    );
    if (list.spendingScriptHash !== list.policyId)
      throw new Error(
        `Manifest ${name} history spending and minting roles must use the same script`,
      );
    return {
      recipe: {
        kind,
        hubPolicyId: metadata.hubPolicyId,
        initializationNonce: {
          transactionId: metadata.initializationNonce.txHash,
          outputIndex: BigInt(metadata.initializationNonce.outputIndex),
        },
        protectionDurationMs: BigInt(metadata.protectionDurationMs),
        inlineLimitBytes: BigInt(metadata.bounds.inlineLimitBytes),
        maxPayloadBytes: BigInt(metadata.bounds.maxPayloadBytes),
        maxPayloadNodes: BigInt(metadata.bounds.maxPayloadNodes),
      },
      list: {
        ...list,
        ...withdrawalValidatorFromManifest(manifest, sourcePath, `${name}Mint`),
      },
      retention: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        `${name}HistoryRetentionSpend`,
      ),
      retirement: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        `${name}HistoryRetirementWithdraw`,
      ),
    };
  };
  return { deposit: load("deposit"), withdrawal: load("withdrawal") };
};

/**
 * A validation-trace member the deployment manifest does not record: the
 * canonical-decode item stages, the prepare resolvers (`resolvers` is the same
 * list), the proof-item validator, the semantic resolvers outside the
 * published script-sources and phase-A sets, and therefore the full `steps`
 * list. The manifest carries no bytes for any of them, and no locally built
 * bundle may stand in for a deployed script, so reading one fails loudly.
 */
const unrecordedValidationTraceMember = (
  sourcePath: string,
  member: string,
): PropertyDescriptor => ({
  enumerable: true,
  get: () => {
    throw new Error(
      `Deployment manifest at "${sourcePath}" does not record validation-trace ${member}; a manifest-sourced contract bundle cannot provide it`,
    );
  },
});

/**
 * The validation-trace dispute chain, every recorded member restored by its
 * manifest name.
 *
 * The CEK and ScriptSources redeemer-item carriers apply their normalizers and
 * executors to the same deployment id and thread policy, so they are one set
 * of scripts published once under the shared redeemer-item names; the CEK
 * carrier uses the first `REDEEMER_ITEM_EXECUTOR_KEYS.length` executors.
 */
export const validationTraceDisputeFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  cekProgramMaterial: SDK.SpendingValidator,
): SDK.FaultProofContractChains["validationTraceDispute"] => {
  const spend = (contract: string) =>
    spendingValidatorFromManifest(network, manifest, sourcePath, contract);
  const byReference = (
    references: Readonly<Record<string, { readonly deployment: string }>>,
  ) =>
    Object.fromEntries(
      Object.entries(references).map(([key, { deployment }]) => [
        key,
        spend(deployment),
      ]),
    );

  const opener = spend("validationTraceDispute");
  const traversalNormalizer = spend(
    "validationTraceDisputeRedeemerItemTraversalNormalizer",
  );
  const outerNormalizer = spend(
    "validationTraceDisputeRedeemerItemOuterNormalizer",
  );
  const sourceAuthenticator = spend(
    "validationTraceDisputeRedeemerItemSourceAuthenticator",
  );
  const executors = SDK.REDEEMER_ITEM_EXECUTOR_REFERENCES.map(
    ({ deploymentEntry }) => spend(deploymentEntry),
  );
  const redeemerNormalizationSemantic = spend(
    VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
  );

  const semanticResolvers: SDK.SpendingValidator[] = [];
  VALIDATION_TRACE_SEMANTIC_KEYS.forEach((key, index) => {
    if (isRecordedValidationTraceSemantic(key)) {
      semanticResolvers[index] = spend(
        validationTraceSemanticContractName(key),
      );
    } else {
      Object.defineProperty(
        semanticResolvers,
        index,
        unrecordedValidationTraceMember(sourcePath, `semantic resolver ${key}`),
      );
    }
  });
  semanticResolvers[VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX] =
    redeemerNormalizationSemantic;

  const chain = {
    firstStep: opener,
    opener,
    source: spend("validationTraceDisputeSource"),
    game: spend("validationTraceDisputeGame"),
    boundary: spend("validationTraceDisputeBoundary"),
    timeout: spend("validationTraceDisputeTimeout"),
    award: spend("validationTraceDisputeAward"),
    cekProgramMaterial,
    cekMaterialTraversal: spend("validationTraceDisputeCekMaterialTraversal"),
    cekCoreStages: byReference(SDK.CEK_CORE_STAGE_REFERENCES),
    cekContextStages: byReference(SDK.CEK_CONTEXT_STAGE_REFERENCES),
    cekContextItemStages: {
      ...byReference(SDK.CEK_CONTEXT_ITEM_REFERENCES),
      traversalNormalizer,
      outerNormalizer,
      sourceAuthenticator,
      executors: executors.slice(0, SDK.REDEEMER_ITEM_EXECUTOR_KEYS.length),
    },
    scriptSourcesStageOneRedeemerStages: {
      envelope: redeemerNormalizationSemantic,
      traversalNormalizer,
      outerNormalizer,
      sourceAuthenticator,
      executors,
      foldMapExecutor: executors[0],
      finalizeFrameExecutor: executors[1],
      settlement: spend("validationTraceDisputeRedeemerItemSettlement"),
    },
    semanticResolvers,
    yields: Object.fromEntries(
      VALIDATION_TRACE_RECORDED_YIELD_KEYS.map((key) => [
        key,
        withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          validationTraceYieldContractName(key),
        ),
      ]),
    ),
  };
  Object.defineProperties(chain, {
    steps: unrecordedValidationTraceMember(
      sourcePath,
      "steps (the full list includes unrecorded members)",
    ),
    proofItem: unrecordedValidationTraceMember(sourcePath, "proof item"),
    canonicalDecodeItemStages: unrecordedValidationTraceMember(
      sourcePath,
      "canonical-decode item stages",
    ),
    prepareResolvers: unrecordedValidationTraceMember(
      sourcePath,
      "prepare resolvers",
    ),
    resolvers: unrecordedValidationTraceMember(sourcePath, "resolvers"),
  });
  return chain as unknown as SDK.FaultProofContractChains["validationTraceDispute"];
};
