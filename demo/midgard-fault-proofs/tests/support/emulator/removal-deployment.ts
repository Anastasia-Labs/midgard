import {
  type FraudProofCatalogueDeploymentInfo,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";

import { type CrossBlockDuplicateEventContractsV1 } from "../../../src/cross-block-duplicate-event/index.js";
import {
  VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationValueAndMintSemanticReferenceScriptDeploymentEntryV1,
} from "../../../src/index.js";
import { type NativeScriptDecodingContractsV1 } from "../../../src/native-script-decoding/index.js";
import { deploymentManifest } from "./header-fixtures.js";
import {
  publishValidationDisputeReferenceScript,
  type RemovalReferenceScriptName,
  type RemovalReferenceScriptPublications,
} from "./reference-scripts.js";

export type RemovalDeploymentReference = {
  readonly scriptHash: string;
  readonly utxo: UTxO;
};

/**
 * Manifest entry pinning the `native-script-decoding` step-01 script hash for
 * removal (#635). The name is caller-chosen because the family predates its
 * catalogue registration: `submitRemoveFraudulentBlock` checks the explicit
 * category record's step-01 hash against whatever entry the record names, and
 * this is the name the emulator manifests use.
 */
export const NATIVE_SCRIPT_DECODING_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofNativeScriptDecoding";
export const CROSS_BLOCK_DUPLICATE_EVENT_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofCrossBlockDuplicateEvent";

export const buildRemovalDeploymentInfo = (
  contracts: MidgardValidators & {
    readonly nativeScriptDecoding?: NativeScriptDecodingContractsV1;
    readonly crossBlockDuplicateEvent?: CrossBlockDuplicateEventContractsV1;
  },
  catalogue: FraudProofCatalogueDeploymentInfo,
  {
    validationDisputePublication,
    validationItemSemanticReference,
    validationItemObserveReference,
    validationCanonicalDecodePrepareReference,
    removalReferenceScripts,
    validationValueAndMintSemanticReferences,
  }: {
    readonly validationDisputePublication?: Awaited<
      ReturnType<typeof publishValidationDisputeReferenceScript>
    >;
    readonly validationItemSemanticReference?: RemovalDeploymentReference;
    readonly validationItemObserveReference?: RemovalDeploymentReference;
    readonly validationCanonicalDecodePrepareReference?: RemovalDeploymentReference;
    readonly removalReferenceScripts?: RemovalReferenceScriptPublications;
    /**
     * #634. Published ValueAndMint semantic-resolver reference scripts, keyed
     * by the ValueAndMint-local semantic index (0..10). Splices in the same
     * shape as the item-semantic / canonical-decode-prepare entries, so a
     * journey that publishes one resolver adds exactly one entry.
     */
    readonly validationValueAndMintSemanticReferences?: readonly (RemovalDeploymentReference & {
      readonly semanticResolverIndex: number;
    })[];
  } = {},
) => {
  const valueAndMintSemanticEntries = Object.fromEntries(
    (validationValueAndMintSemanticReferences ?? []).map(
      ({ semanticResolverIndex, scriptHash, utxo }) => {
        const entryName =
          validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
            semanticResolverIndex,
          );
        if (entryName === undefined) {
          throw new Error(
            `ValueAndMint semantic resolver ${semanticResolverIndex.toString()} has no reference-script deployment entry`,
          );
        }
        return [
          entryName,
          {
            scriptHash,
            refScriptUTxO: {
              txHash: utxo.txHash,
              outputIndex: utxo.outputIndex,
            },
          },
        ] as const;
      },
    ),
  );
  const deploymentEntry = (
    scriptHash: string,
    script: Script,
    referenceName?: RemovalReferenceScriptName,
  ) => {
    const published =
      referenceName === undefined
        ? undefined
        : removalReferenceScripts?.[referenceName];
    return {
      scriptHash,
      refScriptUTxO:
        published === undefined
          ? null
          : {
              txHash: published.txHash,
              outputIndex: published.outputIndex,
            },
      contract: {
        type: script.type,
        cborHex: script.script,
      },
    };
  };
  return deploymentManifest(
    {
      ...valueAndMintSemanticEntries,
      ...(validationItemSemanticReference === undefined
        ? {}
        : {
            [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
              scriptHash: validationItemSemanticReference.scriptHash,
              refScriptUTxO: {
                txHash: validationItemSemanticReference.utxo.txHash,
                outputIndex: validationItemSemanticReference.utxo.outputIndex,
              },
            },
          }),
      ...(validationItemObserveReference === undefined
        ? {}
        : {
            [VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
              scriptHash: validationItemObserveReference.scriptHash,
              refScriptUTxO: {
                txHash: validationItemObserveReference.utxo.txHash,
                outputIndex: validationItemObserveReference.utxo.outputIndex,
              },
            },
          }),
      ...(validationCanonicalDecodePrepareReference === undefined
        ? {}
        : {
            [VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
              {
                scriptHash:
                  validationCanonicalDecodePrepareReference.scriptHash,
                refScriptUTxO: {
                  txHash: validationCanonicalDecodePrepareReference.utxo.txHash,
                  outputIndex:
                    validationCanonicalDecodePrepareReference.utxo.outputIndex,
                },
              },
          }),
      hubOracleMint: { scriptHash: contracts.hubOracle.policyId },
      fraudProofCatalogueMint: {
        scriptHash: contracts.fraudProofCatalogue.policyId,
        fraudProofCatalogue: catalogue,
      },
      fraudProofCatalogueSpend: {
        scriptHash: contracts.fraudProofCatalogue.spendingScriptHash,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofSpend: {
        scriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofDoubleSpend: {
        scriptHash: contracts.fraudProofs.doubleSpend.spendingScriptHash,
      },
      fraudProofNonExistentInput: {
        scriptHash: contracts.fraudProofs.nonExistentInput.spendingScriptHash,
      },
      fraudProofInvalidRange: {
        scriptHash: contracts.fraudProofs.invalidRange.spendingScriptHash,
      },
      fraudProofTransitionTrace: {
        scriptHash: contracts.fraudProofs.transitionTrace.spendingScriptHash,
      },
      fraudProofZeroInput: {
        scriptHash: contracts.fraudProofs.zeroInput.spendingScriptHash,
      },
      fraudProofDaHashPreimage: {
        scriptHash: contracts.fraudProofs.daHashPreimage.spendingScriptHash,
      },
      fraudProofNoReferenceInput: {
        scriptHash: contracts.fraudProofs.noReferenceInput.spendingScriptHash,
      },
      fraudProofReferenceInputNoIdx: {
        scriptHash:
          contracts.fraudProofs.referenceInputNoIdx.spendingScriptHash,
      },
      fraudProofInvalidSignature: {
        scriptHash: contracts.fraudProofs.invalidSignature.spendingScriptHash,
      },
      ...(contracts.nativeScriptDecoding === undefined
        ? {}
        : {
            [NATIVE_SCRIPT_DECODING_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.nativeScriptDecoding.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.crossBlockDuplicateEvent === undefined
        ? {}
        : {
            [CROSS_BLOCK_DUPLICATE_EVENT_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.crossBlockDuplicateEvent.steps[0].spendingScriptHash,
            },
          }),
      fraudProofNonExistentInputNoIndex: {
        scriptHash:
          contracts.fraudProofs.nonExistentInputNoIndex.spendingScriptHash,
        contract: {
          type: contracts.fraudProofs.nonExistentInputNoIndex.spendingScript
            .type,
          cborHex:
            contracts.fraudProofs.nonExistentInputNoIndex.spendingScript.script,
        },
      },
      validationTraceDispute: {
        scriptHash:
          contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
        refScriptUTxO:
          validationDisputePublication === undefined
            ? null
            : {
                txHash: validationDisputePublication.utxo.txHash,
                outputIndex: validationDisputePublication.utxo.outputIndex,
              },
        contract: {
          type: contracts.fraudProofs.validationTraceDispute.spendingScript
            .type,
          cborHex:
            contracts.fraudProofs.validationTraceDispute.spendingScript.script,
        },
      },
      cekProgramMaterialSpend: {
        scriptHash: contracts.cekProgramMaterial.spendingScriptHash,
        contract: {
          type: contracts.cekProgramMaterial.spendingScript.type,
          cborHex: contracts.cekProgramMaterial.spendingScript.script,
        },
      },
      stateQueueMint: deploymentEntry(
        contracts.stateQueue.policyId,
        contracts.stateQueue.mintingScript,
        "stateQueueMint",
      ),
      stateQueueSpend: deploymentEntry(
        contracts.stateQueue.spendingScriptHash,
        contracts.stateQueue.spendingScript,
        "stateQueueSpend",
      ),
      retiredOperatorsMint: deploymentEntry(
        contracts.retiredOperators.policyId,
        contracts.retiredOperators.mintingScript,
        "retiredOperatorsMint",
      ),
      retiredOperatorsSpend: deploymentEntry(
        contracts.retiredOperators.spendingScriptHash,
        contracts.retiredOperators.spendingScript,
        "retiredOperatorsSpend",
      ),
      registeredOperatorsMint: {
        scriptHash: contracts.registeredOperators.policyId,
      },
      registeredOperatorsSpend: deploymentEntry(
        contracts.registeredOperators.spendingScriptHash,
        contracts.registeredOperators.spendingScript,
      ),
      activeOperatorsMint: deploymentEntry(
        contracts.activeOperators.policyId,
        contracts.activeOperators.mintingScript,
        "activeOperatorsMint",
      ),
      activeOperatorsSpend: deploymentEntry(
        contracts.activeOperators.spendingScriptHash,
        contracts.activeOperators.spendingScript,
        "activeOperatorsSpend",
      ),
      schedulerMint: { scriptHash: contracts.scheduler.policyId },
      schedulerSpend: deploymentEntry(
        contracts.scheduler.spendingScriptHash,
        contracts.scheduler.spendingScript,
        "schedulerSpend",
      ),
      settlementMint: { scriptHash: contracts.settlement.policyId },
    },
    validationDisputePublication?.authPolicyDeploymentInfo,
  );
};
