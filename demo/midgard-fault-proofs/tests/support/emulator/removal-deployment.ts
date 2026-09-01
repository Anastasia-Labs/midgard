import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
  type MidgardValidators,
  type ReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";

import { type CanonicalDecodabilityContractsV1 } from "../../../src/canonical-decodability/index.js";
import { type CommittedFieldShapeContractsV1 } from "../../../src/committed-field-shape/index.js";
import { type CrossBlockDuplicateEventContractsV1 } from "../../../src/cross-block-duplicate-event/index.js";
import type { DoubleWithdrawContractsV1 } from "../../../src/double-withdraw/contracts-v1.js";
import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationValueAndMintSemanticReferenceScriptDeploymentEntryV1,
} from "../../../src/index.js";
import { type InputSetUniquenessContractsV1 } from "../../../src/input-set-uniqueness/index.js";
import { type L2TxMistagContractsV1 } from "../../../src/l2-tx-mistag/index.js";
import { type MinFeeContractsV1 } from "../../../src/min-fee-contracts-v1.js";
import { type MintAuthorizationContractsV1 } from "../../../src/mint-authorization/index.js";
import { type MissingNativeScriptTxContractsV1 } from "../../../src/missing-native-script-tx/index.js";
import { type MissingSignatureContractsV1 } from "../../../src/missing-signature/index.js";
import { type NativeScriptDecodingContractsV1 } from "../../../src/native-script-decoding/index.js";
import { type ValueNotPreservedContractsV1 } from "../../../src/value-not-preserved/index.js";
import { type WithdrawalMistagContractsV1 } from "../../../src/withdrawal-mistag/index.js";
import { type WithdrawnInputContractsV1 } from "../../../src/withdrawn-input/index.js";
import { type WithdrawnReferenceInputContractsV1 } from "../../../src/withdrawn-reference-input/index.js";
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
export const MISSING_SIGNATURE_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofMissingSignature";

export const MISSING_NATIVE_SCRIPT_TX_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofMissingNativeScriptTx";

export const WITHDRAWN_REFERENCE_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofWithdrawnReferenceInput";

export const CANONICAL_DECODABILITY_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofCanonicalDecodability";

export const COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofCommittedFieldShape";
export const MIN_FEE_REMOVAL_DEPLOYMENT_ENTRY_V1 = "fraudProofMinFee";
export const DOUBLE_WITHDRAW_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofDoubleWithdraw";
export const CROSS_BLOCK_DUPLICATE_EVENT_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofCrossBlockDuplicateEvent";
export const L2_TX_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1 = "fraudProofL2TxMistag";
export const WITHDRAWN_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofWithdrawnInput";
export const WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofWithdrawalMistag";

/**
 * Manifest entry pinning the `input-set-uniqueness` step-01 script hash for
 * removal.
 */
export const INPUT_SET_UNIQUENESS_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofInputSetUniqueness";

/**
 * Manifest entry pinning the `value-not-preserved` step-01 script hash for
 * removal.
 */
export const VALUE_NOT_PRESERVED_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofValueNotPreserved";

/**
 * Manifest entry pinning the `mint-authorization` step-01 script hash for
 * removal.
 */
export const MINT_AUTHORIZATION_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofMintAuthorization";

const requireReferenceScriptAuthPolicy = (
  policy: MidgardValidators["referenceScriptAuth"],
): ReferenceScriptAuthPolicy => {
  const candidate = policy as Partial<ReferenceScriptAuthPolicy>;
  if (
    policy.mintingScript.type !== "Native" ||
    candidate.expiresAtSlot === undefined ||
    candidate.expiresAtUnixTime === undefined ||
    candidate.timelockDurationMs === undefined
  ) {
    throw new Error(
      "Removal deployment fixture requires the harness native reference-script auth policy",
    );
  }
  return candidate as ReferenceScriptAuthPolicy;
};

export const buildRemovalDeploymentInfo = (
  contracts: MidgardValidators & {
    readonly nativeScriptDecoding?: NativeScriptDecodingContractsV1;
    readonly missingSignature?: MissingSignatureContractsV1;
    readonly missingNativeScriptTx?: MissingNativeScriptTxContractsV1;
    readonly withdrawnReferenceInput?: WithdrawnReferenceInputContractsV1;
    readonly canonicalDecodability?: CanonicalDecodabilityContractsV1;
    readonly committedFieldShape?: CommittedFieldShapeContractsV1;
    readonly minFee?: MinFeeContractsV1;
    readonly doubleWithdraw?: DoubleWithdrawContractsV1;
    readonly crossBlockDuplicateEvent?: CrossBlockDuplicateEventContractsV1;
    readonly l2TxMistag?: L2TxMistagContractsV1;
    readonly withdrawnInput?: WithdrawnInputContractsV1;
    readonly withdrawalMistag?: WithdrawalMistagContractsV1;
    readonly inputSetUniqueness?: InputSetUniquenessContractsV1;
    readonly valueNotPreserved?: ValueNotPreservedContractsV1;
    readonly mintAuthorization?: MintAuthorizationContractsV1;
  },
  catalogue: FraudProofCatalogueDeploymentInfo,
  {
    validationDisputePublication,
    validationItemSemanticReference,
    validationItemObserveReference,
    validationCanonicalDecodePrepareReference,
    removalReferenceScripts,
    fraudProofReferenceScripts,
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
     * Live canonical fraud-proof step publications keyed by their production
     * deployment-entry names. Hash-only catalogue records remain sufficient
     * for steps a focused journey never consumes; every consumed reference
     * step must be supplied here.
     */
    readonly fraudProofReferenceScripts?: Readonly<
      Record<string, RemovalDeploymentReference>
    >;
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
  const fraudProofChainEntries = Object.fromEntries(
    Object.entries(FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY).flatMap(
      ([category, entryNames]) => {
        const categoryName =
          category as keyof MidgardValidators["fraudProofContracts"];
        const steps = contracts.fraudProofContracts[categoryName].steps;
        const requiresFullChain =
          categoryName === "transitionTrace" ||
          FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(categoryName) >= 11;
        if (
          steps.length < entryNames.length ||
          (requiresFullChain && steps.length !== entryNames.length)
        ) {
          throw new Error(
            `${category} removal deployment fixture has the wrong chain length`,
          );
        }
        return entryNames.map((entryName, index) => {
          const published = fraudProofReferenceScripts?.[entryName];
          return [
            entryName,
            {
              scriptHash: steps[index]!.spendingScriptHash,
              ...(published === undefined
                ? {}
                : {
                    refScriptUTxO: {
                      txHash: published.utxo.txHash,
                      outputIndex: published.utxo.outputIndex,
                    },
                  }),
            },
          ];
        });
      },
    ),
  );
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
      // Seed every canonical catalogue entry from the production chain, then
      // let the richer family/publication records below replace individual
      // entries with contract bytes and live reference-script out-refs.  The
      // reverse order silently discarded those fields for registered
      // categories.
      ...fraudProofChainEntries,
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
      ...(contracts.missingSignature === undefined
        ? {}
        : {
            [MISSING_SIGNATURE_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.missingSignature.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.missingNativeScriptTx === undefined
        ? {}
        : {
            [MISSING_NATIVE_SCRIPT_TX_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.missingNativeScriptTx.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.withdrawnReferenceInput === undefined
        ? {}
        : {
            [WITHDRAWN_REFERENCE_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.withdrawnReferenceInput.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.canonicalDecodability === undefined
        ? {}
        : {
            [CANONICAL_DECODABILITY_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.canonicalDecodability.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.committedFieldShape === undefined
        ? {}
        : {
            [COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.committedFieldShape.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.minFee === undefined
        ? {}
        : {
            [MIN_FEE_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash: contracts.minFee.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.doubleWithdraw === undefined
        ? {}
        : {
            [DOUBLE_WITHDRAW_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash: contracts.doubleWithdraw.steps[0].spendingScriptHash,
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
      ...(contracts.l2TxMistag === undefined
        ? {}
        : {
            [L2_TX_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash: contracts.l2TxMistag.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.withdrawnInput === undefined
        ? {}
        : {
            [WITHDRAWN_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1]: deploymentEntry(
              contracts.withdrawnInput.steps[0].spendingScriptHash,
              contracts.withdrawnInput.steps[0].spendingScript,
            ),
          }),
      ...(contracts.withdrawalMistag === undefined
        ? {}
        : {
            [WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.withdrawalMistag.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.inputSetUniqueness === undefined
        ? {}
        : {
            [INPUT_SET_UNIQUENESS_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.inputSetUniqueness.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.valueNotPreserved === undefined
        ? {}
        : {
            [VALUE_NOT_PRESERVED_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.valueNotPreserved.steps[0].spendingScriptHash,
            },
          }),
      ...(contracts.mintAuthorization === undefined
        ? {}
        : {
            [MINT_AUTHORIZATION_REMOVAL_DEPLOYMENT_ENTRY_V1]: {
              scriptHash:
                contracts.mintAuthorization.steps[0].spendingScriptHash,
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
      correctionLockSpend: deploymentEntry(
        contracts.correctionLock.spendingScriptHash,
        contracts.correctionLock.spendingScript,
        "correctionLockSpend",
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
    validationDisputePublication?.authPolicyDeploymentInfo ??
      referenceScriptAuthPolicyDeploymentInfo(
        requireReferenceScriptAuthPolicy(contracts.referenceScriptAuth),
      ),
  );
};
