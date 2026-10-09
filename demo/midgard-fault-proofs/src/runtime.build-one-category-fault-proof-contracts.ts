import {
  buildCanonicalDecodabilityFaultProofContracts,
  buildCommittedFieldShapeFaultProofContracts,
  buildCrossBlockDuplicateEventFaultProofContracts,
  buildDaHashPreimageFaultProofContracts,
  buildDistinctAssetAccumulationLimitFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildDoubleWithdrawFaultProofContracts,
  buildExecutionNativeScriptInvalidFaultProofContracts,
  buildExecutionSourceScriptDecodingFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildFieldItemWidthIllegalFaultProofContracts,
  buildFieldPreimageLengthMismatchFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInputSetUniquenessFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildInvalidSignatureFaultProofContracts,
  buildL2TxMistagFaultProofContracts,
  buildMinAdaFaultProofContracts,
  buildMinFeeFaultProofContracts,
  buildMintAuthorizationFaultProofContracts,
  buildMintDeclaredAssetLimitFaultProofContracts,
  buildMintItemNonCanonicalFaultProofContracts,
  buildMissingRedeemerFaultProofContracts,
  buildMissingScriptSourceFaultProofContracts,
  buildMissingSignatureFaultProofContracts,
  buildNativeScriptDecodingFaultProofContracts,
  buildNativeScriptInvalidFaultProofContracts,
  buildNetworkIdFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildObserverOrderInvalidFaultProofContracts,
  buildObserversForbiddenOnUntaggedNetworkFaultProofContracts,
  buildOutputReferenceScriptDecodingFaultProofContracts,
  buildProtectedOutputSignerMissingFaultProofContracts,
  buildReceivePurposeLanguageFaultProofContracts,
  buildRedeemerCanonicityFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildResolvedOutputNonCanonicalFaultProofContracts,
  buildScriptIntegrityHashMismatchFaultProofContracts,
  buildScriptIntegrityHashMissingFaultProofContracts,
  buildSpendInputSignerMissingFaultProofContracts,
  buildTransactionOutputNonCanonicalFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildUnusedRedeemerFaultProofContracts,
  buildUnusedScriptWitnessFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildValueNotPreservedFaultProofContracts,
  buildWithdrawalMistagFaultProofContracts,
  buildWithdrawnInputFaultProofContracts,
  buildWithdrawnReferenceInputFaultProofContracts,
  buildWitnessScriptDecodingFaultProofContracts,
  buildZeroInputFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  contractDeploymentHistoryBounds,
  type ContractDeploymentInfo,
} from "./inspect-contracts.js";
import { type OneCategoryFaultProofContracts } from "./runtime.category-label.js";
import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

export const buildOneCategoryFaultProofContracts = async ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
  referenceScriptAuthPolicyId,
  categoryName,
  deploymentInfo,
}: {
  readonly blueprint: ReturnType<typeof parseFaultProofBlueprint>;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly categoryName: SupportedFaultProofCategoryName;
}): Promise<OneCategoryFaultProofContracts> => {
  const params = {
    blueprint,
    network,
    hubOraclePolicyId,
    fraudProofCataloguePolicyId,
    referenceScriptAuthPolicyId,
  };
  switch (categoryName) {
    case "doubleSpend":
      return await Effect.runPromise(
        buildDoubleSpendFaultProofContracts(params),
      );
    case "nonExistentInput":
      return await Effect.runPromise(
        buildNonExistentInputFaultProofContracts(params),
      );
    case "nonExistentInputNoIndex":
      return await Effect.runPromise(
        buildInputNoIdxFaultProofContracts(params),
      );
    case "invalidRange":
      return await Effect.runPromise(
        buildInvalidRangeFaultProofContracts(params),
      );
    case "transitionTrace":
      return await Effect.runPromise(
        buildTransitionTraceFaultProofContracts({
          ...params,
          eventHistoryBounds: contractDeploymentHistoryBounds(
            deploymentInfo,
            "transitionTrace",
          ),
        }),
      );
    case "zeroInput":
      return await Effect.runPromise(buildZeroInputFaultProofContracts(params));
    case "validationTraceDispute":
      return await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts(params),
      );
    case "daHashPreimage":
      return await Effect.runPromise(
        buildDaHashPreimageFaultProofContracts(params),
      );
    case "noReferenceInput":
      return await Effect.runPromise(
        buildNoReferenceInputFaultProofContracts(params),
      );
    case "referenceInputNoIdx":
      return await Effect.runPromise(
        buildReferenceInputNoIdxFaultProofContracts(params),
      );
    case "invalidSignature":
      return await Effect.runPromise(
        buildInvalidSignatureFaultProofContracts(params),
      );
    case "fabricatedDeposit":
      return await Effect.runPromise(
        buildFabricatedDepositFaultProofContracts({
          ...params,
          eventHistoryBounds: contractDeploymentHistoryBounds(
            deploymentInfo,
            "fabricatedDeposit",
          ),
        }),
      );
    case "fabricatedWithdrawal":
      return await Effect.runPromise(
        buildFabricatedWithdrawalFaultProofContracts({
          ...params,
          eventHistoryBounds: contractDeploymentHistoryBounds(
            deploymentInfo,
            "fabricatedWithdrawal",
          ),
        }),
      );
    case "nativeScriptDecoding":
      return await Effect.runPromise(
        buildNativeScriptDecodingFaultProofContracts(params),
      );
    case "missingSignature":
      return await Effect.runPromise(
        buildMissingSignatureFaultProofContracts(params),
      );
    case "withdrawnReferenceInput":
      return await Effect.runPromise(
        buildWithdrawnReferenceInputFaultProofContracts(params),
      );
    case "canonicalDecodability":
      return await Effect.runPromise(
        buildCanonicalDecodabilityFaultProofContracts(params),
      );
    case "committedFieldShape":
      return await Effect.runPromise(
        buildCommittedFieldShapeFaultProofContracts(params),
      );
    case "minFee":
      return await Effect.runPromise(buildMinFeeFaultProofContracts(params));
    case "withdrawalMistag":
      return await Effect.runPromise(
        buildWithdrawalMistagFaultProofContracts(params),
      );
    case "doubleWithdraw":
      return await Effect.runPromise(
        buildDoubleWithdrawFaultProofContracts(params),
      );
    case "crossBlockDuplicateEvent":
      return await Effect.runPromise(
        buildCrossBlockDuplicateEventFaultProofContracts(params),
      );
    case "l2TxMistag":
      return await Effect.runPromise(
        buildL2TxMistagFaultProofContracts(params),
      );
    case "withdrawnInput":
      return await Effect.runPromise(
        buildWithdrawnInputFaultProofContracts(params),
      );
    case "valueNotPreserved":
      return await Effect.runPromise(
        buildValueNotPreservedFaultProofContracts(params),
      );
    case "inputSetUniqueness":
      return await Effect.runPromise(
        buildInputSetUniquenessFaultProofContracts(params),
      );
    case "mintAuthorization":
      return await Effect.runPromise(
        buildMintAuthorizationFaultProofContracts(params),
      );
    case "networkId":
      return await Effect.runPromise(buildNetworkIdFaultProofContracts(params));
    case "nativeScriptInvalid":
      return await Effect.runPromise(
        buildNativeScriptInvalidFaultProofContracts(params),
      );
    case "minAda":
      return await Effect.runPromise(buildMinAdaFaultProofContracts(params));
    case "fieldPreimageLengthMismatch":
      return await Effect.runPromise(
        buildFieldPreimageLengthMismatchFaultProofContracts(params),
      );
    case "fieldItemWidthIllegal":
      return await Effect.runPromise(
        buildFieldItemWidthIllegalFaultProofContracts(params),
      );
    case "witnessScriptDecoding":
      return await Effect.runPromise(
        buildWitnessScriptDecodingFaultProofContracts(params),
      );
    case "scriptIntegrityHashMissing":
      return await Effect.runPromise(
        buildScriptIntegrityHashMissingFaultProofContracts(params),
      );
    case "transactionOutputNonCanonical":
      return await Effect.runPromise(
        buildTransactionOutputNonCanonicalFaultProofContracts(params),
      );
    case "mintItemNonCanonical":
      return await Effect.runPromise(
        buildMintItemNonCanonicalFaultProofContracts(params),
      );
    case "resolvedOutputNonCanonical":
      return await Effect.runPromise(
        buildResolvedOutputNonCanonicalFaultProofContracts(params),
      );
    case "mintDeclaredAssetLimit":
      return await Effect.runPromise(
        buildMintDeclaredAssetLimitFaultProofContracts(params),
      );
    case "spendInputSignerMissing":
      return await Effect.runPromise(
        buildSpendInputSignerMissingFaultProofContracts(params),
      );
    case "protectedOutputSignerMissing":
      return await Effect.runPromise(
        buildProtectedOutputSignerMissingFaultProofContracts(params),
      );
    case "observersForbiddenOnUntaggedNetwork":
      return await Effect.runPromise(
        buildObserversForbiddenOnUntaggedNetworkFaultProofContracts(params),
      );
    case "outputReferenceScriptDecoding":
      return await Effect.runPromise(
        buildOutputReferenceScriptDecodingFaultProofContracts(params),
      );
    case "executionSourceScriptDecoding":
      return await Effect.runPromise(
        buildExecutionSourceScriptDecodingFaultProofContracts(params),
      );
    case "observerOrderInvalid":
      return await Effect.runPromise(
        buildObserverOrderInvalidFaultProofContracts(params),
      );
    case "redeemerCanonicity":
      return await Effect.runPromise(
        buildRedeemerCanonicityFaultProofContracts(params),
      );
    case "receivePurposeLanguage":
      return await Effect.runPromise(
        buildReceivePurposeLanguageFaultProofContracts(params),
      );
    case "unusedScriptWitness":
      return await Effect.runPromise(
        buildUnusedScriptWitnessFaultProofContracts(params),
      );
    case "missingScriptSource":
      return await Effect.runPromise(
        buildMissingScriptSourceFaultProofContracts(params),
      );
    case "missingRedeemer":
      return await Effect.runPromise(
        buildMissingRedeemerFaultProofContracts(params),
      );
    case "unusedRedeemer":
      return await Effect.runPromise(
        buildUnusedRedeemerFaultProofContracts(params),
      );
    case "executionNativeScriptInvalid":
      return await Effect.runPromise(
        buildExecutionNativeScriptInvalidFaultProofContracts(params),
      );
    case "scriptIntegrityHashMismatch":
      return await Effect.runPromise(
        buildScriptIntegrityHashMismatchFaultProofContracts(params),
      );
    case "distinctAssetAccumulationLimit":
      return await Effect.runPromise(
        buildDistinctAssetAccumulationLimitFaultProofContracts(params),
      );
  }
};
