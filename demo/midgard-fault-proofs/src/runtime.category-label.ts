import { type FaultProofContracts } from "@al-ft/midgard-sdk";

import { type SupportedFaultProofCategoryName } from "./runtime.require-fault-proof-step-reference-script.js";

export const categoryLabel = (
  categoryName: SupportedFaultProofCategoryName,
): string => {
  switch (categoryName) {
    case "doubleSpend":
      return "double-spend";
    case "nonExistentInput":
      return "non-existent-input";
    case "nonExistentInputNoIndex":
      return "input-no-idx";
    case "invalidRange":
      return "invalid-range";
    case "transitionTrace":
      return "transition-trace";
    case "zeroInput":
      return "zero-input";
    case "validationTraceDispute":
      return "validation-trace-dispute";
    case "daHashPreimage":
      return "da-hash-preimage";
    case "noReferenceInput":
      return "no-reference-input";
    case "referenceInputNoIdx":
      return "reference-input-no-idx";
    case "invalidSignature":
      return "invalid-signature";
    case "fabricatedDeposit":
      return "fabricated-deposit";
    case "fabricatedWithdrawal":
      return "fabricated-withdrawal";
    case "nativeScriptDecoding":
      return "native-script-decoding";
    case "missingSignature":
      return "missing-signature";
    case "missingNativeScriptTx":
      return "missing-native-script-tx";
    case "withdrawnReferenceInput":
      return "withdrawn-reference-input";
    case "canonicalDecodability":
      return "canonical-decodability";
    case "committedFieldShape":
      return "committed-field-shape";
    case "minFee":
      return "min-fee";
    case "withdrawalMistag":
      return "withdrawal-mistag";
    case "doubleWithdraw":
      return "double-withdraw";
    case "crossBlockDuplicateEvent":
      return "cross-block-duplicate-event";
    case "l2TxMistag":
      return "l2-tx-mistag";
    case "withdrawnInput":
      return "withdrawn-input";
    case "valueNotPreserved":
      return "value-not-preserved";
    case "inputSetUniqueness":
      return "input-set-uniqueness";
    case "mintAuthorization":
      return "mint-authorization";
    case "networkId":
      return "network-id";
    case "missingNativeScriptUtxo":
      return "missing-native-script-utxo";
    case "nativeScriptInvalid":
      return "native-script-invalid";
    case "minAda":
      return "min-ada";
    case "fieldPreimageLengthMismatch":
      return "field-preimage-length-mismatch";
    case "fieldItemWidthIllegal":
      return "field-item-width-illegal";
    case "witnessScriptDecoding":
      return "witness-script-decoding";
    case "scriptIntegrityHashMissing":
      return "script-integrity-hash-missing";
    case "transactionOutputNonCanonical":
      return "transaction-output-non-canonical";
    case "mintItemNonCanonical":
      return "mint-item-non-canonical";
    case "resolvedOutputNonCanonical":
      return "resolved-output-non-canonical";
    case "mintDeclaredAssetLimit":
      return "mint-declared-asset-limit";
    case "spendInputSignerMissing":
      return "spend-input-signer-missing";
    case "protectedOutputSignerMissing":
      return "protected-output-signer-missing";
    case "observersForbiddenOnUntaggedNetwork":
      return "observers-forbidden-on-untagged-network";
    case "outputReferenceScriptDecoding":
      return "output-reference-script-decoding";
    case "executionSourceScriptDecoding":
      return "execution-source-script-decoding";
    case "observerOrderInvalid":
      return "observer-order-invalid";
    case "redeemerCanonicity":
      return "redeemer-canonicity";
    case "receivePurposeLanguage":
      return "receive-purpose-language";
    case "unusedScriptWitness":
      return "unused-script-witness";
    case "missingScriptSource":
      return "missing-script-source";
    case "missingRedeemer":
      return "missing-redeemer";
    case "unusedRedeemer":
      return "unused-redeemer";
    case "executionNativeScriptInvalid":
      return "execution-native-script-invalid";
    case "scriptIntegrityHashMismatch":
      return "script-integrity-hash-mismatch";
    case "distinctAssetAccumulationLimit":
      return "distinct-asset-accumulation-limit";
  }
};

export type OneCategoryFaultProofContracts = Pick<
  FaultProofContracts,
  "computationThread" | "fraudProof"
> &
  Partial<{
    readonly [CategoryName in SupportedFaultProofCategoryName]: FaultProofContracts[CategoryName];
  }>;
