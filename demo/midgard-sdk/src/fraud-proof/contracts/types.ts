/**
 * The aggregate FaultProofContracts shape, its chain view, and the top-level builder parameters.
 */

import { Network } from "@lucid-evolution/lucid";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../common.js";
import { type FaultProofBlueprint } from "./blueprint.js";
import { type CanonicalDecodabilityFaultProofContracts } from "./families/canonical-decodability.js";
import { type CommittedFieldShapeFaultProofContracts } from "./families/committed-field-shape.js";
import { type CrossBlockDuplicateEventFaultProofContracts } from "./families/cross-block-duplicate-event.js";
import { type DaHashPreimageFaultProofContracts } from "./families/da-hash-preimage.js";
import { type DistinctAssetAccumulationLimitFaultProofContracts } from "./families/distinct-asset-accumulation-limit.js";
import { type DoubleSpendFaultProofContracts } from "./families/double-spend.js";
import { type DoubleWithdrawFaultProofContracts } from "./families/double-withdraw.js";
import { type ExecutionNativeScriptInvalidFaultProofContracts } from "./families/execution-native-script-invalid.js";
import { type ExecutionSourceScriptDecodingFaultProofContracts } from "./families/execution-source-script-decoding.js";
import { type FabricatedDepositFaultProofContracts } from "./families/fabricated-deposit.js";
import { type FabricatedWithdrawalFaultProofContracts } from "./families/fabricated-withdrawal.js";
import { type FieldItemWidthIllegalFaultProofContracts } from "./families/field-item-width-illegal.js";
import { type FieldPreimageLengthMismatchFaultProofContracts } from "./families/field-preimage-length-mismatch.js";
import { type InputNoIdxFaultProofContracts } from "./families/input-no-idx.js";
import { type InputSetUniquenessFaultProofContracts } from "./families/input-set-uniqueness.js";
import { type InvalidRangeFaultProofContracts } from "./families/invalid-range.js";
import { type InvalidSignatureFaultProofContracts } from "./families/invalid-signature.js";
import { type L2TxMistagFaultProofContracts } from "./families/l2-tx-mistag.js";
import { type MinAdaFaultProofContracts } from "./families/min-ada.js";
import { type MinFeeFaultProofContracts } from "./families/min-fee.js";
import { type MintAuthorizationFaultProofContracts } from "./families/mint-authorization.js";
import { type MintDeclaredAssetLimitFaultProofContracts } from "./families/mint-declared-asset-limit.js";
import { type MissingNativeScriptTxFaultProofContracts } from "./families/missing-native-script-tx.js";
import { type MissingNativeScriptUtxoFaultProofContracts } from "./families/missing-native-script-utxo.js";
import { type MissingRedeemerFaultProofContracts } from "./families/missing-redeemer.js";
import { type MissingScriptSourceFaultProofContracts } from "./families/missing-script-source.js";
import { type MissingSignatureFaultProofContracts } from "./families/missing-signature.js";
import { type NativeScriptDecodingFaultProofContracts } from "./families/native-script-decoding.js";
import { type NativeScriptInvalidFaultProofContracts } from "./families/native-script-invalid.js";
import { type NetworkIdFaultProofContracts } from "./families/network-id.js";
import { type NoReferenceInputFaultProofContracts } from "./families/no-reference-input.js";
import { type NonExistentInputFaultProofContracts } from "./families/non-existent-input.js";
import { type ObserverOrderInvalidFaultProofContracts } from "./families/observer-order-invalid.js";
import { type ObserversForbiddenOnUntaggedNetworkFaultProofContracts } from "./families/observers-forbidden-on-untagged-network.js";
import { type OutputReferenceScriptDecodingFaultProofContracts } from "./families/output-reference-script-decoding.js";
import { type ProtectedOutputSignerMissingFaultProofContracts } from "./families/protected-output-signer-missing.js";
import { type ReceivePurposeLanguageFaultProofContracts } from "./families/receive-purpose-language.js";
import { type RedeemerCanonicityFaultProofContracts } from "./families/redeemer-canonicity.js";
import { type ReferenceInputNoIdxFaultProofContracts } from "./families/reference-input-no-idx.js";
import { type ResolvedOutputNonCanonicalFaultProofContracts } from "./families/resolved-output-non-canonical.js";
import { type ScriptIntegrityHashMismatchFaultProofContracts } from "./families/script-integrity-hash-mismatch.js";
import { type ScriptIntegrityHashMissingFaultProofContracts } from "./families/script-integrity-hash-missing.js";
import { type SpendInputSignerMissingFaultProofContracts } from "./families/spend-input-signer-missing.js";
import { type TransactionOutputNonCanonicalFaultProofContracts } from "./families/transaction-output-non-canonical.js";
import { type TransitionTraceFaultProofContracts } from "./families/transition-trace.js";
import { type UnusedRedeemerFaultProofContracts } from "./families/unused-redeemer.js";
import { type UnusedScriptWitnessFaultProofContracts } from "./families/unused-script-witness.js";
import { type ValidationTraceDisputeFaultProofContracts } from "./families/validation-trace-dispute.js";
import { type ValueNotPreservedFaultProofContracts } from "./families/value-not-preserved.js";
import { type WithdrawalMistagFaultProofContracts } from "./families/withdrawal-mistag.js";
import { type WithdrawnInputFaultProofContracts } from "./families/withdrawn-input.js";
import { type WithdrawnReferenceInputFaultProofContracts } from "./families/withdrawn-reference-input.js";
import { type WitnessScriptDecodingFaultProofContracts } from "./families/witness-script-decoding.js";
import { type ZeroInputFaultProofContracts } from "./families/zero-input.js";

export type FraudProofChain = {
  readonly firstStep: SpendingValidator;
  readonly steps: readonly [SpendingValidator, ...SpendingValidator[]];
};

export type FaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: DoubleSpendFaultProofContracts["doubleSpend"];
  readonly nonExistentInput: NonExistentInputFaultProofContracts["nonExistentInput"];
  readonly noReferenceInput: NoReferenceInputFaultProofContracts["noReferenceInput"];
  readonly invalidRange: InvalidRangeFaultProofContracts["invalidRange"];
  readonly invalidSignature: InvalidSignatureFaultProofContracts["invalidSignature"];
  readonly zeroInput: ZeroInputFaultProofContracts["zeroInput"];
  readonly transitionTrace: TransitionTraceFaultProofContracts["transitionTrace"];
  readonly validationTraceDispute: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
  readonly daHashPreimage: DaHashPreimageFaultProofContracts["daHashPreimage"];
  readonly nonExistentInputNoIndex: InputNoIdxFaultProofContracts["nonExistentInputNoIndex"];
  readonly referenceInputNoIdx: ReferenceInputNoIdxFaultProofContracts["referenceInputNoIdx"];
  readonly fabricatedDeposit: FabricatedDepositFaultProofContracts["fabricatedDeposit"];
  readonly fabricatedWithdrawal: FabricatedWithdrawalFaultProofContracts["fabricatedWithdrawal"];
  readonly nativeScriptDecoding: NativeScriptDecodingFaultProofContracts["nativeScriptDecoding"];
  readonly missingSignature: MissingSignatureFaultProofContracts["missingSignature"];
  readonly missingNativeScriptTx: MissingNativeScriptTxFaultProofContracts["missingNativeScriptTx"];
  readonly withdrawnReferenceInput: WithdrawnReferenceInputFaultProofContracts["withdrawnReferenceInput"];
  readonly canonicalDecodability: CanonicalDecodabilityFaultProofContracts["canonicalDecodability"];
  readonly committedFieldShape: CommittedFieldShapeFaultProofContracts["committedFieldShape"];
  readonly minFee: MinFeeFaultProofContracts["minFee"];
  readonly withdrawalMistag: WithdrawalMistagFaultProofContracts["withdrawalMistag"];
  readonly doubleWithdraw: DoubleWithdrawFaultProofContracts["doubleWithdraw"];
  readonly crossBlockDuplicateEvent: CrossBlockDuplicateEventFaultProofContracts["crossBlockDuplicateEvent"];
  readonly l2TxMistag: L2TxMistagFaultProofContracts["l2TxMistag"];
  readonly withdrawnInput: WithdrawnInputFaultProofContracts["withdrawnInput"];
  readonly valueNotPreserved: ValueNotPreservedFaultProofContracts["valueNotPreserved"];
  readonly inputSetUniqueness: InputSetUniquenessFaultProofContracts["inputSetUniqueness"];
  readonly mintAuthorization: MintAuthorizationFaultProofContracts["mintAuthorization"];
  readonly networkId: NetworkIdFaultProofContracts["networkId"];
  readonly missingNativeScriptUtxo: MissingNativeScriptUtxoFaultProofContracts["missingNativeScriptUtxo"];
  readonly nativeScriptInvalid: NativeScriptInvalidFaultProofContracts["nativeScriptInvalid"];
  readonly minAda: MinAdaFaultProofContracts["minAda"];
  readonly fieldPreimageLengthMismatch: FieldPreimageLengthMismatchFaultProofContracts["fieldPreimageLengthMismatch"];
  readonly fieldItemWidthIllegal: FieldItemWidthIllegalFaultProofContracts["fieldItemWidthIllegal"];
  readonly witnessScriptDecoding: WitnessScriptDecodingFaultProofContracts["witnessScriptDecoding"];
  readonly scriptIntegrityHashMissing: ScriptIntegrityHashMissingFaultProofContracts["scriptIntegrityHashMissing"];
  readonly transactionOutputNonCanonical: TransactionOutputNonCanonicalFaultProofContracts["transactionOutputNonCanonical"];
  readonly resolvedOutputNonCanonical: ResolvedOutputNonCanonicalFaultProofContracts["resolvedOutputNonCanonical"];
  readonly mintDeclaredAssetLimit: MintDeclaredAssetLimitFaultProofContracts["mintDeclaredAssetLimit"];
  readonly spendInputSignerMissing: SpendInputSignerMissingFaultProofContracts["spendInputSignerMissing"];
  readonly protectedOutputSignerMissing: ProtectedOutputSignerMissingFaultProofContracts["protectedOutputSignerMissing"];
  readonly observersForbiddenOnUntaggedNetwork: ObserversForbiddenOnUntaggedNetworkFaultProofContracts["observersForbiddenOnUntaggedNetwork"];
  readonly outputReferenceScriptDecoding: OutputReferenceScriptDecodingFaultProofContracts["outputReferenceScriptDecoding"];
  readonly executionSourceScriptDecoding: ExecutionSourceScriptDecodingFaultProofContracts["executionSourceScriptDecoding"];
  readonly executionNativeScriptInvalid: ExecutionNativeScriptInvalidFaultProofContracts["executionNativeScriptInvalid"];
  readonly observerOrderInvalid: ObserverOrderInvalidFaultProofContracts["observerOrderInvalid"];
  readonly redeemerCanonicity: RedeemerCanonicityFaultProofContracts["redeemerCanonicity"];
  readonly receivePurposeLanguage: ReceivePurposeLanguageFaultProofContracts["receivePurposeLanguage"];
  readonly unusedScriptWitness: UnusedScriptWitnessFaultProofContracts["unusedScriptWitness"];
  readonly missingScriptSource: MissingScriptSourceFaultProofContracts["missingScriptSource"];
  readonly missingRedeemer: MissingRedeemerFaultProofContracts["missingRedeemer"];
  readonly unusedRedeemer: UnusedRedeemerFaultProofContracts["unusedRedeemer"];
  readonly scriptIntegrityHashMismatch: ScriptIntegrityHashMismatchFaultProofContracts["scriptIntegrityHashMismatch"];
  readonly distinctAssetAccumulationLimit: DistinctAssetAccumulationLimitFaultProofContracts["distinctAssetAccumulationLimit"];
};

/**
 * Manifest-restorable category chains. Shared minting/spending policies are
 * deliberately excluded because the deployment ABI does not persist enough
 * information to reconstruct them safely.
 */
export type FaultProofContractChains = Omit<
  FaultProofContracts,
  "computationThread" | "fraudProof"
>;

export type BuildFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
  readonly referenceScriptAuthPolicyId?: string;
};
