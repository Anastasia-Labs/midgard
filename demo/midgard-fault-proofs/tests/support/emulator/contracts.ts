import {
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildCanonicalDecodabilityFaultProofContracts,
  buildCommittedFieldShapeFaultProofContracts,
  buildCrossBlockDuplicateEventFaultProofContracts,
  buildDaHashPreimageFaultProofContracts,
  buildDistinctAssetAccumulationLimitFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildDoubleWithdrawFaultProofContracts,
  buildExecutionSourceScriptDecodingFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildFieldItemWidthIllegalFaultProofContracts,
  buildFieldPreimageLengthMismatchFaultProofContracts,
  buildHubOracleMintingValidator,
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
  buildMissingNativeScriptTxFaultProofContracts,
  buildMissingNativeScriptUtxoFaultProofContracts,
  buildMissingSignatureFaultProofContracts,
  buildNativeScriptDecodingFaultProofContracts,
  buildNativeScriptInvalidFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildObserverOrderInvalidFaultProofContracts,
  buildObserversForbiddenOnUntaggedNetworkFaultProofContracts,
  buildOutputReferenceScriptDecodingFaultProofContracts,
  buildProtectedOutputSignerMissingFaultProofContracts,
  buildRedeemerCanonicityFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildResolvedOutputNonCanonicalFaultProofContracts,
  buildScriptIntegrityHashMismatchFaultProofContracts,
  buildScriptIntegrityHashMissingFaultProofContracts,
  buildSpendInputSignerMissingFaultProofContracts,
  buildTransactionOutputNonCanonicalFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildTxOrderValidators,
  buildValidationTraceDisputeFaultProofContracts,
  buildValueNotPreservedFaultProofContracts,
  buildWithdrawalMistagFaultProofContracts,
  buildWithdrawnInputFaultProofContracts,
  buildWithdrawnReferenceInputFaultProofContracts,
  buildWitnessScriptDecodingFaultProofContracts,
  buildZeroInputFaultProofContracts,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  FAULT_PROOF_SHARED_TITLES,
  type FaultProofContractChains,
  fraudProofContractsToFirstSteps,
  type MidgardValidators,
  type MintingValidator as SdkMintingValidator,
  parseFaultProofBlueprint,
  type SpendingValidator as SdkSpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type SpendingValidator,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { type CanonicalDecodabilityContracts } from "../../../src/canonical-decodability/contracts.js";
import { type CommittedFieldShapeContracts } from "../../../src/committed-field-shape/contracts.js";
import { type CrossBlockDuplicateEventContracts } from "../../../src/cross-block-duplicate-event/index.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES,
  type DistinctAssetAccumulationContracts,
} from "../../../src/distinct-asset-accumulation-limit/contracts.js";
import { type DoubleWithdrawContracts } from "../../../src/double-withdraw/contracts.js";
import { type InputSetUniquenessContracts } from "../../../src/input-set-uniqueness/contracts.js";
import { type L2TxMistagContracts } from "../../../src/l2-tx-mistag/contracts.js";
import { type MinAdaContracts } from "../../../src/min-ada/contracts.js";
import { type MinFeeContracts } from "../../../src/min-fee-contracts.js";
import { type MintAuthorizationContracts } from "../../../src/mint-authorization/contracts.js";
import { type MissingNativeScriptTxContracts } from "../../../src/missing-native-script-tx/contracts.js";
import { type MissingNativeScriptUtxoContracts } from "../../../src/missing-native-script-utxo/contracts.js";
import { type MissingSignatureContracts } from "../../../src/missing-signature/contracts.js";
import { type NativeScriptDecodingContracts } from "../../../src/native-script-decoding/contracts.js";
import { type NativeScriptInvalidContracts } from "../../../src/native-script-invalid/contracts.js";
import { type FabricatedDepositContracts } from "../../../src/submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContracts } from "../../../src/submit-fabricated-withdrawal-step-01.js";
import { type ValueNotPreservedContracts } from "../../../src/value-not-preserved/contracts.js";
import { type WithdrawalMistagContracts } from "../../../src/withdrawal-mistag/contracts.js";
import { type WithdrawnInputContracts } from "../../../src/withdrawn-input/contracts.js";
import { type WithdrawnReferenceInputContracts } from "../../../src/withdrawn-reference-input/contracts.js";
import {
  applyCompiledScript,
  type Blueprint,
  cloneBlueprint,
  getCompiledScript,
  network,
} from "./blueprints.js";
import {
  makeAlwaysSucceedsContracts,
  makeAuthenticatedValidator,
  makeIsolatedAlwaysSucceedsAuthenticatedValidator,
  makeMintingValidator,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "./validators.js";

type EmulatorStepTuple = readonly [
  Pick<SdkSpendingValidator, "spendingScript">,
  ...Pick<SdkSpendingValidator, "spendingScript">[],
];

type SdkStepTuple<Steps extends EmulatorStepTuple> = {
  readonly [Index in keyof Steps]: SdkSpendingValidator;
};

const chainFromSteps = <const Steps extends EmulatorStepTuple>(
  steps: Steps,
) => {
  const sdkSteps = steps.map((step) =>
    makeSpendingValidator(step.spendingScript.script),
  ) as unknown as SdkStepTuple<Steps>;
  return { firstStep: sdkSteps[0], steps: sdkSteps };
};

export const buildMinimalFaultProofContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
  nonceUtxo: UTxO,
  {
    realNonExistentInput = false,
    realInvalidRange = false,
    realTransitionTrace = false,
    realZeroInput = false,
    realDaHashPreimage = false,
    realFabricatedDeposit = false,
    realFabricatedWithdrawal = false,
    realInputNoIdx = false,
    realNoReferenceInput = false,
    realReferenceInputNoIdx = false,
    realInvalidSignature = false,
    realValidationTraceDispute = false,
    realNativeScriptDecoding = false,
    realMissingSignature = false,
    realMissingNativeScriptTx = false,
    realMissingNativeScriptUtxo = false,
    realNativeScriptInvalid = false,
    realMinAda = false,
    realCanonicalDecodability = false,
    realCommittedFieldShape = false,
    realFieldItemWidthIllegal = false,
    realRedeemerCanonicity = false,
    realFieldPreimageLengthMismatch = false,
    realWitnessScriptDecoding = false,
    realScriptIntegrityHashMissing = false,
    realScriptIntegrityHashMismatch = false,
    realWithdrawnReferenceInput = false,
    realMinFee = false,
    realDoubleWithdraw = false,
    realCrossBlockDuplicateEvent = false,
    realSettlement = false,
    realTxOrder = false,
    realL2TxMistag = false,
    realWithdrawnInput = false,
    realWithdrawalMistag = false,
    realInputSetUniqueness = false,
    realValueNotPreserved = false,
    realMintAuthorization = false,
    realDistinctAssetAccumulationLimit = false,
    realMintDeclaredAssetLimit = false,
    realTransactionOutputNonCanonical = false,
    realMintItemNonCanonical = false,
    realResolvedOutputNonCanonical = false,
    realSpendInputSignerMissing = false,
    realProtectedOutputSignerMissing = false,
    realObserversForbiddenOnUntaggedNetwork = false,
    realObserverOrderInvalid = false,
    realOutputReferenceScriptDecoding = false,
    realExecutionSourceScriptDecoding = false,
    alwaysFraudProofCatalogue = false,
    alwaysStateQueue = false,
    referenceScriptAuthPolicyId,
  }: {
    readonly realNonExistentInput?: boolean;
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
    readonly realZeroInput?: boolean;
    readonly realDaHashPreimage?: boolean;
    readonly realFabricatedDeposit?: boolean;
    readonly realFabricatedWithdrawal?: boolean;
    readonly realInputNoIdx?: boolean;
    readonly realNoReferenceInput?: boolean;
    readonly realReferenceInputNoIdx?: boolean;
    readonly realInvalidSignature?: boolean;
    readonly realValidationTraceDispute?: boolean;
    readonly realNativeScriptDecoding?: boolean;
    readonly realMissingSignature?: boolean;
    readonly realMissingNativeScriptTx?: boolean;
    readonly realMissingNativeScriptUtxo?: boolean;
    readonly realNativeScriptInvalid?: boolean;
    readonly realMinAda?: boolean;
    readonly realCanonicalDecodability?: boolean;
    readonly realCommittedFieldShape?: boolean;
    readonly realFieldItemWidthIllegal?: boolean;
    readonly realRedeemerCanonicity?: boolean;
    readonly realFieldPreimageLengthMismatch?: boolean;
    readonly realWitnessScriptDecoding?: boolean;
    readonly realScriptIntegrityHashMissing?: boolean;
    readonly realScriptIntegrityHashMismatch?: boolean;
    readonly realWithdrawnReferenceInput?: boolean;
    readonly realMinFee?: boolean;
    readonly realDoubleWithdraw?: boolean;
    readonly realCrossBlockDuplicateEvent?: boolean;
    readonly realSettlement?: boolean;
    readonly realTxOrder?: boolean;
    readonly realL2TxMistag?: boolean;
    readonly realWithdrawnInput?: boolean;
    readonly realWithdrawalMistag?: boolean;
    readonly realInputSetUniqueness?: boolean;
    readonly realValueNotPreserved?: boolean;
    readonly realMintAuthorization?: boolean;
    readonly realDistinctAssetAccumulationLimit?: boolean;
    readonly realMintDeclaredAssetLimit?: boolean;
    readonly realTransactionOutputNonCanonical?: boolean;
    readonly realMintItemNonCanonical?: boolean;
    readonly realResolvedOutputNonCanonical?: boolean;
    readonly realSpendInputSignerMissing?: boolean;
    readonly realProtectedOutputSignerMissing?: boolean;
    readonly realObserversForbiddenOnUntaggedNetwork?: boolean;
    readonly realObserverOrderInvalid?: boolean;
    readonly realOutputReferenceScriptDecoding?: boolean;
    readonly realExecutionSourceScriptDecoding?: boolean;
    readonly alwaysFraudProofCatalogue?: boolean;
    /**
     * Test-only admission bypass for faults whose malformed header is rejected
     * by the production state queue before the fault-proof family can observe
     * it. The family under test, computation-thread policies, fraud-proof
     * policy, and removal path remain their production implementations.
     */
    readonly alwaysStateQueue?: boolean;
    readonly referenceScriptAuthPolicyId?: string;
  } = {},
): Promise<
  MidgardValidators & {
    readonly computationThread: SdkMintingValidator;
    readonly fabricatedDeposit?: FabricatedDepositContracts;
    readonly fabricatedWithdrawal?: FabricatedWithdrawalContracts;
    readonly nativeScriptDecoding?: NativeScriptDecodingContracts;
    readonly missingSignature?: MissingSignatureContracts;
    readonly missingNativeScriptTx?: MissingNativeScriptTxContracts;
    readonly missingNativeScriptUtxo?: MissingNativeScriptUtxoContracts;
    readonly nativeScriptInvalid?: NativeScriptInvalidContracts;
    readonly minAda?: MinAdaContracts;
    readonly canonicalDecodability?: CanonicalDecodabilityContracts;
    readonly committedFieldShape?: CommittedFieldShapeContracts;
    readonly withdrawnReferenceInput?: WithdrawnReferenceInputContracts;
    readonly minFee?: MinFeeContracts;
    readonly doubleWithdraw?: DoubleWithdrawContracts;
    readonly crossBlockDuplicateEvent?: CrossBlockDuplicateEventContracts;
    readonly l2TxMistag?: L2TxMistagContracts;
    readonly withdrawnInput?: WithdrawnInputContracts;
    readonly withdrawalMistag?: WithdrawalMistagContracts;
    readonly inputSetUniqueness?: InputSetUniquenessContracts;
    readonly valueNotPreserved?: ValueNotPreservedContracts;
    readonly mintAuthorization?: MintAuthorizationContracts;
    readonly distinctAssetAccumulationLimit?: DistinctAssetAccumulationContracts;
  }
> => {
  // This integration test proves the real active-operators slashing and
  // scheduler removal path. Registered/retired operator setup remains
  // scaffolded only where needed to support the focused removal flow.
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = buildHubOracleMintingValidator({
    blueprint: parseFaultProofBlueprint(realBlueprint),
    oneShotOutRef: nonceUtxo,
  });
  const hubOracleAuth: AuthenticatedValidator = {
    ...hubOracle,
    spendingScriptCBOR: hubOracle.mintingScriptCBOR,
    spendingScript: hubOracle.mintingScript as SpendingValidator,
    spendingScriptHash: hubOracle.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracle.policyId),
    ),
  };
  const settlementMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "settlement.mint.mint", [
      hubOracle.policyId,
    ]),
  );
  const withHubOracle = {
    ...base,
    hubOracle: hubOracleAuth,
    ...(realTxOrder
      ? buildTxOrderValidators({
          blueprint: parseFaultProofBlueprint(realBlueprint),
          network,
          hubOraclePolicyId: hubOracle.policyId,
        })
      : {}),
    ...(realSettlement
      ? {
          settlement: {
            ...settlementMinting,
            ...makeSpendingValidator(
              applyCompiledScript(realBlueprint, "settlement.spend.spend", [
                hubOracle.policyId,
                settlementMinting.policyId,
              ]),
            ),
          },
        }
      : {}),
  };

  const fraudProofCatalogue = alwaysFraudProofCatalogue
    ? withHubOracle.fraudProofCatalogue
    : makeAuthenticatedValidator(
        applyCompiledScript(realBlueprint, "fraud_proof_catalogue.mint.mint", [
          hubOracle.policyId,
        ]),
        getCompiledScript(realBlueprint, "fraud_proof_catalogue.spend.else"),
      );
  const withCatalogue = {
    ...withHubOracle,
    fraudProofCatalogue,
  };

  const retiredOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/retired_operators.mint.mint",
      [hubOracle.policyId],
    ),
  );
  const retiredOperators: AuthenticatedValidator = {
    ...retiredOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/retired_operators.spend.spend",
        [retiredOperatorsMinting.policyId],
      ),
    ),
  };
  const withRetiredOperators = {
    ...withCatalogue,
    retiredOperators,
  };

  const registeredOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/registered_operators.mint.mint",
      [retiredOperators.policyId, hubOracle.policyId],
    ),
  );
  const registeredOperators: AuthenticatedValidator = {
    ...registeredOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/registered_operators.spend.spend",
        [registeredOperatorsMinting.policyId],
      ),
    ),
  };
  const withRegisteredOperators = {
    ...withRetiredOperators,
    registeredOperators,
  };

  const activeOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/active_operators.mint.mint",
      [
        hubOracle.policyId,
        registeredOperators.policyId,
        retiredOperators.policyId,
      ],
    ),
  );
  const activeOperators: AuthenticatedValidator = {
    ...activeOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/active_operators.spend.spend",
        [activeOperatorsMinting.policyId, hubOracle.policyId],
      ),
    ),
  };
  const withActiveOperators = {
    ...withRegisteredOperators,
    activeOperators,
  };

  const doubleSpendContracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(realBlueprint),
      network,
      hubOraclePolicyId: hubOracle.policyId,
      fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
    }),
  );
  // Every real family must share the double-spend family's fraud-proof policy:
  // the catalogue, the computation-thread mints, and removal all key on that
  // one policy, so a family compiling to a different policy id is a
  // parameterization bug.
  const buildFamilyContracts = async <
    T extends { readonly fraudProof: { readonly policyId: string } },
    E,
  >(
    enabled: boolean,
    build: (args: {
      readonly blueprint: ReturnType<typeof parseFaultProofBlueprint>;
      readonly network: typeof network;
      readonly hubOraclePolicyId: string;
      readonly fraudProofCataloguePolicyId: string;
      readonly referenceScriptAuthPolicyId: string;
    }) => Effect.Effect<T, E>,
  ): Promise<T | undefined> => {
    if (!enabled) {
      return undefined;
    }
    const familyContracts = await Effect.runPromise(
      build({
        blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
        network,
        hubOraclePolicyId: hubOracle.policyId,
        fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        referenceScriptAuthPolicyId:
          referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
      }),
    );
    expect(familyContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
    return familyContracts;
  };
  const nonExistentInputContracts = await buildFamilyContracts(
    realNonExistentInput,
    buildNonExistentInputFaultProofContracts,
  );
  const invalidRangeContracts = await buildFamilyContracts(
    realInvalidRange,
    buildInvalidRangeFaultProofContracts,
  );
  const transitionTraceContracts = await buildFamilyContracts(
    realTransitionTrace,
    buildTransitionTraceFaultProofContracts,
  );
  const validationTraceDisputeContracts = await buildFamilyContracts(
    realValidationTraceDispute,
    buildValidationTraceDisputeFaultProofContracts,
  );
  const zeroInputContracts = await buildFamilyContracts(
    realZeroInput,
    buildZeroInputFaultProofContracts,
  );
  const daHashPreimageContracts = await buildFamilyContracts(
    realDaHashPreimage,
    buildDaHashPreimageFaultProofContracts,
  );
  const fabricatedDepositContracts = await buildFamilyContracts(
    realFabricatedDeposit,
    buildFabricatedDepositFaultProofContracts,
  );
  const fabricatedWithdrawalContracts = await buildFamilyContracts(
    realFabricatedWithdrawal,
    buildFabricatedWithdrawalFaultProofContracts,
  );
  const inputNoIdxContracts = await buildFamilyContracts(
    realInputNoIdx,
    buildInputNoIdxFaultProofContracts,
  );
  const noReferenceInputContracts = await buildFamilyContracts(
    realNoReferenceInput,
    buildNoReferenceInputFaultProofContracts,
  );
  const referenceInputNoIdxContracts = await buildFamilyContracts(
    realReferenceInputNoIdx,
    buildReferenceInputNoIdxFaultProofContracts,
  );
  const invalidSignatureContracts = await buildFamilyContracts(
    realInvalidSignature,
    buildInvalidSignatureFaultProofContracts,
  );
  const minFeeContracts = await buildFamilyContracts(
    realMinFee,
    buildMinFeeFaultProofContracts,
  );
  const nativeScriptDecodingContracts = await buildFamilyContracts(
    realNativeScriptDecoding,
    buildNativeScriptDecodingFaultProofContracts,
  );
  const missingSignatureContracts = await buildFamilyContracts(
    realMissingSignature,
    buildMissingSignatureFaultProofContracts,
  );
  const missingNativeScriptTxContracts = await buildFamilyContracts(
    realMissingNativeScriptTx,
    buildMissingNativeScriptTxFaultProofContracts,
  );
  const missingNativeScriptUtxoContracts = await buildFamilyContracts(
    realMissingNativeScriptUtxo,
    buildMissingNativeScriptUtxoFaultProofContracts,
  );
  const nativeScriptInvalidContracts = await buildFamilyContracts(
    realNativeScriptInvalid,
    buildNativeScriptInvalidFaultProofContracts,
  );
  const minAdaContracts = await buildFamilyContracts(
    realMinAda,
    buildMinAdaFaultProofContracts,
  );
  const withdrawnReferenceInputContracts = await buildFamilyContracts(
    realWithdrawnReferenceInput,
    buildWithdrawnReferenceInputFaultProofContracts,
  );
  const canonicalDecodabilityContracts = await buildFamilyContracts(
    realCanonicalDecodability,
    buildCanonicalDecodabilityFaultProofContracts,
  );
  const committedFieldShapeContracts = await buildFamilyContracts(
    realCommittedFieldShape,
    buildCommittedFieldShapeFaultProofContracts,
  );
  const fieldItemWidthIllegalContracts = await buildFamilyContracts(
    realFieldItemWidthIllegal,
    buildFieldItemWidthIllegalFaultProofContracts,
  );
  const redeemerCanonicityContracts = await buildFamilyContracts(
    realRedeemerCanonicity,
    buildRedeemerCanonicityFaultProofContracts,
  );
  const fieldPreimageLengthMismatchContracts = await buildFamilyContracts(
    realFieldPreimageLengthMismatch,
    buildFieldPreimageLengthMismatchFaultProofContracts,
  );
  const witnessScriptDecodingContracts = await buildFamilyContracts(
    realWitnessScriptDecoding,
    buildWitnessScriptDecodingFaultProofContracts,
  );
  const scriptIntegrityHashMissingContracts = await buildFamilyContracts(
    realScriptIntegrityHashMissing,
    buildScriptIntegrityHashMissingFaultProofContracts,
  );
  const withdrawnInputContracts = await buildFamilyContracts(
    realWithdrawnInput,
    buildWithdrawnInputFaultProofContracts,
  );
  const withdrawalMistagContracts = await buildFamilyContracts(
    realWithdrawalMistag,
    buildWithdrawalMistagFaultProofContracts,
  );
  const doubleWithdrawContracts = await buildFamilyContracts(
    realDoubleWithdraw,
    buildDoubleWithdrawFaultProofContracts,
  );
  const crossBlockDuplicateEventContracts = await buildFamilyContracts(
    realCrossBlockDuplicateEvent,
    buildCrossBlockDuplicateEventFaultProofContracts,
  );
  const l2TxMistagContracts = await buildFamilyContracts(
    realL2TxMistag,
    buildL2TxMistagFaultProofContracts,
  );
  const valueNotPreservedContracts = await buildFamilyContracts(
    realValueNotPreserved,
    buildValueNotPreservedFaultProofContracts,
  );
  const inputSetUniquenessContracts = await buildFamilyContracts(
    realInputSetUniqueness,
    buildInputSetUniquenessFaultProofContracts,
  );
  const mintAuthorizationContracts = await buildFamilyContracts(
    realMintAuthorization,
    buildMintAuthorizationFaultProofContracts,
  );
  const distinctAssetAccumulationLimitContracts = await buildFamilyContracts(
    realDistinctAssetAccumulationLimit,
    buildDistinctAssetAccumulationLimitFaultProofContracts,
  );
  // Centrally registered proof-thread families: their lifecycle suites drive
  // the very chain the catalogue root folds, so the real chain is opt-in here
  // exactly like every other real family above.
  const mintDeclaredAssetLimitContracts = await buildFamilyContracts(
    realMintDeclaredAssetLimit,
    buildMintDeclaredAssetLimitFaultProofContracts,
  );
  const transactionOutputNonCanonicalContracts = await buildFamilyContracts(
    realTransactionOutputNonCanonical,
    buildTransactionOutputNonCanonicalFaultProofContracts,
  );
  const mintItemNonCanonicalContracts = await buildFamilyContracts(
    realMintItemNonCanonical,
    buildMintItemNonCanonicalFaultProofContracts,
  );
  const resolvedOutputNonCanonicalContracts = await buildFamilyContracts(
    realResolvedOutputNonCanonical,
    buildResolvedOutputNonCanonicalFaultProofContracts,
  );
  const spendInputSignerMissingContracts = await buildFamilyContracts(
    realSpendInputSignerMissing,
    buildSpendInputSignerMissingFaultProofContracts,
  );
  const protectedOutputSignerMissingContracts = await buildFamilyContracts(
    realProtectedOutputSignerMissing,
    buildProtectedOutputSignerMissingFaultProofContracts,
  );
  const observersForbiddenOnUntaggedNetworkContracts =
    await buildFamilyContracts(
      realObserversForbiddenOnUntaggedNetwork,
      buildObserversForbiddenOnUntaggedNetworkFaultProofContracts,
    );
  const observerOrderInvalidContracts = await buildFamilyContracts(
    realObserverOrderInvalid,
    buildObserverOrderInvalidFaultProofContracts,
  );
  const outputReferenceScriptDecodingContracts = await buildFamilyContracts(
    realOutputReferenceScriptDecoding,
    buildOutputReferenceScriptDecodingFaultProofContracts,
  );
  const executionSourceScriptDecodingContracts = await buildFamilyContracts(
    realExecutionSourceScriptDecoding,
    buildExecutionSourceScriptDecodingFaultProofContracts,
  );
  const scriptIntegrityHashMismatchContracts = await buildFamilyContracts(
    realScriptIntegrityHashMismatch,
    buildScriptIntegrityHashMismatchFaultProofContracts,
  );
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(
      withActiveOperators.activeOperators.spendingScriptAddress,
    ).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const schedulerMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "scheduler.mint.mint", [
      hubOracle.policyId,
    ]),
  );
  const scheduler: AuthenticatedValidator = {
    ...schedulerMinting,
    ...makeSpendingValidator(
      applyCompiledScript(realBlueprint, "scheduler.spend.spend", [
        withActiveOperators.registeredOperators.policyId,
        activeOperatorsAddressData,
        withActiveOperators.activeOperators.policyId,
        schedulerMinting.policyId,
        hubOracle.policyId,
      ]),
    ),
  };
  const withScheduler = {
    ...withActiveOperators,
    scheduler,
  };
  // The availability-challenge policy is a stand-in here, mirroring the sdk
  // state-queue harness: these emulator flows never open a challenge, the
  // parameter only has to be a distinct policy id.
  const availabilityChallengePolicyId = base.escapeHatch.policyId;
  const correctionLock = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "correction_lock.spend.spend", [
      hubOracle.policyId,
      availabilityChallengePolicyId,
    ]),
  );
  const stateQueueMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "state_queue.mint.mint", [
      hubOracle.policyId,
      correctionLock.spendingScriptHash,
      withScheduler.activeOperators.policyId,
      activeOperatorsAddressData,
      withScheduler.retiredOperators.policyId,
      withScheduler.scheduler.policyId,
      doubleSpendContracts.fraudProof.policyId,
      withScheduler.settlement.policyId,
      withScheduler.daAttestation.policyId,
      availabilityChallengePolicyId,
      referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
    ]),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "state_queue.spend.spend", [
      stateQueueMinting.policyId,
      withScheduler.daAttestation.policyId,
      availabilityChallengePolicyId,
    ]),
  );
  const stateQueueYields = {
    commit: makeWithdrawalValidator(
      applyCompiledScript(realBlueprint, "state_queue_yields.commit.withdraw", [
        stateQueueMinting.policyId,
        hubOracle.policyId,
        correctionLock.spendingScriptHash,
        withScheduler.activeOperators.policyId,
        activeOperatorsAddressData,
        withScheduler.scheduler.policyId,
        withScheduler.daAttestation.policyId,
      ]),
    ),
    unattestedTimeout: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_unattested.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
        ],
      ),
    ),
    unavailableTimeout: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_unavailable.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
          availabilityChallengePolicyId,
        ],
      ),
    ),
    fraudRemoval: makeWithdrawalValidator(
      applyCompiledScript(
        realBlueprint,
        "state_queue_yields.remove_fraudulent.withdraw",
        [
          stateQueueMinting.policyId,
          hubOracle.policyId,
          correctionLock.spendingScriptHash,
          withScheduler.activeOperators.policyId,
          withScheduler.retiredOperators.policyId,
          doubleSpendContracts.fraudProof.policyId,
        ],
      ),
    ),
    merge: makeWithdrawalValidator(
      applyCompiledScript(realBlueprint, "state_queue_yields.merge.withdraw", [
        stateQueueMinting.policyId,
        hubOracle.policyId,
        correctionLock.spendingScriptHash,
        withScheduler.settlement.policyId,
        withScheduler.daAttestation.policyId,
      ]),
    ),
  };
  const stateQueue = alwaysStateQueue
    ? (() => {
        const isolated = makeIsolatedAlwaysSucceedsAuthenticatedValidator();
        const yieldValidator = makeWithdrawalValidator(
          isolated.mintingScriptCBOR,
        );
        return {
          ...isolated,
          yields: {
            commit: yieldValidator,
            unattestedTimeout: yieldValidator,
            unavailableTimeout: yieldValidator,
            fraudRemoval: yieldValidator,
            merge: yieldValidator,
          },
        };
      })()
    : {
        ...stateQueueMinting,
        ...stateQueueSpending,
        yields: stateQueueYields,
      };

  // The Q39/Q40 submitters take an explicit focused contracts record. Assemble
  // it from the same parameterized chains whose step-01 hashes occupy their
  // canonical production catalogue categories.
  const fabricatedDeposit: FabricatedDepositContracts | undefined =
    fabricatedDepositContracts === undefined
      ? undefined
      : {
          steps: fabricatedDepositContracts.fabricatedDeposit.steps,
          computationThread: fabricatedDepositContracts.computationThread,
          fraudProof: fabricatedDepositContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          categoryId: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID,
        };
  const fieldPreimageCertificateMinting = makeMintingValidator(
    getCompiledScript(
      realBlueprint,
      FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    ),
  );
  const fieldPreimageCertificatePolicyId =
    fieldPreimageCertificateMinting.policyId;

  // Family adapters retain their focused test-facing records, but the scripts
  // now come from the same canonical SDK builders used for production
  // deployment. This keeps catalogue and removal hashes identical without
  // reintroducing pre-registration sidecars.
  const nativeScriptDecoding: NativeScriptDecodingContracts | undefined =
    nativeScriptDecodingContracts === undefined
      ? undefined
      : {
          steps: nativeScriptDecodingContracts.nativeScriptDecoding.steps,
          computationThread: nativeScriptDecodingContracts.computationThread,
          fraudProof: nativeScriptDecodingContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingSignature: MissingSignatureContracts | undefined =
    missingSignatureContracts === undefined
      ? undefined
      : {
          ...missingSignatureContracts.missingSignature,
          steps: missingSignatureContracts.missingSignature.steps,
          computationThread: missingSignatureContracts.computationThread,
          fraudProof: missingSignatureContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingNativeScriptTx: MissingNativeScriptTxContracts | undefined =
    missingNativeScriptTxContracts === undefined
      ? undefined
      : {
          steps: missingNativeScriptTxContracts.missingNativeScriptTx.steps,
          computationThread: missingNativeScriptTxContracts.computationThread,
          fraudProof: missingNativeScriptTxContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const missingNativeScriptUtxo: MissingNativeScriptUtxoContracts | undefined =
    missingNativeScriptUtxoContracts === undefined
      ? undefined
      : {
          steps: missingNativeScriptUtxoContracts.missingNativeScriptUtxo.steps,
          computationThread: missingNativeScriptUtxoContracts.computationThread,
          fraudProof: missingNativeScriptUtxoContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const nativeScriptInvalid: NativeScriptInvalidContracts | undefined =
    nativeScriptInvalidContracts === undefined
      ? undefined
      : {
          steps: nativeScriptInvalidContracts.nativeScriptInvalid.steps,
          computationThread: nativeScriptInvalidContracts.computationThread,
          fraudProof: nativeScriptInvalidContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const minAda: MinAdaContracts | undefined =
    minAdaContracts === undefined
      ? undefined
      : {
          steps: minAdaContracts.minAda.steps,
          yields: minAdaContracts.minAda.yields,
          computationThread: minAdaContracts.computationThread,
          fraudProof: minAdaContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
          referenceScriptAuthPolicyId:
            referenceScriptAuthPolicyId ?? base.referenceScriptAuth.policyId,
        };
  const canonicalDecodability: CanonicalDecodabilityContracts | undefined =
    canonicalDecodabilityContracts === undefined
      ? undefined
      : {
          steps: canonicalDecodabilityContracts.canonicalDecodability.steps,
          computationThread: canonicalDecodabilityContracts.computationThread,
          fraudProof: canonicalDecodabilityContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const committedFieldShape: CommittedFieldShapeContracts | undefined =
    committedFieldShapeContracts === undefined
      ? undefined
      : {
          steps: committedFieldShapeContracts.committedFieldShape.steps,
          computationThread: committedFieldShapeContracts.computationThread,
          fraudProof: committedFieldShapeContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const withdrawnReferenceInput: WithdrawnReferenceInputContracts | undefined =
    withdrawnReferenceInputContracts === undefined
      ? undefined
      : {
          steps: withdrawnReferenceInputContracts.withdrawnReferenceInput.steps,
          computationThread: withdrawnReferenceInputContracts.computationThread,
          fraudProof: withdrawnReferenceInputContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const minFee: MinFeeContracts | undefined = realMinFee
    ? {
        steps: minFeeContracts!.minFee.steps,
        computationThread: minFeeContracts!.computationThread,
        fraudProof: minFeeContracts!.fraudProof,
        hubOraclePolicyId: hubOracle.policyId,
        stateQueuePolicyId: stateQueueMinting.policyId,
        fieldPreimageCertificatePolicyId,
      }
    : undefined;
  const doubleWithdraw: DoubleWithdrawContracts | undefined =
    doubleWithdrawContracts === undefined
      ? undefined
      : {
          steps: doubleWithdrawContracts.doubleWithdraw.steps,
          computationThread: doubleWithdrawContracts.computationThread,
          fraudProof: doubleWithdrawContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const crossBlockDuplicateEvent:
    | CrossBlockDuplicateEventContracts
    | undefined =
    crossBlockDuplicateEventContracts === undefined
      ? undefined
      : {
          steps:
            crossBlockDuplicateEventContracts.crossBlockDuplicateEvent.steps,
          computationThread:
            crossBlockDuplicateEventContracts.computationThread,
          fraudProof: crossBlockDuplicateEventContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const l2TxMistag: L2TxMistagContracts | undefined =
    l2TxMistagContracts === undefined
      ? undefined
      : {
          steps: l2TxMistagContracts.l2TxMistag.steps,
          computationThread: l2TxMistagContracts.computationThread,
          fraudProof: l2TxMistagContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const withdrawnInput: WithdrawnInputContracts | undefined =
    withdrawnInputContracts === undefined
      ? undefined
      : {
          steps: withdrawnInputContracts.withdrawnInput.steps,
          computationThread: withdrawnInputContracts.computationThread,
          fraudProof: withdrawnInputContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const withdrawalMistag: WithdrawalMistagContracts | undefined =
    withdrawalMistagContracts === undefined
      ? undefined
      : {
          steps: withdrawalMistagContracts.withdrawalMistag.steps,
          computationThread: withdrawalMistagContracts.computationThread,
          fraudProof: withdrawalMistagContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const valueNotPreserved: ValueNotPreservedContracts | undefined =
    valueNotPreservedContracts === undefined
      ? undefined
      : {
          ...valueNotPreservedContracts.valueNotPreserved,
          computationThread: valueNotPreservedContracts.computationThread,
          fraudProof: valueNotPreservedContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const inputSetUniqueness: InputSetUniquenessContracts | undefined =
    inputSetUniquenessContracts === undefined
      ? undefined
      : {
          steps: inputSetUniquenessContracts.inputSetUniqueness.steps,
          computationThread: inputSetUniquenessContracts.computationThread,
          fraudProof: inputSetUniquenessContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const mintAuthorization: MintAuthorizationContracts | undefined =
    mintAuthorizationContracts === undefined
      ? undefined
      : {
          steps: mintAuthorizationContracts.mintAuthorization.steps,
          computationThread: mintAuthorizationContracts.computationThread,
          fraudProof: mintAuthorizationContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          fieldPreimageCertificatePolicyId,
        };
  const distinctAssetAccumulationLimit:
    | DistinctAssetAccumulationContracts
    | undefined =
    distinctAssetAccumulationLimitContracts === undefined
      ? undefined
      : {
          steps:
            distinctAssetAccumulationLimitContracts.distinctAssetAccumulationLimit.steps.map(
              (step, index) => ({
                ...step,
                blueprintTitle:
                  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES[index]!,
              }),
            ) as unknown as DistinctAssetAccumulationContracts["steps"],
          computationThread:
            distinctAssetAccumulationLimitContracts.computationThread,
          fraudProof: distinctAssetAccumulationLimitContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
  const fabricatedWithdrawal: FabricatedWithdrawalContracts | undefined =
    fabricatedWithdrawalContracts === undefined
      ? undefined
      : {
          steps: fabricatedWithdrawalContracts.fabricatedWithdrawal.steps,
          computationThread: fabricatedWithdrawalContracts.computationThread,
          fraudProof: fabricatedWithdrawalContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueue.policyId,
          categoryId: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
        };
  const fraudProofContracts: FaultProofContractChains = {
    ...withActiveOperators.fraudProofContracts,
    doubleSpend: doubleSpendContracts.doubleSpend,
    nonExistentInput:
      nonExistentInputContracts?.nonExistentInput ??
      withActiveOperators.fraudProofContracts.nonExistentInput,
    nonExistentInputNoIndex:
      inputNoIdxContracts?.nonExistentInputNoIndex ??
      withActiveOperators.fraudProofContracts.nonExistentInputNoIndex,
    invalidRange:
      invalidRangeContracts?.invalidRange ??
      withActiveOperators.fraudProofContracts.invalidRange,
    transitionTrace:
      transitionTraceContracts?.transitionTrace ??
      withActiveOperators.fraudProofContracts.transitionTrace,
    zeroInput:
      zeroInputContracts?.zeroInput ??
      withActiveOperators.fraudProofContracts.zeroInput,
    validationTraceDispute:
      validationTraceDisputeContracts?.validationTraceDispute ??
      withActiveOperators.fraudProofContracts.validationTraceDispute,
    daHashPreimage:
      daHashPreimageContracts?.daHashPreimage ??
      withActiveOperators.fraudProofContracts.daHashPreimage,
    noReferenceInput:
      noReferenceInputContracts?.noReferenceInput ??
      withActiveOperators.fraudProofContracts.noReferenceInput,
    referenceInputNoIdx:
      referenceInputNoIdxContracts?.referenceInputNoIdx ??
      withActiveOperators.fraudProofContracts.referenceInputNoIdx,
    invalidSignature:
      invalidSignatureContracts?.invalidSignature ??
      withActiveOperators.fraudProofContracts.invalidSignature,
    fabricatedDeposit:
      fabricatedDeposit === undefined
        ? withActiveOperators.fraudProofContracts.fabricatedDeposit
        : chainFromSteps(fabricatedDeposit.steps),
    fabricatedWithdrawal:
      fabricatedWithdrawal === undefined
        ? withActiveOperators.fraudProofContracts.fabricatedWithdrawal
        : chainFromSteps(fabricatedWithdrawal.steps),
    nativeScriptDecoding:
      nativeScriptDecoding === undefined
        ? withActiveOperators.fraudProofContracts.nativeScriptDecoding
        : chainFromSteps(nativeScriptDecoding.steps),
    missingSignature:
      missingSignature === undefined
        ? withActiveOperators.fraudProofContracts.missingSignature
        : {
            ...chainFromSteps(missingSignature.steps),
            forcedStep: makeSpendingValidator(
              missingSignature.forcedStep.spendingScript.script,
            ),
            forcedSigner: makeSpendingValidator(
              missingSignature.forcedSigner.spendingScript.script,
            ),
            forcedWitness: makeSpendingValidator(
              missingSignature.forcedWitness.spendingScript.script,
            ),
          },
    missingNativeScriptTx:
      missingNativeScriptTx === undefined
        ? withActiveOperators.fraudProofContracts.missingNativeScriptTx
        : chainFromSteps(missingNativeScriptTx.steps),
    missingNativeScriptUtxo:
      missingNativeScriptUtxo === undefined
        ? withActiveOperators.fraudProofContracts.missingNativeScriptUtxo
        : chainFromSteps(missingNativeScriptUtxo.steps),
    nativeScriptInvalid:
      nativeScriptInvalid === undefined
        ? withActiveOperators.fraudProofContracts.nativeScriptInvalid
        : chainFromSteps(nativeScriptInvalid.steps),
    minAda:
      minAda === undefined
        ? withActiveOperators.fraudProofContracts.minAda
        : { ...chainFromSteps(minAda.steps), yields: minAda.yields },
    withdrawnReferenceInput:
      withdrawnReferenceInput === undefined
        ? withActiveOperators.fraudProofContracts.withdrawnReferenceInput
        : chainFromSteps(withdrawnReferenceInput.steps),
    canonicalDecodability:
      canonicalDecodability === undefined
        ? withActiveOperators.fraudProofContracts.canonicalDecodability
        : chainFromSteps(canonicalDecodability.steps),
    committedFieldShape:
      committedFieldShape === undefined
        ? withActiveOperators.fraudProofContracts.committedFieldShape
        : chainFromSteps(committedFieldShape.steps),
    minFee:
      minFee === undefined
        ? withActiveOperators.fraudProofContracts.minFee
        : chainFromSteps(minFee.steps),
    withdrawalMistag:
      withdrawalMistag === undefined
        ? withActiveOperators.fraudProofContracts.withdrawalMistag
        : chainFromSteps(withdrawalMistag.steps),
    doubleWithdraw:
      doubleWithdraw === undefined
        ? withActiveOperators.fraudProofContracts.doubleWithdraw
        : chainFromSteps(doubleWithdraw.steps),
    crossBlockDuplicateEvent:
      crossBlockDuplicateEvent === undefined
        ? withActiveOperators.fraudProofContracts.crossBlockDuplicateEvent
        : chainFromSteps(crossBlockDuplicateEvent.steps),
    l2TxMistag:
      l2TxMistag === undefined
        ? withActiveOperators.fraudProofContracts.l2TxMistag
        : chainFromSteps(l2TxMistag.steps),
    withdrawnInput:
      withdrawnInput === undefined
        ? withActiveOperators.fraudProofContracts.withdrawnInput
        : chainFromSteps(withdrawnInput.steps),
    valueNotPreserved:
      valueNotPreservedContracts === undefined
        ? withActiveOperators.fraudProofContracts.valueNotPreserved
        : valueNotPreservedContracts.valueNotPreserved,
    inputSetUniqueness:
      inputSetUniqueness === undefined
        ? withActiveOperators.fraudProofContracts.inputSetUniqueness
        : chainFromSteps(inputSetUniqueness.steps),
    mintAuthorization:
      mintAuthorization === undefined
        ? withActiveOperators.fraudProofContracts.mintAuthorization
        : chainFromSteps(mintAuthorization.steps),
    distinctAssetAccumulationLimit:
      distinctAssetAccumulationLimit === undefined
        ? withActiveOperators.fraudProofContracts.distinctAssetAccumulationLimit
        : chainFromSteps(distinctAssetAccumulationLimit.steps),
    fieldItemWidthIllegal:
      fieldItemWidthIllegalContracts?.fieldItemWidthIllegal ??
      withActiveOperators.fraudProofContracts.fieldItemWidthIllegal,
    fieldPreimageLengthMismatch:
      fieldPreimageLengthMismatchContracts?.fieldPreimageLengthMismatch ??
      withActiveOperators.fraudProofContracts.fieldPreimageLengthMismatch,
    witnessScriptDecoding:
      witnessScriptDecodingContracts?.witnessScriptDecoding ??
      withActiveOperators.fraudProofContracts.witnessScriptDecoding,
    scriptIntegrityHashMissing:
      scriptIntegrityHashMissingContracts?.scriptIntegrityHashMissing ??
      withActiveOperators.fraudProofContracts.scriptIntegrityHashMissing,
    redeemerCanonicity:
      redeemerCanonicityContracts?.redeemerCanonicity ??
      withActiveOperators.fraudProofContracts.redeemerCanonicity,
    mintDeclaredAssetLimit:
      mintDeclaredAssetLimitContracts?.mintDeclaredAssetLimit ??
      withActiveOperators.fraudProofContracts.mintDeclaredAssetLimit,
    transactionOutputNonCanonical:
      transactionOutputNonCanonicalContracts?.transactionOutputNonCanonical ??
      withActiveOperators.fraudProofContracts.transactionOutputNonCanonical,
    mintItemNonCanonical:
      mintItemNonCanonicalContracts?.mintItemNonCanonical ??
      withActiveOperators.fraudProofContracts.mintItemNonCanonical,
    resolvedOutputNonCanonical:
      resolvedOutputNonCanonicalContracts?.resolvedOutputNonCanonical ??
      withActiveOperators.fraudProofContracts.resolvedOutputNonCanonical,
    spendInputSignerMissing:
      spendInputSignerMissingContracts?.spendInputSignerMissing ??
      withActiveOperators.fraudProofContracts.spendInputSignerMissing,
    protectedOutputSignerMissing:
      protectedOutputSignerMissingContracts?.protectedOutputSignerMissing ??
      withActiveOperators.fraudProofContracts.protectedOutputSignerMissing,
    observersForbiddenOnUntaggedNetwork:
      observersForbiddenOnUntaggedNetworkContracts?.observersForbiddenOnUntaggedNetwork ??
      withActiveOperators.fraudProofContracts
        .observersForbiddenOnUntaggedNetwork,
    observerOrderInvalid:
      observerOrderInvalidContracts?.observerOrderInvalid ??
      withActiveOperators.fraudProofContracts.observerOrderInvalid,
    outputReferenceScriptDecoding:
      outputReferenceScriptDecodingContracts?.outputReferenceScriptDecoding ??
      withActiveOperators.fraudProofContracts.outputReferenceScriptDecoding,
    executionSourceScriptDecoding:
      executionSourceScriptDecodingContracts?.executionSourceScriptDecoding ??
      withActiveOperators.fraudProofContracts.executionSourceScriptDecoding,
    scriptIntegrityHashMismatch:
      scriptIntegrityHashMismatchContracts?.scriptIntegrityHashMismatch ??
      withActiveOperators.fraudProofContracts.scriptIntegrityHashMismatch,
  };

  return {
    ...withScheduler,
    ...(fabricatedDeposit === undefined ? {} : { fabricatedDeposit }),
    ...(fabricatedWithdrawal === undefined ? {} : { fabricatedWithdrawal }),
    ...(nativeScriptDecoding === undefined ? {} : { nativeScriptDecoding }),
    ...(missingSignature === undefined ? {} : { missingSignature }),
    ...(missingNativeScriptTx === undefined ? {} : { missingNativeScriptTx }),
    ...(missingNativeScriptUtxo === undefined
      ? {}
      : { missingNativeScriptUtxo }),
    ...(nativeScriptInvalid === undefined ? {} : { nativeScriptInvalid }),
    ...(minAda === undefined ? {} : { minAda }),
    ...(canonicalDecodability === undefined ? {} : { canonicalDecodability }),
    ...(committedFieldShape === undefined ? {} : { committedFieldShape }),
    ...(withdrawnReferenceInput === undefined
      ? {}
      : { withdrawnReferenceInput }),
    ...(minFee === undefined ? {} : { minFee }),
    ...(doubleWithdraw === undefined ? {} : { doubleWithdraw }),
    ...(crossBlockDuplicateEvent === undefined
      ? {}
      : { crossBlockDuplicateEvent }),
    ...(l2TxMistag === undefined ? {} : { l2TxMistag }),
    ...(withdrawnInput === undefined ? {} : { withdrawnInput }),
    ...(withdrawalMistag === undefined ? {} : { withdrawalMistag }),
    ...(inputSetUniqueness === undefined ? {} : { inputSetUniqueness }),
    ...(valueNotPreserved === undefined ? {} : { valueNotPreserved }),
    ...(mintAuthorization === undefined ? {} : { mintAuthorization }),
    ...(distinctAssetAccumulationLimit === undefined
      ? {}
      : { distinctAssetAccumulationLimit }),
    ...(validationTraceDisputeContracts === undefined
      ? {}
      : {
          validationTraceDispute:
            validationTraceDisputeContracts.validationTraceDispute,
        }),
    cekProgramMaterial:
      validationTraceDisputeContracts?.validationTraceDispute
        .cekProgramMaterial ?? withScheduler.cekProgramMaterial,
    // Certification must mint and park the output under the same canonical
    // dual-purpose script every SDK family step is parameterized with.
    fieldPreimageCertificate: {
      ...fieldPreimageCertificateMinting,
      ...makeSpendingValidator(
        fieldPreimageCertificateMinting.mintingScriptCBOR,
      ),
    },
    correctionLock,
    stateQueue,
    // Canonical computation-thread minting validator shared by every family
    // (each family chain is parameterized with the same dual-purpose script).
    computationThread: doubleSpendContracts.computationThread,
    fraudProof: {
      ...doubleSpendContracts.fraudProof,
      policyId: doubleSpendContracts.fraudProof.policyId,
      mintingScript: doubleSpendContracts.fraudProof.mintingScript,
      mintingScriptCBOR: doubleSpendContracts.fraudProof.mintingScriptCBOR,
    },
    fraudProofContracts,
    fraudProofs: fraudProofContractsToFirstSteps(fraudProofContracts),
  };
};
