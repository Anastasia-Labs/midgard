import {
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildInvalidSignatureFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  HUB_ORACLE_ASSET_NAME,
  type MidgardValidators,
  parseFaultProofBlueprint,
  type SpendingValidator as SdkSpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type SpendingValidator,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1,
  type CrossBlockDuplicateEventContractsV1,
} from "../../../src/cross-block-duplicate-event/index.js";
import {
  NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1,
  type NativeScriptDecodingContractsV1,
} from "../../../src/native-script-decoding/contracts-v1.js";
import { type FabricatedDepositContractsV1 } from "../../../src/submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContractsV1 } from "../../../src/submit-fabricated-withdrawal-step-01.js";
import {
  applyCompiledScript,
  type Blueprint,
  cloneBlueprint,
  getCompiledScript,
  network,
} from "./blueprints.js";
import {} from "./catalogue.js";
import {
  makeAlwaysSucceedsContracts,
  makeAuthenticatedValidator,
  makeMintingValidator,
  makeSpendingValidator,
} from "./validators.js";

/**
 * Applies the four-step `native-script-decoding` chain in blueprint-declared
 * parameter order (offchain design §1; the order note lives on
 * `NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1`). Applied backwards, step 04
 * first, because each step is parameterized by its successor's script hash.
 * All four steps deploy as reference scripts (design §10 Q3): step 03's
 * 25,767-byte applied body cannot inline inside the 16,384-byte L1 fault-proof
 * envelope, so the family rides the same oversized reference-script
 * publication pattern as the semantic resolvers below.
 */
export const buildNativeScriptDecodingChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOraclePolicyId: string;
}): readonly [
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
  SdkSpendingValidator,
] => {
  const step04 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step04,
      [
        computationThreadPolicyId,
        fraudProofPolicyId,
        fraudProofTokenAddressData,
      ],
    ),
  );
  const step03 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step03,
      [
        step04.spendingScriptHash,
        computationThreadPolicyId,
        fieldPreimageCertificatePolicyId,
      ],
    ),
  );
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step02,
      [step03.spendingScriptHash, computationThreadPolicyId],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02, step03, step04];
};

export const buildCrossBlockDuplicateEventChainV1 = ({
  realBlueprint,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly realBlueprint: Blueprint;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): readonly [SdkSpendingValidator, SdkSpendingValidator] => {
  const step02 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1.step02,
      [
        fraudProofPolicyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
    ),
  );
  const step01 = makeSpendingValidator(
    applyCompiledScript(
      realBlueprint,
      CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1.step01,
      [step02.spendingScriptHash, computationThreadPolicyId, hubOraclePolicyId],
    ),
  );
  return [step01, step02];
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
    realCrossBlockDuplicateEvent = false,
    alwaysFraudProofCatalogue = false,
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
    readonly realCrossBlockDuplicateEvent?: boolean;
    readonly alwaysFraudProofCatalogue?: boolean;
  } = {},
): Promise<
  MidgardValidators & {
    readonly fabricatedDeposit?: FabricatedDepositContractsV1;
    readonly fabricatedWithdrawal?: FabricatedWithdrawalContractsV1;
    readonly nativeScriptDecoding?: NativeScriptDecodingContractsV1;
    readonly crossBlockDuplicateEvent?: CrossBlockDuplicateEventContractsV1;
  }
> => {
  // This integration test proves the real active-operators slashing and
  // scheduler removal path. Registered/retired operator setup remains
  // scaffolded only where needed to support the focused removal flow.
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = makeMintingValidator(
    applyCompiledScript(realBlueprint, "hub_oracle.mint.mint", [
      new Constr(0, [
        nonceUtxo.txHash.toLowerCase(),
        BigInt(nonceUtxo.outputIndex),
      ]),
      HUB_ORACLE_ASSET_NAME,
    ]),
  );
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
  const withHubOracle = {
    ...base,
    hubOracle: hubOracleAuth,
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

  const activeOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/active_operators.mint.mint",
      [
        hubOracle.policyId,
        withCatalogue.registeredOperators.policyId,
        withCatalogue.retiredOperators.policyId,
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
    ...withCatalogue,
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
  const stateQueueMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "state_queue.mint.mint", [
      hubOracle.policyId,
      withScheduler.activeOperators.policyId,
      activeOperatorsAddressData,
      withScheduler.retiredOperators.policyId,
      withScheduler.scheduler.policyId,
      doubleSpendContracts.fraudProof.policyId,
      withScheduler.settlement.policyId,
      withScheduler.daAttestation.policyId,
    ]),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "state_queue.spend.spend", [
      stateQueueMinting.policyId,
      withScheduler.daAttestation.policyId,
    ]),
  );

  // The two Q39/Q40 families predate their catalogue registration: production
  // deployment resolution cannot build them yet (parent-owned integration
  // work), so their submitters take an explicit contracts record. Assemble it
  // here, from the same parameterized chains whose step-01 hashes the tests
  // register as extra catalogue categories.
  const fabricatedDeposit: FabricatedDepositContractsV1 | undefined =
    fabricatedDepositContracts === undefined
      ? undefined
      : {
          steps: fabricatedDepositContracts.fabricatedDeposit.steps,
          computationThread: fabricatedDepositContracts.computationThread,
          fraudProof: fabricatedDepositContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          categoryId: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
        };
  // Same predates-registration shape for the `native-script-decoding` family
  // (#635): the chain is applied here from the double-spend family's shared
  // computation-thread and fraud-proof policies, with the harness's
  // always-succeeds field-preimage certificate stub standing in for the §8.6
  // certificate policy (#579 ruling A) — production parameterizes step 03 with
  // the real certificate policy instead.
  const nativeScriptDecoding: NativeScriptDecodingContractsV1 | undefined =
    realNativeScriptDecoding
      ? await (async () => {
          const fraudProofTokenAddressData = await Effect.runPromise(
            addressDataFromBech32(
              doubleSpendContracts.fraudProof.spendingScriptAddress,
            ).pipe(
              Effect.map((addressData) =>
                Data.from(Data.to(addressData, AddressData)),
              ),
            ),
          );
          const steps = buildNativeScriptDecodingChainV1({
            realBlueprint,
            computationThreadPolicyId:
              doubleSpendContracts.computationThread.policyId,
            fraudProofPolicyId: doubleSpendContracts.fraudProof.policyId,
            fraudProofTokenAddressData,
            fieldPreimageCertificatePolicyId:
              base.fieldPreimageCertificate.policyId,
            hubOraclePolicyId: hubOracle.policyId,
          });
          return {
            steps,
            computationThread: doubleSpendContracts.computationThread,
            fraudProof: doubleSpendContracts.fraudProof,
            hubOraclePolicyId: hubOracle.policyId,
            stateQueuePolicyId: stateQueueMinting.policyId,
            fieldPreimageCertificatePolicyId:
              base.fieldPreimageCertificate.policyId,
          };
        })()
      : undefined;
  const crossBlockDuplicateEvent:
    | CrossBlockDuplicateEventContractsV1
    | undefined = realCrossBlockDuplicateEvent
    ? await (async () => {
        const fraudProofTokenAddressData = await Effect.runPromise(
          addressDataFromBech32(
            doubleSpendContracts.fraudProof.spendingScriptAddress,
          ).pipe(
            Effect.map((addressData) =>
              Data.from(Data.to(addressData, AddressData)),
            ),
          ),
        );
        return {
          steps: buildCrossBlockDuplicateEventChainV1({
            realBlueprint,
            computationThreadPolicyId:
              doubleSpendContracts.computationThread.policyId,
            fraudProofPolicyId: doubleSpendContracts.fraudProof.policyId,
            fraudProofTokenAddressData,
            hubOraclePolicyId: hubOracle.policyId,
          }),
          computationThread: doubleSpendContracts.computationThread,
          fraudProof: doubleSpendContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
        };
      })()
    : undefined;
  const fabricatedWithdrawal: FabricatedWithdrawalContractsV1 | undefined =
    fabricatedWithdrawalContracts === undefined
      ? undefined
      : {
          steps: fabricatedWithdrawalContracts.fabricatedWithdrawal.steps,
          computationThread: fabricatedWithdrawalContracts.computationThread,
          fraudProof: fabricatedWithdrawalContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          categoryId: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
        };

  return {
    ...withScheduler,
    ...(fabricatedDeposit === undefined ? {} : { fabricatedDeposit }),
    ...(fabricatedWithdrawal === undefined ? {} : { fabricatedWithdrawal }),
    ...(nativeScriptDecoding === undefined ? {} : { nativeScriptDecoding }),
    ...(crossBlockDuplicateEvent === undefined
      ? {}
      : { crossBlockDuplicateEvent }),
    cekProgramMaterial:
      validationTraceDisputeContracts?.validationTraceDispute
        .cekProgramMaterial ?? withScheduler.cekProgramMaterial,
    stateQueue: {
      ...stateQueueMinting,
      ...stateQueueSpending,
    },
    fraudProof: {
      ...doubleSpendContracts.fraudProof,
      policyId: doubleSpendContracts.fraudProof.policyId,
      mintingScript: doubleSpendContracts.fraudProof.mintingScript,
      mintingScriptCBOR: doubleSpendContracts.fraudProof.mintingScriptCBOR,
    },
    fraudProofs: {
      ...withActiveOperators.fraudProofs,
      doubleSpend: doubleSpendContracts.doubleSpend.firstStep,
      nonExistentInput:
        nonExistentInputContracts?.nonExistentInput.firstStep ??
        withActiveOperators.fraudProofs.nonExistentInput,
      invalidRange:
        invalidRangeContracts?.invalidRange.firstStep ??
        withActiveOperators.fraudProofs.invalidRange,
      transitionTrace:
        transitionTraceContracts?.transitionTrace.firstStep ??
        withActiveOperators.fraudProofs.transitionTrace,
      zeroInput:
        zeroInputContracts?.zeroInput.firstStep ??
        withActiveOperators.fraudProofs.zeroInput,
      daHashPreimage:
        daHashPreimageContracts?.daHashPreimage.firstStep ??
        withActiveOperators.fraudProofs.daHashPreimage,
      nonExistentInputNoIndex:
        inputNoIdxContracts?.nonExistentInputNoIndex.firstStep ??
        withActiveOperators.fraudProofs.nonExistentInputNoIndex,
      noReferenceInput:
        noReferenceInputContracts?.noReferenceInput.firstStep ??
        withActiveOperators.fraudProofs.noReferenceInput,
      referenceInputNoIdx:
        referenceInputNoIdxContracts?.referenceInputNoIdx.firstStep ??
        withActiveOperators.fraudProofs.referenceInputNoIdx,
      invalidSignature:
        invalidSignatureContracts?.invalidSignature.firstStep ??
        withActiveOperators.fraudProofs.invalidSignature,
      validationTraceDispute:
        validationTraceDisputeContracts === undefined
          ? withActiveOperators.fraudProofs.validationTraceDispute
          : {
              ...validationTraceDisputeContracts.validationTraceDispute
                .firstStep,
              source:
                validationTraceDisputeContracts.validationTraceDispute.source,
              game: validationTraceDisputeContracts.validationTraceDispute.game,
              boundary:
                validationTraceDisputeContracts.validationTraceDispute.boundary,
              timeout:
                validationTraceDisputeContracts.validationTraceDispute.timeout,
              award:
                validationTraceDisputeContracts.validationTraceDispute.award,
            },
    },
  };
};
