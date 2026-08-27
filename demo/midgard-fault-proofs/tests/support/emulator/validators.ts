import {
  type AuthenticatedValidator,
  type FaultProofContractChains,
  fraudProofContractsToFirstSteps,
  type MidgardValidators,
  type MintingValidator,
  type SpendingValidator as SdkSpendingValidator,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  type MintingPolicy,
  mintingPolicyToId,
  type SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  type WithdrawalValidator,
} from "@lucid-evolution/lucid";

import { type Blueprint, getCompiledScript, network } from "./blueprints.js";

type RepeatedValidatorTuple<
  Length extends number,
  Result extends readonly SdkSpendingValidator[] = readonly [],
> = Result["length"] extends Length
  ? Result
  : RepeatedValidatorTuple<Length, readonly [...Result, SdkSpendingValidator]>;

const repeatValidator = <const Length extends number>(
  validator: SdkSpendingValidator,
  length: Length,
): RepeatedValidatorTuple<Length> =>
  Array.from(
    { length },
    () => validator,
  ) as unknown as RepeatedValidatorTuple<Length>;

const scaffoldChain = <const Length extends number>(
  firstStep: SdkSpendingValidator,
  length: Length,
) => ({
  firstStep,
  steps: repeatValidator(firstStep, length),
});

export const makeMintingValidator = (
  mintingScriptCBOR: string,
): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

export const makeSpendingValidator = (
  spendingScriptCBOR: string,
): SdkSpendingValidator => {
  const spendingScript: SpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

export const makeWithdrawalValidator = (
  withdrawalScriptCBOR: string,
): SdkWithdrawalValidator => {
  const withdrawalScript: WithdrawalValidator = {
    type: "PlutusV3",
    script: withdrawalScriptCBOR,
  };
  return {
    withdrawalScriptCBOR,
    withdrawalScript,
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
  };
};

export const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeMintingValidator(mintingScriptCBOR),
  ...makeSpendingValidator(spendingScriptCBOR),
});

/**
 * A test-only, uniquely hashed `\context -> ()` Plutus V3 program. Unlike the
 * optimized always-succeeds blueprint entries, it does not alias every other
 * scaffold validator's address and policy id. That isolation matters when a
 * topology loader filters UTxOs by both address and policy.
 */
export const makeIsolatedAlwaysSucceedsAuthenticatedValidator =
  (): AuthenticatedValidator => {
    // Flat UPLC 1.1.0 `lambda (con unit ())`, wrapped once as blueprint-style
    // CBOR before Lucid adds the ledger-facing second CBOR layer.
    const isolatedCompiledCode = "450101002499";
    const script = applyDoubleCborEncoding(isolatedCompiledCode);
    return makeAuthenticatedValidator(script, script);
  };

export const alwaysTitle = (
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  category === "midgard"
    ? `${category}.${baseName}_${purpose}.else`
    : `${category}.${baseName}.else`;

export const alwaysScript = (
  blueprint: Blueprint,
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(category, baseName, purpose)),
  );

export const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, "midgard", baseName, "mint"),
    alwaysScript(blueprint, "midgard", baseName, "spend"),
  );

export const makeAlwaysSucceedsContracts = (
  blueprint: Blueprint,
): MidgardValidators => {
  const reserve = {
    ...makeSpendingValidator(
      alwaysScript(blueprint, "midgard", "reserve", "spend"),
    ),
    ...makeWithdrawalValidator(
      alwaysScript(blueprint, "midgard", "reserve", "withdraw"),
    ),
  };
  const alwaysValidationTraceDispute = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "transition_trace", "spend"),
  );
  const nonExistentInputFirstStep = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "non_existent_input", "spend"),
  );
  const nonExistentInputNoIndexFirstStep = makeSpendingValidator(
    alwaysScript(
      blueprint,
      "fraud_proofs",
      "non_existent_input_no_index",
      "spend",
    ),
  );
  const invalidRangeFirstStep = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "invalid_range", "spend"),
  );
  const transitionTraceFirstStep = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "transition_trace", "spend"),
  );
  const zeroInputFirstStep = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "zero_input", "spend"),
  );
  // The always-succeeds blueprint predates the appended production families.
  // Its full chain registry deliberately aliases one scaffold validator at
  // each canonical chain length. Focused emulator suites replace the selected
  // chain with that family's real validators.
  const appendedFamilyFallback = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "double_spend", "spend"),
  );
  const fraudProofContracts: FaultProofContractChains = {
    doubleSpend: scaffoldChain(appendedFamilyFallback, 4),
    nonExistentInput: scaffoldChain(nonExistentInputFirstStep, 4),
    nonExistentInputNoIndex: scaffoldChain(nonExistentInputNoIndexFirstStep, 4),
    invalidRange: scaffoldChain(invalidRangeFirstStep, 2),
    transitionTrace: {
      ...scaffoldChain(transitionTraceFirstStep, 9),
      route: transitionTraceFirstStep,
      finals: repeatValidator(transitionTraceFirstStep, 8),
    },
    zeroInput: scaffoldChain(zeroInputFirstStep, 2),
    validationTraceDispute: {
      ...scaffoldChain(alwaysValidationTraceDispute, 1),
      cekProgramMaterial: alwaysValidationTraceDispute,
      opener: alwaysValidationTraceDispute,
      source: alwaysValidationTraceDispute,
      game: alwaysValidationTraceDispute,
      boundary: alwaysValidationTraceDispute,
      timeout: alwaysValidationTraceDispute,
      award: alwaysValidationTraceDispute,
      proofItem: alwaysValidationTraceDispute,
      canonicalDecodeItemStages: {
        source: alwaysValidationTraceDispute,
        observe: alwaysValidationTraceDispute,
        proof: alwaysValidationTraceDispute,
        settlement: alwaysValidationTraceDispute,
      },
      scriptSourcesStageOneRedeemerStages: {
        envelope: alwaysValidationTraceDispute,
        traversalNormalizer: alwaysValidationTraceDispute,
        outerNormalizer: alwaysValidationTraceDispute,
        foldMapExecutor: alwaysValidationTraceDispute,
        finalizeFrameExecutor: alwaysValidationTraceDispute,
        settlement: alwaysValidationTraceDispute,
      },
      prepareResolvers: repeatValidator(alwaysValidationTraceDispute, 14),
      semanticResolvers: repeatValidator(alwaysValidationTraceDispute, 91),
      resolvers: repeatValidator(alwaysValidationTraceDispute, 14),
    },
    daHashPreimage: scaffoldChain(zeroInputFirstStep, 2),
    noReferenceInput: scaffoldChain(nonExistentInputFirstStep, 4),
    referenceInputNoIdx: scaffoldChain(nonExistentInputNoIndexFirstStep, 4),
    invalidSignature: scaffoldChain(invalidRangeFirstStep, 2),
    fabricatedDeposit: scaffoldChain(appendedFamilyFallback, 4),
    fabricatedWithdrawal: scaffoldChain(appendedFamilyFallback, 4),
    nativeScriptDecoding: scaffoldChain(appendedFamilyFallback, 4),
    missingSignature: scaffoldChain(appendedFamilyFallback, 4),
    missingNativeScriptTx: scaffoldChain(appendedFamilyFallback, 6),
    withdrawnReferenceInput: scaffoldChain(appendedFamilyFallback, 3),
    canonicalDecodability: scaffoldChain(appendedFamilyFallback, 2),
    committedFieldShape: scaffoldChain(appendedFamilyFallback, 2),
    minFee: scaffoldChain(appendedFamilyFallback, 2),
    withdrawalMistag: scaffoldChain(appendedFamilyFallback, 5),
    doubleWithdraw: scaffoldChain(appendedFamilyFallback, 2),
    crossBlockDuplicateEvent: scaffoldChain(appendedFamilyFallback, 2),
    l2TxMistag: scaffoldChain(appendedFamilyFallback, 2),
    withdrawnInput: scaffoldChain(appendedFamilyFallback, 3),
  };
  const fraudProofs = fraudProofContractsToFirstSteps(fraudProofContracts);
  const fieldPreimageV1 = makeSpendingValidator(
    alwaysScript(blueprint, "midgard", "state_queue", "spend"),
  );
  // #579 ruling A: this always-succeeds spend+mint pair used to stand in for the
  // retired `tx_field_receipt_v1` family. It now stands in for the §8.6
  // field-preimage certificate, which is the role the emulator set actually has
  // to fill — the tx-order mint is parameterized by the certificate policy id.
  const fieldPreimageCertificateV1 = {
    ...fieldPreimageV1,
    ...makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
  };

  return {
    referenceScriptAuth: makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
    hubOracle: {
      ...makeMintingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
      ...makeSpendingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
    },
    daParamsGovernor: alwaysAuthenticated(blueprint, "state_queue"),
    daAttestation: alwaysAuthenticated(blueprint, "state_queue"),
    stateQueue: alwaysAuthenticated(blueprint, "state_queue"),
    scheduler: alwaysAuthenticated(blueprint, "scheduler"),
    registeredOperators: alwaysAuthenticated(blueprint, "registered_operators"),
    activeOperators: alwaysAuthenticated(blueprint, "active_operators"),
    retiredOperators: alwaysAuthenticated(blueprint, "retired_operators"),
    escapeHatch: alwaysAuthenticated(blueprint, "escape_hatch"),
    fraudProofCatalogue: alwaysAuthenticated(
      blueprint,
      "fraud_proof_catalogue",
    ),
    fraudProof: alwaysAuthenticated(blueprint, "fraud_proof"),
    deposit: alwaysAuthenticated(blueprint, "deposit"),
    withdrawal: alwaysAuthenticated(blueprint, "withdrawal"),
    txOrder: alwaysAuthenticated(blueprint, "tx_order"),
    fieldPreimageCertificate: fieldPreimageCertificateV1,
    cekProgramMaterial: fieldPreimageV1,
    settlement: alwaysAuthenticated(blueprint, "settlement"),
    reserve,
    payout: alwaysAuthenticated(blueprint, "payout"),
    fraudProofContracts,
    fraudProofs,
  };
};
