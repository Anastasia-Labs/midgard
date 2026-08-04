import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationTraceTree,
  compareOutRefs,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  findOutRefIndex,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_PROTOCOL_V1_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardNativeTxFullV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildPhasMembershipRewardRegistrationTxProgram,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  completeReferenceScriptPublicationTxProgram,
  ConfirmedState,
  createReferenceScriptAuthPolicy,
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  type FraudProofs,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  makeHubOracleDatum,
  type MidgardValidators,
  type MintingValidator,
  OutputReference,
  outputReferenceFromUTxO,
  parseFaultProofBlueprint,
  parsePhasMembershipBlueprint,
  phasMembershipWithdrawalScriptFromBlueprint,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptPublicationFundingTarget,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
  ROOT_DOMAINS,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
  ScriptHashSchema,
  selectReferenceScriptFundingUtxos,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  TransitionStepSchema,
  utxoToStateQueueUTxO,
  type ValidationClaimWitnessV1,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundleV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type BuildTxWithRedeemer,
  CML,
  Constr,
  type CostModels,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type MintingPolicy,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type ProtocolParameters,
  type Script,
  scriptHashToCredential,
  type SpendingValidator,
  toUnit,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
  type WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildCountedRoot,
  encodeData,
  keyValuePhasProof,
  resolveProverSigner,
  submitRemoveFraudulentBlock,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationDisputeValidityRange,
} from "../../src/index.js";
import { submitInit } from "./legacy-submit-emulator.js";

export const moduleDir = dirname(fileURLToPath(import.meta.url));

export const repoRoot = resolve(moduleDir, "../../../..");

export const realBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

export const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

export const network: Network = "Preprod";

export type DiagnosticCardanoParameterOverrides = Pick<
  ProtocolParameters,
  | "minFeeA"
  | "minFeeB"
  | "maxValSize"
  | "maxTxExMem"
  | "maxTxExSteps"
  | "priceMem"
  | "priceStep"
  | "coinsPerUtxoByte"
  | "collateralPercentage"
  | "maxCollateralInputs"
  | "minFeeRefScriptCostPerByte"
  | "costModels"
>;

export const requireJsonRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value as Record<string, unknown>;
};

export const requireFiniteNumber = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = record[key];
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`Diagnostic Cardano parameter ${key} must be finite`);
  }
  return value;
};

export const requireNonNegativeInteger = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = requireFiniteNumber(record, key);
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const requireBigIntParameter = (
  record: Record<string, unknown>,
  key: string,
): bigint => {
  const value = record[key];
  if (
    (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) &&
    (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
  ) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative integer`,
    );
  }
  return BigInt(value);
};

export const requireCostModel = (
  costModels: Record<string, unknown>,
  version: keyof CostModels,
): number[] => {
  const value = costModels[version];
  if (
    !Array.isArray(value) ||
    value.some((entry) => typeof entry !== "number" || !Number.isFinite(entry))
  ) {
    throw new Error(
      `Diagnostic Cardano parameter cost_models.${version} must be a finite-number array`,
    );
  }
  return [...value];
};

export const loadDiagnosticCardanoParameterOverrides =
  (): Partial<DiagnosticCardanoParameterOverrides> => {
    const parameterPath =
      process.env.MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS?.trim();
    if (parameterPath == null || parameterPath.length === 0) {
      return {};
    }
    const parsed = JSON.parse(readFileSync(parameterPath, "utf8")) as unknown;
    if (!Array.isArray(parsed) || parsed.length !== 1) {
      throw new Error(
        "Diagnostic Cardano parameter snapshot must contain exactly one epoch",
      );
    }
    const parameters = requireJsonRecord(
      parsed[0],
      "Diagnostic Cardano parameter snapshot entry",
    );
    const maxTxSize = requireNonNegativeInteger(parameters, "max_tx_size");
    if (maxTxSize !== PROTOCOL_PARAMETERS_DEFAULT.maxTxSize) {
      throw new Error(
        `Diagnostic target max_tx_size must be ${PROTOCOL_PARAMETERS_DEFAULT.maxTxSize.toString()}, found ${maxTxSize.toString()}`,
      );
    }
    const costModelsJson = requireJsonRecord(
      parameters.cost_models,
      "Diagnostic Cardano parameter cost_models",
    );
    const costModels: CostModels = {
      PlutusV1: requireCostModel(costModelsJson, "PlutusV1"),
      PlutusV2: requireCostModel(costModelsJson, "PlutusV2"),
      PlutusV3: requireCostModel(costModelsJson, "PlutusV3"),
    };
    return {
      minFeeA: requireNonNegativeInteger(parameters, "min_fee_a"),
      minFeeB: requireNonNegativeInteger(parameters, "min_fee_b"),
      maxValSize: requireNonNegativeInteger(parameters, "max_val_size"),
      maxTxExMem: requireBigIntParameter(parameters, "max_tx_ex_mem"),
      maxTxExSteps: requireBigIntParameter(parameters, "max_tx_ex_steps"),
      priceMem: requireFiniteNumber(parameters, "price_mem"),
      priceStep: requireFiniteNumber(parameters, "price_step"),
      coinsPerUtxoByte: requireBigIntParameter(
        parameters,
        "coins_per_utxo_size",
      ),
      collateralPercentage: requireNonNegativeInteger(
        parameters,
        "collateral_percent",
      ),
      maxCollateralInputs: requireNonNegativeInteger(
        parameters,
        "max_collateral_inputs",
      ),
      minFeeRefScriptCostPerByte: requireFiniteNumber(
        parameters,
        "min_fee_ref_script_cost_per_byte",
      ),
      costModels,
    };
  };

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  ...loadDiagnosticCardanoParameterOverrides(),
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

export type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

export type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

export const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

export const cloneBlueprint = (blueprint: Blueprint): Blueprint =>
  JSON.parse(JSON.stringify(blueprint)) as Blueprint;

export const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): string => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return found.compiledCode;
};

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
  const fraudProofs: FraudProofs = {
    doubleSpend: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "double_spend", "spend"),
    ),
    nonExistentInput: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "non_existent_input", "spend"),
    ),
    nonExistentInputNoIndex: makeSpendingValidator(
      alwaysScript(
        blueprint,
        "fraud_proofs",
        "non_existent_input_no_index",
        "spend",
      ),
    ),
    invalidRange: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "invalid_range", "spend"),
    ),
    transitionTrace: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "transition_trace", "spend"),
    ),
    zeroInput: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "zero_input", "spend"),
    ),
    validationTraceDispute: {
      ...alwaysValidationTraceDispute,
      source: alwaysValidationTraceDispute,
      game: alwaysValidationTraceDispute,
      boundary: alwaysValidationTraceDispute,
      timeout: alwaysValidationTraceDispute,
      award: alwaysValidationTraceDispute,
    },
    // The always-succeeds devnet blueprint has no dedicated
    // `da_hash_preimage` stub, so Q44 reuses the `zero_input` stub, mirroring
    // how `validationTraceDispute` reuses `transition_trace`.
    daHashPreimage: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "zero_input", "spend"),
    ),
  };
  const fieldPreimageV1 = makeSpendingValidator(
    alwaysScript(blueprint, "midgard", "state_queue", "spend"),
  );
  const fieldReceiptV1 = {
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
    txOrderFieldPreimage: fieldPreimageV1,
    txOrderFieldReceipt: fieldReceiptV1,
    cekProgramMaterial: fieldPreimageV1,
    settlement: alwaysAuthenticated(blueprint, "settlement"),
    reserve,
    payout: alwaysAuthenticated(blueprint, "payout"),
    fraudProofs,
  };
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
    realValidationTraceDispute = false,
    alwaysFraudProofCatalogue = false,
  }: {
    readonly realNonExistentInput?: boolean;
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
    readonly realZeroInput?: boolean;
    readonly realDaHashPreimage?: boolean;
    readonly realValidationTraceDispute?: boolean;
    readonly alwaysFraudProofCatalogue?: boolean;
  } = {},
): Promise<MidgardValidators> => {
  // This integration test proves the real active-operators slashing and
  // scheduler removal path. Registered/retired operator setup remains
  // scaffolded only where needed to support the focused removal flow.
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = makeMintingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "hub_oracle.mint.mint"),
      [
        new Constr(0, [
          nonceUtxo.txHash.toLowerCase(),
          BigInt(nonceUtxo.outputIndex),
        ]),
        HUB_ORACLE_ASSET_NAME,
      ],
    ),
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
        applyParamsToScript(
          getCompiledScript(realBlueprint, "fraud_proof_catalogue.mint.mint"),
          [hubOracle.policyId],
        ),
        getCompiledScript(realBlueprint, "fraud_proof_catalogue.spend.else"),
      );
  const withCatalogue = {
    ...withHubOracle,
    fraudProofCatalogue,
  };

  const activeOperatorsMinting = makeMintingValidator(
    applyParamsToScript(
      getCompiledScript(
        realBlueprint,
        "operator_directory/active_operators.mint.mint",
      ),
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
      applyParamsToScript(
        getCompiledScript(
          realBlueprint,
          "operator_directory/active_operators.spend.spend",
        ),
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
  const nonExistentInputContracts = realNonExistentInput
    ? await Effect.runPromise(
        buildNonExistentInputFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (nonExistentInputContracts !== undefined) {
    expect(nonExistentInputContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const invalidRangeContracts = realInvalidRange
    ? await Effect.runPromise(
        buildInvalidRangeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (invalidRangeContracts !== undefined) {
    expect(invalidRangeContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const transitionTraceContracts = realTransitionTrace
    ? await Effect.runPromise(
        buildTransitionTraceFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (transitionTraceContracts !== undefined) {
    expect(transitionTraceContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const validationTraceDisputeContracts = realValidationTraceDispute
    ? await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  const zeroInputContracts = realZeroInput
    ? await Effect.runPromise(
        buildZeroInputFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (validationTraceDisputeContracts !== undefined) {
    expect(validationTraceDisputeContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  if (zeroInputContracts !== undefined) {
    expect(zeroInputContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const daHashPreimageContracts = realDaHashPreimage
    ? await Effect.runPromise(
        buildDaHashPreimageFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (daHashPreimageContracts !== undefined) {
    expect(daHashPreimageContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(
      withActiveOperators.activeOperators.spendingScriptAddress,
    ).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const schedulerMinting = makeMintingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "scheduler.mint.mint"),
      [hubOracle.policyId],
    ),
  );
  const scheduler: AuthenticatedValidator = {
    ...schedulerMinting,
    ...makeSpendingValidator(
      applyParamsToScript(
        getCompiledScript(realBlueprint, "scheduler.spend.spend"),
        [
          withActiveOperators.registeredOperators.policyId,
          activeOperatorsAddressData,
          withActiveOperators.activeOperators.policyId,
          schedulerMinting.policyId,
          hubOracle.policyId,
        ],
      ),
    ),
  };
  const withScheduler = {
    ...withActiveOperators,
    scheduler,
  };
  const stateQueueMinting = makeMintingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "state_queue.mint.mint"),
      [
        hubOracle.policyId,
        withScheduler.activeOperators.policyId,
        activeOperatorsAddressData,
        withScheduler.retiredOperators.policyId,
        withScheduler.scheduler.policyId,
        doubleSpendContracts.fraudProof.policyId,
        withScheduler.settlement.policyId,
        withScheduler.daAttestation.policyId,
      ],
    ),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "state_queue.spend.spend"),
      [stateQueueMinting.policyId, withScheduler.daAttestation.policyId],
    ),
  );

  return {
    ...withScheduler,
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

export const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export type LucidDataSchema = Parameters<typeof Data.to>[1];

export const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

export const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    "hex",
  );

export const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

export const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

export const ledgerOrderedIndex = (
  candidates: readonly UTxO[],
  target: UTxO,
  label: string,
): bigint => {
  const index = findOutRefIndex([...candidates].sort(compareOutRefs), target);
  if (index === undefined) {
    throw new Error(`Missing ${label} in candidate set`);
  }
  return BigInt(index);
};

export const alignUnixTimeToEmulatorSlotBoundary = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  unixTime: number,
): number => lucid.slotToUnixTime(lucid.unixTimeToSlot(unixTime));

export const firstWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.wallet().getUtxos();
  if (utxo === undefined) {
    throw new Error(`Expected wallet UTxO for ${label}`);
  }
  return utxo;
};

export const expectSingleUtxoWithUnit = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  address: string,
  unit: string,
): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};

export const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 2n,
  activeOperatorsRoot: 3n,
  retiredOperatorsRoot: 4n,
} as const;

export const ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX = {
  root: 0n,
  insertedNode: 1n,
} as const;

export const SCHEDULER_APPOINTMENT_OUTPUT_INDEX = {
  scheduler: 0n,
} as const;

export const h32 = (byte: string): string => byte.repeat(32);

export const deploymentManifest = (
  contracts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown> = {},
) => ({
  referenceScriptAuthPolicy,
  contracts,
});

export const makeNativeTx = ({
  spendInputCbors,
  fee,
  referenceByte,
  outputByte,
  outputCbor,
  witnessByte,
  addrTxWitsPreimageCbor,
  validityIntervalStart = MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
  readonly referenceByte?: string;
  readonly outputByte?: string;
  readonly outputCbor?: Buffer;
  readonly witnessByte?: string;
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor:
        referenceByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(referenceByte), "hex")]),
      outputsPreimageCbor:
        outputCbor !== undefined
          ? encodeCbor([outputCbor])
          : outputByte === undefined
            ? EMPTY_CBOR_LIST
            : encodeCbor([Buffer.from(h32(outputByte), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart,
      validityIntervalEnd,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor:
        addrTxWitsPreimageCbor ??
        (witnessByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(witnessByte), "hex")])),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export const buildCatalogueDeploymentInfo = async (
  fraudProofs: FraudProofs,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: categoryId(index),
        scriptHash: fraudProofs[name].spendingScriptHash,
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }

  const categoriesWithProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    categoriesWithProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }

  return {
    root: trieRootHex(trie),
    categories: categoriesWithProofs,
  };
};

export const registerPhasMembershipRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const phasMembershipScript = phasMembershipWithdrawalScriptFromBlueprint(
    parsePhasMembershipBlueprint(realBlueprint),
  );
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, {
      script: phasMembershipScript,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

export const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
  l2TransactionCount = 0n,
): HeaderV1 => {
  const hasL2Transactions = l2TransactionCount > 0n;
  const eventCommitmentRoot = hasL2Transactions
    ? transactionsRoot
    : EMPTY_MERKLE_TREE_ROOT;
  return {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
    ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
    l2TransactionCount,
    totalEventCount: l2TransactionCount,
    transitionStepCount: l2TransactionCount,
    validationTraceCount: l2TransactionCount,
    transactionsRoot,
    transitionTraceRoot: eventCommitmentRoot,
    eventToStepRoot: eventCommitmentRoot,
    validationTracesRoot: eventCommitmentRoot,
    depositsRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(now),
    endTime: BigInt(now + 1_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
  };
};

export const transitionTraceOutRef = (byte: string): OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

export const transitionTraceDaEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): [string, string] => [
  encodeData(key, keySchema).toString("hex"),
  encodeData(value, valueSchema).toString("hex"),
];

export type ForcedValidationSourceEntryV1 = NonNullable<
  ValidationClaimWitnessV1["source_membership"] extends infer Source
    ? Source extends {
        ForcedValidationSource: { membership: { value: infer V } };
      }
      ? V
      : never
    : never
>;

/**
 * Builds the block-owned roots, forced-source membership, committed claim, and
 * header for a single-forced-transaction validation-trace block from an
 * operator trace. Shared by every forced validation-dispute fixture so the
 * committed-claim shape stays identical across scenarios and only the traces
 * and transition roots vary.
 */
export const buildForcedValidationDisputeCommitments = async ({
  operatorVkey,
  now,
  txOrderId,
  eventKey,
  forcedTransaction,
  operatorTrace,
  preUtxosRoot,
  postUtxosRoot,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly txOrderId: OutputReference;
  readonly eventKey: {
    readonly ForcedTransactionEventKey: {
      readonly tx_order_id: OutputReference;
    };
  };
  readonly forcedTransaction: ForcedValidationSourceEntryV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly preUtxosRoot: string;
  readonly postUtxosRoot: string;
}): Promise<{
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
}> => {
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction" as const,
    pre_utxos_root: preUtxosRoot,
    post_utxos_root: postUtxosRoot,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction" as const,
  };
  const operatorDescriptor = validationTraceDescriptorDataFromCore(
    operatorTrace.tree.descriptor,
  );
  const forcedEntry = transitionTraceDaEntry({
    key: txOrderId,
    keySchema: OutputReference as never,
    value: forcedTransaction,
    valueSchema: ForcedInclusionTxV1Schema,
  });
  const transitionEntry = transitionTraceDaEntry({
    key: step.step_index,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: TransitionStepSchema,
  });
  const eventToStepEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: eventToStepValue,
    valueSchema: EventToStepValueSchema,
  });
  const descriptorEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: operatorDescriptor,
    valueSchema: ValidationTraceDescriptorV1Schema,
  });
  const forcedRoot = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    {
      key: Buffer.from(forcedEntry[0], "hex"),
      value: Buffer.from(forcedEntry[1], "hex"),
    },
  ]);
  const transitionRoot = await buildCountedRoot(ROOT_DOMAINS.transitionTrace, [
    {
      key: Buffer.from(transitionEntry[0], "hex"),
      value: Buffer.from(transitionEntry[1], "hex"),
    },
  ]);
  const eventToStepRoot = await buildCountedRoot(ROOT_DOMAINS.eventToStep, [
    {
      key: Buffer.from(eventToStepEntry[0], "hex"),
      value: Buffer.from(eventToStepEntry[1], "hex"),
    },
  ]);
  const descriptorRoot = await buildCountedRoot(ROOT_DOMAINS.validationTraces, [
    {
      key: Buffer.from(descriptorEntry[0], "hex"),
      value: Buffer.from(descriptorEntry[1], "hex"),
    },
  ]);
  const membership = async (
    root: typeof forcedRoot,
    entry: readonly [string, string],
  ) => ({
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: await keyValuePhasProof(
      {
        root: root.phasRoot,
        count: root.count,
        entries: root.entries,
      },
      Buffer.from(entry[0], "hex"),
      Buffer.from(entry[1], "hex"),
    ),
  });
  const claim: ValidationClaimWitnessV1 = {
    version: 1n,
    descriptor_membership: {
      ...(await membership(descriptorRoot, descriptorEntry)),
      key: eventKey,
      value: operatorDescriptor,
    },
    transition_step_membership: {
      ...(await membership(transitionRoot, transitionEntry)),
      key: 0n,
      value: step,
    },
    event_to_step_membership: {
      ...(await membership(eventToStepRoot, eventToStepEntry)),
      key: eventKey,
      value: eventToStepValue,
    },
    source_membership: {
      ForcedValidationSource: {
        membership: {
          ...(await membership(forcedRoot, forcedEntry)),
          key: txOrderId,
          value: forcedTransaction,
        },
      },
    },
    validation_context_cbor:
      operatorTrace.validationContextCbor.toString("hex"),
    initial_state: validationMachineStateDataFromCore(operatorTrace.states[0]!),
    terminal_state: validationMachineStateDataFromCore(
      operatorTrace.states.at(-1)!,
    ),
    initial_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs[0]!,
    ),
    terminal_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs.at(-1)!,
    ),
  };
  const header: HeaderV1 = {
    ...makeHeader(operatorVkey, now),
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: descriptorRoot.root,
    forcedTransactionCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return { header, claim };
};

/**
 * The empty claimed-delta commitment, i.e. `frontier_commitment(0, [])` on the
 * Aiken side. Every pre-VM-DEFECT-2 rejection fixture pinned the machine
 * state's `ledger_delta_root` to exactly this value, which is the one
 * pre-state in which the deleted `rejected_successor_is_exact` clause was
 * satisfiable.
 */
export const EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1 =
  hashMidgardValidationLedgerDeltaV1([]);

export const outRefCbor = (byte: number, index = 0n): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(Buffer.alloc(32, byte).toString("hex")),
      index,
    ).to_cbor_bytes(),
  );

export const plainOutputCbor = (lovelace: bigint): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace, assets: new Map() },
  });

/**
 * Produces a genuine non-empty claimed ledger-delta commitment through exactly
 * the reference-builder pipeline an accepted transaction uses
 * (`hashMidgardValidationLedgerDeltaV1` over authenticated delete/insert
 * operations carrying real MPF proof descriptors). The value is deliberately
 * *not* synthesised: it is the commitment a real one-input/one-output L2
 * transaction claims.
 */
export const buildNonEmptyClaimedLedgerDeltaRootV1 =
  async (): Promise<Buffer> => {
    const spent = outRefCbor(0x9c);
    const produced = outRefCbor(0x9d);
    const spentOutput = plainOutputCbor(10_000_000n);
    const producedOutput = plainOutputCbor(9_000_000n);
    const mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        { type: "delete", key: spent },
        buildValidationMachineLedgerInsertOpV1({
          key: produced,
          outputCbor: producedOutput,
        }),
      ],
    });
    return hashMidgardValidationLedgerDeltaV1(
      mutationSteps.map(({ operation, proofFoldTrace }) => ({
        ...operation,
        proofDescriptor: proofFoldTrace.descriptor,
      })),
    );
  };

export const restampTraceLedgerDeltaRoot = (
  trace: DeterministicValidationMachineTrace,
  ledgerDeltaRoot: Buffer,
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state) => ({ ...state, ledgerDeltaRoot }));
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      trace.verdict,
      states.at(-1)!.rejectionCodeHash,
    ),
  };
};

export const replaceTerminalState = (
  trace: DeterministicValidationMachineTrace,
  {
    terminal,
    verdict,
    rejectionCode,
    rejectionCodeHash,
  }: {
    readonly terminal: DeterministicValidationMachineTrace["states"][number];
    readonly verdict: "accepted" | "rejected";
    readonly rejectionCode: DeterministicValidationMachineTrace["rejectionCode"];
    readonly rejectionCodeHash: Buffer;
  },
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state, index) =>
    index === trace.states.length - 1 ? terminal : state,
  );
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      verdict,
      rejectionCodeHash,
    ),
    verdict,
    rejectionCode,
  };
};

/**
 * Work root of the exact rejecting-terminal witness the canonical V1 machine
 * requires of a rejection successor, i.e. the Aiken
 * `hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root))`.
 */
export const rejectingTerminalWorkRootV1 = ({
  programCounter,
  rejectionCode,
  priorLedgerRoot,
}: {
  readonly programCounter: number;
  readonly rejectionCode: string;
  readonly priorLedgerRoot: Buffer;
}): Buffer =>
  Buffer.from(
    hashMidgardValidationWorkWitnessV1({
      phase: "terminal",
      programCounter,
      witnessCbor: encodeCbor([
        2n,
        Buffer.from(rejectionCode, "ascii"),
        priorLedgerRoot,
        Buffer.from("80", "hex"),
      ]),
    }),
  );

export type ForcedValidationDisputeFixture = {
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly challengerDescriptor: ReturnType<
    typeof validationTraceDescriptorDataFromCore
  >;
  readonly evidence: ReturnType<typeof buildValidationDisputeEvidenceBundleV1>;
  readonly claimedLedgerDeltaRoot: Buffer;
};

/**
 * VM-DEFECT-2 regression fixture — the adversarial case the pre-fix rejection
 * surface could not express.
 *
 * The forced source carries `operator_validity: TxIsValid`, which
 * `validation-claim-v1.ak` (`forced_verdict_matches`, and the exactly
 * analogous `descriptor.verdict == Accepted` clause every *normal* L2 source
 * is held to) forces into an `Accepted` committed descriptor. The transaction
 * itself has no spend inputs, so the deterministic machine rejects it with
 * `E_EMPTY_INPUTS` at the `inputSets` instruction. The claimed ledger delta is
 * a real non-empty commitment, exactly as a real transaction's would be.
 *
 * The reference TypeScript builder refuses to emit a rejected trace with a
 * non-empty delta (`validation-machine.ts`: "a rejected transaction must
 * commit an exact ledger no-op"), so the operator's chosen claimed-delta
 * commitment is re-stamped onto every state afterwards. That is faithful:
 * `ledger_delta_root` is immutable context chosen by the operator and pinned
 * pre == post by `immutable_context_matches`, and nothing before the
 * `ledgerDelta` phase reads it.
 */
export const buildAcceptedClaimOverRejectingTransactionFixture = async ({
  operatorVkey,
  now,
  claimedLedgerDeltaRoot,
  clearChallengerTerminalDelta = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly claimedLedgerDeltaRoot: Buffer;
  readonly clearChallengerTerminalDelta?: boolean;
}): Promise<ForcedValidationDisputeFixture> => {
  const txOrderId = transitionTraceOutRef("e2");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: plainOutputCbor(100_000_000n),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(forcedSource).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    operator_validity: "TxIsValid" as const,
  };
  const honestTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.EmptyInputs,
    }),
  );
  const restamped = restampTraceLedgerDeltaRoot(
    honestTrace,
    claimedLedgerDeltaRoot,
  );
  // Counterfactual variant: the successor shape the deleted
  // `post.ledger_delta_root == frontier_commitment(0, [])` clause demanded.
  const challengerTrace = clearChallengerTerminalDelta
    ? replaceTerminalState(restamped, {
        terminal: {
          ...restamped.states.at(-1)!,
          ledgerDeltaRoot: EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
        },
        verdict: "rejected",
        rejectionCode: restamped.rejectionCode,
        rejectionCodeHash: restamped.states.at(-1)!.rejectionCodeHash,
      })
    : restamped;
  const operatorTrace = replaceTerminalState(restamped, {
    terminal: {
      ...restamped.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot,
  };
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
export const buildHonestAcceptedValidationDisputeFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedPhase: string }
> => {
  const txOrderId = transitionTraceOutRef("e3");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = CML.PrivateKey.generate_ed25519();
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8a);
  const spentOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxIdV1(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: encodeCbor([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          spendingKey,
        ).to_cbor_bytes(),
      ),
    ]),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(forcedSource).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    operator_validity: "TxIsValid" as const,
  };
  const producedOutRef = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(transactionId),
      0n,
    ).to_cbor_bytes(),
  );
  const expectedLedgerOps = [
    { type: "delete" as const, key: spentOutRef },
    buildValidationMachineLedgerInsertOpV1({
      key: producedOutRef,
      outputCbor: producedOutput,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  const preUtxosRoot = ledgerMutationSteps[0]!.preRoot.toString("hex");
  const postUtxosRoot = ledgerMutationSteps.at(-1)!.postRoot.toString("hex");
  const operatorTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: preUtxosRoot,
      postUtxosRoot,
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
  const disputedLowIndex = operatorTrace.states.findIndex(
    (state) => state.phase === "inputSets",
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      "honest accepted validation trace is missing its inputSets instruction",
    );
  }
  const preState = operatorTrace.states[disputedLowIndex]!;
  const forgedRejectionCode = RejectCodes.EmptyInputs;
  const forgedTerminal = {
    ...preState,
    phase: "terminal" as const,
    programCounter: preState.programCounter + 1,
    workRoot: rejectingTerminalWorkRootV1({
      programCounter: preState.programCounter + 1,
      rejectionCode: forgedRejectionCode,
      priorLedgerRoot: preState.priorLedgerRoot,
    }),
    verdict: "rejected" as const,
    rejectionCodeHash: Buffer.from(
      hashMidgardValidationRejectionCodeV1(forgedRejectionCode),
    ),
  };
  const challengerStates = operatorTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const challengerTrace: DeterministicValidationMachineTrace = {
    ...operatorTrace,
    states: challengerStates,
    tree: buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineStateV1),
      "rejected",
      forgedTerminal.rejectionCodeHash,
    ),
    verdict: "rejected",
    rejectionCode: forgedRejectionCode,
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase: preState.phase,
  };
};

export const runEmulatorLifecycleStage = async <T>(
  stage: string,
  operation: () => Promise<T>,
): Promise<T> => {
  try {
    return await operation();
  } catch (cause) {
    const serializedCause =
      typeof cause === "object" && cause !== null
        ? JSON.stringify(
            cause,
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          )
        : undefined;
    const detail = [
      cause instanceof Error ? (cause.stack ?? cause.message) : String(cause),
      serializedCause,
    ]
      .filter((value) => value !== undefined && value.length > 0)
      .join("\n");
    throw new Error(`emulator lifecycle stage ${stage} failed: ${detail}`);
  }
};

export type CompleteSignedTransactionMeasurement = {
  readonly completeSignedBytes: number;
  readonly l1ByteMargin: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly nativeScriptCount: number;
  readonly redeemerCount: number;
  readonly datumCount: number;
  readonly plutusV1ScriptCount: number;
  readonly plutusV2ScriptCount: number;
  readonly plutusV3ScriptCount: number;
};

export const measureCompleteSignedTransaction = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const redeemers = witnessSet.redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const exUnits = redeemers!.get(index).ex_units();
    executionMemory += exUnits.mem();
    executionSteps += exUnits.steps();
  }
  const completeSignedBytes = transactionCbor.length / 2;
  return {
    completeSignedBytes,
    l1ByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - completeSignedBytes,
    executionMemory,
    executionSteps,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    nativeScriptCount: witnessSet.native_scripts()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    datumCount: witnessSet.plutus_datums()?.len() ?? 0,
    plutusV1ScriptCount: witnessSet.plutus_v1_scripts()?.len() ?? 0,
    plutusV2ScriptCount: witnessSet.plutus_v2_scripts()?.len() ?? 0,
    plutusV3ScriptCount: witnessSet.plutus_v3_scripts()?.len() ?? 0,
  };
};

// Byte attribution for a complete signed transaction: splits the envelope
// into body/witness-set contributors and names every attached script by hash
// so an oversize transaction can be attributed to specific contributors
// rather than guessed at.
export const midgardScriptHashNames = (
  contracts: MidgardValidators,
): ReadonlyMap<string, string> =>
  new Map<string, string>([
    [contracts.stateQueue.policyId, "stateQueueMint"],
    [contracts.stateQueue.spendingScriptHash, "stateQueueSpend"],
    [contracts.activeOperators.policyId, "activeOperatorsMint"],
    [contracts.activeOperators.spendingScriptHash, "activeOperatorsSpend"],
    [contracts.retiredOperators.policyId, "retiredOperatorsMint"],
    [contracts.retiredOperators.spendingScriptHash, "retiredOperatorsSpend"],
    [contracts.registeredOperators.policyId, "registeredOperatorsMint"],
    [
      contracts.registeredOperators.spendingScriptHash,
      "registeredOperatorsSpend",
    ],
    [contracts.scheduler.policyId, "schedulerMint"],
    [contracts.scheduler.spendingScriptHash, "schedulerSpend"],
    [contracts.hubOracle.policyId, "hubOracleMint"],
    [contracts.hubOracle.spendingScriptHash, "hubOracleSpend"],
    [contracts.fraudProof.policyId, "fraudProofMint"],
    [contracts.fraudProof.spendingScriptHash, "fraudProofSpend"],
    [contracts.fraudProofCatalogue.policyId, "fraudProofCatalogueMint"],
    [
      contracts.fraudProofCatalogue.spendingScriptHash,
      "fraudProofCatalogueSpend",
    ],
    [
      contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
      "validationTraceDispute",
    ],
  ]);

export const attributeTransactionBytes = (
  label: string,
  transactionCbor: string,
  scriptNames: ReadonlyMap<string, string>,
): void => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const total = transactionCbor.length / 2;
  const lines: string[] = [
    `[tx-attribution] ${label}`,
    `  total signed bytes = ${total.toString()} (L1 limit ${PROTOCOL_PARAMETERS_DEFAULT.maxTxSize.toString()})`,
    `  body bytes         = ${body.to_cbor_bytes().length.toString()}`,
    `  witness set bytes  = ${witnessSet.to_cbor_bytes().length.toString()}`,
  ];
  // CML list wrappers expose no `to_cbor_bytes`, so list contributions are the
  // sum of their element encodings (the enclosing array header is a few bytes).
  const listBytes = <T extends { to_cbor_bytes: () => Uint8Array }>(list: {
    len: () => number;
    get: (index: number) => T;
  }): number => {
    let bytes = 0;
    for (let index = 0; index < list.len(); index += 1) {
      bytes += list.get(index).to_cbor_bytes().length;
    }
    return bytes;
  };
  const inputs = body.inputs();
  lines.push(
    `  body.inputs           n=${inputs.len().toString()} bytes=${listBytes(inputs).toString()}`,
  );
  const referenceInputs = body.reference_inputs();
  lines.push(
    `  body.reference_inputs n=${(referenceInputs?.len() ?? 0).toString()} bytes=${(referenceInputs === undefined ? 0 : listBytes(referenceInputs)).toString()}`,
  );
  const outputs = body.outputs();
  lines.push(
    `  body.outputs          n=${outputs.len().toString()} bytes=${listBytes(outputs).toString()}`,
  );
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const datum = output.datum();
    const scriptRef = output.script_ref();
    lines.push(
      `    output[${index.toString()}] bytes=${output.to_cbor_bytes().length.toString()} datum=${(datum?.to_cbor_bytes().length ?? 0).toString()} scriptRef=${(scriptRef?.to_cbor_bytes().length ?? 0).toString()}`,
    );
  }
  const mint = body.mint();
  lines.push(
    `  body.mint             policies=${(mint?.policy_count() ?? 0).toString()}`,
  );
  const vkeyWitnesses = witnessSet.vkeywitnesses();
  lines.push(
    `  witness.vkeys         n=${(vkeyWitnesses?.len() ?? 0).toString()} bytes=${(vkeyWitnesses === undefined ? 0 : listBytes(vkeyWitnesses)).toString()}`,
  );
  const redeemers = witnessSet.redeemers();
  lines.push(
    `  witness.redeemers     bytes=${(redeemers?.to_cbor_bytes().length ?? 0).toString()}`,
  );
  const flatRedeemers = redeemers?.to_flat_format();
  for (let index = 0; index < (flatRedeemers?.len() ?? 0); index += 1) {
    const redeemer = flatRedeemers!.get(index);
    lines.push(
      `    redeemer[${index.toString()}] tag=${redeemer.tag().toString()} index=${redeemer.index().toString()} dataBytes=${redeemer.data().to_cbor_bytes().length.toString()}`,
    );
  }
  const datums = witnessSet.plutus_datums();
  lines.push(
    `  witness.datums        n=${(datums?.len() ?? 0).toString()} bytes=${(datums === undefined ? 0 : listBytes(datums)).toString()}`,
  );
  const nativeScripts = witnessSet.native_scripts();
  lines.push(
    `  witness.nativeScripts n=${(nativeScripts?.len() ?? 0).toString()} bytes=${(nativeScripts === undefined ? 0 : listBytes(nativeScripts)).toString()}`,
  );
  let attachedScriptBytes = 0;
  const v3Scripts = witnessSet.plutus_v3_scripts();
  for (let index = 0; index < (v3Scripts?.len() ?? 0); index += 1) {
    const script = v3Scripts!.get(index);
    const hash = script.hash().to_hex();
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV3[${index.toString()}] ${scriptNames.get(hash) ?? "unknown"} hash=${hash} bytes=${bytes.toString()}`,
    );
  }
  const v2Scripts = witnessSet.plutus_v2_scripts();
  for (let index = 0; index < (v2Scripts?.len() ?? 0); index += 1) {
    const script = v2Scripts!.get(index);
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV2[${index.toString()}] hash=${script.hash().to_hex()} bytes=${bytes.toString()}`,
    );
  }
  lines.push(
    `  attached script bytes total = ${attachedScriptBytes.toString()}`,
    `  non-script bytes            = ${(total - attachedScriptBytes).toString()}`,
  );
  console.info(lines.join("\n"));
};

export const captureEmulatorSubmission = async <T>(
  emulator: Emulator,
  operation: () => Promise<T>,
): Promise<{
  readonly result: T;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  readonly transactionCbors: readonly string[];
}> => {
  const submit = emulator.submitTx.bind(emulator);
  let measurement: CompleteSignedTransactionMeasurement | undefined;
  const measurements: CompleteSignedTransactionMeasurement[] = [];
  const transactionCbors: string[] = [];
  emulator.submitTx = async (transaction) => {
    measurement = measureCompleteSignedTransaction(transaction);
    measurements.push(measurement);
    transactionCbors.push(transaction);
    return submit(transaction);
  };
  try {
    const result = await operation();
    if (measurement === undefined) {
      throw new Error("Expected emulator operation to submit a transaction");
    }
    return { result, measurement, measurements, transactionCbors };
  } finally {
    emulator.submitTx = submit;
  }
};

export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
}): Promise<{
  readonly fraudulentBlockOutRef: string;
  readonly headerHash: string;
  readonly stateQueueBlockUnit: string;
  readonly stateQueueRootUnit: string;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRoot: UTxO;
  readonly retiredOperatorsRootUnit: string;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  readonly registeredOperatorsRoot: UTxO;
}> => {
  const hubOracleDatum = await Effect.runPromise(makeHubOracleDatum(contracts));
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const hubOracleUnit = toUnit(
    contracts.hubOracle.policyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const fraudProofCatalogueUnit = toUnit(
    contracts.fraudProofCatalogue.policyId,
    FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  );
  const stateQueueBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const stateQueueRootUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  );
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SCHEDULER_ASSET_NAME,
  );
  const activeOperatorsRootUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  );
  const retiredOperatorsRootUnit = toUnit(
    contracts.retiredOperators.policyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  );
  const activeOperatorNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  );
  const registeredOperatorsRootUnit = toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  );
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: header.startTime,
    endTime: header.startTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  const unsigned = await lucid
    .newTx()
    .validFrom(Number(header.startTime - 120_000n))
    .validTo(Number(header.startTime + 1n))
    .collectFrom([nonceUtxo])
    .mintAssets({ [hubOracleUnit]: 1n }, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      {
        kind: "inline",
        value: Data.to(hubOracleDatum, HubOracleDatum),
      },
      { [hubOracleUnit]: 1n },
    )
    .mintAssets({ [schedulerUnit]: 1n }, Data.to("Init", SchedulerMintRedeemer))
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      { [schedulerUnit]: 1n },
    )
    // Fixed by the authored setup output order: hub oracle, scheduler,
    // state-queue root, active-operators root, retired-operators root, then
    // registered-operators root.
    .mintAssets(
      { [stateQueueRootUnit]: 1n },
      Data.to(
        { InitV1: { output_index: SETUP_OUTPUT_INDEX.stateQueueRoot } },
        StateQueueRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: Data.castTo(confirmedState, ConfirmedState),
        }),
      },
      { [stateQueueRootUnit]: 1n },
    )
    .mintAssets(
      { [activeOperatorsRootUnit]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.activeOperatorsRoot } },
        ActiveOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [activeOperatorsRootUnit]: 1n },
    )
    .mintAssets(
      { [retiredOperatorsRootUnit]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.retiredOperatorsRoot } },
        RetiredOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [retiredOperatorsRootUnit]: 1n },
    )
    .mintAssets({ [registeredOperatorsRootUnit]: 1n }, Data.void())
    .pay.ToContract(
      contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [registeredOperatorsRootUnit]: 1n },
    )
    .mintAssets({ [fraudProofCatalogueUnit]: 1n }, Data.void())
    .pay.ToAddressWithData(
      contracts.fraudProofCatalogue.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(catalogue.root, FraudProofCatalogueDatum),
      },
      { [fraudProofCatalogueUnit]: 1n },
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.fraudProofCatalogue.mintingScript)
    .attach.MintingPolicy(contracts.scheduler.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .attach.MintingPolicy(contracts.activeOperators.mintingScript)
    .attach.MintingPolicy(contracts.retiredOperators.mintingScript)
    .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.initial", async () =>
    lucid.awaitTx(await signed.submit()),
  );

  const [initialActiveOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorsRootUnit,
  );
  if (initialActiveOperatorsRoot === undefined) {
    throw new Error("Setup transaction did not produce active-operators root");
  }
  const registeredOperatorActivationUnit = toUnit(
    contracts.registeredOperators.policyId,
    "00",
  );
  const activeRootWithOperatorDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: header.operatorVkey } },
    data: "",
  });
  const activeOperatorInitialDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      { bond_unlock_time: null, inactivity_strikes: 0n },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.activeOperators.policyId,
      "test active-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          new_active_operator_key: header.operatorVkey,
          active_operator_anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          active_operator_inserted_node_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.insertedNode,
          registered_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.registeredOperators.policyId,
            "test registered-operators activation mint",
          ),
          active_operators_set_was_empty: true,
        },
      },
      ActiveOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const registeredOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.registeredOperators.policyId,
      "test registered-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          activating_operator: header.operatorVkey,
          anchor_element_input_outref: outputReferenceFromUTxO(
            initialActiveOperatorsRoot,
          ),
          anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          hub_oracle_ref_input_index: 0n,
          retired_operators_element_ref_input_index: 0n,
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.activeOperators.policyId,
            "test active-operators activation mint",
          ),
        },
      },
      RegisteredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const activationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-activation.complete",
    () =>
      lucid
        .newTx()
        .collectFrom(
          [initialActiveOperatorsRoot],
          Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
        )
        .mintAssets(
          { [activeOperatorNodeUnit]: 1n },
          activeOperatorsActivateRedeemer,
        )
        .mintAssets(
          { [registeredOperatorActivationUnit]: 1n },
          registeredOperatorsActivateRedeemer,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeRootWithOperatorDatum },
          initialActiveOperatorsRoot.assets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeOperatorInitialDatum },
          { lovelace: 20_000_000n, [activeOperatorNodeUnit]: 1n },
        )
        .attach.MintingPolicy(contracts.activeOperators.mintingScript)
        .attach.Script(contracts.activeOperators.spendingScript)
        .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
        .complete({ localUPLCEval: true }),
  );
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-activation", async () =>
    lucid.awaitTx(await activationSigned.submit()),
  );

  const [hubOracleUtxo] = await lucid.utxosAtWithUnit(
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    hubOracleUnit,
  );
  const [stateQueueRootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueRootUnit,
  );
  const [schedulerUtxo] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  const [activeOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  const [activeOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorsRootUnit,
  );
  const [retiredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.retiredOperators.spendingScriptAddress,
    retiredOperatorsRootUnit,
  );
  const [registeredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.registeredOperators.spendingScriptAddress,
    registeredOperatorsRootUnit,
  );
  if (
    hubOracleUtxo === undefined ||
    stateQueueRootUtxo === undefined ||
    schedulerUtxo === undefined ||
    activeOperatorNode === undefined ||
    activeOperatorsRoot === undefined ||
    retiredOperatorsRoot === undefined ||
    registeredOperatorsRoot === undefined
  ) {
    throw new Error(
      "Setup transaction did not produce all state-queue dependencies",
    );
  }

  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueue.policyId),
  );
  const schedulerAppointmentFeeInput = await firstWalletUtxo(
    lucid,
    "scheduler appointment fee input",
  );
  const appointmentInputs = [schedulerAppointmentFeeInput, schedulerUtxo];
  const appointmentRefs = [activeOperatorNode, registeredOperatorsRoot];
  const schedulerAppointmentRedeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: ledgerOrderedIndex(
      appointmentInputs,
      schedulerUtxo,
      "scheduler appointment input",
    ),
    scheduler_output_index: SCHEDULER_APPOINTMENT_OUTPUT_INDEX.scheduler,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          activeOperatorNode,
          "active-operator node appointment reference input",
        ),
        registered_element_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          registeredOperatorsRoot,
          "registered-operators root appointment reference input",
        ),
      },
    },
  };
  const appointmentUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-appointment.complete",
    () =>
      lucid
        .newTx()
        .collectFrom([schedulerAppointmentFeeInput])
        .collectFrom(
          [schedulerUtxo],
          Data.to(schedulerAppointmentRedeemer, SchedulerSpendRedeemer),
        )
        .readFrom(appointmentRefs)
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: Data.to(
              {
                ActiveOperator: {
                  operator: header.operatorVkey,
                  start_time: header.startTime,
                },
              },
              SchedulerDatum,
            ),
          },
          schedulerUtxo.assets,
        )
        .attach.Script(contracts.scheduler.spendingScript)
        .validFrom(Number(header.startTime - 120_000n))
        .validTo(Number(header.startTime + 1n))
        .complete({ localUPLCEval: true }),
  );
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-appointment", async () =>
    lucid.awaitTx(await appointmentSigned.submit()),
  );

  const [appointedSchedulerUtxo] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  if (appointedSchedulerUtxo === undefined) {
    throw new Error(
      "Scheduler appointment transaction did not preserve scheduler",
    );
  }
  expect(Data.from(appointedSchedulerUtxo.datum!, SchedulerDatum)).toEqual({
    ActiveOperator: {
      operator: header.operatorVkey,
      start_time: header.startTime,
    },
  });

  const commitFeeInput = await firstWalletUtxo(lucid, "commit fee input");
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: appointedSchedulerUtxo,
        additionalRefInputs: [hubOracleUtxo],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    ),
  );
  const commitUnsigned = await runEmulatorLifecycleStage(
    "setup.header-commit.complete",
    () => commitTx.complete({ localUPLCEval: true }),
  );
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.header-commit", async () =>
    lucid.awaitTx(await commitSigned.submit()),
  );

  const [fraudulentBlockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueBlockUnit,
  );
  const [continuedRootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueRootUnit,
  );
  const [continuedActiveOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  if (
    fraudulentBlockUtxo === undefined ||
    continuedRootUtxo === undefined ||
    continuedActiveOperatorNode === undefined
  ) {
    throw new Error(
      "Commit transaction did not produce the expected queue nodes",
    );
  }
  const committedBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(fraudulentBlockUtxo, contracts.stateQueue.policyId),
  );
  const committedHeader = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(committedBlock.datum),
  );
  expect(committedHeader.transactionsRoot).toBe(header.transactionsRoot);
  const continuedRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
  );
  expect(continuedRoot.datum.next).toEqual({ Key: { key: headerHash } });

  return {
    fraudulentBlockOutRef: `${fraudulentBlockUtxo.txHash}#${fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
    stateQueueBlockUnit,
    stateQueueRootUnit,
    hubOracle: hubOracleUtxo,
    scheduler: appointedSchedulerUtxo,
    activeOperatorsRoot,
    activeOperatorsRootUnit,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit,
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit,
    registeredOperatorsRoot,
  };
};

export const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE =
  "V1 validation-trace dispute";

export const validationDisputeControlPublicationTargets = (
  contracts: MidgardValidators,
) =>
  [
    {
      control: "dispute",
      name: VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE,
      script: contracts.fraudProofs.validationTraceDispute.spendingScript,
    },
    {
      control: "source",
      name: "V1 validation-trace source",
      script:
        contracts.fraudProofs.validationTraceDispute.source.spendingScript,
    },
    {
      control: "game",
      name: "V1 validation-trace game",
      script: contracts.fraudProofs.validationTraceDispute.game.spendingScript,
    },
    {
      control: "boundary",
      name: "V1 validation-trace boundary",
      script:
        contracts.fraudProofs.validationTraceDispute.boundary.spendingScript,
    },
    {
      control: "timeout",
      name: "V1 validation-trace timeout",
      script:
        contracts.fraudProofs.validationTraceDispute.timeout.spendingScript,
    },
    {
      control: "award",
      name: "V1 validation-trace award",
      script: contracts.fraudProofs.validationTraceDispute.award.spendingScript,
    },
  ] as const;

export type ValidationDisputeControlPublicationTarget = ReturnType<
  typeof validationDisputeControlPublicationTargets
>[number];

export const publishAuthenticatedValidationDisputeControl = async ({
  lucid,
  target,
  authPolicy,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly target: ValidationDisputeControlPublicationTarget;
  readonly authPolicy: ReturnType<typeof createReferenceScriptAuthPolicy>;
}) => {
  const selectedFundingInputs = selectReferenceScriptFundingUtxos(
    await lucid.wallet().getUtxos(),
    referenceScriptPublicationFundingTarget(1),
  );
  if (selectedFundingInputs.length === 0) {
    throw new Error(
      `Expected a plain-Ada input for authenticated validation-dispute ${target.control} reference-script publication`,
    );
  }
  const referenceScriptsAddress = await lucid.wallet().address();
  const { tx, layout } = await Effect.runPromise(
    completeReferenceScriptPublicationTxProgram({
      lucid,
      selectedFundingInputs,
      walletAddress: referenceScriptsAddress,
      referenceScriptsAddress,
      missingTargets: [target],
      authPolicy,
    }),
  );
  const localOutput = layout.localReferenceOutputs.get(target.name);
  if (localOutput === undefined) {
    throw new Error(
      `Authenticated publication transaction omitted the validation-dispute ${target.control} reference-script output`,
    );
  }
  const signed = await tx.sign.withWallet().complete();
  const publicationMeasurement = measureCompleteSignedTransaction(
    signed.toCBOR(),
  );
  if (publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `Authenticated validation-dispute ${target.control} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const outRef = {
    txHash,
    outputIndex: localOutput.outputIndex,
  };
  const published = await lucid.utxosByOutRef([outRef]);
  if (published.length !== 1) {
    throw new Error(
      `Expected one live validation-dispute ${target.control} reference-script UTxO at ${txHash}#${localOutput.outputIndex.toString()}, found ${published.length.toString()}`,
    );
  }
  return {
    authPolicyDeploymentInfo:
      referenceScriptAuthPolicyDeploymentInfo(authPolicy),
    publicationMeasurement,
    utxo: published[0]!,
  };
};

export const publishValidationDisputeReferenceScript = async ({
  lucid,
  contracts,
  now,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly now: number;
}) => {
  const target = validationDisputeControlPublicationTargets(contracts)[0];
  return publishAuthenticatedValidationDisputeControl({
    lucid,
    target,
    authPolicy: createReferenceScriptAuthPolicy(lucid, now),
  });
};

// Publishes a deployed validator as a plain reference-script UTxO at the
// publisher wallet address, following the hash-checked deployment
// consumption pattern (`requireDeploymentReferenceScript`); the consuming
// submit path re-derives the applied script hash and requires the published
// scriptRef to match it exactly.
export const publishPlainReferenceScriptUtxo = async ({
  lucid,
  script,
  label,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly script: Script;
  readonly label: string;
}): Promise<{
  readonly utxo: UTxO;
  readonly publicationMeasurement: CompleteSignedTransactionMeasurement;
}> => {
  // Park the reference script at an unspendable script credential so no
  // later wallet coin selection can consume the published UTxO mid-flow.
  const parkAddress = credentialToAddress(
    network,
    scriptHashToCredential("2f".repeat(28)),
  );
  const unsigned = await lucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 20_000_000n },
      script,
    )
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const publicationMeasurement = measureCompleteSignedTransaction(signedCbor);
  if (publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `${label} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(`${label} publication omitted its script-ref output`);
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const published = await lucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  if (published.length !== 1 || published[0]!.scriptRef == null) {
    throw new Error(
      `Expected one live ${label} reference-script UTxO at ${txHash}#${scriptRefOutputIndex.toString()}`,
    );
  }
  return { utxo: published[0]!, publicationMeasurement };
};

// The validators `remove-fraudulent-block` needs, in the same roster order as
// `REFERENCE_SCRIPT_NAMES` in `src/remove-fraudulent-block.ts`. Every one of
// these is also a production reference-script publication target (see
// `midgard-node/src/transactions/reference-scripts.ts`), so sourcing them from
// reference inputs is the deployed shape, not a test-only shortcut.
export type RemovalReferenceScriptName =
  | "stateQueueSpend"
  | "stateQueueMint"
  | "activeOperatorsSpend"
  | "activeOperatorsMint"
  | "retiredOperatorsSpend"
  | "retiredOperatorsMint"
  | "schedulerSpend";

export type RemovalReferenceScriptPublications = Readonly<
  Record<RemovalReferenceScriptName, UTxO>
>;

export type RemovalReferenceScriptMeasurements = Readonly<
  Record<RemovalReferenceScriptName, CompleteSignedTransactionMeasurement>
>;

export const publishRemovalReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
}): Promise<{
  readonly published: RemovalReferenceScriptPublications;
  readonly measurements: RemovalReferenceScriptMeasurements;
}> => {
  // Sequential: each publication consumes wallet UTxOs the next one selects
  // from.
  const publish = (name: RemovalReferenceScriptName, script: Script) =>
    publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `state-queue removal ${name}`,
    });
  const stateQueueSpend = await publish(
    "stateQueueSpend",
    contracts.stateQueue.spendingScript,
  );
  const stateQueueMint = await publish(
    "stateQueueMint",
    contracts.stateQueue.mintingScript,
  );
  const activeOperatorsSpend = await publish(
    "activeOperatorsSpend",
    contracts.activeOperators.spendingScript,
  );
  const activeOperatorsMint = await publish(
    "activeOperatorsMint",
    contracts.activeOperators.mintingScript,
  );
  const retiredOperatorsSpend = await publish(
    "retiredOperatorsSpend",
    contracts.retiredOperators.spendingScript,
  );
  const retiredOperatorsMint = await publish(
    "retiredOperatorsMint",
    contracts.retiredOperators.mintingScript,
  );
  const schedulerSpend = await publish(
    "schedulerSpend",
    contracts.scheduler.spendingScript,
  );
  return {
    published: {
      stateQueueSpend: stateQueueSpend.utxo,
      stateQueueMint: stateQueueMint.utxo,
      activeOperatorsSpend: activeOperatorsSpend.utxo,
      activeOperatorsMint: activeOperatorsMint.utxo,
      retiredOperatorsSpend: retiredOperatorsSpend.utxo,
      retiredOperatorsMint: retiredOperatorsMint.utxo,
      schedulerSpend: schedulerSpend.utxo,
    },
    measurements: {
      stateQueueSpend: stateQueueSpend.publicationMeasurement,
      stateQueueMint: stateQueueMint.publicationMeasurement,
      activeOperatorsSpend: activeOperatorsSpend.publicationMeasurement,
      activeOperatorsMint: activeOperatorsMint.publicationMeasurement,
      retiredOperatorsSpend: retiredOperatorsSpend.publicationMeasurement,
      retiredOperatorsMint: retiredOperatorsMint.publicationMeasurement,
      schedulerSpend: schedulerSpend.publicationMeasurement,
    },
  };
};

export const buildRemovalDeploymentInfo = (
  contracts: MidgardValidators,
  catalogue: FraudProofCatalogueDeploymentInfo,
  validationDisputePublication?: Awaited<
    ReturnType<typeof publishValidationDisputeReferenceScript>
  >,
  validationItemSemanticReference?: {
    readonly scriptHash: string;
    readonly utxo: UTxO;
  },
  removalReferenceScripts?: RemovalReferenceScriptPublications,
) => {
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

/**
 * VM-DEFECT-2 dispute-level regression
 * (`docs/exec-plans/evidence/vm-defect-decision-memo.md` §2).
 *
 * The shipped defect made `rejected_successor_is_exact` demand that the
 * rejecting terminal *write* `ledger_delta_root = frontier_commitment(0, [])`
 * while `immutable_context_matches` pins that same field pre == post on every
 * transition. The two are jointly unsatisfiable from any pre-state whose
 * claimed delta is non-empty -- which is every adversarially interesting
 * pre-state, because the challenger is the party who must exhibit a
 * one-step-valid rejection successor to win
 * (`validation-resolver-v1.ak` -> `challenger_wins_with_valid_successor`) and
 * a real invalid transaction always claims a non-empty delta.
 *
 * It shipped because no test ever drove a challenger to an actual win: every
 * rejection fixture pinned the claimed delta to the empty commitment, the one
 * pre-state in which the contradiction vanishes. These tests close that gap
 * end to end on the emulator, against the compiled validators, in both
 * directions (GOAL_SPEC §3 invariant 9 -- soundness is symmetric).
 */
export const runForcedValidationDisputeScenario = async (
  buildFixture: (input: {
    readonly operatorVkey: string;
    readonly now: number;
  }) => Promise<ForcedValidationDisputeFixture>,
  {
    stopAfter,
    onRemovalReferenceScriptPublicationAttempt,
  }: {
    readonly stopAfter?:
      | "prepare-resolution"
      | "prepare-selected"
      | "semantic-resolution";
    readonly onRemovalReferenceScriptPublicationAttempt?: () => void;
  } = {},
) => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const feeUtxoCount = 12;
  const feeUtxoLovelace = 100_000_000n;
  const emulator = new Emulator(
    [
      {
        ...operator,
        assets: {
          lovelace:
            operator.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...operator,
        assets: { lovelace: feeUtxoLovelace },
      })),
      {
        ...challenger,
        assets: {
          lovelace:
            challenger.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...challenger,
        assets: { lovelace: feeUtxoLovelace },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const operatorLucid = await Lucid(emulator, "Custom");
  const challengerLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const operatorSigner = resolveProverSigner({
    network,
    walletSeedPhrase: operator.seedPhrase,
  });
  const challengerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: challenger.seedPhrase,
  });
  const validityRange = () => validationDisputeValidityRange(emulator.now());

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const operatorPaymentCredential = getAddressDetails(
    await operatorLucid.wallet().address(),
  ).paymentCredential;
  if (
    operatorPaymentCredential === undefined ||
    operatorPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected operator wallet to expose a payment key hash");
  }
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await buildFixture({
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
  });
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const publicationSlotConfig = operatorLucid.config().slotConfig;
  if (publicationSlotConfig === undefined) {
    throw new Error(
      "Expected reference-script publisher Lucid to expose its Custom slot config",
    );
  }
  const setupProtocolParameters = emulator.protocolParameters;
  emulator.protocolParameters = {
    ...setupProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
    slotConfig: publicationSlotConfig,
  });
  referenceScriptPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
  const validationDisputePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-authenticated",
    async () => {
      try {
        return await publishValidationDisputeReferenceScript({
          lucid: referenceScriptPublisherLucid,
          contracts,
          now: emulator.now(),
        });
      } finally {
        emulator.protocolParameters = setupProtocolParameters;
      }
    },
  );
  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    validationDisputePublication,
  );
  const initResult = await runEmulatorLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    }),
  );
  const functionalProtocolParameters = emulator.protocolParameters;
  const functionalSlotConfig = challengerLucid.config().slotConfig;
  if (functionalSlotConfig === undefined) {
    throw new Error(
      "Expected functional emulator Lucid to expose its Custom slot config",
    );
  }
  emulator.protocolParameters = {
    ...functionalProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const targetOperatorLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  const targetChallengerLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  targetOperatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  targetChallengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    targetChallengerLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const openResult = await runEmulatorLifecycleStage("open", () =>
    submitValidationDisputeOpen({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      claim: fixture.claim,
      challengerDescriptor: fixture.challengerDescriptor,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  const sourceResult = await runEmulatorLifecycleStage("source", () =>
    submitValidationDisputeVerifySource({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: openResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );

  let threadOutRef = sourceResult.nextThreadOutRef;
  for (const move of fixture.evidence.moves) {
    const revealResult = await runEmulatorLifecycleStage(
      `reveal.${move.role}`,
      () =>
        submitValidationDisputeReveal({
          lucid:
            move.role === "operator"
              ? targetOperatorLucid
              : targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: move.role === "operator" ? operatorSigner : challengerSigner,
          threadOutRef,
          role: move.role,
          proof: move.proof,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
    );
    threadOutRef = revealResult.nextThreadOutRef;
  }

  const resolutionResult = await runEmulatorLifecycleStage(
    "enter-resolution",
    () =>
      submitValidationDisputeEnterResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  const { lowIndex, highIndex } = fixture.evidence.finalDispute;
  const prepareResult = await runEmulatorLifecycleStage(
    "prepare-resolution",
    () =>
      submitValidationDisputePrepareResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: resolutionResult.nextThreadOutRef,
        preState: validationMachineStateDataFromCore(
          fixture.operatorTrace.states[lowIndex]!,
        ),
        operatorPost: validationTraceProofDataFromCore(
          fixture.operatorTrace.tree.proofs[highIndex]!,
        ),
        challengerPost: validationTraceProofDataFromCore(
          fixture.challengerTrace.tree.proofs[highIndex]!,
        ),
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "prepare-resolution") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  const selectedResult = await runEmulatorLifecycleStage(
    "prepare-selected",
    () =>
      submitValidationDisputePrepareSelected({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: prepareResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "prepare-selected") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  const semanticResult = await runEmulatorLifecycleStage(
    "semantic-resolution",
    () =>
      submitValidationDisputeSemanticResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: selectedResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "semantic-resolution") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  const awardResult = await runEmulatorLifecycleStage("award", () =>
    submitValidationDisputeAward({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: semanticResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  // Block removal needs the state-queue, operator-directory and scheduler
  // validators. Publishing them as reference-script UTxOs is what the deployed
  // node does; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit the literal 16,384-byte L1 envelope, so this also
  // proves each of these validators is publishable on L1. Defer the seven
  // submissions until the route has actually reached removal so validation-only
  // and negative scenarios do not mutate the emulator first.
  const removalReferenceScriptPublications = await runEmulatorLifecycleStage(
    "reference-script.publish-removal",
    async () => {
      onRemovalReferenceScriptPublicationAttempt?.();
      const prePublicationProtocolParameters = emulator.protocolParameters;
      emulator.protocolParameters = {
        ...prePublicationProtocolParameters,
        maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      };
      try {
        return await publishRemovalReferenceScripts({
          lucid: referenceScriptPublisherLucid,
          contracts,
        });
      } finally {
        emulator.protocolParameters = prePublicationProtocolParameters;
      }
    },
  );
  const removalDeploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    validationDisputePublication,
    undefined,
    removalReferenceScriptPublications.published,
  );
  const removeNow = BigInt(emulator.now());
  // Block removal runs under the same literal 16,384-byte L1 envelope as every
  // dispute transaction above: `targetChallengerLucid` was constructed with
  // `maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`, and every validator the
  // correction needs is sourced from a published reference-script UTxO instead
  // of being attached inline. Attaching them instead costs 35,634 bytes of
  // witness set and puts the correction 2.3x over the limit.
  const removalCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage("remove-fraudulent-block", () =>
      submitRemoveFraudulentBlock({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: challengerSigner,
        fraudCategory: "validationTraceDispute",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    ),
  );
  const removal = removalCapture.result;
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
    const removalScriptNames = midgardScriptHashNames(contracts);
    removalCapture.transactionCbors.forEach((cbor, index) => {
      attributeTransactionBytes(
        `remove-fraudulent-block tx[${index.toString()}]`,
        cbor,
        removalScriptNames,
      );
    });
  }
  return {
    fixture,
    contracts,
    initResult,
    lowIndex,
    highIndex,
    awardResult,
    removal,
    removalMeasurements: removalCapture.measurements,
    removalReferenceScriptMeasurements:
      removalReferenceScriptPublications.measurements,
    challengerLucid: targetChallengerLucid,
    setup,
  };
};
