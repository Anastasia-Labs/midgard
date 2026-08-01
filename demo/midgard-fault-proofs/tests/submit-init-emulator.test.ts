import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  buildMidgardValidationTraceTree,
  compareOutRefs,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
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
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardNativeTxFullV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildDoubleSpendFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildPhasMembershipRewardRegistrationTxProgram,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  commitCountedRootProgram,
  completeReferenceScriptPublicationTxProgram,
  ConfirmedState,
  createReferenceScriptAuthPolicy,
  DA_PAYLOAD_V1_VERSION,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  EMPTY_MERKLE_TREE_ROOT,
  EMPTY_SPEND_INPUTS_HASH,
  encodeDaPayloadV1,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  FraudProofComputationThreadStepDatum,
  type FraudProofs,
  FraudProofTokenDatum,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  headerHashFromStateQueueUTxO,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  invalidOneStepTransitionFault,
  InvalidRangeStep02Datum,
  invalidRangeViolationReason,
  makeHubOracleDatum,
  type MidgardValidators,
  type MintingValidator,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
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
  sortStateQueueUTxOs,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  TransitionStepSchema,
  utxosToStateQueueUTxOs,
  utxoToStateQueueUTxO,
  type ValidationClaimWitnessV1,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
  type WithdrawalValidator as SdkWithdrawalValidator,
  ZeroInputStep02Datum,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
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
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildInvalidForcedTransactionNoOpWitness,
  buildTransitionFaultProof,
  encodeData,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
  nativeTxFromCoreCompact,
  neSubmitStep01,
  neSubmitStep02,
  neSubmitStep03,
  neSubmitStep04,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  reconstructDaPayloadV1,
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
  submitInit,
  submitInvalidRangeStep01,
  submitInvalidRangeStep02,
  submitRemoveFraudulentBlock,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
  submitTransitionTraceProof,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationResolverIndexV1,
  submitZeroInputStep01,
  submitZeroInputStep02,
  validationDisputeValidityRange,
} from "../src/index.js";
import { buildNonMembershipProof, type TrieEntry } from "../src/ne-proofs.js";
import type { NeInputPreimageEntry } from "../src/ne-submit-step-02.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const realBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");
const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);
const network: Network = "Preprod";

type DiagnosticCardanoParameterOverrides = Pick<
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

const requireJsonRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value as Record<string, unknown>;
};

const requireFiniteNumber = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = record[key];
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`Diagnostic Cardano parameter ${key} must be finite`);
  }
  return value;
};

const requireNonNegativeInteger = (
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

const requireBigIntParameter = (
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

const requireCostModel = (
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

const loadDiagnosticCardanoParameterOverrides =
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

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  ...loadDiagnosticCardanoParameterOverrides(),
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

const cloneBlueprint = (blueprint: Blueprint): Blueprint =>
  JSON.parse(JSON.stringify(blueprint)) as Blueprint;

const getCompiledScript = (blueprint: Blueprint, title: string): string => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return found.compiledCode;
};

const makeMintingValidator = (mintingScriptCBOR: string): MintingValidator => {
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

const makeSpendingValidator = (
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

const makeWithdrawalValidator = (
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

const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeMintingValidator(mintingScriptCBOR),
  ...makeSpendingValidator(spendingScriptCBOR),
});

const alwaysTitle = (
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  category === "midgard"
    ? `${category}.${baseName}_${purpose}.else`
    : `${category}.${baseName}.else`;

const alwaysScript = (
  blueprint: Blueprint,
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(category, baseName, purpose)),
  );

const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, "midgard", baseName, "mint"),
    alwaysScript(blueprint, "midgard", baseName, "spend"),
  );

const makeAlwaysSucceedsContracts = (
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

const buildMinimalFaultProofContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
  nonceUtxo: UTxO,
  {
    realNonExistentInput = false,
    realInvalidRange = false,
    realTransitionTrace = false,
    realZeroInput = false,
    realValidationTraceDispute = false,
    alwaysFraudProofCatalogue = false,
  }: {
    readonly realNonExistentInput?: boolean;
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
    readonly realZeroInput?: boolean;
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

const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    "hex",
  );

const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

const ledgerOrderedIndex = (
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

const alignUnixTimeToEmulatorSlotBoundary = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  unixTime: number,
): number => lucid.slotToUnixTime(lucid.unixTimeToSlot(unixTime));

const firstWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.wallet().getUtxos();
  if (utxo === undefined) {
    throw new Error(`Expected wallet UTxO for ${label}`);
  }
  return utxo;
};

const expectSingleUtxoWithUnit = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  address: string,
  unit: string,
): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};

const positiveNonAdaAssets = (utxo: UTxO) =>
  Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );

const expectStateQueueHeaderOrder = async ({
  lucid,
  contracts,
  expectedHeaderHashes,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly expectedHeaderHashes: readonly string[];
}) => {
  const utxos = await lucid.utxosAt(contracts.stateQueue.spendingScriptAddress);
  const parsedStateQueueUtxos = await Effect.runPromise(
    utxosToStateQueueUTxOs(utxos, contracts.stateQueue.policyId),
  );
  expect(parsedStateQueueUtxos).toHaveLength(expectedHeaderHashes.length + 1);
  expect(
    parsedStateQueueUtxos.map(({ assetName }) => assetName).sort(),
  ).toEqual(
    [
      STATE_QUEUE_ROOT_ASSET_NAME,
      ...expectedHeaderHashes.map(
        (headerHash) => STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      ),
    ].sort(),
  );

  const sortedStateQueueUtxos = await Effect.runPromise(
    Effect.succeed(parsedStateQueueUtxos).pipe(
      Effect.andThen(sortStateQueueUTxOs),
    ),
  );
  expect(sortedStateQueueUtxos).toHaveLength(parsedStateQueueUtxos.length);
  const [root, ...blocks] = sortedStateQueueUtxos;
  if (root === undefined) {
    throw new Error("Expected state-queue topology to include the root node");
  }
  expect(root.assetName).toBe(STATE_QUEUE_ROOT_ASSET_NAME);
  expect(root.datum.key).toBe("Empty");
  expect(root.datum.next).toEqual(
    expectedHeaderHashes[0] === undefined
      ? "Empty"
      : { Key: { key: expectedHeaderHashes[0] } },
  );

  const observedHeaderHashes = await Promise.all(
    blocks.map((block) =>
      Effect.runPromise(headerHashFromStateQueueUTxO(block)),
    ),
  );
  expect(observedHeaderHashes).toEqual(expectedHeaderHashes);
  expect(new Set(observedHeaderHashes).size).toBe(observedHeaderHashes.length);

  for (let index = 0; index < blocks.length; index += 1) {
    const block = blocks[index]!;
    const expectedHeaderHash = expectedHeaderHashes[index]!;
    const nextExpectedHeaderHash = expectedHeaderHashes[index + 1];
    expect(block.datum.key).toEqual({ Key: { key: expectedHeaderHash } });
    expect(block.datum.next).toEqual(
      nextExpectedHeaderHash === undefined
        ? "Empty"
        : { Key: { key: nextExpectedHeaderHash } },
    );
  }
};

const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 2n,
  activeOperatorsRoot: 3n,
  retiredOperatorsRoot: 4n,
} as const;

const ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX = {
  root: 0n,
  insertedNode: 1n,
} as const;

const SCHEDULER_APPOINTMENT_OUTPUT_INDEX = {
  scheduler: 0n,
} as const;

const h32 = (byte: string): string => byte.repeat(32);

const deploymentManifest = (
  contracts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown> = {},
) => ({
  referenceScriptAuthPolicy,
  contracts,
});

type TestOutputReference = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

type TransactionInclusionEntry = {
  readonly inclusion: unknown;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxId: string;
  readonly spendInputCbors: readonly string[];
};

const tx1InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("a1"), outputIndex: 0n },
  { transactionId: h32("a2"), outputIndex: 1n },
];

const tx2InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("b1"), outputIndex: 0n },
  tx1InputsPreimage[1]!,
];

const outputReferenceCbor = (outRef: TestOutputReference): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(outRef.transactionId),
      outRef.outputIndex,
    ).to_cbor_bytes(),
  );

const largeFittingOutputCbor = (
  inlineDatumPayloadBytes: number = 13_600,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace: 100_000_000n, assets: new Map() },
    datum: {
      kind: "inline",
      cbor: Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          CML.PlutusData.new_bytes(
            Buffer.alloc(inlineDatumPayloadBytes, 0xa5),
          ).to_cbor_hex(),
        ),
        "hex",
      ),
    },
  });

const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

const makeNativeTx = ({
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

const compactTxEntry = (
  nativeTx: MidgardNativeTxFullV1,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxIdV1(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFullV1,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));

const buildTransactionInclusionFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly tx1: TransactionInclusionEntry;
  readonly tx2: TransactionInclusionEntry;
  readonly tx1InputsPreimage: readonly TestOutputReference[];
  readonly tx2InputsPreimage: readonly TestOutputReference[];
  readonly tx1SpendInputCbors: readonly string[];
  readonly tx2SpendInputCbors: readonly string[];
}> => {
  const tx1Native = makeNativeTx({
    spendInputCbors: tx1InputsPreimage.map(outputReferenceCbor),
    fee: 0n,
    referenceByte: "13",
    outputByte: "14",
    witnessByte: "20",
  });
  const tx2Native = makeNativeTx({
    spendInputCbors: tx2InputsPreimage.map(outputReferenceCbor),
    fee: 1n,
    referenceByte: "23",
    outputByte: "24",
    witnessByte: "30",
  });
  const tx1 = compactTxEntry(tx1Native);
  const tx2 = compactTxEntry(tx2Native);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [tx1, tx2]) {
    await trie.insert(
      Buffer.from(entry.nativeTxId, "hex"),
      Buffer.from(
        encodeMidgardNativeTxCompactV1(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ),
      ),
    );
  }
  const withProof = async (
    entry: typeof tx1,
  ): Promise<TransactionInclusionEntry> => {
    const txKey = Buffer.from(entry.nativeTxId, "hex");
    const proof = await trie.prove(txKey);
    return {
      inclusion: {
        nativeTxId: entry.nativeTxId,
        nativeTx: entry.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: entry.nativeTx,
      nativeTxId: entry.nativeTxId,
      spendInputCbors: entry.spendInputCbors,
    };
  };
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 2n,
    tx1: await withProof(tx1),
    tx2: await withProof(tx2),
    tx1InputsPreimage,
    tx2InputsPreimage,
    tx1SpendInputCbors: tx1.spendInputCbors,
    tx2SpendInputCbors: tx2.spendInputCbors,
  };
};

const buildInvalidRangeTransactionInclusionFixture = async ({
  blockValidFrom,
  blockValidTo,
}: {
  readonly blockValidFrom: bigint;
  readonly blockValidTo: bigint;
}): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
  readonly normalizedValidityRange: ReturnType<
    typeof normalizeNativeTxValidityRange
  >;
  readonly violationReason: NonNullable<
    ReturnType<typeof invalidRangeViolationReason>
  >;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [outputReferenceCbor(tx1InputsPreimage[0]!)],
    fee: 3n,
    referenceByte: "41",
    outputByte: "42",
    witnessByte: "43",
    validityIntervalStart: blockValidFrom - 1n,
    validityIntervalEnd: blockValidTo,
  });
  const badTx = compactTxEntry(badNativeTx);
  const normalizedValidityRange = normalizeNativeTxValidityRange(
    badTx.nativeTx.body,
  );
  const violationReason = invalidRangeViolationReason({
    blockValidFrom,
    blockValidTo,
    normalizedRange: normalizedValidityRange,
  });
  if (violationReason === null) {
    throw new Error(
      "Invalid-range fixture transaction does not violate block validity.",
    );
  }

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(encodeMidgardNativeTxCompactV1(badNativeTx.compact)),
  );
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          badNativeTx.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
    normalizedValidityRange,
    violationReason,
  };
};

// Zero-input fixture: a bad L2 tx that spends nothing at all, violating the
// "at least one input" ledger rule. Its `spend_inputs_hash` is the hash of the
// empty definite-length CBOR array, which is precisely the constant step-02
// compares against.
const buildZeroInputTransactionInclusionFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly badTx: TransactionInclusionEntry;
}> => {
  const badNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 5n,
    referenceByte: "51",
    outputByte: "52",
    witnessByte: "53",
  });
  const badTx = compactTxEntry(badNativeTx);

  if (
    !nativeTxBodyHasZeroInputViolation({ txBody: badTx.nativeTx.body }) ||
    badTx.spendInputCbors.length !== 0
  ) {
    throw new Error(
      "Zero-input fixture transaction does not spend an empty input list.",
    );
  }
  expect(badTx.nativeTx.body.spend_inputs_hash).toBe(EMPTY_SPEND_INPUTS_HASH);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(encodeMidgardNativeTxCompactV1(badNativeTx.compact)),
  );
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompactV1(
          badNativeTx.compact,
        ).toString("hex"),
        transactionsPhasRoot: trieRootHex(trie),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: badTx.nativeTx,
      nativeTxId: badTx.nativeTxId,
      spendInputCbors: badTx.spendInputCbors,
    },
  };
};

// Non-existent-input fixture: a bad L2 tx spends an input whose producing
// transaction never existed. The transactions trie is keyed by the raw native
// tx id (matching the node); the ledger non-membership is proven against the
// empty prev-ledger (`EMPTY_MERKLE_TREE_ROOT`, the genesis confirmed-state root
// the setup block builds on); and the phantom input's producing tx id is proven
// absent from the block's transactions.
const buildNonExistentInputFixture = async (): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  readonly badInputIndex: bigint;
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly missingInputTxId: string;
  readonly nativeTxId: string;
}> => {
  const phantomOutRef: TestOutputReference = {
    transactionId: h32("de"),
    outputIndex: 0n,
  };
  const badTxNative = makeNativeTx({
    spendInputCbors: [outputReferenceCbor(phantomOutRef)],
    fee: 0n,
    referenceByte: "e3",
    outputByte: "e4",
    witnessByte: "e5",
  });
  const badTx = compactTxEntry(badTxNative);
  const badTxCompactCbor = encodeMidgardNativeTxCompactV1(badTxNative.compact);

  // A second, well-formed L2 tx so the transactions trie is non-trivial (proofs
  // for a single-element trie are degenerate).
  const otherTxNative = makeNativeTx({
    spendInputCbors: [
      outputReferenceCbor({ transactionId: h32("c1"), outputIndex: 0n }),
    ],
    fee: 1n,
    referenceByte: "c3",
    outputByte: "c4",
    witnessByte: "c5",
  });
  const otherTx = compactTxEntry(otherTxNative);
  const otherTxCompactCbor = encodeMidgardNativeTxCompactV1(
    otherTxNative.compact,
  );

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTx.nativeTxId, "hex"),
    Buffer.from(badTxCompactCbor),
  );
  await trie.insert(
    Buffer.from(otherTx.nativeTxId, "hex"),
    Buffer.from(otherTxCompactCbor),
  );
  const transactionsRoot = trieRootHex(trie);
  const membershipProof = await trie.prove(
    Buffer.from(badTx.nativeTxId, "hex"),
  );

  const txsEntries: TrieEntry[] = [
    {
      key: Buffer.from(badTx.nativeTxId, "hex"),
      value: Buffer.from(badTxCompactCbor),
    },
    {
      key: Buffer.from(otherTx.nativeTxId, "hex"),
      value: Buffer.from(otherTxCompactCbor),
    },
  ];
  const txsNonMembershipProofCbor = await buildNonMembershipProof(
    txsEntries,
    Buffer.from(phantomOutRef.transactionId, "hex"),
  );
  const ledgerNonMembershipProofCbor = await buildNonMembershipProof(
    [],
    outputReferenceCbor(phantomOutRef),
  );

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    inclusion: parseSubmitStep01TxInclusion({
      nativeTxId: badTx.nativeTxId,
      nativeTx: badTx.nativeTx,
      nativeTxCompactCbor: badTxCompactCbor.toString("hex"),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: membershipProof.toCBOR().toString("hex"),
    }),
    inputsPreimage: [
      { txId: phantomOutRef.transactionId, index: phantomOutRef.outputIndex },
    ],
    badInputIndex: 0n,
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
    missingInputTxId: phantomOutRef.transactionId,
    nativeTxId: badTx.nativeTxId,
  };
};

const buildCatalogueDeploymentInfo = async (
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

const registerPhasMembershipRewardAccount = async (
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

const registerPexcludesExclusionRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, "pexcludes.exclusion.withdraw"),
  };
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, {
      script: pexcludesScript,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

// Commit a raw transactions MPF root the way the node does: wrap it with the
// counted-root hash under the transactions domain. Fault-proof inclusion then
// authenticates the raw root against this committed value.
const countedTransactionsRoot = (
  rawRoot: string,
  count: bigint,
): Promise<string> =>
  Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: rawRoot,
      count,
    }),
  );

const makeHeader = (
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

const transitionTraceOutRef = (byte: string): OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const transitionTraceDaEntry = <K, V>({
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

const transitionTraceRawEntry = (
  key: string,
  value: string,
): [string, string] => [key, value];

const sortedDaEntries = (
  entries: readonly [string, string][],
): [string, string][] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const buildInvalidForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    `825820${h32("01")}00`,
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const finalDescriptor = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalUtxosRoot = await keyValuePhasRootWithCount([
    {
      key: Buffer.from(finalUtxo[0], "hex"),
      value: finalDescriptor,
    },
  ]);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputByte: "b2",
    witnessByte: "b8",
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: computeMidgardNativeTxIdV1(forcedNativeTx).toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(forcedSource).toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    operator_validity: "FailedScript",
  };
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction",
    pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
    post_utxos_root: finalUtxosRoot.root,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction",
  };
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: forcedTransaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const forcedPreimageEntries = [
    transitionTraceRawEntry(
      forcedEntries[0]![0],
      forcedCanonicalCbor.toString("hex"),
    ),
  ];
  const validationTraceEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: {
        schema_version: 1n,
        machine_version: 1n,
        trace_root: h32("c1"),
        step_count: 1n,
        initial_state_hash: h32("c2"),
        terminal_state_hash: h32("c3"),
        verdict: "Rejected",
        rejection_code_hash: h32("c4"),
      },
      valueSchema: ValidationTraceDescriptorV1Schema,
    }),
  ];
  const traceEntries = [
    transitionTraceDaEntry({
      key: step.step_index,
      keySchema: Data.Integer() as never,
      value: step,
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventToStepEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: eventToStepValue,
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const forcedRoot = await buildCountedRoot(
    ROOT_DOMAINS.forcedTransactionsV1,
    forcedEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const traceRoot = await buildCountedRoot(
    ROOT_DOMAINS.transitionTrace,
    traceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const eventToStepRoot = await buildCountedRoot(
    ROOT_DOMAINS.eventToStep,
    eventToStepEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const validationTracesRoot = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    validationTraceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header: HeaderV1 = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    encodeDaPayloadV1({
      version: DA_PAYLOAD_V1_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: sortedDaEntries([finalUtxo]),
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(traceEntries),
        event_to_step: sortedDaEntries(eventToStepEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries(forcedPreimageEntries),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationTraceEntries),
        counts,
      },
    }),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayloadV1({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
  const fault = invalidOneStepTransitionFault(
    await buildInvalidForcedTransactionNoOpWitness({
      reconstruction,
      stepIndex: 0n,
    }),
  );
  return {
    header,
    headerHash,
    proof: buildTransitionFaultProof({ reconstruction, fault }),
  };
};


type ForcedValidationSourceEntryV1 = NonNullable<
  ValidationClaimWitnessV1["source_membership"] extends infer Source
    ? Source extends { ForcedValidationSource: { membership: { value: infer V } } }
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
const buildForcedValidationDisputeCommitments = async ({
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
  readonly eventKey: { readonly ForcedTransactionEventKey: { readonly tx_order_id: OutputReference } };
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

const buildInvalidForcedValidationDisputeFixture = async ({
  operatorVkey,
  now,
  inlineDatumPayloadBytes = 13_600,
  minimumCompleteItemBytes = MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly inlineDatumPayloadBytes?: number;
  readonly minimumCompleteItemBytes?: number;
}) => {
  const txOrderId = transitionTraceOutRef("e1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: largeFittingOutputCbor(inlineDatumPayloadBytes),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const decodedForcedNativeTx =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(forcedCanonicalCbor);
  if (
    decodeMidgardNativeByteListPreimage(
      decodedForcedNativeTx.witnessSet.addrTxWitsPreimageCbor,
      "test.forced_native_tx.addr_tx_wits",
    ).length !== 0
  ) {
    throw new Error(
      "forced validation-dispute fixture unexpectedly contains vkey witnesses",
    );
  }
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
  const challengerTrace = await Effect.runPromise(
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
  const disputedWitnessIndex = challengerTrace.witnesses.findIndex(
    (witness) =>
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.itemCbor.length > minimumCompleteItemBytes,
  );
  if (disputedWitnessIndex < 0) {
    throw new Error(
      "validation-dispute fixture is missing its selected fitting complete item",
    );
  }
  const completeItemWitness = challengerTrace.witnesses[disputedWitnessIndex]!;
  if (completeItemWitness.auxiliary?.kind !== "transactionFieldItem") {
    throw new Error(
      "validation-dispute fixture selected a non-item canonical witness",
    );
  }
  const completeItemBytes = completeItemWitness.auxiliary.itemCbor.length;
  const operatorRejectionCodeHash = Buffer.alloc(32);
  const operatorStates = challengerTrace.states.map((state, index) =>
    index >= disputedWitnessIndex + 1
      ? {
          ...state,
          workRoot: Buffer.alloc(32, 0x7e),
          ...(index === challengerTrace.states.length - 1
            ? {
                verdict: "accepted" as const,
                rejectionCodeHash: operatorRejectionCodeHash,
              }
            : {}),
        }
      : state,
  );
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineStateV1),
      "accepted",
      operatorRejectionCodeHash,
    ),
    verdict: "accepted",
    rejectionCode: null,
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
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    challengerTrace.tree.descriptor,
  );
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor,
    evidence,
    completeItemBytes,
  };
};

/**
 * The empty claimed-delta commitment, i.e. `frontier_commitment(0, [])` on the
 * Aiken side. Every pre-VM-DEFECT-2 rejection fixture pinned the machine
 * state's `ledger_delta_root` to exactly this value, which is the one
 * pre-state in which the deleted `rejected_successor_is_exact` clause was
 * satisfiable.
 */
const EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1 = hashMidgardValidationLedgerDeltaV1(
  [],
);

const outRefCbor = (byte: number, index = 0n): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(Buffer.alloc(32, byte).toString("hex")),
      index,
    ).to_cbor_bytes(),
  );

const plainOutputCbor = (lovelace: bigint): Buffer =>
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
const buildNonEmptyClaimedLedgerDeltaRootV1 = async (): Promise<Buffer> => {
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

const restampTraceLedgerDeltaRoot = (
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

const replaceTerminalState = (
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
const rejectingTerminalWorkRootV1 = ({
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

type ForcedValidationDisputeFixture = {
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
const buildAcceptedClaimOverRejectingTransactionFixture = async ({
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
const buildHonestAcceptedValidationDisputeFixture = async ({
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

const runEmulatorLifecycleStage = async <T>(
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

type CompleteSignedTransactionMeasurement = {
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

const measureCompleteSignedTransaction = (
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
const midgardScriptHashNames = (
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

const attributeTransactionBytes = (
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

const captureEmulatorSubmission = async <T>(
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

const submitSetupTx = async ({
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

const submitSuccessorBlockTx = async ({
  lucid,
  contracts,
  anchorBlockUnit,
  header,
  hubOracle,
  scheduler,
  activeOperatorNode,
  activeOperatorNodeUnit,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly anchorBlockUnit: string;
  readonly header: HeaderV1;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
}): Promise<{
  readonly continuedAnchorOutRef: string;
  readonly successorOutRef: string;
  readonly successorHeaderHash: string;
  readonly successorBlockUnit: string;
  readonly activeOperatorNode: UTxO;
}> => {
  const [anchorBlockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  if (anchorBlockUtxo === undefined) {
    throw new Error("Expected live state-queue anchor block for successor");
  }
  const anchorBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(anchorBlockUtxo, contracts.stateQueue.policyId),
  );
  const successorHeaderHash = await Effect.runPromise(
    hashBlockHeaderV1(header),
  );
  const successorBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHeaderHash,
  );
  const commitFeeInput = await firstWalletUtxo(
    lucid,
    "successor commit fee input",
  );
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
            "successor commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "successor commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracle,
            "successor commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "successor commit state-queue mint redeemer",
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
        anchorUTxO: anchorBlock,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: scheduler,
        additionalRefInputs: [hubOracle],
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
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const [continuedAnchorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  const [successorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    successorBlockUnit,
  );
  const [continuedActiveOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  if (
    continuedAnchorUtxo === undefined ||
    successorUtxo === undefined ||
    continuedActiveOperatorNode === undefined
  ) {
    throw new Error("Successor commit did not preserve expected queue nodes");
  }
  const continuedAnchor = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedAnchorUtxo, contracts.stateQueue.policyId),
  );
  await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(continuedAnchor.datum),
  );
  expect(continuedAnchor.datum.next).toEqual({
    Key: { key: successorHeaderHash },
  });

  return {
    continuedAnchorOutRef: outRefLabel(continuedAnchorUtxo),
    successorOutRef: outRefLabel(successorUtxo),
    successorHeaderHash,
    successorBlockUnit,
    activeOperatorNode: continuedActiveOperatorNode,
  };
};

const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE = "V1 validation-trace dispute";

const validationDisputeControlPublicationTargets = (
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

type ValidationDisputeControlPublicationTarget = ReturnType<
  typeof validationDisputeControlPublicationTargets
>[number];

const publishAuthenticatedValidationDisputeControl = async ({
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

const publishValidationDisputeReferenceScript = async ({
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
const publishPlainReferenceScriptUtxo = async ({
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
type RemovalReferenceScriptName =
  | "stateQueueSpend"
  | "stateQueueMint"
  | "activeOperatorsSpend"
  | "activeOperatorsMint"
  | "retiredOperatorsSpend"
  | "retiredOperatorsMint"
  | "schedulerSpend";

type RemovalReferenceScriptPublications = Readonly<
  Record<RemovalReferenceScriptName, UTxO>
>;

type RemovalReferenceScriptMeasurements = Readonly<
  Record<RemovalReferenceScriptName, CompleteSignedTransactionMeasurement>
>;

const publishRemovalReferenceScripts = async ({
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

const buildRemovalDeploymentInfo = (
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

type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: HeaderV1;
};

type ProvedDoubleSpendFixture = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly contracts: MidgardValidators;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly transactionInclusion: Awaited<
    ReturnType<typeof buildTransactionInclusionFixture>
  >;
  readonly fraudulentHeader: HeaderV1;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly successors: readonly SuccessorBlockFixture[];
  readonly deploymentInfo: ReturnType<typeof buildRemovalDeploymentInfo>;
  readonly fraudulentBlockOutRef: string;
  readonly submitInitResult: Awaited<ReturnType<typeof submitInit>>;
  readonly step04Result: Awaited<ReturnType<typeof submitStep04>>;
  readonly fraudProofUtxo: UTxO;
  readonly proverPaymentKeyHash: string;
};

type RemovalEvent =
  | { readonly kind: "stateQueue.utxosAt"; readonly call: number }
  | { readonly kind: "scheduler.utxosAtWithUnit"; readonly call: number }
  | { readonly kind: "awaitTx"; readonly txHash: string }
  | { readonly kind: "lease.acquire" }
  | { readonly kind: "lease.renew"; readonly call: number }
  | { readonly kind: "lease.release" }
  | { readonly kind: "lease.fail"; readonly error: string };

const eventIndexes = (
  events: readonly RemovalEvent[],
  kind: RemovalEvent["kind"],
): number[] =>
  events.flatMap((event, index) => (event.kind === kind ? [index] : []));

const createRecordingLeaseCoordinator = (
  events: RemovalEvent[],
): StateQueueMutationLeaseCoordinator => {
  let renewCalls = 0;
  return {
    acquire: async () => {
      events.push({ kind: "lease.acquire" });
      return {
        token: "emulator-fault-proof-removal",
        source: "emulator",
        renew: async () => {
          renewCalls += 1;
          events.push({ kind: "lease.renew", call: renewCalls });
        },
        release: async () => {
          events.push({ kind: "lease.release" });
        },
        fail: async (error: string) => {
          events.push({ kind: "lease.fail", error });
        },
      };
    },
  };
};

const instrumentLucidForRemoval = ({
  lucid,
  contracts,
  events,
  failStateQueueUtxosAtCall,
  failSchedulerUtxosAtWithUnitCall,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly events: RemovalEvent[];
  readonly failStateQueueUtxosAtCall?: number;
  readonly failSchedulerUtxosAtWithUnitCall?: number;
}): Awaited<ReturnType<typeof Lucid>> => {
  let stateQueueUtxosAtCalls = 0;
  let schedulerUtxosAtWithUnitCalls = 0;
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SCHEDULER_ASSET_NAME,
  );
  return new Proxy(lucid, {
    get(target, property, receiver) {
      if (property === "utxosAt") {
        return async (address: string, ...rest: unknown[]) => {
          if (address === contracts.stateQueue.spendingScriptAddress) {
            stateQueueUtxosAtCalls += 1;
            events.push({
              kind: "stateQueue.utxosAt",
              call: stateQueueUtxosAtCalls,
            });
            if (stateQueueUtxosAtCalls === failStateQueueUtxosAtCall) {
              throw new Error("instrumented state-queue topology load failure");
            }
          }
          return await target.utxosAt(address, ...(rest as []));
        };
      }
      if (property === "utxosAtWithUnit") {
        return async (address: string, unit: string, ...rest: unknown[]) => {
          if (
            address === contracts.scheduler.spendingScriptAddress &&
            unit === schedulerUnit
          ) {
            schedulerUtxosAtWithUnitCalls += 1;
            events.push({
              kind: "scheduler.utxosAtWithUnit",
              call: schedulerUtxosAtWithUnitCalls,
            });
            if (
              schedulerUtxosAtWithUnitCalls === failSchedulerUtxosAtWithUnitCall
            ) {
              throw new Error("instrumented scheduler lookup failure");
            }
          }
          return await target.utxosAtWithUnit(address, unit, ...(rest as []));
        };
      }
      if (property === "awaitTx") {
        return async (txHash: string, ...rest: unknown[]) => {
          events.push({ kind: "awaitTx", txHash });
          return await target.awaitTx(txHash, ...(rest as []));
        };
      }
      const value = Reflect.get(target, property, receiver);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
};

const buildProvedDoubleSpendFixture = async ({
  successorCount = 0,
}: {
  readonly successorCount?: number;
} = {}): Promise<ProvedDoubleSpendFixture> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }

  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const transactionInclusion = await buildTransactionInclusionFixture();
  // Removal needs the state-queue, operator-directory and scheduler validators.
  // Publishing them as reference-script UTxOs is what the deployed node does and
  // is what keeps the removal transaction inside the literal 16,384-byte L1
  // envelope; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit that envelope. Published from the prover wallet before
  // the header clock is sampled so the funder's nonce UTxO survives and the
  // whole fixture timeline shifts uniformly.
  const removalReferenceScriptPublications =
    await publishRemovalReferenceScripts({
      lucid: proverLucid,
      contracts,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderPaymentCredential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (
    funderPaymentCredential === undefined ||
    funderPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  const fraudulentHeader = makeHeader(
    funderPaymentCredential.hash,
    headerStartTime,
    await countedTransactionsRoot(
      transactionInclusion.transactionsRoot,
      transactionInclusion.l2TransactionCount,
    ),
    transactionInclusion.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: fraudulentHeader,
  });
  const { headerHash } = setup;

  const successors: SuccessorBlockFixture[] = [];
  let anchorBlockUnit = setup.stateQueueBlockUnit;
  let activeOperatorNode = setup.activeOperatorNode;
  let previousHeader = fraudulentHeader;
  let previousHeaderHash = headerHash;
  for (let index = 0; index < successorCount; index += 1) {
    const successorHeader = {
      ...makeHeader(
        funderPaymentCredential.hash,
        Number(previousHeader.endTime),
        EMPTY_MERKLE_TREE_ROOT,
      ),
      prevHeaderHash: previousHeaderHash,
    };
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      contracts,
      anchorBlockUnit,
      header: successorHeader,
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    successors.push({ ...successor, header: successorHeader });
    anchorBlockUnit = successor.successorBlockUnit;
    activeOperatorNode = successor.activeOperatorNode;
    previousHeader = successorHeader;
    previousHeaderHash = successor.successorHeaderHash;
  }

  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [
      headerHash,
      ...successors.map((successor) => successor.successorHeaderHash),
    ],
  });

  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    undefined,
    undefined,
    removalReferenceScriptPublications.published,
  );
  const fraudulentBlockOutRef =
    successors[0]?.continuedAnchorOutRef ?? setup.fraudulentBlockOutRef;

  const submitInitResult = await submitInit({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudulentBlockOutRef,
    awaitConfirmation: true,
  });

  expect(submitInitResult.txHash).toHaveLength(64);
  expect(submitInitResult.fraudulentHeaderHash).toBe(headerHash);
  expect(submitInitResult.computationThreadAssetName).toBe(
    `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
  );

  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const stepDatum = Data.from(
    firstStepUtxo.datum!,
    FraudProofComputationThreadStepDatum,
  );
  const proverPaymentCredential = getAddressDetails(
    await proverLucid.wallet().address(),
  ).paymentCredential;
  expect(proverPaymentCredential?.type).toBe("Key");
  const proverPaymentKeyHash = proverPaymentCredential!.hash;
  expect(stepDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: null,
  });
  expect(firstStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);
  expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
    [submitInitResult.computationThreadUnit, 1n],
  ]);

  const step01Result = await submitStep01({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(
      transactionInclusion.tx1.inclusion,
    ),
    awaitConfirmation: true,
  });

  expect(step01Result.txHash).toHaveLength(64);
  expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
  const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
    submitInitResult.firstStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFirstStepUtxos).toHaveLength(0);
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step02Datum = Data.from(secondStepUtxo.datum!, DoubleSpendStep02Datum);
  expect(step02Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_id: transactionInclusion.tx1.nativeTxId,
      verified_tx1_spend_inputs_hash:
        transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    },
  });
  expect(secondStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step02Result = await submitStep02({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(secondStepUtxo),
    stateQueueBlockOutRef: fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(
      transactionInclusion.tx2.inclusion,
    ),
    awaitConfirmation: true,
  });

  expect(step02Result.txHash).toHaveLength(64);
  expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
  expect(step02Result.verifiedTx1Id).toBe(transactionInclusion.tx1.nativeTxId);
  expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
  expect(step02Result.verifiedTx1SpendInputsHash).toBe(
    transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
  );
  expect(step02Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
    step01Result.secondStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingSecondStepUtxos).toHaveLength(0);
  const thirdStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step02Result.thirdStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
  expect(step03Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx1_spend_inputs_hash:
        transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
      verified_tx2_spend_inputs_hash:
        transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    },
  });
  expect(thirdStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(1n);

  const step03Result = await submitStep03({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(thirdStepUtxo),
    tx1SpendInputCbors: parseSpendInputCbors(
      transactionInclusion.tx1SpendInputCbors,
      "--tx1-inputs",
    ),
    doubleSpentInputIndex: 1n,
    awaitConfirmation: true,
  });

  expect(step03Result.txHash).toHaveLength(64);
  expect(step03Result.verifiedTx1SpendInputsHash).toBe(
    transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
  );
  expect(step03Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  expect(step03Result.doubleSpentInputIndex).toBe(1);
  expect(step03Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
  );
  expect(step03Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx1SpendInputCbors[1],
  );
  expect(step03Result.tx1SpendInputsWitnessCreated).toBe(true);
  expect(step03Result.tx1SpendInputsWitnessOutRef).toMatch(
    /^[0-9a-f]{64}#\d+$/,
  );
  expect(step03Result.tx1SpendInputsRefInputIndex).toBe(0);
  const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
    step02Result.thirdStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingThirdStepUtxos).toHaveLength(0);
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  const step04Datum = Data.from(fourthStepUtxo.datum!, DoubleSpendStep04Datum);
  expect(step04Datum).toEqual({
    fraud_prover: proverPaymentKeyHash,
    data: {
      verified_tx2_spend_inputs_hash:
        transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
      double_spent_input: midgardTxInput(
        transactionInclusion.tx1InputsPreimage[1]!,
      ),
    },
  });
  expect(fourthStepUtxo.assets[submitInitResult.computationThreadUnit]).toBe(
    1n,
  );

  const step04Result = await submitStep04({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(fourthStepUtxo),
    tx2SpendInputCbors: parseSpendInputCbors(
      transactionInclusion.tx2SpendInputCbors,
      "--tx2-inputs",
    ),
    doubleSpentInputIndex: 1n,
    awaitConfirmation: true,
  });

  expect(step04Result.txHash).toHaveLength(64);
  expect(step04Result.verifiedTx2SpendInputsHash).toBe(
    transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
  );
  expect(step04Result.doubleSpentInputIndex).toBe(1);
  expect(step04Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
  );
  expect(step04Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx2SpendInputCbors[1],
  );
  expect(step04Result.tx2SpendInputsWitnessCreated).toBe(true);
  expect(step04Result.tx2SpendInputsWitnessOutRef).toMatch(
    /^[0-9a-f]{64}#\d+$/,
  );
  expect(step04Result.tx2SpendInputsRefInputIndex).toBe(0);
  expect(step04Result.fraudProofAssetName).toBe(
    submitInitResult.computationThreadAssetName,
  );
  expect(step04Result.fraudProofUnit).toBe(
    toUnit(
      contracts.fraudProof.policyId,
      submitInitResult.computationThreadAssetName,
    ),
  );
  expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
    step04Result.computationThreadMintRedeemerIndex,
  );

  const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFourthStepUtxos).toHaveLength(0);
  const fraudProofUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step04Result.fraudProofAddress,
    step04Result.fraudProofUnit,
  );
  const fraudProofDatum = Data.from(
    fraudProofUtxo.datum!,
    FraudProofTokenDatum,
  );
  expect(fraudProofDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
  });
  expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
  expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
    [step04Result.fraudProofUnit, 1n],
  ]);

  return {
    emulator,
    realBlueprint,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
    transactionInclusion,
    fraudulentHeader,
    headerHash,
    setup,
    successors,
    deploymentInfo,
    fraudulentBlockOutRef,
    submitInitResult,
    step04Result,
    fraudProofUtxo,
    proverPaymentKeyHash,
  };
};

const submitRemovalForFixture = async (
  fixture: ProvedDoubleSpendFixture,
  options: {
    readonly lucid?: Awaited<ReturnType<typeof Lucid>>;
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  } = {},
) => {
  const removeNow = BigInt(fixture.emulator.now());
  return await submitRemoveFraudulentBlock({
    lucid: options.lucid ?? fixture.proverLucid,
    blueprint: fixture.realBlueprint,
    deploymentInfo: fixture.deploymentInfo,
    network,
    signer: fixture.proverSigner,
    fraudulentHeaderHash: fixture.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
    validTo: removeNow + 300_000n,
    ...(options.stateQueueMutationLeaseCoordinator === undefined
      ? {}
      : {
          stateQueueMutationLeaseCoordinator:
            options.stateQueueMutationLeaseCoordinator,
        }),
  });
};

const expectRemovedFraudProofState = async (
  fixture: ProvedDoubleSpendFixture,
) => {
  await expectStateQueueHeaderOrder({
    lucid: fixture.funderLucid,
    contracts: fixture.contracts,
    expectedHeaderHashes: [],
  });
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      fixture.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  for (const successor of fixture.successors) {
    await expect(
      fixture.funderLucid.utxosAtWithUnit(
        fixture.contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.activeOperators.spendingScriptAddress,
      fixture.setup.activeOperatorNodeUnit,
    ),
  ).resolves.toHaveLength(0);
  const [finalSchedulerUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.scheduler.spendingScriptAddress,
    toUnit(fixture.contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  );
  if (finalSchedulerUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the scheduler");
  }
  expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
    "NoActiveOperators",
  );
  const [finalRootUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.stateQueue.spendingScriptAddress,
    fixture.setup.stateQueueRootUnit,
  );
  if (finalRootUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the state-queue root");
  }
  const finalRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(finalRootUtxo, fixture.contracts.stateQueue.policyId),
  );
  expect(finalRoot.datum.next).toBe("Empty");
  const retainedFraudProof = await expectSingleUtxoWithUnit(
    fixture.proverLucid,
    fixture.step04Result.fraudProofAddress,
    fixture.step04Result.fraudProofUnit,
  );
  expect(outRefLabel(retainedFraudProof)).toBe(
    outRefLabel(fixture.fraudProofUtxo),
  );
  expect(retainedFraudProof.assets[fixture.step04Result.fraudProofUnit]).toBe(
    1n,
  );
};

describe("fault-proof emulator integration", () => {
  it("publishes every authenticated validation-dispute control under the exact L1 envelope", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const publisher = generateEmulatorAccount({
      lovelace: 40_000_000_000n,
    });
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected publisher wallet to expose a nonce UTxO");
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
    const targets = validationDisputeControlPublicationTargets(contracts);
    const authPolicy = createReferenceScriptAuthPolicy(lucid, emulator.now());
    const measurements = {} as Record<
      ValidationDisputeControlPublicationTarget["control"],
      CompleteSignedTransactionMeasurement
    >;

    for (const target of targets) {
      const publication = await runEmulatorLifecycleStage(
        `reference-script.publish-authenticated.${target.control}`,
        () =>
          publishAuthenticatedValidationDisputeControl({
            lucid,
            target,
            authPolicy,
          }),
      );
      measurements[target.control] = publication.publicationMeasurement;
    }

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { validationDisputeControlPublications: measurements },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }

    expect(Object.keys(measurements)).toHaveLength(targets.length);
    for (const measurement of Object.values(measurements)) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExMem,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        emulator.protocolParameters.maxTxExSteps,
      );
      expect(measurement.inputCount).toBe(1);
      expect(measurement.referenceInputCount).toBe(0);
      expect(measurement.outputCount).toBe(3);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.nativeScriptCount).toBe(1);
      expect(measurement.redeemerCount).toBe(0);
      expect(measurement.datumCount).toBe(0);
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
    }
  }, 300_000);

  it("proves and removes a non-tail double-spend block by pruning successors first", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const transactionInclusion = await buildTransactionInclusionFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const funderAddress = await funderLucid.wallet().address();
    const funderPaymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    const successor = await submitSuccessorBlockTx({
      lucid: funderLucid,
      contracts,
      anchorBlockUnit: setup.stateQueueBlockUnit,
      header: {
        ...makeHeader(
          funderPaymentCredential.hash,
          Number(fraudulentHeader.endTime),
          EMPTY_MERKLE_TREE_ROOT,
        ),
        prevHeaderHash: headerHash,
      },
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode: setup.activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash, successor.successorHeaderHash],
    });
    const fraudulentBlockOutRef = successor.continuedAnchorOutRef;
    const deploymentEntry = (
      scriptHash: string,
      script: Script,
      referenceName?: RemovalReferenceScriptName,
    ) => {
      const published =
        referenceName === undefined
          ? undefined
          : removalReferenceScriptPublications.published[referenceName];
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
    const deploymentInfo = deploymentManifest({
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
    });

    const result = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(result.txHash).toHaveLength(64);
    expect(result.fraudulentHeaderHash).toBe(headerHash);
    expect(result.computationThreadAssetName).toBe(
      `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    const stepDatum = Data.from(
      firstStepUtxo.datum!,
      FraudProofComputationThreadStepDatum,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    expect(stepDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: null,
    });
    expect(firstStepUtxo.assets[result.computationThreadUnit]).toBe(1n);
    expect(positiveNonAdaAssets(firstStepUtxo)).toEqual([
      [result.computationThreadUnit, 1n],
    ]);

    const step01Result = await submitStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        transactionInclusion.tx1.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(transactionInclusion.tx1.nativeTxId);
    const remainingFirstStepUtxos = await proverLucid.utxosAtWithUnit(
      result.firstStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFirstStepUtxos).toHaveLength(0);
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      DoubleSpendStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_id: transactionInclusion.tx1.nativeTxId,
        verified_tx1_spend_inputs_hash:
          transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
      },
    });
    expect(secondStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step02Result = await submitStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        transactionInclusion.tx2.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.verifiedTx1Id).toBe(
      transactionInclusion.tx1.nativeTxId,
    );
    expect(step02Result.nativeTx2Id).toBe(transactionInclusion.tx2.nativeTxId);
    expect(step02Result.verifiedTx1SpendInputsHash).toBe(
      transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    );
    expect(step02Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    const remainingSecondStepUtxos = await proverLucid.utxosAtWithUnit(
      step01Result.secondStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingSecondStepUtxos).toHaveLength(0);
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    const step03Datum = Data.from(thirdStepUtxo.datum!, DoubleSpendStep03Datum);
    expect(step03Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx1_spend_inputs_hash:
          transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
        verified_tx2_spend_inputs_hash:
          transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
      },
    });
    expect(thirdStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step03Result = await submitStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      tx1SpendInputCbors: parseSpendInputCbors(
        transactionInclusion.tx1SpendInputCbors,
        "--tx1-inputs",
      ),
      doubleSpentInputIndex: 1n,
      awaitConfirmation: true,
    });

    expect(step03Result.txHash).toHaveLength(64);
    expect(step03Result.verifiedTx1SpendInputsHash).toBe(
      transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
    );
    expect(step03Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    expect(step03Result.doubleSpentInputIndex).toBe(1);
    expect(step03Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx1InputsPreimage[1]!),
    );
    expect(step03Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx1SpendInputCbors[1],
    );
    expect(step03Result.tx1SpendInputsWitnessCreated).toBe(true);
    expect(step03Result.tx1SpendInputsWitnessOutRef).toMatch(
      /^[0-9a-f]{64}#\d+$/,
    );
    expect(step03Result.tx1SpendInputsRefInputIndex).toBe(0);
    const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
      step02Result.thirdStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingThirdStepUtxos).toHaveLength(0);
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    const step04Datum = Data.from(
      fourthStepUtxo.datum!,
      DoubleSpendStep04Datum,
    );
    expect(step04Datum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
      data: {
        verified_tx2_spend_inputs_hash:
          transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
        double_spent_input: midgardTxInput(
          transactionInclusion.tx1InputsPreimage[1]!,
        ),
      },
    });
    expect(fourthStepUtxo.assets[result.computationThreadUnit]).toBe(1n);

    const step04Result = await submitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      tx2SpendInputCbors: parseSpendInputCbors(
        transactionInclusion.tx2SpendInputCbors,
        "--tx2-inputs",
      ),
      doubleSpentInputIndex: 1n,
      awaitConfirmation: true,
    });

    expect(step04Result.txHash).toHaveLength(64);
    expect(step04Result.verifiedTx2SpendInputsHash).toBe(
      transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
    );
    expect(step04Result.doubleSpentInputIndex).toBe(1);
    expect(step04Result.doubleSpentInput).toEqual(
      midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
    );
    expect(step04Result.doubleSpentInputCbor).toEqual(
      transactionInclusion.tx2SpendInputCbors[1],
    );
    expect(step04Result.tx2SpendInputsWitnessCreated).toBe(true);
    expect(step04Result.tx2SpendInputsWitnessOutRef).toMatch(
      /^[0-9a-f]{64}#\d+$/,
    );
    expect(step04Result.tx2SpendInputsRefInputIndex).toBe(0);
    expect(step04Result.fraudProofAssetName).toBe(
      result.computationThreadAssetName,
    );
    expect(step04Result.fraudProofUnit).toBe(
      toUnit(contracts.fraudProof.policyId, result.computationThreadAssetName),
    );
    expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
      step04Result.computationThreadMintRedeemerIndex,
    );

    const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
      step03Result.fourthStepAddress,
      result.computationThreadUnit,
    );
    expect(remainingFourthStepUtxos).toHaveLength(0);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    const fraudProofDatum = Data.from(
      fraudProofUtxo.datum!,
      FraudProofTokenDatum,
    );
    expect(fraudProofDatum).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
    });
    expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
    expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
      [step04Result.fraudProofUnit, 1n],
    ]);

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "emulator-fault-proof-removal",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
    });
    expect(removeResult.fraudulentHeaderHash).toBe(headerHash);
    expect(removeResult.fraudProver).toBe(proverPaymentCredential!.hash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [successor.successorHeaderHash, headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Remove transaction did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const [finalRootUtxo] = await funderLucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error(
        "Remove transaction did not preserve the state-queue root",
      );
    }
    const finalRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(finalRootUtxo, contracts.stateQueue.policyId),
    );
    expect(finalRoot.datum.next).toBe("Empty");
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("proves and removes a tail invalid-range block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realInvalidRange: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const invalidRangeInclusion =
      await buildInvalidRangeTransactionInclusionFixture({
        blockValidFrom: BigInt(headerStartTime),
        blockValidTo: BigInt(headerStartTime + 1_000),
      });
    expect(invalidRangeInclusion.violationReason).toBe("lower-before-block");

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        invalidRangeInclusion.transactionsRoot,
        invalidRangeInclusion.l2TransactionCount,
      ),
      invalidRangeInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidRange",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("invalidRange");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.invalidRange.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01Result = await submitInvalidRangeStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        invalidRangeInclusion.badTx.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(
      invalidRangeInclusion.badTx.nativeTxId,
    );
    expect(step01Result.blockValidFrom).toBe(fraudulentHeader.startTime);
    expect(step01Result.blockValidTo).toBe(fraudulentHeader.endTime);
    expect(step01Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    expect(step01Result.violationReason).toBe("lower-before-block");
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Datum = Data.from(
      secondStepUtxo.datum!,
      InvalidRangeStep02Datum,
    );
    expect(step02Datum).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        block_valid_from: fraudulentHeader.startTime,
        block_valid_to: fraudulentHeader.endTime,
        bad_tx_normalized_validity_range:
          invalidRangeInclusion.normalizedValidityRange,
      },
    });

    const step02Result = await submitInvalidRangeStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(step02Result.violationReason).toBe("lower-before-block");
    expect(step02Result.normalizedValidityRange).toEqual(
      invalidRangeInclusion.normalizedValidityRange,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidRange",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("invalidRange");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.invalidRange.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("proves and removes a tail zero-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const zeroInputInclusion =
      await buildZeroInputTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        zeroInputInclusion.transactionsRoot,
        zeroInputInclusion.l2TransactionCount,
      ),
      zeroInputInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("zeroInput");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.zeroInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const step01Result = await submitZeroInputStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(
        zeroInputInclusion.badTx.inclusion,
      ),
      awaitConfirmation: true,
    });

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.nativeTxId).toBe(zeroInputInclusion.badTx.nativeTxId);
    expect(step01Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(secondStepUtxo.datum!, ZeroInputStep02Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: { bad_tx_spend_inputs_hash: EMPTY_SPEND_INPUTS_HASH },
    });

    const step02Result = await submitZeroInputStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      awaitConfirmation: true,
    });

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("zeroInput");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.zeroInput.categoryId,
    );
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("rejects a spending transaction before a zero-input thread can advance", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const transactionInclusion = await buildTransactionInclusionFixture();

    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        transactionInclusion.transactionsRoot,
        transactionInclusion.l2TransactionCount,
      ),
      transactionInclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "zeroInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    await expect(
      submitZeroInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(
          transactionInclusion.tx1.inclusion,
        ),
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      "--tx-inclusion.nativeTx spends at least one input, so it does not violate the zero-input ledger rule.",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);

  it("proves and removes a tail non-existent-input block end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    await registerPexcludesExclusionRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realNonExistentInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const fixture = await buildNonExistentInputFixture();
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const fraudulentHeader = makeHeader(
      funderPaymentCredential.hash,
      headerStartTime,
      await countedTransactionsRoot(
        fixture.transactionsRoot,
        fixture.l2TransactionCount,
      ),
      fixture.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInput",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    expect(initResult.fraudCategoryName).toBe("nonExistentInput");
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.nonExistentInput.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Result = await neSubmitStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.inclusion,
      awaitConfirmation: true,
    });
    expect(step01Result.nativeTxId).toBe(fixture.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Result = await neSubmitStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: fixture.inputsPreimage,
      badInputIndex: fixture.badInputIndex,
      awaitConfirmation: true,
    });
    expect(step02Result.missingInput.tx_id).toBe(fixture.missingInputTxId);

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await neSubmitStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
      awaitConfirmation: true,
    });
    expect(step03Result.missingInputTxId).toBe(fixture.missingInputTxId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04Result = await neSubmitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      txsNonMembershipProofCbor: fixture.txsNonMembershipProofCbor,
      awaitConfirmation: true,
    });
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const proverPaymentKeyHash = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential!.hash;
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInput",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removeResult.fraudCategory).toBe("nonExistentInput");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("submits and removes a tail transition-trace fraud proof end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    proverLucid.selectWallet.fromSeed(prover.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realTransitionTrace: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const traceFixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: funderPaymentCredential.hash,
      now: headerStartTime,
    });
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: traceFixture.header,
    });
    expect(setup.headerHash).toBe(traceFixture.headerHash);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [traceFixture.headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(initResult.fraudCategoryName).toBe("transitionTrace");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.transitionTrace.categoryId}${traceFixture.headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const proofResult = await submitTransitionTraceProof({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      proof: traceFixture.proof,
      awaitConfirmation: true,
    });

    expect(proofResult.txHash).toHaveLength(64);
    expect(proofResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(proofResult.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(proofResult.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(proofResult.fraudProofMintRedeemerIndex).not.toBe(
      proofResult.computationThreadMintRedeemerIndex,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentHeaderHash: traceFixture.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("transitionTrace");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [traceFixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[proofResult.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it.each([
    {
      name: "direct",
      inlineDatumPayloadBytes: 7_976,
      minimumCompleteItemBytes: 0,
      expectedCarriage: "direct" as const,
    },
    {
      name: "reference",
      inlineDatumPayloadBytes: 13_600,
      minimumCompleteItemBytes:
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
      expectedCarriage: "reference" as const,
    },
  ])(
    "opens, bisects, resolves a fitting complete item by $name, and awards a validation dispute",
    async ({
      inlineDatumPayloadBytes,
      minimumCompleteItemBytes,
      expectedCarriage,
    }) => {
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
                operator.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
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
                challenger.assets.lovelace -
                BigInt(feeUtxoCount) * feeUtxoLovelace,
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
      const validityRange = () =>
        validationDisputeValidityRange(emulator.now());

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
      // Re-derive the applied canonical-decode item-semantic validator (the
      // same deterministic build the submit path performs) so its reference
      // script can be published and its body pinned as absent from the proof
      // transactions.
      const validationDisputeSdkContracts = await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
        }),
      );
      const itemSemanticContract =
        validationDisputeSdkContracts.validationTraceDispute
          .semanticResolvers[1];
      const catalogue = await buildCatalogueDeploymentInfo(
        contracts.fraudProofs,
      );
      const operatorPaymentCredential = getAddressDetails(
        await operatorLucid.wallet().address(),
      ).paymentCredential;
      if (
        operatorPaymentCredential === undefined ||
        operatorPaymentCredential.type !== "Key"
      ) {
        throw new Error(
          "Expected operator wallet to expose a payment key hash",
        );
      }
      const headerStartTime =
        alignUnixTimeToEmulatorSlotBoundary(
          operatorLucid,
          emulator.now() + 120_000,
        ) - 1;
      const fixture = await buildInvalidForcedValidationDisputeFixture({
        operatorVkey: operatorPaymentCredential.hash,
        now: headerStartTime,
        inlineDatumPayloadBytes,
        minimumCompleteItemBytes,
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
      const itemSemanticPublication = await runEmulatorLifecycleStage(
        "reference-script.publish-item-semantic",
        async () => {
          emulator.protocolParameters = {
            ...setupProtocolParameters,
            maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
          };
          try {
            return await publishPlainReferenceScriptUtxo({
              lucid: referenceScriptPublisherLucid,
              script: itemSemanticContract.spendingScript,
              label: "validation item-semantic",
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
        {
          scriptHash: itemSemanticContract.spendingScriptHash,
          utxo: itemSemanticPublication.utxo,
        },
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
      const openSubmission = await runEmulatorLifecycleStage("open", () =>
        captureEmulatorSubmission(emulator, () =>
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
        ),
      );
      const openResult = openSubmission.result;
      const publicationMeasurement = openSubmission.measurement;
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
              signer:
                move.role === "operator" ? operatorSigner : challengerSigner,
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
      const semanticSubmission = await runEmulatorLifecycleStage(
        "semantic-resolution",
        () =>
          captureEmulatorSubmission(emulator, () =>
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
          ),
      );
      const semanticResult = semanticSubmission.result;
      const awardSubmission = await runEmulatorLifecycleStage("award", () =>
        captureEmulatorSubmission(emulator, () =>
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
        ),
      );
      const awardResult = awardSubmission.result;
      const proofTransactionMeasurements = {
        referenceScriptPublication:
          validationDisputePublication.publicationMeasurement,
        publication: publicationMeasurement,
        resolution: semanticSubmission.measurement,
        resolutionTransactions: semanticSubmission.measurements,
        award: awardSubmission.measurement,
      };
      const allProofTransactionMeasurements = [
        validationDisputePublication.publicationMeasurement,
        publicationMeasurement,
        ...semanticSubmission.measurements,
        awardSubmission.measurement,
      ];
      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify(
            {
              completeItemBytes: fixture.completeItemBytes,
              transactions: proofTransactionMeasurements,
            },
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          ),
        );
      }

      expect(fixture.evidence.finalDispute.turn).toEqual({
        type: "readyForOneStep",
      });
      expect(fixture.evidence.moves.length).toBeGreaterThan(0);
      expect(prepareResult.resolverIndex).toBe(
        fixture.evidence.oneStepArgument.resolverIndex,
      );
      expect(selectedResult.semanticResolverIndex).toBe(
        fixture.evidence.oneStepArgument.semanticResolverIndex,
      );
      expect(semanticResult.proofItemCarriage).toBe(expectedCarriage);
      if (expectedCarriage === "reference") {
        expect(semanticResult.proofItemReferenceOutRef).toBe(
          semanticResult.proofItemPublication?.outRef,
        );
        expect(semanticResult.proofItemPublication).toMatchObject({
          awaitedConfirmation: true,
        });
        expect(
          semanticResult.proofItemPublication?.completeSignedBytes,
        ).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxSize);
        expect(
          semanticResult.proofItemPublication?.lovelace ?? 0n,
        ).toBeGreaterThan(0n);
      } else {
        expect(semanticResult.proofItemReferenceOutRef).toBeUndefined();
        expect(semanticResult.proofItemPublication).toBeUndefined();
      }
      expect(semanticResult.stageTransactions).toHaveLength(5);
      expect(semanticSubmission.measurements).toHaveLength(
        expectedCarriage === "reference" ? 6 : 5,
      );
      // The semantic-resolution (authentication) proof transaction sources
      // the item-semantic validator from the published reference script: one
      // extra reference input beside the direct route, two beside the
      // published proof item on the reference route.
      expect(
        semanticSubmission.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual(
        expectedCarriage === "reference" ? [0, 2, 0, 1, 0, 0] : [1, 0, 0, 0, 0],
      );
      expect(
        semanticSubmission.measurements.every(
          (measurement) =>
            measurement.completeSignedBytes <=
            PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
        ),
      ).toBe(true);
      // C21-DISPUTE-SUBMIT defect 2: the representative complete-item
      // semantic-resolution transaction stays at or below the literal
      // 16,384-byte L1 envelope and does not embed the ~3.4 KiB applied
      // item-semantic validator body — no Plutus script witness at all; the
      // validator arrives via the published reference script.
      const resolutionMeasurements = semanticSubmission.measurements.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const resolutionCbors = semanticSubmission.transactionCbors.slice(
        expectedCarriage === "reference" ? 1 : 0,
      );
      const authenticationMeasurement = resolutionMeasurements[0]!;
      const authenticationCbor = resolutionCbors[0]!;
      expect(authenticationMeasurement.completeSignedBytes).toBeLessThanOrEqual(
        16_384,
      );
      expect(authenticationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV2ScriptCount).toBe(0);
      expect(authenticationMeasurement.plutusV1ScriptCount).toBe(0);
      expect(authenticationMeasurement.nativeScriptCount).toBe(0);
      expect(itemSemanticContract.spendingScript.script.length).toBeGreaterThan(
        0,
      );
      expect(
        authenticationCbor.includes(itemSemanticContract.spendingScript.script),
      ).toBe(false);
      expect(
        semanticResult.stageTransactions?.map(
          (transaction) => transaction.completeSignedBytes,
        ),
      ).toEqual(
        semanticSubmission.measurements
          .slice(expectedCarriage === "reference" ? 1 : 0)
          .map((measurement) => measurement.completeSignedBytes),
      );
      expect(semanticResult.nextThreadOutRef).toBe(awardResult.threadOutRef);
      expect(awardResult.txHash).toHaveLength(64);
      expect(awardResult.fraudProofUnit).toBe(
        toUnit(
          contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      );
      expect(publicationMeasurement.l1ByteMargin).toBeGreaterThan(0);
      expect(publicationMeasurement.referenceInputCount).toBe(3);
      expect(
        validationDisputePublication.publicationMeasurement.nativeScriptCount,
      ).toBe(1);
      expect(publicationMeasurement.plutusV3ScriptCount).toBe(0);
      expect(semanticSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(awardSubmission.measurement.l1ByteMargin).toBeGreaterThan(0);
      for (const measurement of allProofTransactionMeasurements) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExMem,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          emulator.protocolParameters.maxTxExSteps,
        );
      }
      await expect(
        targetChallengerLucid.utxosAtWithUnit(
          contracts.fraudProof.spendingScriptAddress,
          awardResult.fraudProofUnit,
        ),
      ).resolves.toHaveLength(1);
    },
    300_000,
  );

  it("coordinates non-tail removal with lease acquire, refetch, renew, and release ordering", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.fraudulentHeaderHash).toBe(fixture.headerHash);
    expect(removeResult.fraudProver).toBe(fixture.proverPaymentKeyHash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.successors[0]!.successorHeaderHash, fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndexes = eventIndexes(events, "lease.renew");
    const awaitTxIndexes = eventIndexes(events, "awaitTx");
    const releaseIndex = eventIndexes(events, "lease.release")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(3);
    expect(renewIndexes).toHaveLength(4);
    expect(awaitTxIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(renewIndexes[0]!).toBeLessThan(awaitTxIndexes[0]!);
    expect(awaitTxIndexes[0]!).toBeLessThan(renewIndexes[1]!);
    expect(renewIndexes[1]!).toBeLessThan(stateQueueLoadIndexes[2]!);
    expect(stateQueueLoadIndexes[2]!).toBeLessThan(renewIndexes[2]!);
    expect(renewIndexes[2]!).toBeLessThan(awaitTxIndexes[1]!);
    expect(awaitTxIndexes[1]!).toBeLessThan(renewIndexes[3]!);
    expect(renewIndexes[3]!).toBeLessThan(releaseIndex);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("rejects non-tail removal without a state-queue mutation lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });

    await expect(submitRemovalForFixture(fixture)).rejects.toThrow(
      "requires a live Midgard node state-queue mutation lease",
    );
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when post-acquire topology refetch fails", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failStateQueueUtxosAtCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented state-queue topology load failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(failIndex);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented state-queue topology load failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when removal preparation fails after acquisition", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failSchedulerUtxosAtWithUnitCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented scheduler lookup failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const schedulerIndexes = eventIndexes(events, "scheduler.utxosAtWithUnit");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndex = eventIndexes(events, "lease.renew")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(schedulerIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(1);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(renewIndex);
    expect(renewIndex).toBeLessThan(schedulerIndexes[1]!);
    expect(schedulerIndexes[1]!).toBeLessThan(failIndex);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented scheduler lookup failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("removes a tail double-spend block without acquiring a lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture();
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
    });

    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    expect(eventIndexes(events, "lease.acquire")).toHaveLength(0);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(1);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(1);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("removes a non-tail double-spend block with multiple successors in queue order", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 2 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [
        fixture.successors[0]!.successorHeaderHash,
        fixture.successors[1]!.successorHeaderHash,
        fixture.headerHash,
      ],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
      "OperatorAlreadySlashed",
    ]);

    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(4);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(6);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(3);
    expect(eventIndexes(events, "lease.release")).toHaveLength(1);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);

    await expectRemovedFraudProofState(fixture);
  }, 300_000);
});


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
const runForcedValidationDisputeScenario = async (
  buildFixture: (input: {
    readonly operatorVkey: string;
    readonly now: number;
  }) => Promise<ForcedValidationDisputeFixture>,
  {
    stopAfter,
  }: {
    readonly stopAfter?:
      | "prepare-resolution"
      | "prepare-selected"
      | "semantic-resolution";
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
  // Block removal needs the state-queue, operator-directory and scheduler
  // validators. Publishing them as reference-script UTxOs is what the deployed
  // node does; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit the literal 16,384-byte L1 envelope, so this also
  // proves each of these validators is publishable on L1.
  const removalReferenceScriptPublications = await runEmulatorLifecycleStage(
    "reference-script.publish-removal",
    async () => {
      emulator.protocolParameters = {
        ...setupProtocolParameters,
        maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      };
      try {
        return await publishRemovalReferenceScripts({
          lucid: referenceScriptPublisherLucid,
          contracts,
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
    undefined,
    removalReferenceScriptPublications.published,
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
        deploymentInfo,
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

describe("validation-dispute soundness with a non-empty claimed ledger delta", () => {
  it("lets a challenger win against an operator who claimed Accepted over a non-empty claimed ledger delta", async () => {
    const claimedLedgerDeltaRoot = await buildNonEmptyClaimedLedgerDeltaRootV1();
    // Guard against the fixture silently degrading back into the empty-delta
    // special case that hid VM-DEFECT-2.
    expect(claimedLedgerDeltaRoot).toHaveLength(32);
    expect(
      claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);

    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildAcceptedClaimOverRejectingTransactionFixture({
          operatorVkey,
          now,
          claimedLedgerDeltaRoot,
        }),
    );
    const { fixture, lowIndex, highIndex } = result;

    // The disputed boundary is the rejecting terminal itself, which is the
    // only boundary at which `rejected_successor_is_exact` is exercised.
    expect(highIndex).toBe(lowIndex + 1);
    expect(highIndex).toBe(fixture.challengerTrace.states.length - 1);
    const preState = fixture.challengerTrace.states[lowIndex]!;
    const challengerSuccessor = fixture.challengerTrace.states[highIndex]!;
    expect(preState.phase).toBe("inputSets");
    expect(preState.verdict).toBe("pending");
    expect(challengerSuccessor.phase).toBe("terminal");
    expect(challengerSuccessor.verdict).toBe("rejected");
    // Non-vacuity: both endpoints of the proved transition carry the same
    // genuinely non-empty claimed delta. Under the deleted clause the
    // successor would have had to carry frontier_commitment(0, []) instead.
    expect(preState.ledgerDeltaRoot.equals(claimedLedgerDeltaRoot)).toBe(true);
    expect(
      challengerSuccessor.ledgerDeltaRoot.equals(claimedLedgerDeltaRoot),
    ).toBe(true);
    expect(
      challengerSuccessor.ledgerDeltaRoot.equals(
        EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
      ),
    ).toBe(false);
    // The operator really did commit `Accepted`, which the forced-source
    // binding forces from `operator_validity: TxIsValid`.
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.evidence.moves.length).toBeGreaterThan(0);
    expect(fixture.evidence.oneStepArgument.resolverIndex).toBe(
      validationResolverIndexV1("InputSets"),
    );

    // The challenger reached the award and removed the operator's block.
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.awardResult?.fraudProofUnit).toBe(
      toUnit(
        result.contracts.fraudProof.policyId,
        result.initResult.computationThreadAssetName,
      ),
    );
    await expect(
      result.challengerLucid!.utxosAtWithUnit(
        result.contracts.fraudProof.spendingScriptAddress,
        result.awardResult!.fraudProofUnit,
      ),
    ).resolves.toHaveLength(1);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);

    // Winning the dispute is worthless if the correction cannot be executed on
    // L1. Every removal transaction must fit the literal 16,384-byte envelope
    // with zero attached validator bodies -- each script is reference-sourced.
    const removalMeasurements = result.removalMeasurements ?? [];
    expect(removalMeasurements.length).toBeGreaterThan(0);
    for (const measurement of removalMeasurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      );
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
      expect(measurement.nativeScriptCount).toBe(0);
      // The scripts have to come from somewhere: reference inputs carry them.
      expect(measurement.referenceInputCount).toBeGreaterThanOrEqual(7);
    }
    // And every validator the correction needs is itself publishable on L1.
    const referenceScriptMeasurements = Object.values(
      result.removalReferenceScriptMeasurements ?? {},
    );
    expect(referenceScriptMeasurements).toHaveLength(7);
    for (const measurement of referenceScriptMeasurements) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);

  it("rejects the cleared-delta rejection successor the deleted VM-DEFECT-2 clause required", async () => {
    const claimedLedgerDeltaRoot = await buildNonEmptyClaimedLedgerDeltaRootV1();
    expect(
      claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);

    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildAcceptedClaimOverRejectingTransactionFixture({
            operatorVkey,
            now,
            claimedLedgerDeltaRoot,
            clearChallengerTerminalDelta: true,
          }),
        { stopAfter: "semantic-resolution" },
      ),
    ).rejects.toThrow(/emulator lifecycle stage prepare-selected failed/u);
  }, 600_000);

  it("cannot be defeated when the operator honestly accepted a valid transaction carrying a non-empty ledger delta", async () => {
    // Same disputed instruction, same rejection code, same non-empty claimed
    // delta as the challenger-wins case; only the transaction's validity
    // differs. Soundness must be symmetric (GOAL_SPEC §3 invariant 9).
    let observed:
      | Awaited<ReturnType<typeof buildHonestAcceptedValidationDisputeFixture>>
      | undefined;
    await expect(
      runForcedValidationDisputeScenario(async ({ operatorVkey, now }) => {
        observed = await buildHonestAcceptedValidationDisputeFixture({
          operatorVkey,
          now,
        });
        return observed;
      }),
    ).rejects.toThrow(
      /emulator lifecycle stage (prepare-selected|semantic-resolution) failed/u,
    );

    const fixture = observed;
    if (fixture === undefined) {
      throw new Error("honest-operator mirror fixture was never constructed");
    }
    expect(fixture.disputedPhase).toBe("inputSets");
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.claimedLedgerDeltaRoot).toHaveLength(32);
    expect(
      fixture.claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);
    const { lowIndex, highIndex } = fixture.evidence.finalDispute;
    expect(highIndex).toBe(lowIndex + 1);
    expect(fixture.challengerTrace.states[lowIndex]!.phase).toBe("inputSets");
    const forgedSuccessor = fixture.challengerTrace.states[highIndex]!;
    expect(forgedSuccessor.phase).toBe("terminal");
    expect(forgedSuccessor.verdict).toBe("rejected");
    // The forgery is maximally strong: the immutable claimed delta is carried
    // through unchanged, so it is not rejected for a context mismatch.
    expect(
      forgedSuccessor.ledgerDeltaRoot.equals(fixture.claimedLedgerDeltaRoot),
    ).toBe(true);
  }, 900_000);
});
