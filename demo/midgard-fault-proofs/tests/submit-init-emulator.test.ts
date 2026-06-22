import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  compareOutRefs,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  findOutRefIndex,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
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
  buildDoubleSpendFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildPhasMembershipRewardRegistrationTxProgram,
  buildTransitionTraceFaultProofContracts,
  ConfirmedState,
  DA_PAYLOAD_V2_VERSION,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayloadV2,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxSchema,
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
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  Header,
  headerHashFromStateQueueUTxO,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  invalidOneStepTransitionFault,
  InvalidRangeStep02Datum,
  invalidRangeViolationReason,
  makeHubOracleDatum,
  type MidgardValidators,
  type MintingValidator,
  normalizeNativeTxValidityRange,
  OutputReference,
  outputReferenceFromUTxO,
  parseFaultProofBlueprint,
  parsePhasMembershipBlueprint,
  phasMembershipWithdrawalScriptFromBlueprint,
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
  sortStateQueueUTxOs,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  TransitionStepSchema,
  utxosToStateQueueUTxOs,
  utxoToStateQueueUTxO,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type BuildTxWithRedeemer,
  CML,
  Constr,
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
  keyValuePhasRootWithCount,
  nativeTxFromCoreCompact,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  reconstructDaPayloadV2,
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
} from "../src/index.js";

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
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
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
    realInvalidRange = false,
    realTransitionTrace = false,
    alwaysFraudProofCatalogue = false,
  }: {
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
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
      ],
    ),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "state_queue.spend.spend"),
      [stateQueueMinting.policyId],
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
      invalidRange:
        invalidRangeContracts?.invalidRange.firstStep ??
        withActiveOperators.fraudProofs.invalidRange,
      transitionTrace:
        transitionTraceContracts?.transitionTrace.firstStep ??
        withActiveOperators.fraudProofs.transitionTrace,
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
): number => {
  const provider = lucid.config().provider as {
    readonly time?: number;
    readonly slot?: number;
  };
  if (typeof provider.time !== "number" || typeof provider.slot !== "number") {
    return unixTime;
  }
  return (
    provider.time - provider.slot * 1000 + lucid.unixTimeToSlot(unixTime) * 1000
  );
};

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

const deploymentManifest = (contracts: Record<string, unknown>) => ({
  referenceScriptAuthPolicy: {},
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

const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

const makeNativeTx = ({
  spendInputCbors,
  fee,
  referenceByte,
  outputByte,
  witnessByte,
  validityIntervalStart = MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
  readonly referenceByte: string;
  readonly outputByte: string;
  readonly witnessByte: string;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor: encodeCbor([
        Buffer.from(h32(referenceByte), "hex"),
      ]),
      outputsPreimageCbor: encodeCbor([Buffer.from(h32(outputByte), "hex")]),
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
      addrTxWitsPreimageCbor: encodeCbor([
        Buffer.from(h32(witnessByte), "hex"),
      ]),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const compactTxEntry = (
  nativeTx: MidgardNativeTxFull,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxId(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));

const buildTransactionInclusionFixture = async (): Promise<{
  readonly transactionsRoot: string;
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
        encodeMidgardNativeTxCompact(
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
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          entry === tx1 ? tx1Native.compact : tx2Native.compact,
        ).toString("hex"),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
      nativeTx: entry.nativeTx,
      nativeTxId: entry.nativeTxId,
      spendInputCbors: entry.spendInputCbors,
    };
  };
  return {
    transactionsRoot: trieRootHex(trie),
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
    Buffer.from(encodeMidgardNativeTxCompact(badNativeTx.compact)),
  );
  const proof = await trie.prove(Buffer.from(badTx.nativeTxId, "hex"));

  return {
    transactionsRoot: trieRootHex(trie),
    badTx: {
      inclusion: {
        nativeTxId: badTx.nativeTxId,
        nativeTx: badTx.nativeTx,
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          badNativeTx.compact,
        ).toString("hex"),
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

const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
): Header => ({
  prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
  transactionsRoot,
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  startTime: BigInt(now),
  endTime: BigInt(now + 1_000),
  prevHeaderHash: GENESIS_HEADER_HASH,
  operatorVkey,
  protocolVersion: GENESIS_PROTOCOL_VERSION,
});

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

const transitionTraceTxBody = (byte: string) => ({
  spend_inputs: h32(byte),
  reference_inputs: h32("b1"),
  outputs: h32("b2"),
  fee: 0n,
  validity_interval: {
    lower_bound: {
      bound_type: "NegativeInfinity",
      is_inclusive: true,
    },
    upper_bound: {
      bound_type: "PositiveInfinity",
      is_inclusive: false,
    },
  },
  required_observers: h32("b3"),
  required_signer_hashes: h32("b4"),
  mint: h32("b5"),
  script_integrity_hash: h32("b6"),
  auxiliary_data_hash: h32("b7"),
  network_id: "Testnet",
});

const buildInvalidForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry("01", "02");
  const finalUtxosRoot = await keyValuePhasRootWithCount([
    {
      key: Buffer.from(finalUtxo[0], "hex"),
      value: Buffer.from(finalUtxo[1], "hex"),
    },
  ]);
  const forcedTransaction = {
    tx_compact: {
      body: transitionTraceTxBody("b0"),
      wits: h32("b8"),
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
      valueSchema: ForcedInclusionTxSchema,
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
    ROOT_DOMAINS.forcedTransactions,
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
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const header: Header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadCbor = encodeDaPayloadV2({
    version: DA_PAYLOAD_V2_VERSION,
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
      counts,
    },
  });
  const reconstruction = await reconstructDaPayloadV2({
    payloadCbor,
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
  readonly header: Header;
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
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
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
        { Init: { output_index: SETUP_OUTPUT_INDEX.stateQueueRoot } },
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
  await lucid.awaitTx(await signed.submit());

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
  const activationUnsigned = await lucid
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
    .complete({ localUPLCEval: true });
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await activationSigned.submit());

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
  const appointmentUnsigned = await lucid
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
    .complete({ localUPLCEval: true });
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await appointmentSigned.submit());

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
  const commitValidTo = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(lucid, Number(header.endTime)),
  );
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time: commitValidTo - 1n + 30n,
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
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

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
    getHeaderFromStateQueueDatum(committedBlock.datum),
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
  readonly header: Header;
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
  const successorHeaderHash = await Effect.runPromise(hashBlockHeader(header));
  const successorBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHeaderHash,
  );
  const commitFeeInput = await firstWalletUtxo(
    lucid,
    "successor commit fee input",
  );
  const commitValidTo = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(lucid, Number(header.endTime)),
  );
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time: commitValidTo - 1n + 30n,
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
  await Effect.runPromise(getHeaderFromStateQueueDatum(continuedAnchor.datum));
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

const buildRemovalDeploymentInfo = (
  contracts: MidgardValidators,
  catalogue: FraudProofCatalogueDeploymentInfo,
) => {
  const deploymentEntry = (scriptHash: string, script: Script) => ({
    scriptHash,
    refScriptUTxO: null,
    contract: {
      type: script.type,
      cborHex: script.script,
    },
  });
  return deploymentManifest({
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
    fraudProofInvalidRange: {
      scriptHash: contracts.fraudProofs.invalidRange.spendingScriptHash,
    },
    fraudProofTransitionTrace: {
      scriptHash: contracts.fraudProofs.transitionTrace.spendingScriptHash,
    },
    stateQueueMint: deploymentEntry(
      contracts.stateQueue.policyId,
      contracts.stateQueue.mintingScript,
    ),
    stateQueueSpend: deploymentEntry(
      contracts.stateQueue.spendingScriptHash,
      contracts.stateQueue.spendingScript,
    ),
    retiredOperatorsMint: deploymentEntry(
      contracts.retiredOperators.policyId,
      contracts.retiredOperators.mintingScript,
    ),
    retiredOperatorsSpend: deploymentEntry(
      contracts.retiredOperators.spendingScriptHash,
      contracts.retiredOperators.spendingScript,
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
    ),
    activeOperatorsSpend: deploymentEntry(
      contracts.activeOperators.spendingScriptHash,
      contracts.activeOperators.spendingScript,
    ),
    schedulerMint: { scriptHash: contracts.scheduler.policyId },
    schedulerSpend: deploymentEntry(
      contracts.scheduler.spendingScriptHash,
      contracts.scheduler.spendingScript,
    ),
    settlementMint: { scriptHash: contracts.settlement.policyId },
  });
};

type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: Header;
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
  readonly fraudulentHeader: Header;
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
    transactionInclusion.transactionsRoot,
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

  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
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
    requireReferenceScripts: false,
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
      transactionInclusion.transactionsRoot,
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
    const deploymentEntry = (scriptHash: string, script: Script) => ({
      scriptHash,
      refScriptUTxO: null,
      contract: {
        type: script.type,
        cborHex: script.script,
      },
    });
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
      ),
      stateQueueSpend: deploymentEntry(
        contracts.stateQueue.spendingScriptHash,
        contracts.stateQueue.spendingScript,
      ),
      retiredOperatorsMint: deploymentEntry(
        contracts.retiredOperators.policyId,
        contracts.retiredOperators.mintingScript,
      ),
      retiredOperatorsSpend: deploymentEntry(
        contracts.retiredOperators.spendingScriptHash,
        contracts.retiredOperators.spendingScript,
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
      ),
      activeOperatorsSpend: deploymentEntry(
        contracts.activeOperators.spendingScriptHash,
        contracts.activeOperators.spendingScript,
      ),
      schedulerMint: { scriptHash: contracts.scheduler.policyId },
      schedulerSpend: deploymentEntry(
        contracts.scheduler.spendingScriptHash,
        contracts.scheduler.spendingScript,
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
      requireReferenceScripts: false,
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
      invalidRangeInclusion.transactionsRoot,
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

    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
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
      requireReferenceScripts: false,
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

    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);
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
      requireReferenceScripts: false,
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
  }, 180_000);
});
