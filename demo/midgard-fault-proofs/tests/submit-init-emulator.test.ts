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
  buildNonExistentInputFaultProofContracts,
  ConfirmedState,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
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
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  makeHubOracleDatum,
  type MidgardValidators,
  type MintingValidator,
  NonExistentInputStep02Datum,
  NonExistentInputStep03Datum,
  NonExistentInputStep04Datum,
  parseFaultProofBlueprint,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
  resolveMintPolicyTxInfoRedeemerIndexFromPolicySet,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
  ScriptHashSchema,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  utxoToStateQueueUTxO,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
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
  buildMembershipProof,
  buildNonMembershipProof,
  computeTrieRoot,
  nativeTxFromCoreCompact,
  neSubmitInit,
  neSubmitStep01,
  neSubmitStep02,
  neSubmitStep03,
  neSubmitStep04,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  resolveProverSigner,
  submitInit,
  submitRemoveFraudulentBlock,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const realBlueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
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
  };

  return {
    hubOracle: {
      ...makeMintingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
      ...makeSpendingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
    },
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

  const fraudProofCatalogue = makeAuthenticatedValidator(
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
  const nonExistentInputContracts = await Effect.runPromise(
    buildNonExistentInputFaultProofContracts({
      blueprint: parseFaultProofBlueprint(realBlueprint),
      network,
      hubOraclePolicyId: hubOracle.policyId,
      fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
    }),
  );
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
      nonExistentInput: nonExistentInputContracts.nonExistentInput.firstStep,
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

const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 2n,
  activeOperatorsRoot: 3n,
} as const;

const ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX = {
  root: 0n,
  insertedNode: 1n,
} as const;

const SCHEDULER_APPOINTMENT_OUTPUT_INDEX = {
  scheduler: 0n,
} as const;

const COMMIT_OUTPUT_INDEX = {
  activeOperatorNode: 2n,
} as const;

const h32 = (byte: string): string => byte.repeat(32);

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
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
  readonly referenceByte: string;
  readonly outputByte: string;
  readonly witnessByte: string;
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
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
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

const phasMembershipRewardAddress = (script: Script): string => {
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(0, credential).to_address().to_bech32();
};

const registerPhasMembershipRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, "phas.membership.withdraw"),
  };
  const unsigned = await lucid
    .newTx()
    .register.Stake(phasMembershipRewardAddress(phasMembershipScript))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
  prevUtxosRoot = EMPTY_MERKLE_TREE_ROOT,
): Header => ({
  prevUtxosRoot,
  utxosRoot: EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot,
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  startTime: BigInt(now),
  endTime: BigInt(now + 1_000),
  prevHeaderHash: GENESIS_HEADER_HASH,
  operatorVkey,
  protocolVersion: GENESIS_PROTOCOL_VERSION,
});

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
    // state-queue root, active-operators root, then registered-operators root.
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
  const activationInputs = [initialActiveOperatorsRoot];
  const activationMintPolicies = [
    contracts.activeOperators.policyId,
    contracts.registeredOperators.policyId,
  ];
  const activeOperatorsActivateRedeemerTxInfoIndex =
    resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
      policyIds: activationMintPolicies,
      targetPolicyId: contracts.activeOperators.policyId,
      precedingSpendRedeemerCount: activationInputs.length,
    });
  const registeredOperatorsActivateRedeemerTxInfoIndex =
    resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
      policyIds: activationMintPolicies,
      targetPolicyId: contracts.registeredOperators.policyId,
      precedingSpendRedeemerCount: activationInputs.length,
    });
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
  const activationUnsigned = await lucid
    .newTx()
    .collectFrom(
      [initialActiveOperatorsRoot],
      Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
    )
    .mintAssets(
      { [activeOperatorNodeUnit]: 1n },
      Data.to(
        {
          ActivateOperator: {
            new_active_operator_key: header.operatorVkey,
            new_active_operator_bond_unlock_time: null,
            active_operator_anchor_element_input_index: ledgerOrderedIndex(
              activationInputs,
              initialActiveOperatorsRoot,
              "active-operators root activation input",
            ),
            active_operator_anchor_element_output_index:
              ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
            active_operator_inserted_node_output_index:
              ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.insertedNode,
            registered_operators_redeemer_index:
              registeredOperatorsActivateRedeemerTxInfoIndex,
          },
        },
        ActiveOperatorMintRedeemer,
      ),
    )
    .mintAssets(
      { [registeredOperatorActivationUnit]: 1n },
      Data.to(
        {
          ActivateOperator: {
            activating_operator: header.operatorVkey,
            anchor_element_input_index: 0n,
            removed_node_input_index: 0n,
            anchor_element_output_index:
              ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
            hub_oracle_ref_input_index: 0n,
            retired_operators_element_ref_input_index: 0n,
            active_operators_redeemer_index:
              activeOperatorsActivateRedeemerTxInfoIndex,
          },
        },
        RegisteredOperatorMintRedeemer,
      ),
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
  const commitScriptInputs = [stateQueueRoot.utxo, activeOperatorNode];
  const commitAllInputs = [commitFeeInput, ...commitScriptInputs];
  const commitRefInputs = [hubOracleUtxo, appointedSchedulerUtxo];
  const commitValidTo = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(lucid, Number(header.endTime)),
  );
  const latestBlockInputIndex = ledgerOrderedIndex(
    commitAllInputs,
    stateQueueRoot.utxo,
    "state-queue root input",
  );
  const activeOperatorInputIndex = ledgerOrderedIndex(
    commitAllInputs,
    activeOperatorNode,
    "active-operator input",
  );
  const stateQueueSpendRedeemerIndex = ledgerOrderedIndex(
    commitScriptInputs,
    stateQueueRoot.utxo,
    "state-queue spend redeemer",
  );
  const activeOperatorSpendRedeemerIndex = ledgerOrderedIndex(
    commitScriptInputs,
    activeOperatorNode,
    "active-operator spend redeemer",
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
        schedulerRefInputIndex: ledgerOrderedIndex(
          commitRefInputs,
          appointedSchedulerUtxo,
          "scheduler reference input",
        ),
        additionalRefInputs: [hubOracleUtxo],
        activeOperatorInput: activeOperatorNode,
        activeOperatorInputIndex,
        activeOperatorSpendRedeemer: {
          UpdateBondHoldNewState: {
            active_operator: header.operatorVkey,
            active_node_input_index: activeOperatorInputIndex,
            active_node_output_index: COMMIT_OUTPUT_INDEX.activeOperatorNode,
            hub_oracle_ref_input_index: ledgerOrderedIndex(
              commitRefInputs,
              hubOracleUtxo,
              "hub oracle reference input",
            ),
            state_queue_input_index: latestBlockInputIndex,
            state_queue_redeemer_index: stateQueueSpendRedeemerIndex,
          },
        },
        activeOperatorSpendRedeemerTxInfoIndex:
          activeOperatorSpendRedeemerIndex,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        latestBlockInputIndex,
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
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit,
    registeredOperatorsRoot,
  };
};

describe("submit-init emulator smoke", () => {
  it("mints the computation-thread token and completes double-spend fault proof", async () => {
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
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: makeHeader(
        funderPaymentCredential.hash,
        headerStartTime,
        transactionInclusion.transactionsRoot,
      ),
    });
    const { fraudulentBlockOutRef, headerHash } = setup;
    const deploymentEntry = (scriptHash: string, script: Script) => ({
      scriptHash,
      refScriptUTxO: null,
      contract: {
        type: script.type,
        cborHex: script.script,
      },
    });
    const deploymentInfo = {
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
      retiredOperatorsMint: {
        scriptHash: contracts.retiredOperators.policyId,
      },
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
    };

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
    });
    expect(removeResult.fraudulentHeaderHash).toBe(headerHash);
    expect(removeResult.fraudProver).toBe(proverPaymentCredential!.hash);

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
  }, 120_000);

  it("mints the fraud-proof token for a non-existent-input fault proof", async () => {
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
    // Steps 03/04 withdraw from the pexcludes exclusion validator, so its reward
    // account must be registered too.
    const pexcludesScript: Script = {
      type: "PlutusV3",
      script: getCompiledScript(realBlueprint, "pexcludes.exclusion.withdraw"),
    };
    {
      const unsigned = await funderLucid
        .newTx()
        .register.Stake(phasMembershipRewardAddress(pexcludesScript))
        .complete({ localUPLCEval: true });
      const signed = await unsigned.sign.withWallet().complete();
      await funderLucid.awaitTx(await signed.submit());
    }
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

    // --- Non-existent-input fixture --------------------------------------
    // A bad transaction that spends an input produced by a transaction that
    // does not exist in the block (so it is absent from both the prev-utxos
    // ledger and the block's transaction set).
    const missingOutRef = { transactionId: h32("de"), outputIndex: 0n };
    const missingInputCbor = outputReferenceCbor(missingOutRef);
    // A bad transaction whose single spend input is the non-existent UTxO,
    // committed by the node's native transaction root (same encoding as the
    // double-spend fixture above).
    const badNativeTx = makeNativeTx({
      spendInputCbors: [missingInputCbor],
      fee: 0n,
      referenceByte: "11",
      outputByte: "12",
      witnessByte: "18",
    });
    const badNativeTxId = computeMidgardNativeTxId(badNativeTx).toString("hex");
    const badNativeTxCompactCbor = encodeMidgardNativeTxCompact(
      badNativeTx.compact,
    ).toString("hex");
    const badNativeTxCompact = nativeTxFromCoreCompact(badNativeTx.compact);

    // Transactions trie committed by the node: keyed by the raw 32-byte native
    // tx id, valued by the native compact-tx CBOR.
    const txEntries = [
      {
        key: Buffer.from(badNativeTxId, "hex"),
        value: encodeMidgardNativeTxCompact(badNativeTx.compact),
      },
    ];
    const transactionsRoot = await computeTrieRoot(txEntries);
    const txMembershipProofCbor = await buildMembershipProof(
      txEntries,
      Buffer.from(badNativeTxId, "hex"),
    );

    // The committed block's prev-ledger root is the genesis (empty) utxo root,
    // so the missing input is proven absent against the empty ledger trie. The
    // ledger trie is keyed by the Cardano `TransactionInput` CBOR, and the
    // transactions trie by the raw 32-byte tx id.
    const prevUtxosRoot = EMPTY_MERKLE_TREE_ROOT;
    const ledgerNonMembershipProof = await buildNonMembershipProof(
      [],
      missingInputCbor,
    );
    const txsNonMembershipProof = await buildNonMembershipProof(
      txEntries,
      Buffer.from(missingOutRef.transactionId, "hex"),
    );
    const spendInputCbors = [missingInputCbor.toString("hex")];

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
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: makeHeader(
        funderPaymentCredential.hash,
        headerStartTime,
        transactionsRoot,
        prevUtxosRoot,
      ),
    });
    const { fraudulentBlockOutRef } = setup;

    const deploymentEntry = (scriptHash: string, script: Script) => ({
      scriptHash,
      refScriptUTxO: null,
      contract: { type: script.type, cborHex: script.script },
    });
    const deploymentInfo = {
      hubOracleMint: { scriptHash: contracts.hubOracle.policyId },
      fraudProofCatalogueMint: {
        scriptHash: contracts.fraudProofCatalogue.policyId,
        fraudProofCatalogue: catalogue,
      },
      fraudProofCatalogueSpend: {
        scriptHash: contracts.fraudProofCatalogue.spendingScriptHash,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofSpend: { scriptHash: contracts.fraudProof.spendingScriptHash },
      fraudProofNonExistentInput: {
        scriptHash: contracts.fraudProofs.nonExistentInput.spendingScriptHash,
      },
      stateQueueMint: deploymentEntry(
        contracts.stateQueue.policyId,
        contracts.stateQueue.mintingScript,
      ),
      stateQueueSpend: deploymentEntry(
        contracts.stateQueue.spendingScriptHash,
        contracts.stateQueue.spendingScript,
      ),
    };

    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;

    const initResult = await neSubmitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.nonExistentInput.categoryId}${initResult.fraudulentHeaderHash}`,
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
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: badNativeTxId,
        nativeTx: badNativeTxCompact,
        nativeTxCompactCbor: badNativeTxCompactCbor,
        txMembershipProofCbor,
      }),
      awaitConfirmation: true,
    });
    expect(step01Result.nativeTxId).toBe(badNativeTxId);
    expect(step01Result.badTxInputsHash).toBe(
      badNativeTxCompact.body.spend_inputs_hash,
    );

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(secondStepUtxo.datum!, NonExistentInputStep02Datum).data,
    ).toEqual({
      bad_tx_inputs_hash: badNativeTxCompact.body.spend_inputs_hash,
      blocks_prev_utxos_root: prevUtxosRoot,
      blocks_transactions_root: transactionsRoot,
    });

    const step02Result = await neSubmitStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      spendInputCbors,
      badInputIndex: 0n,
      awaitConfirmation: true,
    });
    expect(step02Result.missingInput).toEqual({
      tx_id: missingOutRef.transactionId,
      output_index: missingOutRef.outputIndex,
    });

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(thirdStepUtxo.datum!, NonExistentInputStep03Datum).data,
    ).toEqual({
      missing_input: {
        tx_id: missingOutRef.transactionId,
        output_index: missingOutRef.outputIndex,
      },
      blocks_prev_utxos_root: prevUtxosRoot,
      blocks_transactions_root: transactionsRoot,
    });

    const step03Result = await neSubmitStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      nonMembershipProofCbor: ledgerNonMembershipProof,
      awaitConfirmation: true,
    });
    expect(step03Result.missingInputTxId).toBe(missingOutRef.transactionId);

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(
      Data.from(fourthStepUtxo.datum!, NonExistentInputStep04Datum).data,
    ).toEqual({
      missing_input_tx_id: missingOutRef.transactionId,
      blocks_transactions_root: transactionsRoot,
    });

    const step04Result = await neSubmitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      nonMembershipProofCbor: txsNonMembershipProof,
      awaitConfirmation: true,
    });
    expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
      step04Result.computationThreadMintRedeemerIndex,
    );

    const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(remainingFourthStepUtxos).toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentCredential!.hash,
    });
    expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
    expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
      [step04Result.fraudProofUnit, 1n],
    ]);
  }, 120_000);
});
