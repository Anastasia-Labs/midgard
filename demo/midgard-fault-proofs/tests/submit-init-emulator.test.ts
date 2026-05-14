import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  CML,
  Constr,
  Data,
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  applyDoubleCborEncoding,
  applyParamsToScript,
  credentialToAddress,
  generateEmulatorAccount,
  getAddressDetails,
  mintingPolicyToId,
  scriptHashToCredential,
  toUnit,
  validatorToAddress,
  validatorToScriptHash,
  type MintingPolicy,
  type Network,
  type Script,
  type SpendingValidator,
  type UTxO,
  type WithdrawalValidator,
} from "@lucid-evolution/lucid";
import {
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  FraudProofCatalogueDatum,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  HUB_ORACLE_ASSET_NAME,
  Header,
  HubOracleDatum,
  MerkleRoot,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  ScriptHashSchema,
  buildDoubleSpendFaultProofContracts,
  encodeLinkedListNodeView,
  hashBlockHeader,
  makeHubOracleDatum,
  parseFaultProofBlueprint,
  type AuthenticatedValidator,
  type FraudProofCatalogueDeploymentInfo,
  type FraudProofs,
  type MidgardValidators,
  type MintingValidator,
  type SpendingValidator as SdkSpendingValidator,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
  parseSubmitStep02TxInclusion,
  parseSubmitStep03Tx1Inputs,
  parseSubmitStep04Tx2Inputs,
  resolveProverSigner,
  submitInit,
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
  const found = blueprint.validators.find((validator) => validator.title === title);
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

const makeAlwaysSucceedsContracts = (blueprint: Blueprint): MidgardValidators => {
  const reserve = {
    ...makeSpendingValidator(alwaysScript(blueprint, "midgard", "reserve", "spend")),
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
    fraudProofCatalogue: alwaysAuthenticated(blueprint, "fraud_proof_catalogue"),
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
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = makeMintingValidator(
    applyParamsToScript(
      getCompiledScript(realBlueprint, "hub_oracle.mint.mint"),
      [
        new Constr(0, [nonceUtxo.txHash.toLowerCase(), BigInt(nonceUtxo.outputIndex)]),
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
  const doubleSpendContracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(realBlueprint),
      network,
      hubOraclePolicyId: hubOracle.policyId,
      fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
    }),
  );

  return {
    ...withCatalogue,
    fraudProof: {
      ...doubleSpendContracts.fraudProof,
      policyId: doubleSpendContracts.fraudProof.policyId,
      mintingScript: doubleSpendContracts.fraudProof.mintingScript,
      mintingScriptCBOR: doubleSpendContracts.fraudProof.mintingScriptCBOR,
    },
    fraudProofs: {
      ...withCatalogue.fraudProofs,
      doubleSpend: doubleSpendContracts.doubleSpend.firstStep,
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
  trie.hash === null || trie.hash === undefined
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

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
      referenceInputsPreimageCbor: encodeCbor([Buffer.from(h32(referenceByte), "hex")]),
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
      addrTxWitsPreimageCbor: encodeCbor([Buffer.from(h32(witnessByte), "hex")]),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const compactTxEntry = (
  nativeTx: MidgardNativeTxFull,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxIdFromFull(nativeTx).toString("hex"),
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
      Buffer.from(encodeMidgardNativeTxCompact(
        entry === tx1 ? tx1Native.compact : tx2Native.compact,
      )),
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
): Header => ({
  prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot,
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  startTime: BigInt(now),
  endTime: BigInt(now + 1_000),
  prevHeaderHash: "00".repeat(28),
  operatorVkey,
  protocolVersion: 0n,
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
}): Promise<{ readonly fraudulentBlockOutRef: string; readonly headerHash: string }> => {
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

  const unsigned = await lucid
    .newTx()
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
    .mintAssets({ [fraudProofCatalogueUnit]: 1n }, Data.void())
    .pay.ToAddressWithData(
      contracts.fraudProofCatalogue.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(catalogue.root, FraudProofCatalogueDatum),
      },
      { [fraudProofCatalogueUnit]: 1n },
    )
    .mintAssets({ [stateQueueBlockUnit]: 1n }, Data.void())
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: { Key: { key: headerHash } },
          next: "Empty",
          data: Data.castTo(header, Header),
        }),
      },
      { [stateQueueBlockUnit]: 1n },
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.fraudProofCatalogue.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  return {
    fraudulentBlockOutRef: `${txHash}#2`,
    headerHash,
  };
};

describe("submit-init emulator smoke", () => {
  it(
    "mints the computation-thread token and completes double-spend fault proof",
    async () => {
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
      const catalogue = await buildCatalogueDeploymentInfo(
        contracts.fraudProofs,
      );
      const transactionInclusion = await buildTransactionInclusionFixture();
      const funderAddress = await funderLucid.wallet().address();
      const funderPaymentCredential =
        getAddressDetails(funderAddress).paymentCredential;
      if (
        funderPaymentCredential === undefined ||
        funderPaymentCredential.type !== "Key"
      ) {
        throw new Error("Expected funder wallet to expose a payment key hash");
      }
      const { fraudulentBlockOutRef, headerHash } = await submitSetupTx({
        lucid: funderLucid,
        contracts,
        nonceUtxo,
        catalogue,
        header: makeHeader(
          funderPaymentCredential.hash,
          emulator.now(),
          transactionInclusion.transactionsRoot,
        ),
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
        stateQueueMint: { scriptHash: contracts.stateQueue.policyId },
      };

      const result = await submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        }),
        fraudulentBlockOutRef,
        awaitConfirmation: true,
      });

      expect(result.txHash).toHaveLength(64);
      expect(result.fraudulentHeaderHash).toBe(headerHash);
      expect(result.computationThreadAssetName).toBe(
        `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
      );

      const firstStepUtxos = await proverLucid.utxosAtWithUnit(
        result.firstStepAddress,
        result.computationThreadUnit,
      );
      expect(firstStepUtxos).toHaveLength(1);
      const stepDatum = Data.from(
        firstStepUtxos[0]!.datum!,
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
      expect(firstStepUtxos[0]!.assets[result.computationThreadUnit]).toBe(1n);
      expect(
        Object.entries(firstStepUtxos[0]!.assets).filter(
          ([unit, amount]) => unit !== "lovelace" && amount > 0n,
        ),
      ).toEqual([[result.computationThreadUnit, 1n]]);

      const step01Result = await submitStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        }),
        threadOutRef: `${firstStepUtxos[0]!.txHash}#${firstStepUtxos[0]!.outputIndex.toString()}`,
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
      const secondStepUtxos = await proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        result.computationThreadUnit,
      );
      expect(secondStepUtxos).toHaveLength(1);
      const step02Datum = Data.from(
        secondStepUtxos[0]!.datum!,
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
      expect(secondStepUtxos[0]!.assets[result.computationThreadUnit]).toBe(1n);

      const step02Result = await submitStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        }),
        threadOutRef: `${secondStepUtxos[0]!.txHash}#${secondStepUtxos[0]!.outputIndex.toString()}`,
        stateQueueBlockOutRef: fraudulentBlockOutRef,
        txInclusion: parseSubmitStep02TxInclusion(
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
      const thirdStepUtxos = await proverLucid.utxosAtWithUnit(
        step02Result.thirdStepAddress,
        result.computationThreadUnit,
      );
      expect(thirdStepUtxos).toHaveLength(1);
      const step03Datum = Data.from(
        thirdStepUtxos[0]!.datum!,
        DoubleSpendStep03Datum,
      );
      expect(step03Datum).toEqual({
        fraud_prover: proverPaymentCredential!.hash,
        data: {
          verified_tx1_spend_inputs_hash:
            transactionInclusion.tx1.nativeTx.body.spend_inputs_hash,
          verified_tx2_spend_inputs_hash:
            transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
        },
      });
      expect(thirdStepUtxos[0]!.assets[result.computationThreadUnit]).toBe(1n);

      const step03Result = await submitStep03({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        }),
        threadOutRef: `${thirdStepUtxos[0]!.txHash}#${thirdStepUtxos[0]!.outputIndex.toString()}`,
        tx1SpendInputCbors: parseSubmitStep03Tx1Inputs(
          transactionInclusion.tx1SpendInputCbors,
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
      expect(step03Result.doubleSpentInputCbor).toEqual(
        transactionInclusion.tx1SpendInputCbors[1],
      );
      const remainingThirdStepUtxos = await proverLucid.utxosAtWithUnit(
        step02Result.thirdStepAddress,
        result.computationThreadUnit,
      );
      expect(remainingThirdStepUtxos).toHaveLength(0);
      const fourthStepUtxos = await proverLucid.utxosAtWithUnit(
        step03Result.fourthStepAddress,
        result.computationThreadUnit,
      );
      expect(fourthStepUtxos).toHaveLength(1);
      const step04Datum = Data.from(
        fourthStepUtxos[0]!.datum!,
        DoubleSpendStep04Datum,
      );
      expect(step04Datum).toEqual({
        fraud_prover: proverPaymentCredential!.hash,
        data: {
          verified_tx2_spend_inputs_hash:
            transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
          double_spent_input_cbor: transactionInclusion.tx1SpendInputCbors[1],
        },
      });
      expect(fourthStepUtxos[0]!.assets[result.computationThreadUnit]).toBe(1n);

      const step04Result = await submitStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        }),
        threadOutRef: `${fourthStepUtxos[0]!.txHash}#${fourthStepUtxos[0]!.outputIndex.toString()}`,
        tx2SpendInputCbors: parseSubmitStep04Tx2Inputs(
          transactionInclusion.tx2SpendInputCbors,
        ),
        doubleSpentInputIndex: 1n,
        awaitConfirmation: true,
      });

      expect(step04Result.txHash).toHaveLength(64);
      expect(step04Result.verifiedTx2SpendInputsHash).toBe(
        transactionInclusion.tx2.nativeTx.body.spend_inputs_hash,
      );
      expect(step04Result.doubleSpentInputIndex).toBe(1);
      expect(step04Result.doubleSpentInputCbor).toEqual(
        transactionInclusion.tx2SpendInputCbors[1],
      );
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
      const fraudProofUtxos = await proverLucid.utxosAtWithUnit(
        step04Result.fraudProofAddress,
        step04Result.fraudProofUnit,
      );
      expect(fraudProofUtxos).toHaveLength(1);
      const fraudProofDatum = Data.from(
        fraudProofUtxos[0]!.datum!,
        FraudProofTokenDatum,
      );
      expect(fraudProofDatum).toEqual({
        fraud_prover: proverPaymentCredential!.hash,
      });
      expect(fraudProofUtxos[0]!.assets[step04Result.fraudProofUnit]).toBe(1n);
      expect(
        Object.entries(fraudProofUtxos[0]!.assets).filter(
          ([unit, amount]) => unit !== "lovelace" && amount > 0n,
        ),
      ).toEqual([[step04Result.fraudProofUnit, 1n]]);
    },
    120_000,
  );
});
