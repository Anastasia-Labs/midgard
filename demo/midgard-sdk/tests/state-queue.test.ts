import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  CML,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type BuildTxWithRedeemer,
  type MintingPolicy,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  scriptHashToCredential,
  type SpendingValidator,
  toUnit,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorSpendRedeemer,
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  ConfirmedState,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  type Header as HeaderType,
  headerHashFromStateQueueUTxO,
  HUB_ORACLE_ASSET_NAME,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  selectPureAdaFeeInput,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const realBlueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);
const network: Network = "Preprod";
const outputReference = {
  transactionId: "44".repeat(32),
  outputIndex: 0n,
};
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

describe("state-queue fee input selection", () => {
  it("selects a pure-ADA fee input over a larger token-bearing input", async () => {
    const tokenBearing = {
      txHash: "aa".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: {
        lovelace: 10_000_000n,
        [`${"bb".repeat(28)}01`]: 1n,
      },
      datum: undefined,
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;
    const pureAda = {
      ...tokenBearing,
      txHash: "cc".repeat(32),
      outputIndex: 1,
      assets: { lovelace: 5_000_000n },
    } as UTxO;

    await expect(
      Effect.runPromise(selectPureAdaFeeInput([tokenBearing, pureAda])),
    ).resolves.toBe(pureAda);
  });

  it("rejects token-only operator wallet views for fee selection", async () => {
    const tokenBearing = {
      txHash: "dd".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: {
        lovelace: 10_000_000n,
        [`${"ee".repeat(28)}01`]: 1n,
      },
      datum: undefined,
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;

    const result = await Effect.runPromise(
      Effect.either(selectPureAdaFeeInput([tokenBearing])),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("pure-ADA");
    }
  });

  it("rejects datum and script-ref wallet outputs for fee selection", async () => {
    const withDatum = {
      txHash: "12".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: { lovelace: 10_000_000n },
      datum: "d87980",
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;
    const withScriptRef = {
      ...withDatum,
      txHash: "13".repeat(32),
      datum: undefined,
      scriptRef: { type: "Native", script: "8200" },
    } as UTxO;

    const result = await Effect.runPromise(
      Effect.either(selectPureAdaFeeInput([withDatum, withScriptRef])),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("pure-ADA");
    }
  });
});

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

type StateQueueTestContracts = {
  readonly hubOracle: AuthenticatedValidator;
  readonly stateQueue: AuthenticatedValidator;
  readonly scheduler: AuthenticatedValidator;
  readonly activeOperators: AuthenticatedValidator;
  readonly retiredOperators: AuthenticatedValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly settlement: AuthenticatedValidator;
};

const h32 = (byte: string): string => byte.repeat(32);

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

const makeMintingValidator = (mintingScriptCBOR: string) => {
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

const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeMintingValidator(mintingScriptCBOR),
  ...makeSpendingValidator(spendingScriptCBOR),
});

const alwaysTitle = (baseName: string, purpose: "spend" | "mint"): string =>
  `midgard.${baseName}_${purpose}.else`;

const alwaysScript = (
  blueprint: Blueprint,
  baseName: string,
  purpose: "spend" | "mint",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(baseName, purpose)),
  );

const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, baseName, "mint"),
    alwaysScript(blueprint, baseName, "spend"),
  );

const buildTestContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
): Promise<StateQueueTestContracts> => {
  const hubOracleScript = alwaysScript(alwaysBlueprint, "hub_oracle", "mint");
  const base = {
    hubOracle: makeAuthenticatedValidator(hubOracleScript, hubOracleScript),
    scheduler: alwaysAuthenticated(alwaysBlueprint, "scheduler"),
    activeOperators: alwaysAuthenticated(alwaysBlueprint, "active_operators"),
    retiredOperators: alwaysAuthenticated(alwaysBlueprint, "retired_operators"),
    fraudProof: alwaysAuthenticated(alwaysBlueprint, "fraud_proof"),
    settlement: alwaysAuthenticated(alwaysBlueprint, "settlement"),
  };
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(base.activeOperators.spendingScriptAddress).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const stateQueueMintingScriptCBOR = applyParamsToScript(
    getCompiledScript(realBlueprint, "state_queue.mint.mint"),
    [
      base.hubOracle.policyId,
      base.activeOperators.policyId,
      activeOperatorsAddressData,
      base.retiredOperators.policyId,
      base.scheduler.policyId,
      base.fraudProof.policyId,
      base.settlement.policyId,
    ],
  );
  const stateQueueMinting = makeMintingValidator(stateQueueMintingScriptCBOR);
  const stateQueueSpendingScriptCBOR = applyParamsToScript(
    getCompiledScript(realBlueprint, "state_queue.spend.spend"),
    [stateQueueMinting.policyId],
  );

  return {
    ...base,
    stateQueue: {
      ...stateQueueMinting,
      ...makeSpendingValidator(stateQueueSpendingScriptCBOR),
    },
  };
};

type LucidDataSchema = Parameters<typeof Data.to>[1];

const roundTrip = <A>(value: A, schema: LucidDataSchema): A =>
  Data.from(Data.to(value, schema), schema) as A;

const trieRootHex = (trie: Trie): string =>
  trie.hash === null || trie.hash === undefined
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

const outputReferenceCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeNativeTx = (
  spendInputCbors: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
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
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const buildTransactionsRoot = async (): Promise<string> => {
  const tx1 = makeNativeTx(
    [outputReferenceCbor(h32("11"), 0n), outputReferenceCbor(h32("22"), 1n)],
    1n,
  );
  const tx2 = makeNativeTx(
    [outputReferenceCbor(h32("33"), 0n), outputReferenceCbor(h32("44"), 2n)],
    2n,
  );
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);

  for (const nativeTx of [tx1, tx2]) {
    await trie.insert(
      computeMidgardNativeTxId(nativeTx),
      encodeMidgardNativeTxCompact(nativeTx.compact),
    );
  }

  return trieRootHex(trie);
};

const isOnlyLovelace = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  operator,
  schedulerStartTime,
  stateQueueGenesisTime,
  initValidFrom,
  initValidTo,
  fraudulentHeaderHash,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: StateQueueTestContracts;
  readonly nonceUtxo: UTxO;
  readonly operator: string;
  readonly schedulerStartTime: bigint;
  readonly stateQueueGenesisTime: bigint;
  readonly initValidFrom: bigint;
  readonly initValidTo: bigint;
  readonly fraudulentHeaderHash: string;
}): Promise<{
  readonly hubOracle: UTxO;
  readonly stateQueueRoot: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly retiredOperatorsRoot: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly fraudProof: UTxO;
}> => {
  const hubOracleAssets = {
    [toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME)]: 1n,
  };
  const schedulerAssets = {
    [toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME)]: 1n,
  };
  const stateQueueAssets = {
    [toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
  };
  const activeOperatorsAssets = {
    [toUnit(
      contracts.activeOperators.policyId,
      ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    )]: 1n,
  };
  const retiredOperatorsAssets = {
    [toUnit(
      contracts.retiredOperators.policyId,
      RETIRED_OPERATORS_ROOT_ASSET_NAME,
    )]: 1n,
  };
  const fraudProofAssetName =
    "00".repeat(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT) + fraudulentHeaderHash;
  const fraudProofAssets = {
    [toUnit(contracts.fraudProof.policyId, fraudProofAssetName)]: 1n,
  };
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: stateQueueGenesisTime,
    endTime: stateQueueGenesisTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  const rootNodeDatum = (data: unknown): string =>
    encodeLinkedListNodeView({
      key: "Empty",
      next: "Empty",
      data: data as never,
    });

  const unsigned = await lucid
    .newTx()
    .validFrom(Number(initValidFrom))
    .validTo(Number(initValidTo))
    .collectFrom([nonceUtxo])
    .mintAssets(hubOracleAssets, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      { kind: "inline", value: Data.void() },
      hubOracleAssets,
    )
    .mintAssets(schedulerAssets, Data.void())
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            ActiveOperator: {
              operator,
              start_time: schedulerStartTime,
            },
          },
          SchedulerDatum,
        ),
      },
      schedulerAssets,
    )
    .mintAssets(
      stateQueueAssets,
      Data.to({ Init: { output_index: 2n } }, StateQueueRedeemer),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: rootNodeDatum(Data.castTo(confirmedState, ConfirmedState)),
      },
      stateQueueAssets,
    )
    .mintAssets(activeOperatorsAssets, Data.void())
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      { kind: "inline", value: rootNodeDatum("") },
      activeOperatorsAssets,
    )
    .mintAssets(retiredOperatorsAssets, Data.void())
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      { kind: "inline", value: rootNodeDatum("") },
      retiredOperatorsAssets,
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      { kind: "inline", value: Data.void() },
      { lovelace: 20_000_000n },
    )
    .mintAssets(fraudProofAssets, Data.void())
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to({ fraud_prover: operator }, FraudProofTokenDatum),
      },
      fraudProofAssets,
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.scheduler.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .attach.MintingPolicy(contracts.activeOperators.mintingScript)
    .attach.MintingPolicy(contracts.retiredOperators.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  const [stateQueueRoot] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    Object.keys(stateQueueAssets)[0]!,
  );
  const [hubOracle] = await lucid.utxosAtWithUnit(
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    Object.keys(hubOracleAssets)[0]!,
  );
  const [scheduler] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    Object.keys(schedulerAssets)[0]!,
  );
  const [activeOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    Object.keys(activeOperatorsAssets)[0]!,
  );
  const [retiredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.retiredOperators.spendingScriptAddress,
    Object.keys(retiredOperatorsAssets)[0]!,
  );
  const [fraudProof] = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    Object.keys(fraudProofAssets)[0]!,
  );
  const activeOperatorInput = (
    await lucid.utxosAt(contracts.activeOperators.spendingScriptAddress)
  ).find(isOnlyLovelace);

  if (
    hubOracle === undefined ||
    stateQueueRoot === undefined ||
    scheduler === undefined ||
    activeOperatorsRoot === undefined ||
    retiredOperatorsRoot === undefined ||
    activeOperatorInput === undefined ||
    fraudProof === undefined
  ) {
    throw new Error("Setup transaction did not produce all expected UTxOs");
  }

  return {
    hubOracle,
    stateQueueRoot,
    scheduler,
    activeOperatorsRoot,
    retiredOperatorsRoot,
    activeOperatorInput,
    fraudProof,
  };
};

const makeCommitActiveOperatorRedeemer = ({
  contracts,
  operator,
  activeOperatorInput,
  hubOracle,
  continuedActiveOperatorDatum,
}: {
  readonly contracts: StateQueueTestContracts;
  readonly operator: string;
  readonly activeOperatorInput: UTxO;
  readonly hubOracle: UTxO;
  readonly continuedActiveOperatorDatum: string;
}): BuildTxWithRedeemer =>
  ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: operator,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorInput,
            "emulator commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              output.datum === continuedActiveOperatorDatum,
            "emulator commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracle,
            "emulator commit hub oracle",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "emulator commit state queue mint",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;

const submitCommitHeaderTx = async ({
  lucid,
  contracts,
  anchor,
  header,
  operator,
  scheduler,
  hubOracle,
  activeOperatorInput,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: StateQueueTestContracts;
  readonly anchor: StateQueueUTxO;
  readonly header: HeaderType;
  readonly operator: string;
  readonly scheduler: UTxO;
  readonly hubOracle: UTxO;
  readonly activeOperatorInput: UTxO;
}): Promise<{
  readonly block: StateQueueUTxO;
  readonly activeOperatorInput: UTxO;
}> => {
  const continuedActiveOperatorDatum = Data.void();
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: anchor,
        newHeader: header,
        schedulerRefInput: scheduler,
        additionalRefInputs: [hubOracle],
        activeOperatorInput,
        activeOperatorSpendRedeemer: makeCommitActiveOperatorRedeemer({
          contracts,
          operator,
          activeOperatorInput,
          hubOracle,
          continuedActiveOperatorDatum,
        }),
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorInput.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    ),
  );
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const blockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const [blockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    blockUnit,
  );
  const nextActiveOperatorInput = (
    await lucid.utxosAt(contracts.activeOperators.spendingScriptAddress)
  ).find(isOnlyLovelace);
  if (blockUtxo === undefined || nextActiveOperatorInput === undefined) {
    throw new Error("Commit transaction did not produce expected UTxOs");
  }
  return {
    block: await Effect.runPromise(
      utxoToStateQueueUTxO(blockUtxo, contracts.stateQueue.policyId),
    ),
    activeOperatorInput: nextActiveOperatorInput,
  };
};

describe("state-queue ABI", () => {
  it("round-trips CommitBlockHeader and RemoveFraudulentBlockHeader", () => {
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: "11".repeat(28),
            scheduler_ref_input_index: 3n,
            active_operators_input_index: 4n,
            active_operators_redeemer_index: 5n,
          },
        },
        StateQueueRedeemer,
      ),
    ).toMatchObject({
      CommitBlockHeader: { active_operators_redeemer_index: 5n },
    });
    expect(roundTrip("LinkedListMutation", StateQueueSpendRedeemer)).toBe(
      "LinkedListMutation",
    );

    const removeRedeemer = {
      RemoveFraudulentBlockHeader: {
        fraudulent_operator: "22".repeat(28),
        fraudulent_blocks_header_hash: "33".repeat(28),
        slashing_approach: {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index: 0n,
            retired_operators_element_ref_input_index: 1n,
          },
        },
        fraud_proof_ref_input_index: 3n,
        block_removal_approach: {
          RemoveLastFraudulentBlock: {
            anchor_element_input_outref: outputReference,
            anchor_element_output_index: 5n,
          },
        },
      },
    };
    expect(roundTrip(removeRedeemer, StateQueueRedeemer)).toEqual(
      removeRedeemer,
    );

    expect(
      roundTrip(
        {
          RemoveFraudulentBlockHeader: {
            ...removeRedeemer.RemoveFraudulentBlockHeader,
            slashing_approach: {
              SlashActiveOperator: {
                active_operators_redeemer_index: 6n,
              },
            },
            block_removal_approach: {
              RemoveFraudulentBlocksLink: {
                fraudulent_node_input_outref: outputReference,
                fraudulent_node_output_index: 7n,
              },
            },
          },
        },
        StateQueueRedeemer,
      ),
    ).toMatchObject({
      RemoveFraudulentBlockHeader: {
        slashing_approach: {
          SlashActiveOperator: {
            active_operators_redeemer_index: 6n,
          },
        },
      },
    });
  });
});

describe("state-queue emulator builders", () => {
  it("commits a block carrying a native transactions_root and removes it through the real tail-removal path", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 60_000_000_000n });
    const emulator = new Emulator([funder], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(funder.seedPhrase);

    const contracts = await buildTestContracts(realBlueprint, alwaysBlueprint);
    // This is a state-queue-focused smoke test. Non-state-queue contracts are
    // scaffolded with always-succeeds scripts, while the state-queue validator
    // still reads their datums/assets/redeemers through its real checks.
    const funderAddress = await lucid.wallet().address();
    const paymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
      throw new Error("Expected emulator wallet to expose a payment key hash");
    }
    const operator = paymentCredential.hash;
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 10_000n;
    const genesisTime = initValidTo - 1n;
    const transactionsRoot = await buildTransactionsRoot();
    const header: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot,
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: GENESIS_PROTOCOL_VERSION,
    };
    const headerHash = await Effect.runPromise(hashBlockHeader(header));
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected wallet to expose a setup nonce UTxO");
    }

    const setup = await submitSetupTx({
      lucid,
      contracts,
      nonceUtxo,
      operator,
      schedulerStartTime: genesisTime,
      stateQueueGenesisTime: genesisTime,
      initValidFrom,
      initValidTo,
      fraudulentHeaderHash: headerHash,
    });
    const stateQueueRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(setup.stateQueueRoot, contracts.stateQueue.policyId),
    );
    const commit = await submitCommitHeaderTx({
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      activeOperatorInput: setup.activeOperatorInput,
    });

    const blockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
    const rootUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_ROOT_ASSET_NAME,
    );
    const [continuedRootUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      rootUnit,
    );
    if (continuedRootUtxo === undefined) {
      throw new Error(
        "Commit transaction did not preserve the state-queue root",
      );
    }
    const committedBlock = commit.block;
    const continuedRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
    );
    const committedHeader = await Effect.runPromise(
      getHeaderFromStateQueueDatum(committedBlock.datum),
    );
    expect(committedHeader.transactionsRoot).toBe(transactionsRoot);
    await expect(
      Effect.runPromise(headerHashFromStateQueueUTxO(committedBlock)),
    ).resolves.toBe(headerHash);
    expect(continuedRoot.datum.next).toEqual({ Key: { key: headerHash } });

    const removeTx = incompleteRemoveLastFraudulentBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: continuedRoot,
        fraudulentBlockUTxO: committedBlock,
        fraudulentOperator: operator,
        fraudulentBlocksHeaderHash: headerHash,
        fraudProofRefInput: setup.fraudProof,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    );
    const removeUnsigned = await removeTx.complete({ localUPLCEval: true });
    const removeSigned = await removeUnsigned.sign.withWallet().complete();
    await lucid.awaitTx(await removeSigned.submit());

    await expect(
      lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        blockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      rootUnit,
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
  });

  it("removes the immediate successor of a fraud-proved non-tail block", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 60_000_000_000n });
    const emulator = new Emulator([funder], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(funder.seedPhrase);

    const contracts = await buildTestContracts(realBlueprint, alwaysBlueprint);
    const funderAddress = await lucid.wallet().address();
    const paymentCredential =
      getAddressDetails(funderAddress).paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
      throw new Error("Expected emulator wallet to expose a payment key hash");
    }
    const operator = paymentCredential.hash;
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 10_000n;
    const genesisTime = initValidTo - 1n;
    const firstHeader: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: await buildTransactionsRoot(),
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: GENESIS_PROTOCOL_VERSION,
    };
    const firstHeaderHash = await Effect.runPromise(
      hashBlockHeader(firstHeader),
    );
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected wallet to expose a setup nonce UTxO");
    }
    const setup = await submitSetupTx({
      lucid,
      contracts,
      nonceUtxo,
      operator,
      schedulerStartTime: genesisTime,
      stateQueueGenesisTime: genesisTime,
      initValidFrom,
      initValidTo,
      fraudulentHeaderHash: firstHeaderHash,
    });
    const stateQueueRoot = await Effect.runPromise(
      utxoToStateQueueUTxO(setup.stateQueueRoot, contracts.stateQueue.policyId),
    );
    const firstCommit = await submitCommitHeaderTx({
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header: firstHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      activeOperatorInput: setup.activeOperatorInput,
    });
    const secondHeader: HeaderType = {
      ...firstHeader,
      prevUtxosRoot: firstHeader.utxosRoot,
      startTime: firstHeader.endTime,
      endTime: firstHeader.endTime + 1_000n,
      prevHeaderHash: firstHeaderHash,
    };
    const secondHeaderHash = await Effect.runPromise(
      hashBlockHeader(secondHeader),
    );
    const secondCommit = await submitCommitHeaderTx({
      lucid,
      contracts,
      anchor: firstCommit.block,
      header: secondHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      activeOperatorInput: firstCommit.activeOperatorInput,
    });

    const firstBlockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + firstHeaderHash,
    );
    const secondBlockUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + secondHeaderHash,
    );
    const [continuedFirstBlockUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      firstBlockUnit,
    );
    if (continuedFirstBlockUtxo === undefined) {
      throw new Error("Second commit did not preserve the first block");
    }
    const continuedFirstBlock = await Effect.runPromise(
      utxoToStateQueueUTxO(
        continuedFirstBlockUtxo,
        contracts.stateQueue.policyId,
      ),
    );
    expect(continuedFirstBlock.datum.next).toEqual({
      Key: { key: secondHeaderHash },
    });

    const removeSuccessorTx = incompleteRemoveFraudulentBlocksLinkTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        fraudulentBlockUTxO: continuedFirstBlock,
        removedBlockUTxO: secondCommit.block,
        fraudulentOperator: operator,
        fraudulentBlocksHeaderHash: firstHeaderHash,
        fraudProofRefInput: setup.fraudProof,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    );
    const removeUnsigned = await removeSuccessorTx.complete({
      localUPLCEval: true,
    });
    const removeSigned = await removeUnsigned.sign.withWallet().complete();
    await lucid.awaitTx(await removeSigned.submit());

    await expect(
      lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        secondBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalFirstBlockUtxo] = await lucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      firstBlockUnit,
    );
    if (finalFirstBlockUtxo === undefined) {
      throw new Error("Successor removal did not preserve the first block");
    }
    const finalFirstBlock = await Effect.runPromise(
      utxoToStateQueueUTxO(finalFirstBlockUtxo, contracts.stateQueue.policyId),
    );
    expect(finalFirstBlock.datum.next).toBe("Empty");
  });
});
