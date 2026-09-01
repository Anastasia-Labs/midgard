import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type BuildTxWithRedeemer,
  CML,
  credentialToAddress,
  Data,
  type Data as LucidData,
  Emulator,
  fromText,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
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
  castConfirmedStateToData,
  ConfirmedState,
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  encodeStateQueueYieldRedeemerV1,
  fetchSortedStateQueueUTxOsProgram,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  headerHashFromStateQueueUTxO,
  type HeaderV1 as HeaderType,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  type LinkedListNodeView,
  makeGenesisConfirmedStateV1,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOperatorWalletInputs,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  resolveFraudProverRewardOutputIndex,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  scriptRewardAddress,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
  updateLatestBlocksDatumAndGetTheNewHeaderV1Program,
  utxoToStateQueueUTxO,
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
const outputReference = {
  transactionId: "44".repeat(32),
  outputIndex: 0n,
};

describe("fraud-prover reward builder exactness", () => {
  const proverAddress = "addr_test1vq53_prover";
  const reward = {
    proverEnterpriseAddress: proverAddress,
    lovelace: 400_000_000n,
  } as const;
  const output = (overrides: Record<string, unknown> = {}) => ({
    address: proverAddress,
    assets: { lovelace: reward.lovelace },
    datum: undefined,
    datumHash: undefined,
    scriptRef: undefined,
    ...overrides,
  });
  const context = (outputs: readonly unknown[]) =>
    ({ outputs }) as unknown as Parameters<BuildTxWithRedeemer>[0];

  it("accepts exactly one ADA-only NoDatum/no-reference-script prover output", () => {
    expect(
      resolveFraudProverRewardOutputIndex(
        context([output({ address: "addr_test1vother" }), output()]),
        reward,
        "Q53 reward",
      ),
    ).toBe(1n);
  });

  it.each([
    ["underpayment", output({ assets: { lovelace: reward.lovelace - 1n } })],
    ["overpayment", output({ assets: { lovelace: reward.lovelace + 1n } })],
    [
      "token",
      output({ assets: { lovelace: reward.lovelace, ["ab".repeat(28)]: 1n } }),
    ],
    ["inline datum", output({ datum: "d87980" })],
    ["datum hash", output({ datumHash: "ab".repeat(32) })],
    [
      "reference script",
      output({ scriptRef: { type: "PlutusV3", script: "00" } }),
    ],
  ])("rejects %s mutation", (_label, mutated) => {
    expect(() =>
      resolveFraudProverRewardOutputIndex(
        context([mutated]),
        reward,
        "Q53 reward",
      ),
    ).toThrow();
  });

  it("rejects any second output to the prover credential", () => {
    expect(() =>
      resolveFraudProverRewardOutputIndex(
        context([output(), output({ assets: { lovelace: 2_000_000n } })]),
        reward,
        "Q53 reward",
      ),
    ).toThrow(/exactly one output/);
  });
});
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

describe("state-queue operator funding inputs", () => {
  it("retains token-bearing preset wallet inputs", async () => {
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
      Effect.runPromise(
        requireOperatorWalletInputs(
          [tokenBearing, pureAda],
          "state_queue commit tx",
        ),
      ),
    ).resolves.toEqual([tokenBearing, pureAda]);
  });

  it("accepts token-only operator wallet views for preset funding", async () => {
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

    await expect(
      Effect.runPromise(
        requireOperatorWalletInputs([tokenBearing], "state_queue commit tx"),
      ),
    ).resolves.toEqual([tokenBearing]);
  });

  it("rejects empty operator wallet views for preset funding", async () => {
    const result = await Effect.runPromise(
      Effect.either(requireOperatorWalletInputs([], "state_queue commit tx")),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("operator wallet inputs");
    }
  });

  it("does not impose datum or script-ref filters on preset wallet inputs", async () => {
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

    await expect(
      Effect.runPromise(
        requireOperatorWalletInputs(
          [withDatum, withScriptRef],
          "state_queue commit tx",
        ),
      ),
    ).resolves.toEqual([withDatum, withScriptRef]);
  });
});

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  readonly parameters?: readonly unknown[];
};

type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

type StateQueueTestContracts = {
  readonly hubOracle: AuthenticatedValidator;
  readonly correctionLock: SdkSpendingValidator;
  readonly computationThread: AuthenticatedValidator;
  readonly daAttestation: AuthenticatedValidator;
  readonly stateQueue: AuthenticatedValidator;
  readonly commitYield: SdkSpendingValidator;
  readonly fraudRemovalYield: SdkSpendingValidator;
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

const applyAllBlueprintParamsToScript = (
  blueprint: Blueprint,
  title: string,
  params: LucidData[],
): string => {
  const validator = blueprint.validators.find(
    (candidate) => candidate.title === title,
  );
  if (validator === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  if (validator.parameters === undefined) {
    throw new Error(`Validator "${title}" does not declare parameters`);
  }
  if (params.length !== validator.parameters.length) {
    throw new Error(
      `Validator "${title}" requires exactly ${validator.parameters.length.toString()} parameters, received ${params.length.toString()}`,
    );
  }
  return applyParamsToScript(validator.compiledCode, params);
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
    daAttestation: alwaysAuthenticated(alwaysBlueprint, "state_queue"),
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
  const computationThread = alwaysAuthenticated(alwaysBlueprint, "payout");
  const availabilityChallenge = alwaysAuthenticated(
    alwaysBlueprint,
    "escape_hatch",
  );
  const correctionLock = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "correction_lock.spend.spend",
      [base.hubOracle.policyId, availabilityChallenge.policyId],
    ),
  );
  const stateQueueMintingScriptCBOR = applyAllBlueprintParamsToScript(
    realBlueprint,
    "state_queue.mint.mint",
    [
      base.hubOracle.policyId,
      correctionLock.spendingScriptHash,
      base.activeOperators.policyId,
      activeOperatorsAddressData,
      base.retiredOperators.policyId,
      base.scheduler.policyId,
      base.fraudProof.policyId,
      base.settlement.policyId,
      base.daAttestation.policyId,
      availabilityChallenge.policyId,
      base.scheduler.policyId,
    ],
  );
  const stateQueueMinting = makeMintingValidator(stateQueueMintingScriptCBOR);
  const stateQueueSpendingScriptCBOR = applyAllBlueprintParamsToScript(
    realBlueprint,
    "state_queue.spend.spend",
    [
      stateQueueMinting.policyId,
      base.daAttestation.policyId,
      availabilityChallenge.policyId,
    ],
  );
  const commitYield = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "state_queue_yields.commit.withdraw",
      [
        stateQueueMinting.policyId,
        base.hubOracle.policyId,
        correctionLock.spendingScriptHash,
        base.activeOperators.policyId,
        activeOperatorsAddressData,
        base.scheduler.policyId,
        base.daAttestation.policyId,
      ],
    ),
  );
  const fraudRemovalYield = makeSpendingValidator(
    applyAllBlueprintParamsToScript(
      realBlueprint,
      "state_queue_yields.remove_fraudulent.withdraw",
      [
        stateQueueMinting.policyId,
        base.hubOracle.policyId,
        correctionLock.spendingScriptHash,
        base.activeOperators.policyId,
        base.retiredOperators.policyId,
        base.fraudProof.policyId,
      ],
    ),
  );

  return {
    ...base,
    correctionLock,
    commitYield,
    fraudRemovalYield,
    computationThread,
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
): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
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
      computeMidgardNativeTxIdV1(nativeTx),
      encodeMidgardNativeTxCompactV1(nativeTx.compact),
    );
  }

  return trieRootHex(trie);
};

const TWO_TRANSACTION_HEADER_COMMITMENTS = {
  transitionTraceRoot: h32("55"),
  eventToStepRoot: h32("66"),
  validationTracesRoot: h32("77"),
  l2TransactionCount: 2n,
  totalEventCount: 2n,
  transitionStepCount: 2n,
  validationTraceCount: 2n,
} as const;

describe("state-queue header validation boundary", () => {
  it("validates every source root/count pair before building a header", async () => {
    const lucid = {
      wallet: () => ({
        address: async () =>
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
      }),
    } as unknown as Parameters<
      typeof updateLatestBlocksDatumAndGetTheNewHeaderV1Program
    >[0];
    const latestBlocksDatum: LinkedListNodeView = {
      key: "Empty",
      next: "Empty",
      data: castConfirmedStateToData(
        makeGenesisConfirmedStateV1(10n),
      ) as LinkedListNodeView["data"],
    };
    const sourceRoots = {
      withdrawalsRoot: h32("11"),
      transactionsRoot: h32("22"),
      depositsRoot: h32("33"),
    };
    const commitments = {
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
      transitionTraceRoot: h32("44"),
      eventToStepRoot: h32("55"),
      validationTracesRoot: h32("66"),
      withdrawalCount: 1n,
      l2TransactionCount: 1n,
      depositCount: 1n,
      totalEventCount: 3n,
      transitionStepCount: 3n,
      validationTraceCount: 1n,
    };
    const updateProgram = (roots: typeof sourceRoots = sourceRoots) =>
      updateLatestBlocksDatumAndGetTheNewHeaderV1Program(
        lucid,
        latestBlocksDatum,
        h32("77"),
        roots.transactionsRoot,
        roots.depositsRoot,
        roots.withdrawalsRoot,
        commitments,
        11n,
        {
          blockSlot: 0n,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
        },
      );
    const update = (roots: typeof sourceRoots = sourceRoots) =>
      Effect.runPromise(updateProgram(roots));

    await expect(update()).resolves.toMatchObject({
      header: {
        withdrawalsRoot: sourceRoots.withdrawalsRoot,
        transactionsRoot: sourceRoots.transactionsRoot,
        depositsRoot: sourceRoots.depositsRoot,
      },
    });

    for (const [label, rootField] of [
      ["withdrawals", "withdrawalsRoot"],
      ["transactions", "transactionsRoot"],
      ["deposits", "depositsRoot"],
    ] as const) {
      const invalid = await Effect.runPromise(
        Effect.either(
          updateProgram({
            ...sourceRoots,
            [rootField]: EMPTY_MERKLE_TREE_ROOT,
          }),
        ),
      );
      expect(invalid._tag).toBe("Left");
      if (invalid._tag === "Left") {
        expect(String(invalid.left.cause)).toContain(`${label}_root`);
      }
    }
  });
});

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
  readonly correctionLock: UTxO;
  readonly commitYield: UTxO;
  readonly fraudRemovalYield: UTxO;
}> => {
  const hubOracleAssets = {
    [toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME)]: 1n,
  };
  const correctionLockAssets = {
    [toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME)]: 1n,
  };
  const schedulerAssets = {
    [toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME)]: 1n,
  };
  const commitYieldAssets = {
    [toUnit(
      contracts.scheduler.policyId,
      fromText(
        REFERENCE_SCRIPT_AUTH_TOKEN_NAMES["state-queue commit withdrawal"],
      ),
    )]: 1n,
  };
  const fraudRemovalYieldAssets = {
    [toUnit(
      contracts.scheduler.policyId,
      fromText(
        REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[
          "state-queue fraud-removal withdrawal"
        ],
      ),
    )]: 1n,
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
  const sharedAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.activeOperators.spendingScriptAddress),
  );
  const stateQueueAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.stateQueue.spendingScriptAddress),
  );
  const fraudProofAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
  );
  const hubOracleDatum = Data.to(
    {
      registered_operators: contracts.activeOperators.policyId,
      active_operators: contracts.activeOperators.policyId,
      retired_operators: contracts.retiredOperators.policyId,
      scheduler: contracts.scheduler.policyId,
      state_queue: contracts.stateQueue.policyId,
      fraud_proof_catalogue: contracts.fraudProof.policyId,
      fraud_proof: contracts.fraudProof.policyId,
      deposit: contracts.fraudProof.policyId,
      withdrawal: contracts.fraudProof.policyId,
      tx_order: contracts.fraudProof.policyId,
      settlement: contracts.settlement.policyId,
      payout: contracts.fraudProof.policyId,
      registered_operators_addr: sharedAddressData,
      active_operators_addr: sharedAddressData,
      retired_operators_addr: sharedAddressData,
      scheduler_addr: sharedAddressData,
      state_queue_addr: stateQueueAddressData,
      fraud_proof_catalogue_addr: fraudProofAddressData,
      fraud_proof_addr: fraudProofAddressData,
      deposit_addr: fraudProofAddressData,
      withdrawal_addr: fraudProofAddressData,
      tx_order_addr: fraudProofAddressData,
      settlement_addr: sharedAddressData,
      reserve_addr: sharedAddressData,
      payout_addr: fraudProofAddressData,
      reserve_observer: contracts.activeOperators.policyId,
    },
    HubOracleDatum,
  );

  const walletAddress = await lucid.wallet().address();
  const unsigned = await lucid
    .newTx()
    .validFrom(Number(initValidFrom))
    .validTo(Number(initValidTo))
    .collectFrom([nonceUtxo])
    .mintAssets({ ...hubOracleAssets, ...correctionLockAssets }, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      { kind: "inline", value: hubOracleDatum },
      hubOracleAssets,
    )
    .pay.ToContract(
      contracts.correctionLock.spendingScriptAddress,
      { kind: "inline", value: Data.to("Idle", CorrectionLockDatum) },
      // The correction-lock validator conserves the singleton's value exactly
      // across Idle -> Locked, whose larger inline datum raises the min-ada
      // floor. Fund the lock above that floor so Lucid never bumps the
      // continuation output's lovelace.
      { ...correctionLockAssets, lovelace: 5_000_000n },
    )
    .mintAssets(
      { ...schedulerAssets, ...commitYieldAssets, ...fraudRemovalYieldAssets },
      Data.void(),
    )
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
    .pay.ToAddressWithData(
      walletAddress,
      undefined,
      { ...commitYieldAssets, lovelace: 20_000_000n },
      contracts.commitYield.spendingScript,
    )
    .pay.ToAddressWithData(
      walletAddress,
      undefined,
      { ...fraudRemovalYieldAssets, lovelace: 20_000_000n },
      contracts.fraudRemovalYield.spendingScript,
    )
    .register.Stake(
      scriptRewardAddress(network, contracts.commitYield.spendingScript),
    )
    .register.Stake(
      scriptRewardAddress(network, contracts.fraudRemovalYield.spendingScript),
    )
    .mintAssets(
      stateQueueAssets,
      Data.to({ InitV1: { output_index: 5n } }, StateQueueRedeemer),
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
  const [correctionLock] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    Object.keys(correctionLockAssets)[0]!,
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
  const [commitYield] = await lucid.utxosAtWithUnit(
    walletAddress,
    Object.keys(commitYieldAssets)[0]!,
  );
  const [fraudRemovalYield] = await lucid.utxosAtWithUnit(
    walletAddress,
    Object.keys(fraudRemovalYieldAssets)[0]!,
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
    fraudProof === undefined ||
    correctionLock === undefined ||
    commitYield === undefined ||
    fraudRemovalYield === undefined
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
    correctionLock,
    commitYield,
    fraudRemovalYield,
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
  emulator,
  lucid,
  contracts,
  anchor,
  header,
  operator,
  scheduler,
  hubOracle,
  correctionLock,
  commitYield,
  activeOperatorInput,
}: {
  readonly emulator: Emulator;
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: StateQueueTestContracts;
  readonly anchor: StateQueueUTxO;
  readonly header: HeaderType;
  readonly operator: string;
  readonly scheduler: UTxO;
  readonly hubOracle: UTxO;
  readonly correctionLock: UTxO;
  readonly commitYield: UTxO;
  readonly activeOperatorInput: UTxO;
}): Promise<{
  readonly block: StateQueueUTxO;
  readonly activeOperatorInput: UTxO;
}> => {
  const orderedStateQueue = await Effect.runPromise(
    fetchSortedStateQueueUTxOsProgram(lucid, {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    }),
  );
  const canonicalTail = orderedStateQueue.at(-1);
  if (
    canonicalTail === undefined ||
    canonicalTail.utxo.txHash !== anchor.utxo.txHash ||
    canonicalTail.utxo.outputIndex !== anchor.utxo.outputIndex
  ) {
    throw new Error("Commit helper received a stale state-queue tail");
  }
  const confirmedStateRefInput =
    orderedStateQueue.length === 1 ? undefined : orderedStateQueue[0]!.utxo;
  const headStateQueueNodeRefInput =
    orderedStateQueue.length <= 2 ? undefined : orderedStateQueue[1]!.utxo;
  const continuedActiveOperatorDatum = Data.void();
  const validityStartSlot = lucid.currentSlot() + 1;
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
        correctionLockRefInput: {
          utxo: correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        headStateQueueNodeRefInput,
        additionalRefInputs: [hubOracle],
        activeOperatorInput,
        validFrom: BigInt(lucid.slotToUnixTime(validityStartSlot)),
        validTo: header.endTime + 1n,
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
        yieldWitness: {
          referenceInput: commitYield,
          script: contracts.commitYield.spendingScript,
        },
      },
    ),
  );
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  emulator.awaitSlot(1);
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
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
  it("encodes YieldStateQueueV1 as the canonical fieldless constructor", () => {
    expect(encodeStateQueueYieldRedeemerV1()).toBe("d87980");
  });

  it("uses the exact sole L04 InitV1 and MergeToConfirmedStateV1 language", () => {
    const init = { InitV1: { output_index: 2n } } as const;
    const initCbor = Data.to(init, StateQueueRedeemer);
    expect(initCbor).toBe("d8799f02ff");
    expect(roundTrip(init, StateQueueRedeemer)).toEqual(init);

    const proofMerge = {
      MergeToConfirmedStateV1: {
        yield_to_ref_input_index: 0n,
        header_node_key: "11".repeat(28),
        confirmed_state_input_outref: outputReference,
        confirmed_state_output_index: 0n,
        m_settlement_redeemer_index: 1n,
        merged_block_withdrawals_root: "21".repeat(32),
        merged_block_forced_transactions_root: "22".repeat(32),
        merged_block_transactions_root: "23".repeat(32),
        merged_block_deposits_root: "24".repeat(32),
        merged_block_transition_trace_root: "25".repeat(32),
        merged_block_event_to_step_root: "26".repeat(32),
        merged_block_validation_traces_root: "27".repeat(32),
        merged_block_withdrawal_count: 1n,
        merged_block_forced_transaction_count: 2n,
        merged_block_l2_transaction_count: 3n,
        merged_block_deposit_count: 4n,
        merged_block_total_event_count: 10n,
        merged_block_transition_step_count: 10n,
        merged_block_validation_trace_count: 5n,
      },
    } as const;
    const mergeCbor = Data.to(proofMerge, StateQueueRedeemer);
    expect(mergeCbor).toBe(
      "d87f9f00581c11111111111111111111111111111111111111111111111111111111d8799f5820444444444444444444444444444444444444444444444444444444444444444400ff00d8799f01ff58202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222225820232323232323232323232323232323232323232323232323232323232323232358202424242424242424242424242424242424242424242424242424242424242424582025252525252525252525252525252525252525252525252525252525252525255820262626262626262626262626262626262626262626262626262626262626262658202727272727272727272727272727272727272727272727272727272727272727010203040a0a05ff",
    );
    expect(roundTrip(proofMerge, StateQueueRedeemer)).toEqual(proofMerge);
    expect(initCbor.startsWith("d8799f")).toBe(true);
    expect(mergeCbor.startsWith("d87f9f")).toBe(true);
    expect(() =>
      Data.to({ InitV2: { output_index: 2n } } as never, StateQueueRedeemer),
    ).toThrow();
    expect(() =>
      Data.to(
        {
          MergeToConfirmedStateV2: proofMerge.MergeToConfirmedStateV1,
        } as never,
        StateQueueRedeemer,
      ),
    ).toThrow();
    expect(() => Data.from("d87f80", StateQueueRedeemer)).toThrow();
  });

  it("round-trips CommitBlockHeader and RemoveFraudulentBlockHeader", () => {
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            yield_to_ref_input_index: 0n,
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: "11".repeat(28),
            scheduler_ref_input_index: 3n,
            active_operators_input_index: 4n,
            active_operators_redeemer_index: 5n,
            m_confirmed_state_ref_input_index: null,
            m_head_state_queue_node_ref_input_index: null,
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
        yield_to_ref_input_index: 0n,
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
                m_fraud_prover_reward_output_index: 8n,
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
            m_fraud_prover_reward_output_index: 8n,
          },
        },
      },
    });

    // D3: a bond-consuming slash that routes no reward encodes the index as
    // `null`, which the on-chain guard accepts only while the compiled
    // `env.fraud_prover_reward` is zero.
    const rewardlessRetiredSlash = {
      RemoveFraudulentBlockHeader: {
        ...removeRedeemer.RemoveFraudulentBlockHeader,
        slashing_approach: {
          SlashRetiredOperator: {
            retired_operators_redeemer_index: 9n,
            m_fraud_prover_reward_output_index: null,
          },
        },
      },
    };
    expect(roundTrip(rewardlessRetiredSlash, StateQueueRedeemer)).toEqual(
      rewardlessRetiredSlash,
    );
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
    // Lucid omits validity_start when it maps to slot zero. Advance one
    // emulator slot so the real initializer receives a closed range.
    emulator.awaitSlot(1);
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 120_000n;
    const genesisTime = initValidTo - 1n;
    const transactionsRoot = await buildTransactionsRoot();
    const header: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
      ...TWO_TRANSACTION_HEADER_COMMITMENTS,
      transactionsRoot,
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: 1n,
    };
    const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
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
      emulator,
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
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
      getHeaderV1FromStateQueueDatum(committedBlock.datum),
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
        fraudProofPolicyId: contracts.fraudProof.policyId,
        hubOracleRefInput: setup.hubOracle,
        correctionLockInput: {
          utxo: setup.correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        correctionLockSpendingScript: contracts.correctionLock.spendingScript,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: setup.fraudRemovalYield,
          script: contracts.fraudRemovalYield.spendingScript,
        },
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
    // Lucid omits validity_start when it maps to slot zero. Advance one
    // emulator slot so the real initializer receives a closed range.
    emulator.awaitSlot(1);
    const initValidFrom = BigInt(emulator.now());
    const initValidTo = initValidFrom + 120_000n;
    const genesisTime = initValidTo - 1n;
    const firstHeader: HeaderType = {
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
      ...TWO_TRANSACTION_HEADER_COMMITMENTS,
      transactionsRoot: await buildTransactionsRoot(),
      depositsRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: genesisTime,
      endTime: genesisTime + 1_000n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: GENESIS_HEADER_HASH,
      operatorVkey: operator,
      protocolVersion: 1n,
    };
    const firstHeaderHash = await Effect.runPromise(
      hashBlockHeaderV1(firstHeader),
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
      emulator,
      lucid,
      contracts,
      anchor: stateQueueRoot,
      header: firstHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
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
      hashBlockHeaderV1(secondHeader),
    );
    const secondCommit = await submitCommitHeaderTx({
      emulator,
      lucid,
      contracts,
      anchor: firstCommit.block,
      header: secondHeader,
      operator,
      scheduler: setup.scheduler,
      hubOracle: setup.hubOracle,
      correctionLock: setup.correctionLock,
      commitYield: setup.commitYield,
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
        fraudProofPolicyId: contracts.fraudProof.policyId,
        hubOracleRefInput: setup.hubOracle,
        correctionLockInput: {
          utxo: setup.correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        correctionLockSpendingScript: contracts.correctionLock.spendingScript,
        slashing: {
          kind: "operatorAlreadySlashed",
          activeOperatorsElementRefInput: setup.activeOperatorsRoot,
          retiredOperatorsElementRefInput: setup.retiredOperatorsRoot,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: setup.fraudRemovalYield,
          script: contracts.fraudRemovalYield.spendingScript,
        },
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
