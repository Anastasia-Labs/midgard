import { describe, expect, it } from "vitest";
import { encode } from "cborg";
import fs from "node:fs";
import path from "node:path";
import {
  CML,
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  createCostModels,
  generateEmulatorAccount,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  deriveMidgardNativeTxBodyCompactFromFull,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxFull,
  midgardNativeTxFullToCardanoTxEncoding,
  type MidgardNativeTxBodyFull,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetFull,
} from "@/midgard-tx-codec/index.js";
import { findSpentAndProducedUTxOs, breakDownTx } from "@/utils.js";
import {
  type PlutusEvaluationResult,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidation,
  type QueuedTx,
} from "@/validation/index.js";
import { evaluatePlutusTxLocally } from "@/validation/local-plutus-eval.js";

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const TEST_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const alwaysSucceedsBlueprintPath = path.resolve(
  __dirname,
  "../blueprints/always-succeeds/plutus.json",
);
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];
const alwaysSucceedsBlueprint = JSON.parse(
  fs.readFileSync(alwaysSucceedsBlueprintPath, "utf8"),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const loadAlwaysSucceedsCompiledCode = (title: string): string => {
  const compiledCode = alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === title,
  )?.compiledCode;
  if (compiledCode === undefined) {
    throw new Error(`missing always-succeeds blueprint entry: ${title}`);
  }
  return compiledCode;
};

const ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.deposit_spend.else",
);
const ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.deposit_mint.else",
);
const ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.reserve_withdraw.else",
);
const ALWAYS_FAILS_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.always_fail.else",
);
const DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.datum_equals_redeemer_spend.spend",
);
const TEST_COST_MODELS = createCostModels(PROTOCOL_PARAMETERS_DEFAULT.costModels);

const makeAlwaysSucceedsScript = (
  compiledCode: string,
): InstanceType<typeof CML.Script> =>
  CML.Script.new_plutus_v3(
    CML.PlutusV3Script.from_raw_bytes(Buffer.from(compiledCode, "hex")),
  );

const makeLanguageList = (
  languages: readonly number[],
): InstanceType<typeof CML.LanguageList> => {
  const result = CML.LanguageList.new();
  for (const language of new Set(languages)) {
    result.add(language);
  }
  return result;
};

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const makeOutput = (address: string, lovelace: bigint): Buffer =>
  Buffer.from(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

const makePubKeyOutput = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
  lovelace: bigint,
): Buffer =>
  makePubKeyValueOutput(keyHash, CML.Value.from_coin(lovelace));

const makePubKeyValueOutput = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
  value: InstanceType<typeof CML.Value>,
): Buffer =>
  Buffer.from(
    CML.TransactionOutput.new(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      value,
    ).to_cbor_bytes(),
  );

const makeSingleAssetValue = (
  lovelace: bigint,
  policyId: Uint8Array,
  assetName: Uint8Array,
  quantity: bigint,
): InstanceType<typeof CML.Value> => {
  const assets = CML.MapAssetNameToCoin.new();
  assets.insert(CML.AssetName.from_raw_bytes(assetName), quantity);
  const multiasset = CML.MultiAsset.new();
  multiasset.insert_assets(CML.ScriptHash.from_raw_bytes(policyId), assets);
  return CML.Value.new(lovelace, multiasset);
};

const makeMultiAssetValue = (
  lovelace: bigint,
  entries: readonly {
    readonly policyId: Uint8Array;
    readonly assetName: Uint8Array;
    readonly quantity: bigint;
  }[],
): InstanceType<typeof CML.Value> => {
  const multiasset = CML.MultiAsset.new();
  for (const entry of entries) {
    const policy = CML.ScriptHash.from_raw_bytes(entry.policyId);
    const assets =
      multiasset.get_assets(policy) ?? CML.MapAssetNameToCoin.new();
    assets.insert(
      CML.AssetName.from_raw_bytes(entry.assetName),
      entry.quantity,
    );
    multiasset.insert_assets(policy, assets);
  }
  return CML.Value.new(lovelace, multiasset);
};

const makeValueOutput = (
  address: string,
  value: InstanceType<typeof CML.Value>,
): Buffer =>
  Buffer.from(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      value,
    ).to_cbor_bytes(),
  );

const makeScriptOutput = (
  scriptHash: InstanceType<typeof CML.ScriptHash>,
  lovelace: bigint,
  opts?: {
    readonly datum?: InstanceType<typeof CML.PlutusData>;
    readonly scriptRef?: InstanceType<typeof CML.Script>;
  },
): Buffer =>
  Buffer.from(
    CML.TransactionOutput.new(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(scriptHash),
      ).to_address(),
      CML.Value.from_coin(lovelace),
      opts?.datum === undefined
        ? undefined
        : CML.DatumOption.new_datum(opts.datum),
      opts?.scriptRef,
    ).to_cbor_bytes(),
  );

const makeLegacyRedeemersCbor = (
  items: readonly {
    readonly tag: number;
    readonly index: bigint;
    readonly data?: Uint8Array;
    readonly exUnits?: readonly [bigint, bigint];
  }[],
): Buffer =>
  Buffer.from(
    encode(
      items.map((item) => [
        item.tag,
        item.index,
        Buffer.from(item.data ?? Buffer.alloc(0)),
        [item.exUnits?.[0] ?? 0n, item.exUnits?.[1] ?? 0n],
      ]),
    ),
  );

const makePlutusIntegerData = (value: bigint): InstanceType<typeof CML.PlutusData> =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

const makeMintPreimage = (
  policyId: Uint8Array,
  assetName: Uint8Array,
  quantity: bigint,
): Buffer =>
  makeMintPreimageFromEntries([{ policyId, assetName, quantity }]);

const makeMintPreimageFromEntries = (
  entries: readonly {
    readonly policyId: Uint8Array;
    readonly assetName: Uint8Array;
    readonly quantity: bigint;
  }[],
): Buffer =>
  Buffer.from(
    encode(
      entries.reduce<Map<Buffer, Map<Buffer, bigint>>>((policies, entry) => {
        const policyKey = Buffer.from(entry.policyId);
        const assets = policies.get(policyKey) ?? new Map<Buffer, bigint>();
        assets.set(Buffer.from(entry.assetName), entry.quantity);
        policies.set(policyKey, assets);
        return policies;
      }, new Map()),
    ),
  );

const makeEmptyMintMapPreimage = (): Buffer => Buffer.from(encode(new Map()));

const makeEmptyMintPolicyAssetsPreimage = (policyId: Uint8Array): Buffer =>
  Buffer.from(encode(new Map([[Buffer.from(policyId), new Map()]])));

const buildNativeTx = (opts?: {
  readonly redeemerTxWitsPreimageCbor?: Buffer;
  readonly datumWitnessItems?: readonly Uint8Array[];
  readonly requiredObserverItems?: readonly Uint8Array[];
  readonly scriptWitnessItems?: readonly Uint8Array[];
  readonly witnessMode?: "none" | "valid" | "invalid";
  readonly witnessSignerPrivateKey?: InstanceType<typeof CML.PrivateKey>;
  readonly mintPreimageCbor?: Buffer;
  readonly networkId?: bigint;
  readonly outputCount?: number;
  readonly outputCbors?: readonly Buffer[];
  readonly scriptIntegrityHash?: Buffer;
  readonly version?: bigint;
}): {
  tx: MidgardNativeTxFull;
  txId: Buffer;
  txCbor: Buffer;
  inputOutRef: Buffer;
  referenceInputOutRef: Buffer;
  outputCbor: Buffer;
} => {
  const spendInput = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex("11".repeat(32)),
      0n,
    ).to_cbor_bytes(),
  );
  const referenceInput = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex("22".repeat(32)),
      1n,
    ).to_cbor_bytes(),
  );
  const defaultOutput = Buffer.from(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(TEST_ADDRESS),
      CML.Value.from_coin(3_000_000n),
    ).to_cbor_bytes(),
  );
  const outputCount = Math.max(1, opts?.outputCount ?? 1);
  const outputCbors =
    opts?.outputCbors !== undefined
      ? [...opts.outputCbors]
      : Array.from({ length: outputCount }, () => Buffer.from(defaultOutput));
  const outputCbor = outputCbors[0];

  const spendInputsPreimageCbor = encodeByteList([spendInput]);
  const referenceInputsPreimageCbor = encodeByteList([referenceInput]);
  const outputsPreimageCbor = encodeByteList(outputCbors);
  const requiredObserversPreimageCbor = encodeByteList(
    opts?.requiredObserverItems ?? [],
  );
  const witnessMode = opts?.witnessMode ?? "none";
  const witnessSignerPrivateKey =
    witnessMode === "none"
      ? undefined
      : (opts?.witnessSignerPrivateKey ?? CML.PrivateKey.generate_ed25519());
  const requiredSignersPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(witnessSignerPrivateKey.to_public().hash().to_raw_bytes()),
        ]);
  const mintPreimageCbor = opts?.mintPreimageCbor ?? EMPTY_CBOR_LIST;

  const scriptTxWitsPreimageCbor = encodeByteList(
    opts?.scriptWitnessItems ?? [],
  );
  const redeemerTxWitsPreimageCbor =
    opts?.redeemerTxWitsPreimageCbor ?? EMPTY_CBOR_LIST;
  const datumTxWitsPreimageCbor = encodeByteList(
    opts?.datumWitnessItems ?? [],
  );
  const version = opts?.version ?? MIDGARD_NATIVE_TX_VERSION;

  const body: MidgardNativeTxBodyFull = {
    spendInputsRoot: computeHash32(spendInputsPreimageCbor),
    spendInputsPreimageCbor,
    referenceInputsRoot: computeHash32(referenceInputsPreimageCbor),
    referenceInputsPreimageCbor,
    outputsRoot: computeHash32(outputsPreimageCbor),
    outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversRoot: computeHash32(requiredObserversPreimageCbor),
    requiredObserversPreimageCbor,
    requiredSignersRoot: computeHash32(requiredSignersPreimageCbor),
    requiredSignersPreimageCbor,
    mintRoot: computeHash32(mintPreimageCbor),
    mintPreimageCbor,
    scriptIntegrityHash:
      opts?.scriptIntegrityHash ?? computeHash32(EMPTY_CBOR_NULL),
    auxiliaryDataHash: computeHash32(EMPTY_CBOR_NULL),
    networkId: opts?.networkId ?? 0n,
  };

  const bodyHash = computeHash32(
    encodeMidgardNativeTxBodyCompact(
      deriveMidgardNativeTxBodyCompactFromFull(body),
    ),
  );
  const signedBodyHash =
    witnessMode === "invalid" ? Buffer.alloc(32, 0x7f) : bodyHash;
  const addrTxWitsPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              witnessSignerPrivateKey,
            ).to_cbor_bytes(),
          ),
        ]);

  const witnessSet: MidgardNativeTxWitnessSetFull = {
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
    scriptTxWitsRoot: computeHash32(scriptTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor,
    redeemerTxWitsRoot: computeHash32(redeemerTxWitsPreimageCbor),
    redeemerTxWitsPreimageCbor,
    datumTxWitsRoot: computeHash32(datumTxWitsPreimageCbor),
    datumTxWitsPreimageCbor,
  };

  const tx: MidgardNativeTxFull = {
    version,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid", version),
    body,
    witnessSet,
  };

  const txCbor = encodeMidgardNativeTxFull(tx);
  const txId = computeMidgardNativeTxIdFromFull(tx);

  return {
    tx,
    txId,
    txCbor,
    inputOutRef: spendInput,
    referenceInputOutRef: referenceInput,
    outputCbor,
  };
};

const attachComputedScriptIntegrityHash = (
  fixture: ReturnType<typeof buildNativeTx>,
  usedLanguages: readonly number[],
): ReturnType<typeof buildNativeTx> => {
  const cardanoTx = CML.Transaction.from_cbor_bytes(
    midgardNativeTxFullToCardanoTxEncoding(fixture.tx),
  );
  const redeemers = cardanoTx.witness_set().redeemers();
  if (redeemers === undefined) {
    throw new Error("expected redeemers when computing script integrity hash");
  }

  const scriptDataHash = CML.calc_script_data_hash(
    redeemers,
    cardanoTx.witness_set().plutus_datums() ?? CML.PlutusDataList.new(),
    TEST_COST_MODELS,
    makeLanguageList(usedLanguages),
  );
  if (scriptDataHash === undefined) {
    throw new Error("failed to compute script integrity hash");
  }

  const body: MidgardNativeTxBodyFull = {
    ...fixture.tx.body,
    scriptIntegrityHash: Buffer.from(scriptDataHash.to_raw_bytes()),
  };
  const tx: MidgardNativeTxFull = {
    ...fixture.tx,
    body,
    compact: deriveMidgardNativeTxCompact(
      body,
      fixture.tx.witnessSet,
      fixture.tx.compact.validity,
      fixture.tx.version,
    ),
  };

  return {
    ...fixture,
    tx,
    txId: computeMidgardNativeTxIdFromFull(tx),
    txCbor: encodeMidgardNativeTxFull(tx),
  };
};

const evaluatePlutusTxWithLocalPhaseTwo = async ({
  txCborHex,
  additionalUtxos,
}: {
  readonly txCborHex: string;
  readonly additionalUtxos: readonly import("@lucid-evolution/lucid").UTxO[];
}): Promise<PlutusEvaluationResult> => {
  const emulator = new Emulator(
    [generateEmulatorAccount({ lovelace: 10_000_000_000n })],
    PROTOCOL_PARAMETERS_DEFAULT,
  );
  const lucid = await Lucid(emulator, "Custom");
  try {
    evaluatePlutusTxLocally(lucid, txCborHex, additionalUtxos);
    return { kind: "accepted" };
  } catch (error) {
    return {
      kind: "script_invalid",
      detail:
        error instanceof Error
          ? (error.stack ?? error.message)
          : String(error),
    };
  }
};

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

const mkQueued = (txId: Buffer, txCbor: Buffer): QueuedTx => ({
  txId,
  txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

const runBothPhases = async (
  txId: Buffer,
  txCbor: Buffer,
  preState: Map<string, Buffer>,
  opts?: {
    readonly evaluatePlutusTx?: (args: {
      readonly txId: Buffer;
      readonly txCborHex: string;
      readonly additionalUtxos: readonly import("@lucid-evolution/lucid").UTxO[];
    }) => Promise<PlutusEvaluationResult>;
  },
) => {
  const phaseA = await Effect.runPromise(
    runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
  );
  const phaseB = await Effect.runPromise(
    runPhaseBValidation(phaseA.accepted, preState, {
      nowCardanoSlotNo: 0n,
      bucketConcurrency: 1,
      evaluatePlutusTx: opts?.evaluatePlutusTx,
    }),
  );
  return { phaseA, phaseB };
};

describe("native transaction integration", () => {
  it("accepts a canonical native tx in phase A", async () => {
    const { txId, txCbor, inputOutRef } = buildNativeTx();
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    const accepted = result.accepted[0];
    expect(accepted.txId.equals(txId)).toBe(true);
    expect(accepted.processedTx.spent).toHaveLength(1);
    expect(accepted.processedTx.spent[0].equals(inputOutRef)).toBe(true);
    expect(accepted.processedTx.produced).toHaveLength(1);
    expect(accepted.witnessKeyHashes).toStrictEqual([]);
    expect(accepted.nativeScriptHashes).toStrictEqual([]);
    expect(accepted.outputSum.coin()).toBe(3_000_000n);
  });

  it("accepts required signer when native witness signature is valid", async () => {
    const { txId, txCbor } = buildNativeTx({ witnessMode: "valid" });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    expect(result.accepted[0].witnessKeyHashes.length).toBe(1);
  });

  it("accepts native txs when network id is omitted via sentinel", async () => {
    const { txId, txCbor } = buildNativeTx({
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });

  it("rejects non-empty auxiliary data hashes during phase A admission", async () => {
    const base = buildNativeTx();
    const tx: MidgardNativeTxFull = {
      ...base.tx,
      body: {
        ...base.tx.body,
        auxiliaryDataHash: Buffer.from("99".repeat(32), "hex"),
      },
    };
    const fullTx: MidgardNativeTxFull = {
      ...tx,
      compact: deriveMidgardNativeTxCompact(
        tx.body,
        tx.witnessSet,
        tx.compact.validity,
        tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdFromFull(fullTx);
    const txCbor = encodeMidgardNativeTxFull(fullTx);

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.AuxDataForbidden);
  });

  it("rejects native txs with invalid vkey witness signatures", async () => {
    const { txId, txCbor } = buildNativeTx({ witnessMode: "invalid" });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidSignature);
  });

  it("accepts structurally valid Plutus witness bundles in phase A and rejects them when no evaluator is configured", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("deadbeef", "hex"),
    );
    const wrappedPlutusScript = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = wrappedPlutusScript.hash();
    const redeemerBytes = makeLegacyRedeemersCbor([
      { tag: CML.RedeemerTag.Spend, index: 0n },
    ]);
    const datum = makePlutusIntegerData(42n);
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(wrappedPlutusScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: redeemerBytes,
      datumWitnessItems: [Buffer.from(datum.to_cbor_bytes())],
      scriptIntegrityHash: Buffer.from("77".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].requiresPlutusEvaluation).toBe(true);
    expect(phaseA.accepted[0].plutusScriptHashes).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(
      RejectCodes.PlutusEvaluationUnavailable,
    );
  });

  it("rejects phase A when plutus observer evaluation cannot be reconstructed without network id", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(observerScript.hash().to_raw_bytes());
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("deadcafe", "hex"),
    );
    const wrappedPlutusScript = CML.Script.new_plutus_v3(plutusScript);
    const { txId, txCbor } = buildNativeTx({
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      requiredObserverItems: [observerScriptHash],
      scriptWitnessItems: [Buffer.from(wrappedPlutusScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("71".repeat(32), "hex"),
    });

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(result.rejected[0].detail).toContain("network_id is required");
  });

  it("rejects duplicate required observers after normalization", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerHash = Buffer.from(observerScript.hash().to_raw_bytes());
    const observerCredential = Buffer.from(
      CML.Credential.new_script(
        CML.ScriptHash.from_raw_bytes(observerHash),
      ).to_cbor_bytes(),
    );
    const { txId, txCbor } = buildNativeTx({
      requiredObserverItems: [observerHash, observerCredential],
    });

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(result.rejected[0].detail).toContain("duplicate required observer");
  });

  it("rejects duplicate reference inputs in phase A", async () => {
    const base = buildNativeTx();
    const duplicatedReferenceInputsPreimageCbor = encodeByteList([
      base.referenceInputOutRef,
      base.referenceInputOutRef,
    ]);
    const tx: MidgardNativeTxFull = {
      ...base.tx,
      body: {
        ...base.tx.body,
        referenceInputsRoot: computeHash32(duplicatedReferenceInputsPreimageCbor),
        referenceInputsPreimageCbor: duplicatedReferenceInputsPreimageCbor,
      },
    };
    const fullTx: MidgardNativeTxFull = {
      ...tx,
      compact: deriveMidgardNativeTxCompact(
        tx.body,
        tx.witnessSet,
        tx.compact.validity,
        tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdFromFull(fullTx);
    const txCbor = encodeMidgardNativeTxFull(fullTx);

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.DuplicateInputInTx);
    expect(result.rejected[0].detail).toContain("duplicate reference input");
  });

  it("accepts required observers when satisfied by an inline native script witness", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(observerScript.hash().to_raw_bytes());
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerScriptHash],
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(observerScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: observerKey,
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(observerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].requiredObserverHashes).toStrictEqual([
      observerScriptHash.toString("hex"),
    ]);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts required observers when satisfied by a reference native script", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(observerScript.hash().to_raw_bytes());
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerScriptHash],
      witnessMode: "valid",
      witnessSignerPrivateKey: observerKey,
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        CML.Script.new_native(observerScript),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(observerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects required observers when a matching reference native script is not satisfied by tx signers", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const spendSignerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(observerScript.hash().to_raw_bytes());
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerScriptHash],
      witnessMode: "valid",
      witnessSignerPrivateKey: spendSignerKey,
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        CML.Script.new_native(observerScript),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(spendSignerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.NativeScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(
      observerScriptHash.toString("hex"),
    );
  });

  it("rejects required observers when no matching script witness is available", async () => {
    const observerHash = Buffer.from("44".repeat(28), "hex");
    const signerKey = CML.PrivateKey.generate_ed25519();
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerHash],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(observerHash.toString("hex"));
  });

  it("rejects extraneous inline native script witnesses", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const extraneousScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(extraneousScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseB.rejected[0].detail).toContain(
      "extraneous native script witness",
    );
  });

  it("accepts non-empty mint when satisfied by an inline native mint policy script", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("01", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(mintScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [mintedOutput],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].mintPolicyHashes).toStrictEqual([
      policyId.toString("hex"),
    ]);
    expect(phaseA.accepted[0].mintedValue.has_multiassets()).toBe(true);
    expect(phaseA.accepted[0].burnedValue.is_zero()).toBe(true);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts non-empty mint when satisfied by a reference native mint policy script", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("02", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [mintedOutput],
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        CML.Script.new_native(mintScript),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts multi-policy mint when one policy is inline and one is supplied by reference script", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const inlineScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const referenceScriptChildren = CML.NativeScriptList.new();
    referenceScriptChildren.add(
      CML.NativeScript.new_script_pubkey(signerKey.to_public().hash()),
    );
    const referenceScript = CML.NativeScript.new_script_all(
      referenceScriptChildren,
    );
    const inlinePolicyId = Buffer.from(inlineScript.hash().to_raw_bytes());
    const referencePolicyId = Buffer.from(referenceScript.hash().to_raw_bytes());
    const inlineAssetName = Buffer.from("0a", "hex");
    const referenceAssetName = Buffer.from("0b", "hex");
    const mintPreimageCbor = makeMintPreimageFromEntries([
      {
        policyId: inlinePolicyId,
        assetName: inlineAssetName,
        quantity: 1n,
      },
      {
        policyId: referencePolicyId,
        assetName: referenceAssetName,
        quantity: 1n,
      },
    ]);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeMultiAssetValue(3_000_000n, [
        {
          policyId: inlinePolicyId,
          assetName: inlineAssetName,
          quantity: 1n,
        },
        {
          policyId: referencePolicyId,
          assetName: referenceAssetName,
          quantity: 1n,
        },
      ]),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(inlineScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [mintedOutput],
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        CML.Script.new_native(referenceScript),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].mintPolicyHashes).toStrictEqual([
      inlinePolicyId.toString("hex"),
      referencePolicyId.toString("hex"),
    ].sort((a, b) => a.localeCompare(b)));
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects non-empty mint when no matching native mint policy witness is available", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const policyId = Buffer.from("55".repeat(28), "hex");
    const assetName = Buffer.from("03", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [mintedOutput],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(policyId.toString("hex"));
  });

  it("rejects non-empty mint when a matching native mint policy script does not verify", async () => {
    const requiredSignerKey = CML.PrivateKey.generate_ed25519();
    const spendSignerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      requiredSignerKey.to_public().hash(),
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("04", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      witnessMode: "valid",
      witnessSignerPrivateKey: spendSignerKey,
      outputCbors: [mintedOutput],
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        CML.Script.new_native(mintScript),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(spendSignerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.NativeScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(policyId.toString("hex"));
  });

  it("rejects malformed native mint preimages", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const malformedMintPreimageCbor = makeMintPreimage(
      Buffer.alloc(27, 0x11),
      Buffer.from("05", "hex"),
      1n,
    );
    const { txId, txCbor } = buildNativeTx({
      mintPreimageCbor: malformedMintPreimageCbor,
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
  });

  it("rejects empty top-level mint maps instead of treating them as no mint", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const { txId, txCbor } = buildNativeTx({
      mintPreimageCbor: makeEmptyMintMapPreimage(),
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseA.rejected[0].detail).toContain("Midgard mint map cannot be empty");
  });

  it("rejects empty per-policy mint asset maps instead of treating them as no mint", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const policyId = Buffer.from("66".repeat(28), "hex");
    const { txId, txCbor } = buildNativeTx({
      mintPreimageCbor: makeEmptyMintPolicyAssetsPreimage(policyId),
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseA.rejected[0].detail).toContain(
      "Mint policy asset map cannot be empty",
    );
  });

  it("rejects mint value-preservation mismatches when minted assets are missing from outputs", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("06", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(mintScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [makeOutput(TEST_ADDRESS, 3_000_000n)],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });

  it("accepts non-empty burns when burned assets are removed from outputs", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const burnScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = Buffer.from(burnScript.hash().to_raw_bytes());
    const assetName = Buffer.from("07", "hex");
    const burnPreimageCbor = makeMintPreimage(policyId, assetName, -1n);
    const inputValue = makeSingleAssetValue(3_000_000n, policyId, assetName, 1n);
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor: burnPreimageCbor,
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(burnScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [makeOutput(TEST_ADDRESS, 3_000_000n)],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyValueOutput(signerKey.to_public().hash(), inputValue),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].mintedValue.is_zero()).toBe(true);
    expect(phaseA.accepted[0].burnedValue.has_multiassets()).toBe(true);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts inline Plutus script spends when the evaluator succeeds", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("deadbeef", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = plutusScriptRef.hash();
    const datum = makePlutusIntegerData(99n);
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      datumWitnessItems: [Buffer.from(datum.to_cbor_bytes())],
      scriptIntegrityHash: Buffer.from("88".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    let evaluationCalls = 0;
    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: async ({ txCborHex, additionalUtxos }) => {
        evaluationCalls += 1;
        const tx = CML.Transaction.from_cbor_hex(txCborHex);
        expect(tx.witness_set().plutus_v3_scripts()?.len()).toBe(1);
        expect(tx.witness_set().plutus_datums()?.len()).toBe(1);
        expect(tx.witness_set().redeemers()).toBeDefined();
        expect(tx.body().script_data_hash()).toBeDefined();
        expect(additionalUtxos).toHaveLength(2);
        expect(additionalUtxos[0]?.datum).toBeDefined();
        expect(additionalUtxos[0]?.scriptRef).toBeUndefined();
        return { kind: "accepted" };
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].requiresPlutusEvaluation).toBe(true);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
    expect(evaluationCalls).toBe(1);
  });

  it("accepts inline Plutus mint policies when the evaluator succeeds", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("1234abcd", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const policyId = Buffer.from(plutusScriptRef.hash().to_raw_bytes());
    const assetName = Buffer.from("08", "hex");
    const mintPreimageCbor = makeMintPreimage(policyId, assetName, 1n);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      mintPreimageCbor,
      scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
        { tag: CML.RedeemerTag.Mint, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("8c".repeat(32), "hex"),
      outputCbors: [mintedOutput],
    });

    const preState = new Map<string, Buffer>([
      [inputOutRef.toString("hex"), makeScriptOutput(plutusScriptRef.hash(), 3_000_000n)],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: async () => ({ kind: "accepted" }),
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].mintPolicyHashes).toStrictEqual([
      policyId.toString("hex"),
    ]);
    expect(phaseA.accepted[0].requiresPlutusEvaluation).toBe(true);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts native-authored Plutus txs with pubkey inputs under local phase-two evaluation", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const withdrawScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX,
    );
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(withdrawScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Reward, index: 0n },
      ]),
      requiredObserverItems: [
        Buffer.from(withdrawScript.hash().to_raw_bytes()),
      ],
      networkId: 0n,
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });
    const withScriptDataHash = attachComputedScriptIntegrityHash(base, [
      CML.Language.PlutusV3,
    ]);
    const resignedWitnesses = encodeByteList([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(
            withScriptDataHash.tx.compact.transactionBodyHash,
          ),
          signerKey,
        ).to_cbor_bytes(),
      ),
    ]);
    const witnessSet: MidgardNativeTxWitnessSetFull = {
      ...withScriptDataHash.tx.witnessSet,
      addrTxWitsRoot: computeHash32(resignedWitnesses),
      addrTxWitsPreimageCbor: resignedWitnesses,
    };
    const tx: MidgardNativeTxFull = {
      ...withScriptDataHash.tx,
      witnessSet,
      compact: deriveMidgardNativeTxCompact(
        withScriptDataHash.tx.body,
        witnessSet,
        withScriptDataHash.tx.compact.validity,
        withScriptDataHash.tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdFromFull(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);
    const { inputOutRef, referenceInputOutRef } = withScriptDataHash;

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts reference-script Plutus spends when the evaluator succeeds", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("4d696467617264", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = plutusScriptRef.hash();
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("89".repeat(32), "hex"),
    });

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        plutusScriptRef,
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    let evaluationCalls = 0;
    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: async ({ additionalUtxos }) => {
        evaluationCalls += 1;
        expect(additionalUtxos).toHaveLength(2);
        expect(additionalUtxos[1]?.scriptRef?.type).toBe("PlutusV3");
        return { kind: "accepted" };
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
    expect(evaluationCalls).toBe(1);
  });

  it("rejects Plutus witness bundles when the evaluator reports script failure", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("cafebabe", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = plutusScriptRef.hash();
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("8a".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [inputOutRef.toString("hex"), makeScriptOutput(scriptHash, 3_000_000n)],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: async () => ({
        kind: "script_invalid",
        detail: "UPLC evaluation failed",
      }),
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain("UPLC evaluation failed");
  });

  it("surfaces evaluator infrastructure failures instead of turning them into script rejections", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("beadbead", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = plutusScriptRef.hash();
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("7a".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [inputOutRef.toString("hex"), makeScriptOutput(scriptHash, 3_000_000n)],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    await expect(
      runBothPhases(txId, txCbor, preState, {
        evaluatePlutusTx: async () => {
          throw new Error("fetch failed");
        },
      }),
    ).rejects.toThrow(/fetch failed/i);
  });

  it("still enforces value preservation after Plutus evaluator success", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("00112233", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const scriptHash = plutusScriptRef.hash();
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptIntegrityHash: Buffer.from("8b".repeat(32), "hex"),
      outputCbors: [makeOutput(TEST_ADDRESS, 4_000_000n)],
    });

    const preState = new Map<string, Buffer>([
      [inputOutRef.toString("hex"), makeScriptOutput(scriptHash, 3_000_000n)],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: async () => ({ kind: "accepted" }),
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });

  it("accepts inline always-succeeds Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(99n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects inline always-fails Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_FAILS_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(99n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects inline Plutus spends when the datum and redeemer do not match", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX,
    );
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(1n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: CML.RedeemerTag.Spend,
          index: 0n,
          data: Buffer.from(makePlutusIntegerData(2n).to_cbor_bytes()),
        },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects inline Plutus spends when the script_data_hash is wrong", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(3n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
    });
    const withScriptDataHash = attachComputedScriptIntegrityHash(base, [
      CML.Language.PlutusV3,
    ]);
    const badScriptDataHash = Buffer.from(withScriptDataHash.tx.body.scriptIntegrityHash);
    badScriptDataHash[0] ^= 0xff;
    const tx: MidgardNativeTxFull = {
      ...withScriptDataHash.tx,
      body: {
        ...withScriptDataHash.tx.body,
        scriptIntegrityHash: badScriptDataHash,
      },
    };
    tx.compact = deriveMidgardNativeTxCompact(
      tx.body,
      tx.witnessSet,
      tx.compact.validity,
      tx.version,
    );
    const txId = computeMidgardNativeTxIdFromFull(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);
    const { inputOutRef, referenceInputOutRef } = withScriptDataHash;

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects inline Plutus spends when the spend redeemer index is wrong", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(5n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 1n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects inline Plutus spends when the redeemer purpose does not match the script purpose", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(6n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Mint, index: 0n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("accepts reference-script always-succeeds Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(101n);
    const base = buildNativeTx({
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const referenceOutput = Buffer.from(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        spendScript,
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects Plutus spent inputs when the matching script is neither attached inline nor supplied by reference input", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(102n);
    const base = buildNativeTx({
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(scriptHash.to_hex());
  });

  it("rejects inline Plutus spent inputs when the script is present but no matching redeemer is provided", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(103n);
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      scriptIntegrityHash: Buffer.from("91".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(
      "does not carry redeemers",
    );
  });

  it("accepts always-succeeds Plutus spend plus mint txs with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const mintScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX);
    const datum = makePlutusIntegerData(7n);
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("08", "hex");
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const base = buildNativeTx({
      mintPreimageCbor: makeMintPreimage(policyId, assetName, 1n),
      scriptWitnessItems: [
        Buffer.from(spendScript.to_cbor_bytes()),
        Buffer.from(mintScript.to_cbor_bytes()),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
        { tag: CML.RedeemerTag.Mint, index: 0n },
      ]),
      outputCbors: [mintedOutput],
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(spendScript.hash(), 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts always-succeeds Plutus observers with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
    const withdrawScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX,
    );
    const datum = makePlutusIntegerData(11n);
    const base = buildNativeTx({
      scriptWitnessItems: [
        Buffer.from(spendScript.to_cbor_bytes()),
        Buffer.from(withdrawScript.to_cbor_bytes()),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
        { tag: CML.RedeemerTag.Reward, index: 0n },
      ]),
      requiredObserverItems: [
        Buffer.from(withdrawScript.hash().to_raw_bytes()),
      ],
      networkId: 0n,
    });
    const { txId, txCbor, inputOutRef, referenceInputOutRef } =
      attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(spendScript.hash(), 3_000_000n, { datum }),
      ],
      [referenceInputOutRef.toString("hex"), makeOutput(TEST_ADDRESS, 2_000_000n)],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState, {
      evaluatePlutusTx: evaluatePlutusTxWithLocalPhaseTwo,
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects malformed native required observer preimages", async () => {
    const base = buildNativeTx();
    const malformedRequiredObserversPreimageCbor = encodeByteList([
      Buffer.from("01", "hex"),
    ]);
    const tx: MidgardNativeTxFull = {
      ...base.tx,
      body: {
        ...base.tx.body,
        requiredObserversRoot: computeHash32(
          malformedRequiredObserversPreimageCbor,
        ),
        requiredObserversPreimageCbor: malformedRequiredObserversPreimageCbor,
      },
    };
    tx.compact = deriveMidgardNativeTxCompact(tx.body, tx.witnessSet, "TxIsValid");
    const txId = computeMidgardNativeTxIdFromFull(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
  });

  it("rejects malformed native required signer preimages", async () => {
    const base = buildNativeTx();
    const malformedRequiredSignersPreimageCbor = encodeByteList([
      Buffer.alloc(27, 0x11),
    ]);
    const tx: MidgardNativeTxFull = {
      ...base.tx,
      body: {
        ...base.tx.body,
        requiredSignersRoot: computeHash32(malformedRequiredSignersPreimageCbor),
        requiredSignersPreimageCbor: malformedRequiredSignersPreimageCbor,
      },
    };
    tx.compact = deriveMidgardNativeTxCompact(tx.body, tx.witnessSet, "TxIsValid");
    const txId = computeMidgardNativeTxIdFromFull(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
  });

  it("rejects legacy Cardano payloads in native-only phase A", async () => {
    const cardanoTx = txFixtures[0];
    const result = await Effect.runPromise(
      runPhaseAValidation(
        [
          mkQueued(
            Buffer.from(cardanoTx.txId, "hex"),
            Buffer.from(cardanoTx.cborHex, "hex"),
          ),
        ],
        phaseAConfig,
      ),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.CborDeserialization);
  });

  it("rejects legacy Cardano payloads in native-only utility decoders", async () => {
    const cardanoTxBytes = Buffer.from(txFixtures[0].cborHex, "hex");
    const cardanoTxId = Buffer.from(txFixtures[0].txId, "hex");

    await expect(Effect.runPromise(breakDownTx(cardanoTxBytes))).rejects.toThrow();
    await expect(
      Effect.runPromise(findSpentAndProducedUTxOs(cardanoTxBytes, cardanoTxId)),
    ).rejects.toThrow();
  });

  it("breaks down native txs into spent/produced outputs", async () => {
    const { txId, txCbor, inputOutRef, outputCbor } = buildNativeTx();
    const result = await Effect.runPromise(breakDownTx(txCbor));

    expect(result.txId.equals(txId)).toBe(true);
    expect(result.spent).toHaveLength(1);
    expect(result.spent[0].equals(inputOutRef)).toBe(true);
    expect(result.produced).toHaveLength(1);
    expect(result.produced[0].output.equals(outputCbor)).toBe(true);
    expect(result.produced[0].address).toBe(TEST_ADDRESS);
  });

  it("finds spent/produced sets for native tx payloads", async () => {
    const { txId, txCbor, inputOutRef, outputCbor } = buildNativeTx();
    const result = await Effect.runPromise(findSpentAndProducedUTxOs(txCbor, txId));
    const expectedOutRef = Buffer.from(
      CML.TransactionInput.new(CML.TransactionHash.from_raw_bytes(txId), 0n).to_cbor_bytes(),
    );

    expect(result.spent).toHaveLength(1);
    expect(result.spent[0].equals(inputOutRef)).toBe(true);
    expect(result.produced).toHaveLength(1);
    expect(result.produced[0].outref.equals(expectedOutRef)).toBe(true);
    expect(result.produced[0].output.equals(outputCbor)).toBe(true);
  });

  it("uses indexed outrefs for multiple produced outputs", async () => {
    const { txId, txCbor } = buildNativeTx({ outputCount: 2 });
    const result = await Effect.runPromise(findSpentAndProducedUTxOs(txCbor, txId));
    const txHash = CML.TransactionHash.from_raw_bytes(txId);
    const expectedOutRef0 = Buffer.from(CML.TransactionInput.new(txHash, 0n).to_cbor_bytes());
    const expectedOutRef1 = Buffer.from(CML.TransactionInput.new(txHash, 1n).to_cbor_bytes());

    expect(result.produced).toHaveLength(2);
    expect(result.produced[0].outref.equals(expectedOutRef0)).toBe(true);
    expect(result.produced[1].outref.equals(expectedOutRef1)).toBe(true);
  });
});
