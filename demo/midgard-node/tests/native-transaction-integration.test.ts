import fs from "node:fs";
import path from "node:path";

import {
  decodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeScript,
  deriveMidgardNativeTxBodyCompactV1,
  deriveMidgardNativeTxCompactV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxWitnessSetCanonicalV1,
  type MidgardVersionedScript,
  type ScriptLanguageName,
  ScriptLanguageTags,
} from "@al-ft/midgard-core/codec";
import {
  plutusDataToCborHex,
  type QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  txOutRefData,
} from "@al-ft/midgard-validation";
import { buildMidgardCanonicalCekProgramV1 } from "@al-ft/midgard-validation/cek-program";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { encode } from "cborg";
import { Effect } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { breakDownTx, findSpentAndProducedUTxOs } from "@/utils.js";

import {
  hashMidgardV1Script,
  hashPlutusV3Script,
  makeMidgardTxOutput,
  protectOutputAddressBytes,
} from "./midgard-output-helpers.js";

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);
const EMPTY_REDEEMER_DATA = Buffer.from(Data.to(new Constr(0, [])), "hex");

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
const MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_spend_guard.else",
);
const MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.midgard_v1_spend_out_ref_guard.else");
const MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_receive_guard.else",
);
const MIDGARD_V1_OBSERVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_observe_guard.else",
);
const MIDGARD_V1_CONTEXT_PROBE_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_context_probe.else",
);
const PLUTUS_V3_CONTEXT_PROBE_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.plutus_v3_context_probe.else",
);

type ScriptWitnessItem = MidgardVersionedScript | Uint8Array;

const testProgramMaterial = new Map<string, MidgardCekProgramMaterialEntryV1>();

const registerTestProgramMaterial = (
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
): void => {
  for (const entry of entries) {
    const key = Buffer.from(entry.root).toString("hex");
    const prior = testProgramMaterial.get(key);
    if (
      prior !== undefined &&
      (prior.kind !== entry.kind ||
        !Buffer.from(prior.preimage).equals(entry.preimage))
    ) {
      throw new Error(`test CEK program material collision at ${key}`);
    }
    testProgramMaterial.set(key, entry);
  }
};

const canonicalizeTestProofScript = (
  script: MidgardVersionedScript,
): MidgardVersionedScript => {
  if (script.language === "NativeCardano") return script;
  try {
    decodeMidgardCekProgramEnvelopeV1(script.scriptBytes);
    return script;
  } catch {
    try {
      const canonical = buildMidgardCanonicalCekProgramV1(script.scriptBytes);
      registerTestProgramMaterial(canonical.material.values());
      return {
        language: script.language,
        scriptBytes: canonical.envelopeCbor,
      };
    } catch {
      // Keep malformed authoring bytes only so consensus admission can prove
      // that it rejects unsupported program encodings fail closed.
      return script;
    }
  }
};

const makeAlwaysSucceedsScript = (compiledCode: string): CML.Script => {
  const canonical = canonicalizeTestProofScript({
    language: "PlutusV3",
    scriptBytes: Buffer.from(compiledCode, "hex"),
  });
  return CML.Script.new_plutus_v3(
    CML.PlutusV3Script.from_raw_bytes(canonical.scriptBytes),
  );
};

const makeRawUplcWitness = (scriptHex: string): MidgardVersionedScript =>
  canonicalizeTestProofScript({
    language: "MidgardV1",
    scriptBytes: Buffer.from(scriptHex, "hex"),
  });

const cmlScriptToScriptWitness = (
  script: CML.Script,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decoded = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 !== undefined) {
    return {
      language: "PlutusV3",
      scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
    };
  }
  throw new Error(
    "native integration tests only support NativeCardano, PlutusV3, and MidgardV1 witnesses",
  );
};

const scriptWitnessItemToVersioned = (
  item: ScriptWitnessItem,
  index: number,
): MidgardVersionedScript => {
  if ("language" in item) {
    return item;
  }
  const bytes = Buffer.from(item);
  try {
    return cmlScriptToScriptWitness(CML.Script.from_cbor_bytes(bytes));
  } catch {
    try {
      const decoded = decodeMidgardNativeScript(bytes);
      return {
        language: "NativeCardano",
        scriptBytes: decoded.cbor,
        nativeScript: decoded.script,
      };
    } catch (e) {
      throw new Error(`invalid script witness item #${index}`, {
        cause: e,
      });
    }
  }
};

const makeTypedPlutusV3Witness = (scriptHex: string): CML.Script => {
  const canonical = canonicalizeTestProofScript({
    language: "PlutusV3",
    scriptBytes: Buffer.from(scriptHex, "hex"),
  });
  return CML.Script.new_plutus_v3(
    CML.PlutusV3Script.from_raw_bytes(canonical.scriptBytes),
  );
};

const midgardV1Hash = (scriptHex: string): string =>
  hashMidgardV1Script(
    canonicalizeTestProofScript({
      language: "MidgardV1",
      scriptBytes: Buffer.from(scriptHex, "hex"),
    }).scriptBytes,
  );

const plutusV3Hash = (scriptHex: string): string =>
  hashPlutusV3Script(
    canonicalizeTestProofScript({
      language: "PlutusV3",
      scriptBytes: Buffer.from(scriptHex, "hex"),
    }).scriptBytes,
  );

const uniqueScriptWitnessItems = (
  scripts: readonly CML.Script[],
): readonly MidgardVersionedScript[] =>
  Array.from(
    new Map(
      scripts.map((script) => [
        script.hash().to_hex(),
        cmlScriptToScriptWitness(script),
      ]),
    ).values(),
  );

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const makeOutRef = (txHashByte: number, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(
        Buffer.alloc(32, txHashByte).toString("hex"),
      ),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeOutput = (address: string, lovelace: bigint): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

const makePubKeyOutput = (
  keyHash: CML.Ed25519KeyHash,
  lovelace: bigint,
): Buffer => makePubKeyValueOutput(keyHash, CML.Value.from_coin(lovelace));

const makePubKeyValueOutput = (
  keyHash: CML.Ed25519KeyHash,
  value: CML.Value,
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
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
): CML.Value => {
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
): CML.Value => {
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

const makeValueOutput = (address: string, value: CML.Value): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      value,
    ).to_cbor_bytes(),
  );

const makeScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(scriptHash),
      ).to_address(),
      CML.Value.from_coin(lovelace),
      opts?.datum !== undefined
        ? CML.DatumOption.new_datum(opts.datum)
        : undefined,
      opts?.scriptRef,
    ).to_cbor_bytes(),
  );

const makeDatumHashScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  datumHash: CML.DatumHash,
  scriptRef?: CML.Script,
): Buffer => {
  const output = CML.ConwayFormatTxOut.new(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(scriptHash),
    ).to_address(),
    CML.Value.from_coin(lovelace),
  );
  output.set_datum_option(CML.DatumOption.new_hash(datumHash));
  if (scriptRef !== undefined) {
    output.set_script_reference(scriptRef);
  }
  return Buffer.from(
    CML.TransactionOutput.new_conway_format_tx_out(output).to_cbor_bytes(),
  );
};

const makeProtectedScriptOutput = (
  scriptHash: CML.ScriptHash,
  lovelace: bigint,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  protectOutputAddressBytes(makeScriptOutput(scriptHash, lovelace, opts));

const makeScriptValueOutput = (
  scriptHash: CML.ScriptHash,
  value: CML.Value,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(scriptHash),
      ).to_address(),
      value,
      opts?.datum !== undefined
        ? CML.DatumOption.new_datum(opts.datum)
        : undefined,
      opts?.scriptRef,
    ).to_cbor_bytes(),
  );

const makeProtectedScriptValueOutput = (
  scriptHash: CML.ScriptHash,
  value: CML.Value,
  opts?: {
    readonly datum?: CML.PlutusData;
    readonly scriptRef?: CML.Script;
  },
): Buffer =>
  protectOutputAddressBytes(makeScriptValueOutput(scriptHash, value, opts));

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
        Buffer.from(item.data ?? EMPTY_REDEEMER_DATA),
        [
          item.exUnits?.[0] ?? 1_000_000_000n,
          item.exUnits?.[1] ?? 1_000_000_000n,
        ],
      ]),
    ),
  );

const makePlutusDataBytes = (value: unknown): Buffer =>
  Buffer.from(plutusDataToCborHex(value), "hex");

const makeOutRefDataBytes = (outRef: Buffer): Buffer =>
  makePlutusDataBytes(txOutRefData(outRef.toString("hex")));

const makePlutusContextProbeRedeemer = (opts: {
  readonly expectedOutputReference: Buffer;
  readonly expectedDatum: bigint;
  readonly expectedSigner: string;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedPolicy: string;
  readonly expectedAssetName: Buffer;
  readonly expectedMintQuantity: bigint;
  readonly expectedObserver: string;
}): Buffer =>
  makePlutusDataBytes(
    new Constr(0, [
      txOutRefData(opts.expectedOutputReference.toString("hex")),
      opts.expectedDatum,
      opts.expectedSigner,
      txOutRefData(opts.expectedFirstReference.toString("hex")),
      txOutRefData(opts.expectedSecondReference.toString("hex")),
      opts.expectedPolicy,
      opts.expectedAssetName.toString("hex"),
      opts.expectedMintQuantity,
      opts.expectedObserver,
    ]),
  );

const makeMidgardV1ContextProbeRedeemer = (opts: {
  readonly expectedSpendScriptHash: string;
  readonly expectedOwnRef: Buffer;
  readonly expectedFirstInput: Buffer;
  readonly expectedSecondInput: Buffer;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedAssetName: Buffer;
  readonly expectedMintQuantity: bigint;
  readonly expectedMintRedeemer: unknown;
  readonly expectedObserveRedeemer: unknown;
  readonly expectedReceiveScriptHash: string;
  readonly expectedReceiveRedeemer: unknown;
}): Buffer =>
  makePlutusDataBytes(
    new Constr(0, [
      opts.expectedSpendScriptHash,
      txOutRefData(opts.expectedOwnRef.toString("hex")),
      txOutRefData(opts.expectedFirstInput.toString("hex")),
      txOutRefData(opts.expectedSecondInput.toString("hex")),
      txOutRefData(opts.expectedFirstReference.toString("hex")),
      txOutRefData(opts.expectedSecondReference.toString("hex")),
      opts.expectedFirstOutputScriptHash,
      opts.expectedSecondOutputScriptHash,
      opts.expectedSigner,
      opts.expectedObserver,
      opts.expectedPolicy,
      opts.expectedAssetName.toString("hex"),
      opts.expectedMintQuantity,
      opts.expectedMintRedeemer,
      opts.expectedObserveRedeemer,
      opts.expectedReceiveScriptHash,
      opts.expectedReceiveRedeemer,
    ]),
  );

const makePlutusIntegerData = (value: bigint): CML.PlutusData =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

const makeMintPreimage = (
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
  readonly requiredObserverItems?: readonly Uint8Array[];
  readonly scriptWitnessItems?: readonly ScriptWitnessItem[];
  readonly witnessMode?: "none" | "valid" | "invalid";
  readonly witnessSignerPrivateKey?: CML.PrivateKey;
  readonly mintPreimageCbor?: Buffer;
  readonly networkId?: bigint;
  readonly outputCount?: number;
  readonly outputCbors?: readonly Buffer[];
  readonly scriptIntegrityHash?: Buffer;
  readonly scriptLanguages?: readonly ScriptLanguageName[];
  readonly version?: bigint;
  readonly spendInputOutRefs?: readonly Buffer[];
  readonly referenceInputOutRefs?: readonly Buffer[];
}): {
  tx: MidgardNativeTxFullV1;
  txId: Buffer;
  txCbor: Buffer;
  inputOutRef: Buffer;
  referenceInputOutRef: Buffer;
  outputCbor: Buffer;
} => {
  const spendInputs = opts?.spendInputOutRefs ?? [makeOutRef(0x11, 0n)];
  const referenceInputs = opts?.referenceInputOutRefs ?? [makeOutRef(0x22, 1n)];
  const spendInput = spendInputs[0];
  const referenceInput = referenceInputs[0];
  const defaultOutput = Buffer.from(
    makeMidgardTxOutput(
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

  const spendInputsPreimageCbor = encodeByteList(spendInputs);
  const referenceInputsPreimageCbor = encodeByteList(referenceInputs);
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
          Buffer.from(
            witnessSignerPrivateKey.to_public().hash().to_raw_bytes(),
          ),
        ]);
  const mintPreimageCbor = opts?.mintPreimageCbor ?? EMPTY_CBOR_LIST;

  const scriptTxWitsPreimageCbor =
    opts?.scriptWitnessItems === undefined
      ? EMPTY_CBOR_LIST
      : encodeMidgardVersionedScriptListPreimage(
          opts.scriptWitnessItems
            .map(scriptWitnessItemToVersioned)
            .map(canonicalizeTestProofScript),
        );
  const redeemerTxWitsPreimageCbor =
    opts?.redeemerTxWitsPreimageCbor ?? EMPTY_CBOR_LIST;
  const scriptIntegrityHash =
    opts?.scriptIntegrityHash ??
    (opts?.scriptLanguages !== undefined
      ? computeScriptIntegrityHashForLanguages(
          deriveMidgardNativeTxWitnessSetCompactV1({
            addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
            scriptTxWitsPreimageCbor,
            redeemerTxWitsPreimageCbor,
          }).redeemerTxWitsHash,
          opts.scriptLanguages,
        )
      : computeHash32(EMPTY_CBOR_NULL));
  const version = opts?.version ?? MIDGARD_NATIVE_TX_V1_VERSION;

  const body: MidgardNativeTxBodyCanonicalV1 = {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash,
    auxiliaryDataHash: computeHash32(EMPTY_CBOR_NULL),
    networkId: opts?.networkId ?? 0n,
  };

  const signedBodyHash =
    witnessMode === "invalid"
      ? Buffer.alloc(32, 0x7f)
      : computeMidgardNativeTxIdV1({
          version,
          transactionBody: deriveMidgardNativeTxBodyCompactV1(body),
          transactionWitnessSetHash: Buffer.alloc(32),
          validity: "TxIsValid",
        });
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

  const witnessSet: MidgardNativeTxWitnessSetCanonicalV1 = {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };

  const tx: MidgardNativeTxFullV1 = {
    version,
    validity: "TxIsValid",
    compact: deriveMidgardNativeTxCompactV1(
      body,
      witnessSet,
      "TxIsValid",
      version,
    ),
    body,
    witnessSet,
  };

  const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
  const txId = computeMidgardNativeTxIdV1(tx);

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
  usedLanguages: readonly (number | ScriptLanguageName)[],
): ReturnType<typeof buildNativeTx> => {
  const languages = usedLanguages.map((language): ScriptLanguageName => {
    if (language === CML.Language.PlutusV3) {
      return "PlutusV3";
    }
    if (language === ScriptLanguageTags.MidgardV1) {
      return "MidgardV1";
    }
    if (language === "PlutusV3" || language === "MidgardV1") {
      return language;
    }
    throw new Error(`unsupported script language in test: ${String(language)}`);
  });

  const scriptIntegrityHash = computeScriptIntegrityHashForLanguages(
    deriveMidgardNativeTxWitnessSetCompactV1(fixture.tx.witnessSet)
      .redeemerTxWitsHash,
    languages,
  );

  const body: MidgardNativeTxBodyCanonicalV1 = {
    ...fixture.tx.body,
    scriptIntegrityHash,
  };
  const tx: MidgardNativeTxFullV1 = {
    ...fixture.tx,
    body,
    compact: deriveMidgardNativeTxCompactV1(
      body,
      fixture.tx.witnessSet,
      fixture.tx.validity,
      fixture.tx.version,
    ),
  };

  return {
    ...fixture,
    tx,
    txId: computeMidgardNativeTxIdV1(tx),
    txCbor: encodeMidgardNativeTxCanonicalV1(tx),
  };
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
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([
    ...testProgramMaterial.values(),
  ]),
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

const runBothPhases = async (
  txId: Buffer,
  txCbor: Buffer,
  preState: Map<string, Buffer>,
  phaseBOptions?: {
    readonly enforceScriptBudget?: boolean;
  },
) => {
  const phaseA = await Effect.runPromise(
    runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
  );
  const { accepted, rejected } = await Effect.runPromise(
    runPhaseBValidationWithPatch(phaseA.accepted, preState, {
      nowCardanoSlotNo: 0n,
      bucketConcurrency: 1,
      ...phaseBOptions,
    }),
  );
  return { phaseA, phaseB: { accepted, rejected } };
};

type BothPhaseResult = Awaited<ReturnType<typeof runBothPhases>>;
type PhaseAResult = BothPhaseResult["phaseA"];
type PhaseBResult = BothPhaseResult["phaseB"];
type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

const expectPhaseAAcceptsOne = (phaseA: PhaseAResult) => {
  expect(
    phaseA.rejected,
    JSON.stringify(phaseA.rejected, null, 2),
  ).toHaveLength(0);
  expect(phaseA.accepted).toHaveLength(1);
};

const expectPhaseBAcceptsOne = (phaseB: PhaseBResult) => {
  expect(
    phaseB.rejected,
    JSON.stringify(phaseB.rejected, null, 2),
  ).toHaveLength(0);
  expect(phaseB.accepted).toHaveLength(1);
};

const expectBothPhasesAcceptOne = ({ phaseA, phaseB }: BothPhaseResult) => {
  expectPhaseAAcceptsOne(phaseA);
  expectPhaseBAcceptsOne(phaseB);
};

const expectPhaseBRejectsOne = (
  phaseB: PhaseBResult,
  code: RejectCode,
  detail?: string,
) => {
  expect(phaseB.accepted).toHaveLength(0);
  expect(phaseB.rejected).toHaveLength(1);
  expect(phaseB.rejected[0].code).toBe(code);
  if (detail !== undefined) {
    expect(phaseB.rejected[0].detail).toContain(detail);
  }
};

const expectPhaseAAcceptsAndPhaseBRejectsOne = (
  result: BothPhaseResult,
  code: RejectCode,
  detail?: string,
) => {
  expectPhaseAAcceptsOne(result.phaseA);
  expectPhaseBRejectsOne(result.phaseB, code, detail);
};

const makeScriptSpendPreState = (opts: {
  readonly inputOutRef: Buffer;
  readonly referenceInputOutRef: Buffer;
  readonly scriptHash: CML.ScriptHash;
  readonly datum?: CML.PlutusData;
  readonly referenceOutput?: Buffer;
}): Map<string, Buffer> =>
  new Map<string, Buffer>([
    [
      opts.inputOutRef.toString("hex"),
      makeScriptOutput(
        opts.scriptHash,
        3_000_000n,
        opts.datum === undefined ? undefined : { datum: opts.datum },
      ),
    ],
    [
      opts.referenceInputOutRef.toString("hex"),
      opts.referenceOutput ?? makeOutput(TEST_ADDRESS, 2_000_000n),
    ],
  ]);

const runPlutusV3SpendScenario = async (opts: {
  readonly spendScript: CML.Script;
  readonly datum?: CML.PlutusData;
  readonly txOptions?: Parameters<typeof buildNativeTx>[0];
  readonly attachScriptIntegrityHash?: boolean;
  readonly referenceOutput?: Buffer;
  readonly phaseBOptions?: Parameters<typeof runBothPhases>[3];
}) => {
  const base = buildNativeTx(opts.txOptions);
  const tx =
    opts.attachScriptIntegrityHash === false
      ? base
      : attachComputedScriptIntegrityHash(base, [CML.Language.PlutusV3]);
  const preState = makeScriptSpendPreState({
    inputOutRef: tx.inputOutRef,
    referenceInputOutRef: tx.referenceInputOutRef,
    scriptHash: opts.spendScript.hash(),
    datum: opts.datum,
    referenceOutput: opts.referenceOutput,
  });

  return {
    ...tx,
    ...(await runBothPhases(tx.txId, tx.txCbor, preState, opts.phaseBOptions)),
  };
};

type MidgardV1ContextProbeRedeemerOverrides = Partial<{
  readonly expectedFirstInput: Buffer;
  readonly expectedSecondInput: Buffer;
  readonly expectedFirstReference: Buffer;
  readonly expectedSecondReference: Buffer;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedMintQuantity: bigint;
  readonly expectedReceiveScriptHash: string;
}>;

const runMidgardV1ContextProbeScenario = async (opts?: {
  readonly marker?: number;
  readonly redeemerOverrides?: MidgardV1ContextProbeRedeemerOverrides;
}) => {
  const signerKey = CML.PrivateKey.generate_ed25519();
  const probeHash = midgardV1Hash(MIDGARD_V1_CONTEXT_PROBE_SCRIPT_HEX);
  const mintHash = midgardV1Hash(ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX);
  const observerHash = midgardV1Hash(MIDGARD_V1_OBSERVE_GUARD_SCRIPT_HEX);
  const receiveHash = midgardV1Hash(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX);
  const secondOutputHash = midgardV1Hash(ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX);
  const scriptSpendOutRef = makeOutRef(0x10, 0n);
  const pubkeySpendOutRef = makeOutRef(0x30, 0n);
  const firstReferenceOutRef = makeOutRef(0x41, 0n);
  const secondReferenceOutRef = makeOutRef(0x42, 0n);
  const assetName = Buffer.from("10", "hex");
  const mintPolicyId = Buffer.from(mintHash, "hex");
  const emptyRedeemer = new Constr(0, []);
  const receiveRedeemer = 99n;
  const observeRedeemer = 77n;
  const base = buildNativeTx({
    spendInputOutRefs: [pubkeySpendOutRef, scriptSpendOutRef],
    referenceInputOutRefs: [secondReferenceOutRef, firstReferenceOutRef],
    mintPreimageCbor: makeMintPreimage([
      { policyId: mintPolicyId, assetName: assetName, quantity: 1n },
    ]),
    scriptWitnessItems: [
      makeRawUplcWitness(MIDGARD_V1_CONTEXT_PROBE_SCRIPT_HEX),
      makeRawUplcWitness(ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX),
      makeRawUplcWitness(MIDGARD_V1_OBSERVE_GUARD_SCRIPT_HEX),
      makeRawUplcWitness(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX),
    ],
    redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        data: makeMidgardV1ContextProbeRedeemer({
          expectedSpendScriptHash: probeHash,
          expectedOwnRef: scriptSpendOutRef,
          expectedFirstInput:
            opts?.redeemerOverrides?.expectedFirstInput ?? scriptSpendOutRef,
          expectedSecondInput:
            opts?.redeemerOverrides?.expectedSecondInput ?? pubkeySpendOutRef,
          expectedFirstReference:
            opts?.redeemerOverrides?.expectedFirstReference ??
            firstReferenceOutRef,
          expectedSecondReference:
            opts?.redeemerOverrides?.expectedSecondReference ??
            secondReferenceOutRef,
          expectedFirstOutputScriptHash:
            opts?.redeemerOverrides?.expectedFirstOutputScriptHash ??
            receiveHash,
          expectedSecondOutputScriptHash:
            opts?.redeemerOverrides?.expectedSecondOutputScriptHash ??
            secondOutputHash,
          expectedSigner:
            opts?.redeemerOverrides?.expectedSigner ??
            signerKey.to_public().hash().to_hex(),
          expectedObserver:
            opts?.redeemerOverrides?.expectedObserver ?? observerHash,
          expectedPolicy: opts?.redeemerOverrides?.expectedPolicy ?? mintHash,
          expectedAssetName: assetName,
          expectedMintQuantity:
            opts?.redeemerOverrides?.expectedMintQuantity ?? 1n,
          expectedMintRedeemer: emptyRedeemer,
          expectedObserveRedeemer: observeRedeemer,
          expectedReceiveScriptHash:
            opts?.redeemerOverrides?.expectedReceiveScriptHash ?? receiveHash,
          expectedReceiveRedeemer: receiveRedeemer,
        }),
      },
      { tag: MidgardRedeemerTag.Mint, index: 0n },
      {
        tag: MidgardRedeemerTag.Reward,
        index: 0n,
        data: makePlutusDataBytes(observeRedeemer),
      },
      {
        tag: MidgardRedeemerTag.Receiving,
        index: 0n,
        data: makePlutusDataBytes(receiveRedeemer),
      },
    ]),
    requiredObserverItems: [Buffer.from(observerHash, "hex")],
    networkId: 0n,
    scriptLanguages: ["MidgardV1"],
    witnessMode: "valid",
    witnessSignerPrivateKey: signerKey,
    outputCbors: [
      makeProtectedScriptValueOutput(
        CML.ScriptHash.from_hex(receiveHash),
        makeSingleAssetValue(3_000_000n, mintPolicyId, assetName, 1n),
      ),
      makeScriptOutput(CML.ScriptHash.from_hex(secondOutputHash), 1_000_000n),
    ],
  });
  const preState = new Map<string, Buffer>([
    [
      scriptSpendOutRef.toString("hex"),
      makeScriptOutput(CML.ScriptHash.from_hex(probeHash), 3_000_000n, {
        datum: makePlutusIntegerData(7n),
      }),
    ],
    [
      pubkeySpendOutRef.toString("hex"),
      makePubKeyOutput(signerKey.to_public().hash(), 1_000_000n),
    ],
    [
      firstReferenceOutRef.toString("hex"),
      makeOutput(TEST_ADDRESS, 2_000_000n),
    ],
    [
      secondReferenceOutRef.toString("hex"),
      makeOutput(TEST_ADDRESS, 2_000_000n),
    ],
  ]);
  return {
    ...(await runBothPhases(base.txId, base.txCbor, preState)),
    probeHash,
  };
};

describe("native transaction integration", () => {
  beforeEach(() => {
    testProgramMaterial.clear();
  });

  it("accepts a canonical native tx in phase A", async () => {
    const { txId, txCbor, inputOutRef } = buildNativeTx();
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(
      result.rejected,
      JSON.stringify(result.rejected, null, 2),
    ).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    const accepted = result.accepted[0];
    expect(accepted.ledgerTx.txId.equals(txId)).toBe(true);
    expect(accepted.graph.spentOutRefHexes).toStrictEqual([
      inputOutRef.toString("hex"),
    ]);
    expect(accepted.graph.produced).toHaveLength(1);
    expect(accepted.derived.witnessKeyHashHexes).toStrictEqual([]);
    expect(accepted.derived.nativeScriptHashHexes).toStrictEqual([]);
    expect(accepted.derived.outputSum.lovelace).toBe(3_000_000n);
  });

  it("accepts required signer when native witness signature is valid", async () => {
    const { txId, txCbor } = buildNativeTx({ witnessMode: "valid" });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(
      result.rejected,
      JSON.stringify(result.rejected, null, 2),
    ).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    expect(result.accepted[0].derived.witnessKeyHashHexes.length).toBe(1);
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
    const tx: MidgardNativeTxFullV1 = {
      ...base.tx,
      body: {
        ...base.tx.body,
        auxiliaryDataHash: Buffer.from("99".repeat(32), "hex"),
      },
    };
    const fullTx: MidgardNativeTxFullV1 = {
      ...tx,
      compact: deriveMidgardNativeTxCompactV1(
        tx.body,
        tx.witnessSet,
        tx.validity,
        tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdV1(fullTx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(fullTx);

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

  it("rejects malformed Plutus witness programs fail closed in phase A", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("deadbeef", "hex"),
    );
    const wrappedPlutusScript = CML.Script.new_plutus_v3(plutusScript);
    const redeemerBytes = makeLegacyRedeemersCbor([
      { tag: CML.RedeemerTag.Spend, index: 0n },
    ]);
    const { phaseA } = await runPlutusV3SpendScenario({
      spendScript: wrappedPlutusScript,
      datum: makePlutusIntegerData(42n),
      attachScriptIntegrityHash: false,
      txOptions: {
        scriptWitnessItems: [Buffer.from(wrappedPlutusScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: redeemerBytes,
        scriptLanguages: ["PlutusV3"],
      },
    });

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.ScriptProgramEncoding);
    expect(phaseA.rejected[0].detail).toContain(
      "not a canonical bounded V1 program envelope",
    );
  });

  it("rejects phase A when plutus observer evaluation cannot be reconstructed without network id", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(
      observerScript.hash().to_raw_bytes(),
    );
    const wrappedPlutusScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
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
    const tx: MidgardNativeTxFullV1 = {
      ...base.tx,
      body: {
        ...base.tx.body,
        referenceInputsPreimageCbor: duplicatedReferenceInputsPreimageCbor,
      },
    };
    const fullTx: MidgardNativeTxFullV1 = {
      ...tx,
      compact: deriveMidgardNativeTxCompactV1(
        tx.body,
        tx.witnessSet,
        tx.validity,
        tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdV1(fullTx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(fullTx);

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
    const observerScriptHash = Buffer.from(
      observerScript.hash().to_raw_bytes(),
    );
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(
      phaseA.rejected,
      JSON.stringify(phaseA.rejected, null, 2),
    ).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].derived.requiredObserverHashHexes).toStrictEqual([
      observerScriptHash.toString("hex"),
    ]);
    expect(
      phaseB.rejected,
      JSON.stringify(phaseB.rejected, null, 2),
    ).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts one inline native script witness reused for observer and mint", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const script = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const scriptHash = Buffer.from(script.hash().to_raw_bytes());
    const assetName = Buffer.from("61", "hex");
    const mintPreimageCbor = makeMintPreimage([
      { policyId: scriptHash, assetName: assetName, quantity: 1n },
    ]);
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, scriptHash, assetName, 1n),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [scriptHash],
      mintPreimageCbor,
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(script).to_cbor_bytes()),
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].derived.requiredObserverHashHexes).toStrictEqual([
      scriptHash.toString("hex"),
    ]);
    expect(phaseA.accepted[0].derived.mintPolicyHashHexes).toStrictEqual([
      scriptHash.toString("hex"),
    ]);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts protected native receive outputs through an inline native script witness", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const receiveScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const receiveHash = Buffer.from(receiveScript.hash().to_raw_bytes());
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [
        Buffer.from(CML.Script.new_native(receiveScript).to_cbor_bytes()),
      ],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [
        makeProtectedScriptOutput(
          CML.ScriptHash.from_raw_bytes(receiveHash),
          3_000_000n,
        ),
      ],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    expectBothPhasesAcceptOne(await runBothPhases(txId, txCbor, preState));
  });

  it("accepts required observers when satisfied by a reference native script", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(
      observerScript.hash().to_raw_bytes(),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerScriptHash],
      witnessMode: "valid",
      witnessSignerPrivateKey: observerKey,
    });

    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
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

    expectBothPhasesAcceptOne(await runBothPhases(txId, txCbor, preState));
  });

  it("rejects required observers when a matching reference native script is not satisfied by tx signers", async () => {
    const observerKey = CML.PrivateKey.generate_ed25519();
    const spendSignerKey = CML.PrivateKey.generate_ed25519();
    const observerScript = CML.NativeScript.new_script_pubkey(
      observerKey.to_public().hash(),
    );
    const observerScriptHash = Buffer.from(
      observerScript.hash().to_raw_bytes(),
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      requiredObserverItems: [observerScriptHash],
      witnessMode: "valid",
      witnessSignerPrivateKey: spendSignerKey,
    });

    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
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

    expectPhaseAAcceptsAndPhaseBRejectsOne(
      await runBothPhases(txId, txCbor, preState),
      RejectCodes.NativeScriptInvalid,
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    expectPhaseAAcceptsAndPhaseBRejectsOne(
      await runBothPhases(txId, txCbor, preState),
      RejectCodes.MissingRequiredWitness,
      observerHash.toString("hex"),
    );
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    expectPhaseAAcceptsAndPhaseBRejectsOne(
      await runBothPhases(txId, txCbor, preState),
      RejectCodes.InvalidFieldType,
      "extraneous native script witness",
    );
  });

  it("rejects extraneous inline non-native script witnesses", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const extraneousScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { txId, txCbor, inputOutRef, referenceInputOutRef } = buildNativeTx({
      scriptWitnessItems: [Buffer.from(extraneousScript.to_cbor_bytes())],
      scriptIntegrityHash: Buffer.from("73".repeat(32), "hex"),
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    expectPhaseAAcceptsAndPhaseBRejectsOne(
      await runBothPhases(txId, txCbor, preState),
      RejectCodes.InvalidFieldType,
      "extraneous non-native script witness",
    );
  });

  it("accepts non-empty mint when satisfied by an inline native mint policy script", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("01", "hex");
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].derived.mintPolicyHashHexes).toStrictEqual([
      policyId.toString("hex"),
    ]);
    expect(
      phaseA.accepted[0].derived.mintDelta.assets
        .get(policyId.toString("hex"))
        ?.get(assetName.toString("hex")),
    ).toBe(1n);
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
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      makeMidgardTxOutput(
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
    expect(
      phaseB.rejected,
      JSON.stringify(phaseB.rejected, null, 2),
    ).toHaveLength(0);
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
    const referencePolicyId = Buffer.from(
      referenceScript.hash().to_raw_bytes(),
    );
    const inlineAssetName = Buffer.from("0a", "hex");
    const referenceAssetName = Buffer.from("0b", "hex");
    const mintPreimageCbor = makeMintPreimage([
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
      makeMidgardTxOutput(
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
    expect(phaseA.accepted[0].derived.mintPolicyHashHexes).toStrictEqual(
      [inlinePolicyId.toString("hex"), referencePolicyId.toString("hex")].sort(
        (a, b) => a.localeCompare(b),
      ),
    );
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects non-empty mint when no matching native mint policy witness is available", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const policyId = Buffer.from("55".repeat(28), "hex");
    const assetName = Buffer.from("03", "hex");
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
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
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      makeMidgardTxOutput(
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
    const malformedMintPreimageCbor = makeMintPreimage([
      {
        policyId: Buffer.alloc(27, 0x11),
        assetName: Buffer.from("05", "hex"),
        quantity: 1n,
      },
    ]);
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

  it("rejects empty top-level mint maps instead of treating them as no mint", () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    expect(() =>
      buildNativeTx({
        mintPreimageCbor: makeEmptyMintMapPreimage(),
        witnessMode: "valid",
        witnessSignerPrivateKey: signerKey,
      }),
    ).toThrow(/native-V1 mint must be an empty array or non-empty map/u);
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
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
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
    const burnPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: -1n },
    ]);
    const inputValue = makeSingleAssetValue(
      3_000_000n,
      policyId,
      assetName,
      1n,
    );
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
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(
      phaseA.accepted[0].derived.mintDelta.assets
        .get(policyId.toString("hex"))
        ?.get(assetName.toString("hex")),
    ).toBe(-1n);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects malformed inline Plutus mint policies fail closed in phase A", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("1234abcd", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const policyId = Buffer.from(plutusScriptRef.hash().to_raw_bytes());
    const assetName = Buffer.from("08", "hex");
    const mintPreimageCbor = makeMintPreimage([
      { policyId: policyId, assetName: assetName, quantity: 1n },
    ]);
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
      scriptLanguages: ["PlutusV3"],
      outputCbors: [mintedOutput],
    });

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(plutusScriptRef.hash(), 3_000_000n),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.ScriptProgramEncoding);
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
            computeMidgardNativeTxIdV1(withScriptDataHash.tx),
          ),
          signerKey,
        ).to_cbor_bytes(),
      ),
    ]);
    const witnessSet: MidgardNativeTxWitnessSetCanonicalV1 = {
      ...withScriptDataHash.tx.witnessSet,
      addrTxWitsPreimageCbor: resignedWitnesses,
    };
    const tx: MidgardNativeTxFullV1 = {
      ...withScriptDataHash.tx,
      witnessSet,
      compact: deriveMidgardNativeTxCompactV1(
        withScriptDataHash.tx.body,
        witnessSet,
        withScriptDataHash.tx.validity,
        withScriptDataHash.tx.version,
      ),
    };
    const txId = computeMidgardNativeTxIdV1(tx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    const { inputOutRef, referenceInputOutRef } = withScriptDataHash;

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(
      phaseB.rejected,
      JSON.stringify(phaseB.rejected, null, 2),
    ).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects malformed reference-script Plutus spends at the authenticated material boundary", async () => {
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("4d696467617264", "hex"),
    );
    const plutusScriptRef = CML.Script.new_plutus_v3(plutusScript);
    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        plutusScriptRef,
      ).to_cbor_bytes(),
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript: plutusScriptRef,
      referenceOutput,
      attachScriptIntegrityHash: false,
      txOptions: {
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.CekProgramMaterial);
    expect(phaseB.rejected[0].detail).toContain(
      "invalid reference-script CEK material",
    );
  });

  it("rejects Plutus witness bundles when the evaluator reports script failure", async () => {
    const plutusScriptRef = makeAlwaysSucceedsScript(ALWAYS_FAILS_SCRIPT_HEX);
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript: plutusScriptRef,
      txOptions: {
        scriptWitnessItems: [Buffer.from(plutusScriptRef.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3"],
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("still enforces value preservation after local Plutus success", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(99n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
        outputCbors: [makeOutput(TEST_ADDRESS, 4_000_000n)],
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });

  it("accepts inline always-succeeds Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(99n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts local Plutus evaluation when declared ex-units cover the spent budget", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(99n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          {
            tag: CML.RedeemerTag.Spend,
            index: 0n,
            exUnits: [1_000_000_000n, 1_000_000_000n],
          },
        ]),
      },
      phaseBOptions: { enforceScriptBudget: true },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects local Plutus evaluation when declared ex-units are too low", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(99n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          {
            tag: CML.RedeemerTag.Spend,
            index: 0n,
            exUnits: [0n, 0n],
          },
        ]),
      },
      phaseBOptions: { enforceScriptBudget: true },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain("budget exceeded");
  });

  it("rejects inline always-fails Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(ALWAYS_FAILS_SCRIPT_HEX);
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(99n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
      },
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
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(1n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          {
            tag: CML.RedeemerTag.Spend,
            index: 0n,
            data: Buffer.from(makePlutusIntegerData(2n).to_cbor_bytes()),
          },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects Plutus datum-hash spends at the Midgard output boundary", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(9n);
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
        makeDatumHashScriptOutput(
          scriptHash,
          3_000_000n,
          CML.hash_plutus_data(datum),
        ),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidOutput);
    expect(phaseB.rejected[0].detail).toContain("value must be an array");
  });

  it("rejects produced outputs with datum hashes in phase A", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const scriptHash = spendScript.hash();
    const datum = makePlutusIntegerData(9n);
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      outputCbors: [
        makeDatumHashScriptOutput(
          scriptHash,
          3_000_000n,
          CML.hash_plutus_data(datum),
        ),
      ],
      scriptIntegrityHash: Buffer.from("92".repeat(32), "hex"),
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.accepted).toHaveLength(0);
    expect(phaseA.rejected).toHaveLength(1);
    expect(phaseA.rejected[0].code).toBe(RejectCodes.InvalidOutput);
    expect(phaseA.rejected[0].detail).toContain("value must be an array");
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(0);
  });

  it("rejects script integrity hashes that do not match Phase B language views", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
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
    const badScriptDataHash = Buffer.from(
      withScriptDataHash.tx.body.scriptIntegrityHash,
    );
    badScriptDataHash[0] ^= 0xff;
    const tx: MidgardNativeTxFullV1 = {
      ...withScriptDataHash.tx,
      body: {
        ...withScriptDataHash.tx.body,
        scriptIntegrityHash: badScriptDataHash,
      },
    };
    (tx as { compact: typeof tx.compact }).compact =
      deriveMidgardNativeTxCompactV1(
        tx.body,
        tx.witnessSet,
        tx.validity,
        tx.version,
      );
    const txId = computeMidgardNativeTxIdV1(tx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    const { inputOutRef, referenceInputOutRef } = withScriptDataHash;

    const preState = new Map<string, Buffer>([
      [
        inputOutRef.toString("hex"),
        makeScriptOutput(scriptHash, 3_000_000n, { datum }),
      ],
      [
        referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(txId, txCbor, preState);

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseB.rejected[0].detail).toContain(
      "script_integrity_hash mismatch",
    );
  });

  it("rejects when the required PlutusV3 language view is missing", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["MidgardV1"],
    });

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      new Map<string, Buffer>([
        [
          base.inputOutRef.toString("hex"),
          makeScriptOutput(spendScript.hash(), 3_000_000n, {
            datum: makePlutusIntegerData(1n),
          }),
        ],
        [
          base.referenceInputOutRef.toString("hex"),
          makeOutput(TEST_ADDRESS, 2_000_000n),
        ],
      ]),
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseB.rejected[0].detail).toContain(
      "script_integrity_hash mismatch",
    );
  });

  it("rejects extraneous language views", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(1n),
      attachScriptIntegrityHash: false,
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
        scriptLanguages: ["PlutusV3", "MidgardV1"],
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseB.rejected[0].detail).toContain(
      "script_integrity_hash mismatch",
    );
  });

  it("rejects inline Plutus spends when the spend redeemer index is wrong", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(5n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 1n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("rejects duplicate Plutus redeemer pointers before local evaluation", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
    expect(phaseB.rejected[0].detail).toContain("duplicate redeemer");
  });

  it("rejects inline Plutus spends when the redeemer purpose does not match the script purpose", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(6n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Mint, index: 0n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toBeTruthy();
  });

  it("accepts reference-script always-succeeds Plutus spends with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        spendScript,
      ).to_cbor_bytes(),
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(101n),
      referenceOutput,
      txOptions: {
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects Plutus spent inputs when the matching script is neither attached inline nor supplied by reference input", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const scriptHash = spendScript.hash();
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(102n),
      txOptions: {
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(scriptHash.to_hex());
  });

  it("rejects inline Plutus spent inputs when the script is present but no matching redeemer is provided", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(103n),
      attachScriptIntegrityHash: false,
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        scriptIntegrityHash: Buffer.from("91".repeat(32), "hex"),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain("missing redeemer");
  });

  it("accepts always-succeeds Plutus spend plus mint txs with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const mintScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX,
    );
    const datum = makePlutusIntegerData(7n);
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("08", "hex");
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum,
      txOptions: {
        mintPreimageCbor: makeMintPreimage([
          { policyId: policyId, assetName: assetName, quantity: 1n },
        ]),
        scriptWitnessItems: uniqueScriptWitnessItems([spendScript, mintScript]),
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
          { tag: CML.RedeemerTag.Mint, index: 0n },
        ]),
        outputCbors: [mintedOutput],
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts always-succeeds Plutus observers with the real local evaluator", async () => {
    const spendScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const withdrawScript = makeAlwaysSucceedsScript(
      ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(11n),
      txOptions: {
        scriptWitnessItems: uniqueScriptWitnessItems([
          spendScript,
          withdrawScript,
        ]),
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          { tag: CML.RedeemerTag.Spend, index: 0n },
          { tag: CML.RedeemerTag.Reward, index: 0n },
        ]),
        requiredObserverItems: [
          Buffer.from(withdrawScript.hash().to_raw_bytes()),
        ],
        networkId: 0n,
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts context-sensitive Plutus spends when datum and redeemer match", async () => {
    const spendScript = makeTypedPlutusV3Witness(
      DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX,
    );
    const { phaseA, phaseB } = await runPlutusV3SpendScenario({
      spendScript,
      datum: makePlutusIntegerData(7n),
      txOptions: {
        scriptWitnessItems: [Buffer.from(spendScript.to_cbor_bytes())],
        redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
          {
            tag: CML.RedeemerTag.Spend,
            index: 0n,
            data: makePlutusDataBytes(7n),
          },
        ]),
      },
    });

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].ledgerTx.requiresPlutusEvaluation).toBe(true);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts the PlutusV3 context probe with sorted references, mint, observer, signer, and redeemers", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const probeScript = makeTypedPlutusV3Witness(
      PLUTUS_V3_CONTEXT_PROBE_SCRIPT_HEX,
    );
    const mintScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX,
    );
    const observerScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX,
    );
    const scriptSpendOutRef = makeOutRef(0x10, 0n);
    const pubkeySpendOutRef = makeOutRef(0x30, 0n);
    const firstReferenceOutRef = makeOutRef(0x41, 0n);
    const secondReferenceOutRef = makeOutRef(0x42, 0n);
    const assetName = Buffer.from("0c", "hex");
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const mintedOutput = makeValueOutput(
      TEST_ADDRESS,
      makeSingleAssetValue(4_000_000n, policyId, assetName, 1n),
    );
    const datum = makePlutusIntegerData(7n);
    const base = buildNativeTx({
      spendInputOutRefs: [pubkeySpendOutRef, scriptSpendOutRef],
      referenceInputOutRefs: [secondReferenceOutRef, firstReferenceOutRef],
      mintPreimageCbor: makeMintPreimage([
        { policyId: policyId, assetName: assetName, quantity: 1n },
      ]),
      scriptWitnessItems: uniqueScriptWitnessItems([
        probeScript,
        mintScript,
        observerScript,
      ]),
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: CML.RedeemerTag.Spend,
          index: 0n,
          data: makePlutusContextProbeRedeemer({
            expectedOutputReference: scriptSpendOutRef,
            expectedDatum: 7n,
            expectedSigner: signerKey.to_public().hash().to_hex(),
            expectedFirstReference: firstReferenceOutRef,
            expectedSecondReference: secondReferenceOutRef,
            expectedPolicy: mintScript.hash().to_hex(),
            expectedAssetName: assetName,
            expectedMintQuantity: 1n,
            expectedObserver: observerScript.hash().to_hex(),
          }),
        },
        { tag: CML.RedeemerTag.Mint, index: 0n },
        { tag: CML.RedeemerTag.Reward, index: 0n },
      ]),
      requiredObserverItems: [
        Buffer.from(observerScript.hash().to_raw_bytes()),
      ],
      networkId: 0n,
      scriptLanguages: ["PlutusV3"],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [mintedOutput],
    });

    const preState = new Map<string, Buffer>([
      [
        scriptSpendOutRef.toString("hex"),
        makeScriptOutput(probeScript.hash(), 3_000_000n, { datum }),
      ],
      [
        pubkeySpendOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 1_000_000n),
      ],
      [
        firstReferenceOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
      [
        secondReferenceOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(
      phaseA.rejected,
      JSON.stringify(phaseA.rejected, null, 2),
    ).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects the PlutusV3 context probe when an expected signer is absent", async () => {
    const actualSignerKey = CML.PrivateKey.generate_ed25519();
    const probeScript = makeTypedPlutusV3Witness(
      PLUTUS_V3_CONTEXT_PROBE_SCRIPT_HEX,
    );
    const mintScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX,
    );
    const observerScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX,
    );
    const scriptSpendOutRef = makeOutRef(0x10, 0n);
    const pubkeySpendOutRef = makeOutRef(0x30, 0n);
    const firstReferenceOutRef = makeOutRef(0x41, 0n);
    const secondReferenceOutRef = makeOutRef(0x42, 0n);
    const assetName = Buffer.from("0d", "hex");
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const datum = makePlutusIntegerData(7n);
    const base = buildNativeTx({
      spendInputOutRefs: [pubkeySpendOutRef, scriptSpendOutRef],
      referenceInputOutRefs: [secondReferenceOutRef, firstReferenceOutRef],
      mintPreimageCbor: makeMintPreimage([
        { policyId: policyId, assetName: assetName, quantity: 1n },
      ]),
      scriptWitnessItems: uniqueScriptWitnessItems([
        probeScript,
        mintScript,
        observerScript,
      ]),
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: CML.RedeemerTag.Spend,
          index: 0n,
          data: makePlutusContextProbeRedeemer({
            expectedOutputReference: scriptSpendOutRef,
            expectedDatum: 7n,
            expectedSigner: "ff".repeat(28),
            expectedFirstReference: firstReferenceOutRef,
            expectedSecondReference: secondReferenceOutRef,
            expectedPolicy: mintScript.hash().to_hex(),
            expectedAssetName: assetName,
            expectedMintQuantity: 1n,
            expectedObserver: observerScript.hash().to_hex(),
          }),
        },
        { tag: CML.RedeemerTag.Mint, index: 0n },
        { tag: CML.RedeemerTag.Reward, index: 0n },
      ]),
      requiredObserverItems: [
        Buffer.from(observerScript.hash().to_raw_bytes()),
      ],
      networkId: 0n,
      scriptLanguages: ["PlutusV3"],
      witnessMode: "valid",
      witnessSignerPrivateKey: actualSignerKey,
      outputCbors: [
        makeValueOutput(
          TEST_ADDRESS,
          makeSingleAssetValue(4_000_000n, policyId, assetName, 1n),
        ),
      ],
    });

    const preState = new Map<string, Buffer>([
      [
        scriptSpendOutRef.toString("hex"),
        makeScriptOutput(probeScript.hash(), 3_000_000n, { datum }),
      ],
      [
        pubkeySpendOutRef.toString("hex"),
        makePubKeyOutput(actualSignerKey.to_public().hash(), 1_000_000n),
      ],
      [
        firstReferenceOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
      [
        secondReferenceOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
  });

  it("accepts the MidgardV1 context probe with sorted inputs, authored outputs, mint, observer, signer, and redeemers", async () => {
    expectBothPhasesAcceptOne(
      await runMidgardV1ContextProbeScenario({
        marker: 0xb3,
      }),
    );
  });

  it("rejects the MidgardV1 context probe when the expected first sorted spend input is wrong", async () => {
    const { phaseA, phaseB, probeHash } =
      await runMidgardV1ContextProbeScenario({
        marker: 0xb4,
        redeemerOverrides: {
          expectedFirstInput: makeOutRef(0x09, 0n),
        },
      });

    expectPhaseAAcceptsOne(phaseA);
    expectPhaseBRejectsOne(phaseB, RejectCodes.PlutusScriptInvalid, probeHash);
  });

  it("rejects the MidgardV1 context probe when the expected first sorted reference input is wrong", async () => {
    const { phaseA, phaseB, probeHash } =
      await runMidgardV1ContextProbeScenario({
        marker: 0xb5,
        redeemerOverrides: {
          expectedFirstReference: makeOutRef(0x40, 0n),
        },
      });

    expectPhaseAAcceptsOne(phaseA);
    expectPhaseBRejectsOne(phaseB, RejectCodes.PlutusScriptInvalid, probeHash);
  });

  it("rejects the MidgardV1 context probe when expected output order is swapped", async () => {
    const { phaseA, phaseB, probeHash } =
      await runMidgardV1ContextProbeScenario({
        marker: 0xb6,
        redeemerOverrides: {
          expectedFirstOutputScriptHash: midgardV1Hash(
            ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
          ),
          expectedSecondOutputScriptHash: midgardV1Hash(
            MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX,
          ),
        },
      });

    expectPhaseAAcceptsOne(phaseA);
    expectPhaseBRejectsOne(phaseB, RejectCodes.PlutusScriptInvalid, probeHash);
  });

  it("rejects the MidgardV1 context probe when an expected receive redeemer-map key is absent", async () => {
    const { phaseA, phaseB, probeHash } =
      await runMidgardV1ContextProbeScenario({
        marker: 0xb7,
        redeemerOverrides: {
          expectedReceiveScriptHash: "ee".repeat(28),
        },
      });

    expectPhaseAAcceptsOne(phaseA);
    expectPhaseBRejectsOne(phaseB, RejectCodes.PlutusScriptInvalid, probeHash);
  });

  it("accepts MidgardV1 spends with an inline raw UPLC witness", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(CML.ScriptHash.from_hex(scriptHash), 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseA.accepted[0].ledgerTx.requiresPlutusEvaluation).toBe(true);
    expect(plutusV3Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX)).not.toBe(
      scriptHash,
    );
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects MidgardV1 spends when the guard redeemer is wrong", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(41n),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(CML.ScriptHash.from_hex(scriptHash), 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(scriptHash);
  });

  it("accepts MidgardV1 receive scripts through protected outputs", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const scriptHash = midgardV1Hash(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          data: makePlutusDataBytes(99n),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [
        makeProtectedScriptOutput(
          CML.ScriptHash.from_hex(scriptHash),
          3_000_000n,
        ),
      ],
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects PlutusV3 receive scripts before building a Cardano context", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const receiveScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
    );
    const base = buildNativeTx({
      scriptWitnessItems: [Buffer.from(receiveScript.to_cbor_bytes())],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          data: makePlutusDataBytes(99n),
        },
      ]),
      scriptLanguages: ["PlutusV3"],
      witnessMode: "valid",
      witnessSignerPrivateKey: signerKey,
      outputCbors: [
        makeProtectedScriptOutput(receiveScript.hash(), 3_000_000n),
      ],
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makePubKeyOutput(signerKey.to_public().hash(), 3_000_000n),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(
      "ReceivingScript requires MidgardV1 context",
    );
  });

  it("rejects MidgardV1 datum-hash spends at the Midgard output boundary", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const datum = makePlutusIntegerData(5n);
    const base = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeDatumHashScriptOutput(
          CML.ScriptHash.from_hex(scriptHash),
          3_000_000n,
          CML.hash_plutus_data(datum),
        ),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.InvalidOutput);
    expect(phaseB.rejected[0].detail).toContain("value must be an array");
  });

  it("rejects MidgardV1 spends when only a typed PlutusV3 reference script is available", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const referenceScript = makeTypedPlutusV3Witness(
      MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX,
    );
    const base = buildNativeTx({
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });
    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        referenceScript,
      ).to_cbor_bytes(),
    );

    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(CML.ScriptHash.from_hex(scriptHash), 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [base.referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(scriptHash);
  });

  it("enforces MidgardV1 script budgets after Harmonic evaluation", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
          exUnits: [1_000_000_000n, 1_000_000_000n],
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });
    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(CML.ScriptHash.from_hex(scriptHash), 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const covered = await runBothPhases(base.txId, base.txCbor, preState, {
      enforceScriptBudget: true,
    });
    expect(covered.phaseB.rejected).toHaveLength(0);
    expect(covered.phaseB.accepted).toHaveLength(1);

    const lowBudget = buildNativeTx({
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
          exUnits: [0n, 0n],
        },
      ]),
      scriptLanguages: ["MidgardV1"],
    });
    const lowBudgetResult = await runBothPhases(
      lowBudget.txId,
      lowBudget.txCbor,
      new Map<string, Buffer>([
        [
          lowBudget.inputOutRef.toString("hex"),
          makeScriptOutput(CML.ScriptHash.from_hex(scriptHash), 3_000_000n, {
            datum: makePlutusIntegerData(1n),
          }),
        ],
        [
          lowBudget.referenceInputOutRef.toString("hex"),
          makeOutput(TEST_ADDRESS, 2_000_000n),
        ],
      ]),
      { enforceScriptBudget: true },
    );

    expect(lowBudgetResult.phaseB.accepted).toHaveLength(0);
    expect(lowBudgetResult.phaseB.rejected).toHaveLength(1);
    expect(lowBudgetResult.phaseB.rejected[0].code).toBe(
      RejectCodes.PlutusScriptInvalid,
    );
    expect(lowBudgetResult.phaseB.rejected[0].detail).toContain(
      "budget exceeded",
    );
  });

  it("derives MidgardV1 spend redeemer indexes from sorted out-refs", async () => {
    const scriptHash = midgardV1Hash(MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX);
    const firstOutRef = makeOutRef(0x10, 0n);
    const secondOutRef = makeOutRef(0x20, 0n);
    const authoredInputOrder = [secondOutRef, firstOutRef];
    const sortedIndexed = buildNativeTx({
      spendInputOutRefs: [secondOutRef, firstOutRef],
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makeOutRefDataBytes(firstOutRef),
        },
        {
          tag: MidgardRedeemerTag.Spend,
          index: 1n,
          data: makeOutRefDataBytes(secondOutRef),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
      outputCbors: [makeOutput(TEST_ADDRESS, 6_000_000n)],
    });
    const scriptOutput = makeScriptOutput(
      CML.ScriptHash.from_hex(scriptHash),
      3_000_000n,
      { datum: makePlutusIntegerData(1n) },
    );
    const preState = new Map<string, Buffer>([
      [firstOutRef.toString("hex"), scriptOutput],
      [secondOutRef.toString("hex"), scriptOutput],
      [
        sortedIndexed.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const sortedIndexedResult = await runBothPhases(
      sortedIndexed.txId,
      sortedIndexed.txCbor,
      preState,
    );

    expect(sortedIndexedResult.phaseA.rejected).toHaveLength(0);
    expect(sortedIndexedResult.phaseA.accepted).toHaveLength(1);
    expect(sortedIndexedResult.phaseB.rejected).toHaveLength(0);
    expect(sortedIndexedResult.phaseB.accepted).toHaveLength(1);

    const authoredIndexed = buildNativeTx({
      spendInputOutRefs: authoredInputOrder,
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makeOutRefDataBytes(secondOutRef),
        },
        {
          tag: MidgardRedeemerTag.Spend,
          index: 1n,
          data: makeOutRefDataBytes(firstOutRef),
        },
      ]),
      scriptLanguages: ["MidgardV1"],
      outputCbors: [makeOutput(TEST_ADDRESS, 6_000_000n)],
    });
    const authoredIndexedResult = await runBothPhases(
      authoredIndexed.txId,
      authoredIndexed.txCbor,
      preState,
    );

    expect(authoredIndexedResult.phaseA.rejected).toHaveLength(0);
    expect(authoredIndexedResult.phaseA.accepted).toHaveLength(1);
    expect(authoredIndexedResult.phaseB.accepted).toHaveLength(0);
    expect(authoredIndexedResult.phaseB.rejected).toHaveLength(1);
    expect(authoredIndexedResult.phaseB.rejected[0].code).toBe(
      RejectCodes.PlutusScriptInvalid,
    );
    expect(authoredIndexedResult.phaseB.rejected[0].detail).toContain(
      scriptHash,
    );
  });

  it("accepts mixed MidgardV1 spend plus PlutusV3 mint transactions", async () => {
    const spendHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const mintScript = makeTypedPlutusV3Witness(
      ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX,
    );
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("0e", "hex");
    const base = buildNativeTx({
      mintPreimageCbor: makeMintPreimage([
        { policyId: policyId, assetName: assetName, quantity: 1n },
      ]),
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
        Buffer.from(mintScript.to_cbor_bytes()),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
        },
        { tag: CML.RedeemerTag.Mint, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [
        makeValueOutput(
          TEST_ADDRESS,
          makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
        ),
      ],
    });
    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(CML.ScriptHash.from_hex(spendHash), 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("accepts mixed PlutusV3 spend plus MidgardV1 receive transactions", async () => {
    const spendScript = makeTypedPlutusV3Witness(
      DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX,
    );
    const receiveHash = midgardV1Hash(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        Buffer.from(spendScript.to_cbor_bytes()),
        makeRawUplcWitness(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: CML.RedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(7n),
        },
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          data: makePlutusDataBytes(99n),
        },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [
        makeProtectedScriptOutput(
          CML.ScriptHash.from_hex(receiveHash),
          3_000_000n,
        ),
      ],
    });
    const preState = new Map<string, Buffer>([
      [
        base.inputOutRef.toString("hex"),
        makeScriptOutput(spendScript.hash(), 3_000_000n, {
          datum: makePlutusIntegerData(7n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects mixed transactions when a later MidgardV1 receive leg fails", async () => {
    const spendScript = makeTypedPlutusV3Witness(
      DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX,
    );
    const receiveHash = midgardV1Hash(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX);
    const base = buildNativeTx({
      scriptWitnessItems: [
        Buffer.from(spendScript.to_cbor_bytes()),
        makeRawUplcWitness(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: CML.RedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(7n),
        },
        {
          tag: MidgardRedeemerTag.Receiving,
          index: 0n,
          data: makePlutusDataBytes(98n),
        },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [
        makeProtectedScriptOutput(
          CML.ScriptHash.from_hex(receiveHash),
          3_000_000n,
        ),
      ],
    });

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      new Map<string, Buffer>([
        [
          base.inputOutRef.toString("hex"),
          makeScriptOutput(spendScript.hash(), 3_000_000n, {
            datum: makePlutusIntegerData(7n),
          }),
        ],
        [
          base.referenceInputOutRef.toString("hex"),
          makeOutput(TEST_ADDRESS, 2_000_000n),
        ],
      ]),
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(receiveHash);
  });

  it("rejects mixed transactions when a later PlutusV3 mint leg fails", async () => {
    const spendHash = midgardV1Hash(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX);
    const mintScript = makeTypedPlutusV3Witness(ALWAYS_FAILS_SCRIPT_HEX);
    const policyId = Buffer.from(mintScript.hash().to_raw_bytes());
    const assetName = Buffer.from("0f", "hex");
    const base = buildNativeTx({
      mintPreimageCbor: makeMintPreimage([
        { policyId: policyId, assetName: assetName, quantity: 1n },
      ]),
      scriptWitnessItems: [
        makeRawUplcWitness(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX),
        Buffer.from(mintScript.to_cbor_bytes()),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          data: makePlutusDataBytes(42n),
        },
        { tag: CML.RedeemerTag.Mint, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [
        makeValueOutput(
          TEST_ADDRESS,
          makeSingleAssetValue(3_000_000n, policyId, assetName, 1n),
        ),
      ],
    });

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      new Map<string, Buffer>([
        [
          base.inputOutRef.toString("hex"),
          makeScriptOutput(CML.ScriptHash.from_hex(spendHash), 3_000_000n, {
            datum: makePlutusIntegerData(1n),
          }),
        ],
        [
          base.referenceInputOutRef.toString("hex"),
          makeOutput(TEST_ADDRESS, 2_000_000n),
        ],
      ]),
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.PlutusScriptInvalid);
    expect(phaseB.rejected[0].detail).toContain(mintScript.hash().to_hex());
  });

  it("accepts mixed transactions when the same UPLC bytes are supplied in both script hash domains", async () => {
    const scriptHex = ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX;
    const plutusHash = CML.ScriptHash.from_hex(plutusV3Hash(scriptHex));
    const midgardHash = CML.ScriptHash.from_hex(midgardV1Hash(scriptHex));
    const firstOutRef = makeOutRef(0x10, 0n);
    const secondOutRef = makeOutRef(0x20, 0n);
    const base = buildNativeTx({
      spendInputOutRefs: [secondOutRef, firstOutRef],
      scriptWitnessItems: [
        makeRawUplcWitness(scriptHex),
        Buffer.from(makeTypedPlutusV3Witness(scriptHex).to_cbor_bytes()),
      ],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
        { tag: CML.RedeemerTag.Spend, index: 1n },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [makeOutput(TEST_ADDRESS, 6_000_000n)],
    });
    const preState = new Map<string, Buffer>([
      [
        firstOutRef.toString("hex"),
        makeScriptOutput(plutusHash, 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        secondOutRef.toString("hex"),
        makeScriptOutput(midgardHash, 3_000_000n, {
          datum: makePlutusIntegerData(2n),
        }),
      ],
      [
        base.referenceInputOutRef.toString("hex"),
        makeOutput(TEST_ADDRESS, 2_000_000n),
      ],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });

  it("rejects mixed reference-script transactions when only one typed PlutusV3 reference is available", async () => {
    const scriptHex = ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX;
    const plutusHash = CML.ScriptHash.from_hex(plutusV3Hash(scriptHex));
    const midgardHash = CML.ScriptHash.from_hex(midgardV1Hash(scriptHex));
    const firstOutRef = makeOutRef(0x10, 0n);
    const secondOutRef = makeOutRef(0x20, 0n);
    const base = buildNativeTx({
      spendInputOutRefs: [secondOutRef, firstOutRef],
      redeemerTxWitsPreimageCbor: makeLegacyRedeemersCbor([
        { tag: CML.RedeemerTag.Spend, index: 0n },
        { tag: CML.RedeemerTag.Spend, index: 1n },
      ]),
      scriptLanguages: ["PlutusV3", "MidgardV1"],
      outputCbors: [makeOutput(TEST_ADDRESS, 6_000_000n)],
    });
    const referenceOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.from_coin(2_000_000n),
        undefined,
        makeTypedPlutusV3Witness(scriptHex),
      ).to_cbor_bytes(),
    );
    const preState = new Map<string, Buffer>([
      [
        firstOutRef.toString("hex"),
        makeScriptOutput(plutusHash, 3_000_000n, {
          datum: makePlutusIntegerData(1n),
        }),
      ],
      [
        secondOutRef.toString("hex"),
        makeScriptOutput(midgardHash, 3_000_000n, {
          datum: makePlutusIntegerData(2n),
        }),
      ],
      [base.referenceInputOutRef.toString("hex"), referenceOutput],
    ]);

    const { phaseA, phaseB } = await runBothPhases(
      base.txId,
      base.txCbor,
      preState,
    );

    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
    expect(phaseB.accepted).toHaveLength(0);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
    expect(phaseB.rejected[0].detail).toContain(midgardHash.to_hex());
  });

  it("rejects malformed native required observer preimages", async () => {
    const base = buildNativeTx();
    const malformedRequiredObserversPreimageCbor = encodeByteList([
      Buffer.from("01", "hex"),
    ]);
    const tx: MidgardNativeTxFullV1 = {
      ...base.tx,
      body: {
        ...base.tx.body,
        requiredObserversPreimageCbor: malformedRequiredObserversPreimageCbor,
      },
    };
    (tx as { compact: typeof tx.compact }).compact =
      deriveMidgardNativeTxCompactV1(tx.body, tx.witnessSet, "TxIsValid");
    const txId = computeMidgardNativeTxIdV1(tx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);

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
    const tx: MidgardNativeTxFullV1 = {
      ...base.tx,
      body: {
        ...base.tx.body,
        requiredSignersPreimageCbor: malformedRequiredSignersPreimageCbor,
      },
    };
    (tx as { compact: typeof tx.compact }).compact =
      deriveMidgardNativeTxCompactV1(tx.body, tx.witnessSet, "TxIsValid");
    const txId = computeMidgardNativeTxIdV1(tx);
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);

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

    await expect(
      Effect.runPromise(breakDownTx(cardanoTxBytes)),
    ).rejects.toThrow();
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
    const result = await Effect.runPromise(
      findSpentAndProducedUTxOs(txCbor, txId),
    );
    const expectedOutRef = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(txId),
        0n,
      ).to_cbor_bytes(),
    );

    expect(result.spent).toHaveLength(1);
    expect(result.spent[0].equals(inputOutRef)).toBe(true);
    expect(result.produced).toHaveLength(1);
    expect(result.produced[0].outref.equals(expectedOutRef)).toBe(true);
    expect(result.produced[0].output.equals(outputCbor)).toBe(true);
  });

  it("uses indexed outrefs for multiple produced outputs", async () => {
    const { txId, txCbor } = buildNativeTx({ outputCount: 2 });
    const result = await Effect.runPromise(
      findSpentAndProducedUTxOs(txCbor, txId),
    );
    const txHash = CML.TransactionHash.from_raw_bytes(txId);
    const expectedOutRef0 = Buffer.from(
      CML.TransactionInput.new(txHash, 0n).to_cbor_bytes(),
    );
    const expectedOutRef1 = Buffer.from(
      CML.TransactionInput.new(txHash, 1n).to_cbor_bytes(),
    );

    expect(result.produced).toHaveLength(2);
    expect(result.produced[0].outref.equals(expectedOutRef0)).toBe(true);
    expect(result.produced[1].outref.equals(expectedOutRef1)).toBe(true);
  });
});
