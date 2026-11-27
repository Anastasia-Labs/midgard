import fs from "node:fs";
import path from "node:path";

import {
  decodeMidgardNativeScript,
  encodeCbor,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import {
  hashMidgardScript,
  hashPlutusV3Script,
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

export const EMPTY_CBOR_LIST = Buffer.from([0x80]);

export const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

export const EMPTY_REDEEMER_DATA = Buffer.from(
  Data.to(new Constr(0, [])),
  "hex",
);

export const TEST_ADDRESS =
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

export const txFixtures = JSON.parse(
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

export const ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.deposit_spend.else",
);

export const ALWAYS_SUCCEEDS_MINT_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.deposit_mint.else",
);

export const ALWAYS_SUCCEEDS_WITHDRAW_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.reserve_withdraw.else");

export const ALWAYS_FAILS_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.always_fail.else",
);

export const DATUM_EQUALS_REDEEMER_SPEND_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.datum_equals_redeemer_spend.spend");

export const MIDGARD_SPEND_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_spend_guard.else",
);

export const MIDGARD_SPEND_OUT_REF_GUARD_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.midgard_v1_spend_out_ref_guard.else");

export const MIDGARD_RECEIVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_receive_guard.else",
);

export const MIDGARD_OBSERVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_observe_guard.else",
);

export const MIDGARD_CONTEXT_PROBE_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_context_probe.else",
);

export const PLUTUS_V3_CONTEXT_PROBE_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.plutus_v3_context_probe.else");

export type ScriptWitnessItem = MidgardVersionedScript | Uint8Array;

export const makeAlwaysSucceedsScript = (compiledCode: string): CML.Script => {
  return CML.Script.new_plutus_v3(
    CML.PlutusV3Script.from_raw_bytes(Buffer.from(compiledCode, "hex")),
  );
};

export const makeRawUplcWitness = (
  scriptHex: string,
): MidgardVersionedScript => ({
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

export const scriptWitnessItemToVersioned = (
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

export const makeTypedPlutusV3Witness = (scriptHex: string): CML.Script => {
  return CML.Script.new_plutus_v3(
    CML.PlutusV3Script.from_raw_bytes(Buffer.from(scriptHex, "hex")),
  );
};

export const midgardV1Hash = (scriptHex: string): string =>
  hashMidgardScript(Buffer.from(scriptHex, "hex"));

export const plutusV3Hash = (scriptHex: string): string =>
  hashPlutusV3Script(Buffer.from(scriptHex, "hex"));

export const uniqueScriptWitnessItems = (
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

export const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encodeCbor(items.map((item) => Buffer.from(item))));

export const makeOutRef = (txHashByte: number, outputIndex: bigint): Buffer =>
  makeOutRefCbor(txHashByte, outputIndex);

export const makeOutput = (address: string, lovelace: bigint): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

export const makePubKeyOutput = (
  keyHash: CML.Ed25519KeyHash,
  lovelace: bigint,
): Buffer => makePubKeyValueOutput(keyHash, CML.Value.from_coin(lovelace));

export const makePubKeyValueOutput = (
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

export const makeSingleAssetValue = (
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

export const makeMultiAssetValue = (
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

export const makeValueOutput = (address: string, value: CML.Value): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      value,
    ).to_cbor_bytes(),
  );
