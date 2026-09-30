import { execFile } from "node:child_process";
import { type Server } from "node:net";
import { promisify } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  WatcherL1AdapterError,
  type WatcherL1AdapterErrorCode,
  type WatcherL1TransportAttestationContext,
} from "../../src/l1/l1-adapter.js";

export type MutableRecord = Record<string, any>;

export const normalizeUntrustedL1Block = normalizeWatcherL1Block as unknown as (
  context: unknown,
  observation: unknown,
) => ReturnType<typeof normalizeWatcherL1Block>;

export const execFileAsync = promisify(execFile);

export const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContext
>();

export const tlsIdentities = new Map<string, string>();

export const listen = async (
  server: Server,
  target: string | number,
): Promise<void> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    const onListen = () => {
      server.off("error", reject);
      resolve();
    };
    if (typeof target === "string") {
      server.listen(target, onListen);
    } else {
      server.listen(target, "127.0.0.1", onListen);
    }
  });

export const blake2b256 = (bytesHex: string): string =>
  computeHash32(Buffer.from(bytesHex, "hex")).toString("hex");

export const providerMetadata = (
  providerId = "provider-a",
  identityByte = "aa",
): MutableRecord => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  network: "Preprod",
  providerId,
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: identityByte.repeat(32),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256: identityByte.repeat(32),
  },
});

export const provider = (providerId = "provider-a", identityByte = "aa") =>
  transportContexts.get(`external:${providerId}:${identityByte}`)!;

export const publicBytes = (bytesHex: string): MutableRecord => ({
  ...makeWatcherL1PublicBytes(bytesHex),
});

export const transaction = (
  seedHex: string,
  outputIndex: string,
  redeemerEncoding: "legacy" | "map" = "legacy",
  includeScriptDataHash = true,
  isValid = true,
  includeCollateralReturn = false,
): MutableRecord => {
  const nativeScript = CML.NativeScript.new_script_all(
    CML.NativeScriptList.new(),
  );
  const nativeScripts = CML.NativeScriptList.new();
  nativeScripts.add(nativeScript);
  const datum = CML.PlutusData.from_cbor_hex("01");
  const datums = CML.PlutusDataList.new();
  datums.add(datum);
  const address = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
  );
  const output = CML.TransactionOutput.new(
    address,
    CML.Value.from_coin(2_000_000n + BigInt(outputIndex)),
    CML.DatumOption.new_datum(datum),
    CML.Script.new_native(nativeScript),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    BigInt(`0x${seedHex}`),
  );
  const collateralReturn = CML.TransactionOutput.new(
    address,
    CML.Value.from_coin(1_500_000n),
    undefined,
    undefined,
  );
  if (includeCollateralReturn) {
    body.set_collateral_return(collateralReturn);
    body.set_total_collateral(500_000n);
  }
  if (includeScriptDataHash) {
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(
        Buffer.alloc(32, Number(BigInt(outputIndex) % 256n)),
      ),
    );
  }
  const mintData = CML.PlutusData.from_cbor_hex("d87980");
  const spendData = CML.PlutusData.from_cbor_hex("d8798101");
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_native_scripts(nativeScripts);
  witnessSet.set_plutus_datums(datums);
  if (redeemerEncoding === "legacy") {
    const redeemers = CML.LegacyRedeemerList.new();
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Mint,
        10n,
        mintData,
        CML.ExUnits.new(5n, 7n),
      ),
    );
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Spend,
        BigInt(outputIndex),
        spendData,
        CML.ExUnits.new(11n, 13n),
      ),
    );
    witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  } else {
    const redeemers = CML.MapRedeemerKeyToRedeemerVal.new();
    redeemers.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Mint, 10n),
      CML.RedeemerVal.new(mintData, CML.ExUnits.new(5n, 7n)),
    );
    redeemers.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Spend, BigInt(outputIndex)),
      CML.RedeemerVal.new(spendData, CML.ExUnits.new(11n, 13n)),
    );
    witnessSet.set_redeemers(
      CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemers),
    );
  }
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    isValid,
    undefined,
  );
  const bodyBytes = body.to_canonical_cbor_hex();
  const txHash = blake2b256(bodyBytes);
  const datumBytes = datum.to_canonical_cbor_hex();
  const scriptBytes = nativeScript.to_canonical_cbor_hex();
  return {
    txHash,
    fullTransaction: publicBytes(fullTransaction.to_canonical_cbor_hex()),
    body: publicBytes(bodyBytes),
    witnessSet: publicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: isValid
      ? [
          {
            outRef: `${txHash}#0`,
            outputIndex: "0",
            output: publicBytes(output.to_canonical_cbor_hex()),
            datum: {
              datumHash: blake2b256(datumBytes),
              bytes: publicBytes(datumBytes),
            },
            referenceScript: {
              scriptHash: nativeScript.hash().to_hex(),
              language: "Native",
              bytes: publicBytes(scriptBytes),
            },
          },
        ]
      : includeCollateralReturn
        ? [
            {
              outRef: `${txHash}#1`,
              outputIndex: "1",
              output: publicBytes(collateralReturn.to_canonical_cbor_hex()),
              datum: null,
              referenceScript: null,
            },
          ]
        : [],
    scripts: [
      {
        scriptHash: nativeScript.hash().to_hex(),
        language: "Native",
        bytes: publicBytes(scriptBytes),
      },
    ],
    datums: [
      {
        datumHash: blake2b256("01"),
        bytes: publicBytes("01"),
      },
    ],
    redeemers: [
      {
        purpose: "mint",
        index: "10",
        bytes: publicBytes("d87980"),
      },
      {
        purpose: "spend",
        index: outputIndex,
        bytes: publicBytes("d8798101"),
      },
    ],
  };
};

export const observation = (): MutableRecord => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "provider-a",
  chainPoint: {
    blockHash: "11".repeat(32),
    parentBlockHash: "10".repeat(32),
    slot: "76543210",
    blockNo: "2345678",
    depth: "15",
  },
  transactions: [transaction("a20081825820", "10"), transaction("a100", "2")],
});

export const rejected = (
  action: () => unknown,
  code: WatcherL1AdapterErrorCode,
  path: string,
): WatcherL1AdapterError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherL1AdapterError);
    const adapterError = error as WatcherL1AdapterError;
    expect(adapterError).toMatchObject({ code, path });
    return adapterError;
  }
  throw new Error("Expected L1 adapter rejection");
};
