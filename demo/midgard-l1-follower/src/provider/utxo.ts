import {
  type Address,
  type Assets,
  CML,
  type Credential,
  getAddressDetails,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { OutputSummary, OutRef, ScriptType } from "../types.js";

const LUCID_SCRIPT_TYPES: Readonly<Record<ScriptType, Script["type"]>> = {
  native: "Native",
  plutus_v1: "PlutusV1",
  plutus_v2: "PlutusV2",
  plutus_v3: "PlutusV3",
};

/** The CBOR head of a byte string of `length` bytes. */
const bytesHead = (length: number): Buffer => {
  if (length < 24) return Buffer.from([0x40 | length]);
  if (length < 0x100) return Buffer.from([0x58, length]);
  if (length < 0x10000) {
    const head = Buffer.alloc(3);
    head[0] = 0x59;
    head.writeUInt16BE(length, 1);
    return head;
  }
  const head = Buffer.alloc(5);
  head[0] = 0x5a;
  head.writeUInt32BE(length, 1);
  return head;
};

/**
 * Lucid's script text: a native script's CBOR, or a Plutus script's bytes
 * as one CBOR byte string (what `PlutusV3Script.to_cbor_hex()` yields).
 */
const lucidScript = (
  scriptRef: NonNullable<OutputSummary["scriptRef"]>,
): Script => ({
  type: LUCID_SCRIPT_TYPES[scriptRef.type],
  script:
    scriptRef.type === "native"
      ? scriptRef.bytes.toString("hex")
      : Buffer.concat([
          bytesHead(scriptRef.bytes.length),
          scriptRef.bytes,
        ]).toString("hex"),
});

/** A Byron address's header type is 8 (CIP-19). */
const isByron = (address: Buffer): boolean =>
  address.length > 0 && (address[0] as number) >> 4 === 8;

/** An address's raw bytes as Lucid text: bech32, or base58 for Byron. */
export const addressText = (address: Buffer): Address => {
  const hex = address.toString("hex");
  if (isByron(address)) {
    const byron = CML.ByronAddress.from_cbor_hex(hex);
    try {
      return byron.to_base58();
    } finally {
      byron.free();
    }
  }
  const parsed = CML.Address.from_hex(hex);
  try {
    return parsed.to_bech32(undefined);
  } finally {
    parsed.free();
  }
};

/** A Lucid address (bech32 or Byron base58) as raw bytes. */
export const addressBytes = (address: Address): Buffer =>
  Buffer.from(getAddressDetails(address).address.hex, "hex");

const assetsOf = (output: OutputSummary): Assets => {
  const assets: Assets = { lovelace: output.lovelace };
  for (const [policyId, names] of output.assets)
    for (const [name, quantity] of names)
      assets[`${policyId}${name}`] = quantity;
  return assets;
};

/** One output as a Lucid UTxO, the shape `coreToUtxo` gives for the same bytes. */
export const toLucidUtxo = (outRef: OutRef, output: OutputSummary): UTxO => ({
  txHash: outRef.txHash.toString("hex"),
  outputIndex: outRef.index,
  address: addressText(output.address),
  assets: assetsOf(output),
  datumHash: output.datumHash?.toString("hex") ?? undefined,
  datum: output.datum?.toString("hex") ?? undefined,
  scriptRef:
    output.scriptRef === null ? undefined : lucidScript(output.scriptRef),
});

/** What a `getUtxos`-style query names: one address, or one payment credential. */
export type UtxoSubject =
  | Readonly<{ by: "address"; address: Buffer }>
  | Readonly<{ by: "payment_credential"; hash: Buffer }>;

export const utxoSubject = (
  addressOrCredential: Address | Credential,
): UtxoSubject =>
  typeof addressOrCredential === "string"
    ? { by: "address", address: addressBytes(addressOrCredential) }
    : {
        by: "payment_credential",
        hash: Buffer.from(addressOrCredential.hash, "hex"),
      };

/** A Lucid unit (policy id hex + asset name hex) as its parts. */
export const splitUnit = (
  unit: string,
): Readonly<{ policyId: Buffer; assetName: Buffer }> => {
  if (!/^(?:[0-9a-f]{2}){28,60}$/u.test(unit))
    throw new TypeError(
      `unit ${unit} is not policy id hex plus asset name hex`,
    );
  return {
    policyId: Buffer.from(unit.slice(0, 56), "hex"),
    assetName: Buffer.from(unit.slice(56), "hex"),
  };
};

/** Whether the output holds a positive quantity of `unit`. */
export const holdsUnit = (output: OutputSummary, unit: string): boolean =>
  (output.assets.get(unit.slice(0, 56))?.get(unit.slice(56)) ?? 0n) > 0n;

/** Whether the output holds any asset under `policyId`. */
export const holdsPolicy = (output: OutputSummary, policyId: string): boolean =>
  [...(output.assets.get(policyId)?.values() ?? [])].some(
    (quantity) => quantity > 0n,
  );
