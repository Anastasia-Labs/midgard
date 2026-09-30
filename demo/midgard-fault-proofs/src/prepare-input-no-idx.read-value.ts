import * as SDK from "@al-ft/midgard-sdk";

export const INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION =
  "midgard-input-no-idx-evidence-v1" as const;

export type InputNoIdxRejectionCode =
  | "block_has_no_transactions"
  | "transactions_root_mismatch"
  | "bad_tx_not_committed"
  | "bad_input_index_out_of_range"
  | "producing_tx_not_committed"
  | "no_violating_input"
  | "input_exists_in_producing_tx"
  | "malformed_native_output"
  | "preimage_commitment_mismatch";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class InputNoIdxRejection extends Error {
  readonly code: InputNoIdxRejectionCode;
  readonly detail: string;

  constructor(code: InputNoIdxRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "InputNoIdxRejectionV1";
    this.code = code;
    this.detail = detail;
  }
}

export const reject = (
  code: InputNoIdxRejectionCode,
  detail: string,
): never => {
  throw new InputNoIdxRejection(code, detail);
};

// ## Canonical native output projection
//
// Step 04 carries the producing transaction's complete outputs preimage as
// structured `MidgardTxOutput` PlutusData and re-encodes it on-chain with
// `encode_midgard_tx_output`. These readers invert exactly that encoder
// (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/components.ak:73-181,
// 341-377, 538-585`); anything they cannot invert is refused rather than
// guessed.

export type Cursor = { readonly value: number };

export const byteAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): number => {
  const value = bytes[offset];
  if (value === undefined) {
    return reject("malformed_native_output", `${label} exceeds the output`);
  }
  return value;
};

const readUint = (
  bytes: Buffer,
  offset: number,
  label: string,
): readonly [bigint, number] => {
  const tag = byteAt(bytes, offset, label);
  if (tag <= 23) {
    return [BigInt(tag), offset + 1];
  }
  const width =
    tag === 24 ? 1 : tag === 25 ? 2 : tag === 26 ? 4 : tag === 27 ? 8 : 0;
  if (width === 0 || offset + 1 + width > bytes.length) {
    return reject(
      "malformed_native_output",
      `${label} is not a canonical uint`,
    );
  }
  let value = 0n;
  for (let index = 0; index < width; index += 1) {
    value = value * 256n + BigInt(byteAt(bytes, offset + 1 + index, label));
  }
  return [value, offset + 1 + width];
};

const readDefiniteLength = (
  bytes: Buffer,
  offset: number,
  major: 2 | 5,
  label: string,
): readonly [number, number] => {
  const tag = byteAt(bytes, offset, label);
  const base = major << 5;
  if (tag >= base && tag <= base + 23) {
    return [tag - base, offset + 1];
  }
  const width =
    tag === base + 24 ? 1 : tag === base + 25 ? 2 : tag === base + 26 ? 4 : 0;
  if (width === 0 || offset + 1 + width > bytes.length) {
    return reject(
      "malformed_native_output",
      `${label} is not a canonical definite-length header`,
    );
  }
  let length = 0;
  for (let index = 0; index < width; index += 1) {
    length = length * 256 + byteAt(bytes, offset + 1 + index, label);
  }
  return [length, offset + 1 + width];
};

export const readBytes = (
  bytes: Buffer,
  offset: number,
  label: string,
): readonly [Buffer, number] => {
  const [length, next] = readDefiniteLength(bytes, offset, 2, label);
  const end = next + length;
  if (end > bytes.length) {
    return reject("malformed_native_output", `${label} exceeds the output`);
  }
  return [Buffer.from(bytes.subarray(next, end)), end];
};

const credentialFrom = (
  isScript: boolean,
  hash: Buffer,
): SDK.MidgardCredential =>
  isScript
    ? { ScriptCredential: [hash.toString("hex")] }
    : { PubKeyCredential: [hash.toString("hex")] };

/** Inverts `encode_midgard_address`. */
export const readAddress = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardAddress, number] => {
  const [payload, next] = readBytes(bytes, offset, "output.address");
  if (payload.length !== 29 && payload.length !== 57) {
    return reject(
      "malformed_native_output",
      `output.address must contain 29 or 57 bytes, got ${payload.length.toString()}`,
    );
  }
  const header = payload[0]!;
  const addressType = Math.floor(header / 16);
  const networkNibble = header - addressType * 16;
  const isProtected = networkNibble >= 8;
  const networkId = isProtected ? networkNibble - 8 : networkNibble;
  if (networkId !== 0 && networkId !== 1) {
    return reject("malformed_native_output", "output.address network id");
  }
  const paymentIsScript =
    addressType === 1 || addressType === 3 || addressType === 7;
  const paymentCredential = credentialFrom(
    paymentIsScript,
    Buffer.from(payload.subarray(1, 29)),
  );
  let stakeCredential: SDK.MidgardCredential | null = null;
  if (payload.length === 57) {
    if (addressType > 3) {
      return reject(
        "malformed_native_output",
        "output.address credential layout",
      );
    }
    stakeCredential = credentialFrom(
      addressType === 2 || addressType === 3,
      Buffer.from(payload.subarray(29, 57)),
    );
  } else if (addressType !== 6 && addressType !== 7) {
    return reject(
      "malformed_native_output",
      "output.address credential layout",
    );
  }
  return [
    {
      protected: isProtected,
      network_id: BigInt(networkId),
      payment_credential: paymentCredential,
      stake_credential: stakeCredential,
    },
    next,
  ];
};

/** Inverts `encode_midgard_value`; units stay `policy_id ++ asset_name`. */
export const readValue = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardValue, number] => {
  if (byteAt(bytes, offset, "output.value") !== 0x82) {
    return reject("malformed_native_output", "output.value must be a pair");
  }
  const [lovelace, afterLovelace] = readUint(
    bytes,
    offset + 1,
    "output.value.lovelace",
  );
  const [policyCount, afterHeader] = readDefiniteLength(
    bytes,
    afterLovelace,
    5,
    "output.value.assets",
  );
  const assets = new Map<string, bigint>();
  let cursor = afterHeader;
  for (let policy = 0; policy < policyCount; policy += 1) {
    const [policyId, afterPolicy] = readBytes(
      bytes,
      cursor,
      "output.value.policy",
    );
    if (policyId.length !== 28) {
      return reject(
        "malformed_native_output",
        "output.value.policy byte length",
      );
    }
    const [assetCount, afterAssetHeader] = readDefiniteLength(
      bytes,
      afterPolicy,
      5,
      "output.value.policy_assets",
    );
    if (assetCount === 0) {
      return reject(
        "malformed_native_output",
        "output.value.policy_assets empty",
      );
    }
    cursor = afterAssetHeader;
    for (let asset = 0; asset < assetCount; asset += 1) {
      const [assetName, afterName] = readBytes(
        bytes,
        cursor,
        "output.value.asset_name",
      );
      const [quantity, afterQuantity] = readUint(
        bytes,
        afterName,
        "output.value.quantity",
      );
      if (quantity <= 0n) {
        return reject("malformed_native_output", "output.value.quantity");
      }
      assets.set(
        Buffer.concat([policyId, assetName]).toString("hex"),
        quantity,
      );
      cursor = afterQuantity;
    }
  }
  return [{ lovelace, assets }, cursor];
};

const SCRIPT_LANGUAGE_BY_TAG: Readonly<
  Record<number, SDK.MidgardScriptLanguage>
> = {
  0: "NativeCardanoScript",
  3: "PlutusV3Script",
  128: "MidgardV1Script",
};

/** Inverts `encode_midgard_versioned_script`. */
export const readVersionedScript = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardVersionedScript, number] => {
  if (byteAt(bytes, offset, "output.script_ref") !== 0x82) {
    return reject(
      "malformed_native_output",
      "output.script_ref must be a pair",
    );
  }
  const first = byteAt(bytes, offset + 1, "output.script_ref.language");
  let tag: number;
  let cursor: number;
  if (first === 0 || first === 3) {
    tag = first;
    cursor = offset + 2;
  } else if (first === 24) {
    tag = byteAt(bytes, offset + 2, "output.script_ref.language");
    cursor = offset + 3;
  } else {
    return reject(
      "malformed_native_output",
      "output.script_ref.language encoding",
    );
  }
  const language = SCRIPT_LANGUAGE_BY_TAG[tag];
  if (language === undefined) {
    return reject("malformed_native_output", "output.script_ref.language tag");
  }
  const [scriptBytes, next] = readBytes(
    bytes,
    cursor,
    "output.script_ref.script_bytes",
  );
  return [{ language, script_bytes: scriptBytes.toString("hex") }, next];
};
