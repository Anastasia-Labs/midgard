import {
  encodeCborBytes,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";

import {
  tag4ByteAt,
  type Tag4Output,
  type Tag4OutputAsset,
  tag4UintAt,
  type Tag4VersionedScript,
} from "./detect.mpf-proof-from-witness.js";

const tag4DefiniteLengthAt = (
  bytes: Buffer,
  offset: number,
  major: 2 | 5,
  label: string,
): { readonly length: number; readonly nextOffset: number } => {
  const tag = tag4ByteAt(bytes, offset, label);
  const shortBase = major << 5;
  if (tag >= shortBase && tag <= shortBase + 23) {
    return { length: tag - shortBase, nextOffset: offset + 1 };
  }
  const byteCount =
    tag === shortBase + 24
      ? 1
      : tag === shortBase + 25
        ? 2
        : tag === shortBase + 26
          ? 4
          : 0;
  if (byteCount === 0 || offset + 1 + byteCount > bytes.length) {
    throw new Error(`${label} is not an Aiken V1 definite-length value`);
  }
  let length = 0;
  for (let index = 0; index < byteCount; index += 1) {
    length = length * 256 + tag4ByteAt(bytes, offset + 1 + index, label);
  }
  return { length, nextOffset: offset + 1 + byteCount };
};

const tag4BytesAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Buffer; readonly nextOffset: number } => {
  const header = tag4DefiniteLengthAt(bytes, offset, 2, label);
  const end = header.nextOffset + header.length;
  if (end > bytes.length) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  return {
    value: Buffer.from(bytes.subarray(header.nextOffset, end)),
    nextOffset: end,
  };
};

const tag4MapHeaderAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly length: number; readonly nextOffset: number } =>
  tag4DefiniteLengthAt(bytes, offset, 5, label);

const tag4AddressAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Buffer; readonly nextOffset: number } => {
  if (tag4ByteAt(bytes, offset, label) !== 0x58 || offset + 2 > bytes.length) {
    throw new Error(`${label} must use the Aiken V1 address byte encoding`);
  }
  const length = tag4ByteAt(bytes, offset + 1, label);
  if (length !== 29 && length !== 57) {
    throw new Error(`${label} must contain 29 or 57 bytes`);
  }
  const end = offset + 2 + length;
  if (end > bytes.length) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  const address = Buffer.from(bytes.subarray(offset + 2, end));
  const header = address[0]!;
  const addressType = Math.floor(header / 16);
  const networkNibble = header - addressType * 16;
  const networkId = networkNibble >= 8 ? networkNibble - 8 : networkNibble;
  if (networkId !== 0 && networkId !== 1) {
    throw new Error(`${label} has an unsupported network id`);
  }
  if (
    (length === 57 && addressType > 3) ||
    (length === 29 && addressType !== 6 && addressType !== 7)
  ) {
    throw new Error(`${label} has an invalid credential layout`);
  }
  return { value: address, nextOffset: end };
};

const tag4ValueAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): {
  readonly lovelace: bigint;
  readonly assets: readonly Tag4OutputAsset[];
  readonly nextOffset: number;
} => {
  if (tag4ByteAt(bytes, offset, label) !== 0x82) {
    throw new Error(`${label} must be the Aiken V1 value pair`);
  }
  const lovelace = tag4UintAt(bytes, offset + 1, `${label}.lovelace`);
  const policyMap = tag4MapHeaderAt(
    bytes,
    lovelace.nextOffset,
    `${label}.assets`,
  );
  let cursor = policyMap.nextOffset;
  const assets: Tag4OutputAsset[] = [];
  for (let policyIndex = 0; policyIndex < policyMap.length; policyIndex += 1) {
    if (
      tag4ByteAt(bytes, cursor, `${label}.policy`) !== 0x58 ||
      tag4ByteAt(bytes, cursor + 1, `${label}.policy`) !== 28
    ) {
      throw new Error(`${label}.policy must contain exactly 28 bytes`);
    }
    const policyEnd = cursor + 30;
    if (policyEnd > bytes.length) {
      throw new Error(`${label}.policy exceeds the output byte length`);
    }
    const policyId = Buffer.from(bytes.subarray(cursor + 2, policyEnd));
    const assetMap = tag4MapHeaderAt(
      bytes,
      policyEnd,
      `${label}.policy_assets`,
    );
    if (assetMap.length === 0) {
      throw new Error(`${label}.policy_assets must not be empty`);
    }
    cursor = assetMap.nextOffset;
    for (let assetIndex = 0; assetIndex < assetMap.length; assetIndex += 1) {
      const assetName = tag4BytesAt(bytes, cursor, `${label}.asset_name`);
      if (assetName.value.length > 32) {
        throw new Error(`${label}.asset_name exceeds 32 bytes`);
      }
      const quantity = tag4UintAt(
        bytes,
        assetName.nextOffset,
        `${label}.quantity`,
      );
      if (quantity.value === 0n) {
        throw new Error(`${label}.quantity must be positive`);
      }
      assets.push({
        policyId,
        assetName: assetName.value,
        quantity: quantity.value,
      });
      cursor = quantity.nextOffset;
    }
  }
  return {
    lovelace: lovelace.value,
    assets,
    nextOffset: cursor,
  };
};

const tag4ScriptAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: Tag4VersionedScript; readonly nextOffset: number } => {
  if (tag4ByteAt(bytes, offset, label) !== 0x82) {
    throw new Error(`${label} must be the Aiken V1 versioned-script pair`);
  }
  const firstTag = tag4ByteAt(bytes, offset + 1, `${label}.language`);
  let language: bigint;
  let cursor: number;
  if (firstTag === 0 || firstTag === 3) {
    language = BigInt(firstTag);
    cursor = offset + 2;
  } else {
    if (firstTag !== 24) {
      throw new Error(`${label}.language has an invalid integer encoding`);
    }
    language = BigInt(tag4ByteAt(bytes, offset + 2, `${label}.language`));
    cursor = offset + 3;
  }
  if (language !== 0n && language !== 3n && language !== 128n) {
    throw new Error(`${label}.language is not supported by Aiken V1`);
  }
  const scriptBytes = tag4BytesAt(bytes, cursor, `${label}.bytes`);
  return {
    value: {
      language,
      bytes: scriptBytes.value,
    } as Tag4VersionedScript,
    nextOffset: scriptBytes.nextOffset,
  };
};

export const decodeTag4Output = (bytes: Buffer): Tag4Output => {
  const entryTag = tag4ByteAt(bytes, 0, "output");
  if (entryTag < 0xa2 || entryTag > 0xa4) {
    throw new Error("output must contain two to four Aiken V1 map entries");
  }
  const entryCount = entryTag - 0xa0;
  if (tag4ByteAt(bytes, 1, "output.address_key") !== 0) {
    throw new Error("output address key must be zero");
  }
  const address = tag4AddressAt(bytes, 2, "output.address");
  if (tag4ByteAt(bytes, address.nextOffset, "output.value_key") !== 1) {
    throw new Error("output value key must be one");
  }
  const value = tag4ValueAt(bytes, address.nextOffset + 1, "output.value");
  let cursor = value.nextOffset;
  let datumCbor: Buffer | undefined;
  let scriptRef: Tag4VersionedScript | undefined;
  if (entryCount > 2) {
    const extraKey = tag4ByteAt(bytes, cursor, "output.extra_key");
    cursor += 1;
    if (extraKey === 2) {
      const datum = tag4BytesAt(bytes, cursor, "output.datum");
      datumCbor = datum.value;
      cursor = datum.nextOffset;
      if (entryCount === 4) {
        if (tag4ByteAt(bytes, cursor, "output.script_key") !== 3) {
          throw new Error("output script key must be three");
        }
        const script = tag4ScriptAt(bytes, cursor + 1, "output.script_ref");
        scriptRef = script.value;
        cursor = script.nextOffset;
      }
    } else {
      if (extraKey !== 3 || entryCount !== 3) {
        throw new Error("output optional fields have an invalid layout");
      }
      const script = tag4ScriptAt(bytes, cursor, "output.script_ref");
      scriptRef = script.value;
      cursor = script.nextOffset;
    }
  }
  if (cursor !== bytes.length) {
    throw new Error("output contains trailing bytes");
  }
  return {
    address: address.value,
    lovelace: value.lovelace,
    assets: value.assets,
    ...(datumCbor === undefined ? {} : { datumCbor }),
    ...(scriptRef === undefined ? {} : { scriptRef }),
  };
};

const tag4DefiniteMapHeader = (length: number): Buffer => {
  if (length < 0 || length > 0xffff_ffff) {
    throw new Error("Aiken V1 map length exceeds its encoding domain");
  }
  if (length <= 23) {
    return Buffer.from([0xa0 + length]);
  }
  if (length <= 0xff) {
    return Buffer.from([0xb8, length]);
  }
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = 0xb9;
    header.writeUInt16BE(length, 1);
    return header;
  }
  const header = Buffer.alloc(5);
  header[0] = 0xba;
  header.writeUInt32BE(length, 1);
  return header;
};

export const encodeTag4Value = ({
  lovelace,
  assets,
}: Pick<Tag4Output, "lovelace" | "assets">): Buffer => {
  const groups: Tag4OutputAsset[][] = [];
  for (const asset of assets) {
    const previous = groups.at(-1);
    if (
      previous !== undefined &&
      previous[0]!.policyId.equals(asset.policyId)
    ) {
      previous.push(asset);
    } else {
      groups.push([asset]);
    }
  }
  return Buffer.concat([
    Buffer.from([0x82]),
    encodeCborUnsigned(lovelace),
    tag4DefiniteMapHeader(groups.length),
    ...groups.flatMap((group) => [
      encodeCborBytes(group[0]!.policyId),
      tag4DefiniteMapHeader(group.length),
      ...group.flatMap((asset) => [
        encodeCborBytes(asset.assetName),
        encodeCborUnsigned(asset.quantity),
      ]),
    ]),
  ]);
};
