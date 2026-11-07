/**
 * The recursive CBOR codec as it stood before the iterative rewrite, kept
 * verbatim (renamed with a `legacy` prefix) as the differential oracle. It
 * overflows the JS stack on deep input, which is why it left `src`.
 * `cborg` stays a devDependency of this package only for this file.
 */
import { decodeFirst, encode, rfc8949EncodeOptions } from "cborg";

import {
  type CborItemSpan,
  compareCborKeyBytes,
  ensureSafeLength,
  err,
  FATAL_UTF8_DECODER,
  readArgument,
} from "../src/codec/cbor.read-argument.js";
import {
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "../src/codec/errors.js";

type CborReadOptions = {
  readonly allowTags?: boolean;
};

const DECODER_OPTIONS = {
  strict: true,
  allowIndefinite: false,
  allowUndefined: false,
  useMaps: true,
  rejectDuplicateMapKeys: true,
};

export const legacySkipCborItem = (
  bytes: Uint8Array,
  offset: number,
  options: CborReadOptions = {},
): CborItemSpan => {
  const start = offset;
  const header = readArgument(bytes, offset);

  switch (header.major) {
    case 0:
    case 1:
      return { start, end: header.nextOffset, major: header.major };
    case 2:
    case 3: {
      const length = ensureSafeLength(header.value, offset);
      const end = header.nextOffset + length;
      if (end > bytes.length) {
        throw err("CBOR string exceeds input length", `offset=${offset}`);
      }
      if (header.major === 3) {
        if (
          length >= 3 &&
          bytes[header.nextOffset] === 0xef &&
          bytes[header.nextOffset + 1] === 0xbb &&
          bytes[header.nextOffset + 2] === 0xbf
        ) {
          throw err(
            "CBOR text string must not begin with a UTF-8 BOM",
            `offset=${offset}`,
          );
        }
        try {
          FATAL_UTF8_DECODER.decode(bytes.subarray(header.nextOffset, end));
        } catch {
          throw err("CBOR text string is not valid UTF-8", `offset=${offset}`);
        }
      }
      return { start, end, major: header.major };
    }
    case 4: {
      let cursor = header.nextOffset;
      const length = ensureSafeLength(header.value, offset);
      for (let i = 0; i < length; i += 1) {
        cursor = legacySkipCborItem(bytes, cursor, options).end;
      }
      return { start, end: cursor, major: header.major };
    }
    case 5: {
      let cursor = header.nextOffset;
      const length = ensureSafeLength(header.value, offset);
      const seen = new Set<string>();
      let previousKey: Buffer | undefined;
      for (let i = 0; i < length; i += 1) {
        const key = legacySkipCborItem(bytes, cursor, options);
        const keyBytes = Buffer.from(bytes.subarray(key.start, key.end));
        const keyHex = keyBytes.toString("hex");
        if (seen.has(keyHex)) {
          throw err("Duplicate CBOR map key", `offset=${key.start}`);
        }
        seen.add(keyHex);
        if (
          previousKey !== undefined &&
          compareCborKeyBytes(previousKey, keyBytes) > 0
        ) {
          throw err(
            "Non-canonical CBOR map key ordering",
            `offset=${key.start}`,
          );
        }
        previousKey = keyBytes;
        cursor = legacySkipCborItem(bytes, key.end, options).end;
      }
      return { start, end: cursor, major: header.major };
    }
    case 6:
      if (options.allowTags !== true) {
        throw err(
          "CBOR tags are not valid in this Midgard codec",
          `offset=${offset}`,
        );
      }
      return {
        start,
        end: legacySkipCborItem(bytes, header.nextOffset, options).end,
        major: header.major,
      };
    case 7: {
      if (
        header.additional === 20 ||
        header.additional === 21 ||
        header.additional === 22
      ) {
        return { start, end: header.nextOffset, major: header.major };
      }
      if (header.additional === 23) {
        throw err("CBOR undefined is not valid", `offset=${offset}`);
      }
      throw err(
        "CBOR simple values and floats are not valid",
        `offset=${offset}`,
      );
    }
    default:
      throw err("Unsupported CBOR major type", `offset=${offset}`);
  }
};

export const legacyAssertCanonicalCbor = (
  bytes: Uint8Array,
  fieldName = "cbor",
  options: CborReadOptions = {},
): void => {
  const span = legacySkipCborItem(bytes, 0, options);
  if (span.end !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} has trailing bytes`,
      `offset=${span.end}`,
    );
  }
};

/**
 * `cborg` declares `decodeFirst` as returning `[any, Uint8Array]`. The decoded
 * value is untrusted input that every caller validates, so it is laundered to
 * `unknown` at this one boundary rather than letting `any` spread through the
 * decoders.
 */
const decodeFirstUnknown = (
  bytes: Uint8Array,
): readonly [unknown, Uint8Array] =>
  decodeFirst(bytes, DECODER_OPTIONS) as readonly [unknown, Uint8Array];

export const legacyDecodeSingleCbor = (
  bytes: Uint8Array,
  options: CborReadOptions = {},
): unknown => {
  try {
    legacyAssertCanonicalCbor(bytes, "cbor", options);
    const [value] = decodeFirstUnknown(bytes);
    return value;
  } catch (e) {
    if (e instanceof MidgardTxCodecError) {
      throw e;
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Failed to decode CBOR",
      String(e),
    );
  }
};

export const legacyEncodeCbor = (value: unknown): Buffer => {
  try {
    return Buffer.from(encode(value, rfc8949EncodeOptions));
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborEncode,
      "Failed to encode CBOR",
      String(e),
    );
  }
};
