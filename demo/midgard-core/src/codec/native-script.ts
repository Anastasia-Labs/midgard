import { blake2b } from "@noble/hashes/blake2.js";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type MidgardNativeScript =
  | { readonly type: "sig"; readonly keyHash: Buffer }
  | { readonly type: "all"; readonly scripts: readonly MidgardNativeScript[] }
  | { readonly type: "any"; readonly scripts: readonly MidgardNativeScript[] }
  | {
      readonly type: "atLeast";
      readonly required: bigint;
      readonly scripts: readonly MidgardNativeScript[];
    }
  | { readonly type: "after"; readonly slot: bigint }
  | { readonly type: "before"; readonly slot: bigint };

export type DecodedMidgardNativeScript = {
  readonly script: MidgardNativeScript;
  readonly cbor: Buffer;
};

export type MidgardNativeScriptVerifierInput = {
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly witnessSigners: ReadonlySet<string>;
};

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const KEY_HASH_LENGTH = 28;

export const encodeMidgardNativeScript = (
  script: MidgardNativeScript,
): Buffer => {
  switch (script.type) {
    case "sig":
      if (script.keyHash.length !== KEY_HASH_LENGTH) {
        fail("Native script key hash must be 28 bytes");
      }
      return encodeCborArrayRaw([
        encodeCborUnsigned(0n),
        encodeCborBytes(script.keyHash),
      ]);
    case "all":
      return encodeCborArrayRaw([
        encodeCborUnsigned(1n),
        encodeCborArrayRaw(script.scripts.map(encodeMidgardNativeScript)),
      ]);
    case "any":
      return encodeCborArrayRaw([
        encodeCborUnsigned(2n),
        encodeCborArrayRaw(script.scripts.map(encodeMidgardNativeScript)),
      ]);
    case "atLeast":
      if (script.required < 0n) {
        fail("Native script atLeast required count must be non-negative");
      }
      return encodeCborArrayRaw([
        encodeCborUnsigned(3n),
        encodeCborUnsigned(script.required),
        encodeCborArrayRaw(script.scripts.map(encodeMidgardNativeScript)),
      ]);
    case "after":
      return encodeCborArrayRaw([
        encodeCborUnsigned(4n),
        encodeCborUnsigned(script.slot),
      ]);
    case "before":
      return encodeCborArrayRaw([
        encodeCborUnsigned(5n),
        encodeCborUnsigned(script.slot),
      ]);
  }
};

const decodeNativeScriptAt = (
  bytes: Uint8Array,
  offset: number,
  depth: number,
): { readonly script: MidgardNativeScript; readonly nextOffset: number } => {
  if (depth > 4096) {
    fail("Native script nesting is too deep");
  }
  const header = readCborArrayHeader(bytes, offset, "native_script");
  let cursor = header.nextOffset;
  const tag = readCborUnsigned(bytes, cursor, "native_script.tag");
  cursor = tag.nextOffset;

  switch (tag.value) {
    case 0n: {
      if (header.length !== 2) {
        fail("Native sig script must have 2 fields");
      }
      const keyHash = readCborBytes(bytes, cursor, "native_script.sig.key_hash");
      cursor = keyHash.nextOffset;
      if (keyHash.value.length !== KEY_HASH_LENGTH) {
        fail("Native script key hash must be 28 bytes");
      }
      return { script: { type: "sig", keyHash: keyHash.value }, nextOffset: cursor };
    }
    case 1n:
    case 2n: {
      if (header.length !== 2) {
        fail("Native all/any script must have 2 fields");
      }
      const children = readCborArrayHeader(bytes, cursor, "native_script.children");
      cursor = children.nextOffset;
      const scripts: MidgardNativeScript[] = [];
      for (let i = 0; i < children.length; i += 1) {
        const child = decodeNativeScriptAt(bytes, cursor, depth + 1);
        scripts.push(child.script);
        cursor = child.nextOffset;
      }
      return {
        script: { type: tag.value === 1n ? "all" : "any", scripts },
        nextOffset: cursor,
      };
    }
    case 3n: {
      if (header.length !== 3) {
        fail("Native atLeast script must have 3 fields");
      }
      const required = readCborUnsigned(bytes, cursor, "native_script.atLeast.required");
      cursor = required.nextOffset;
      const children = readCborArrayHeader(bytes, cursor, "native_script.atLeast.children");
      cursor = children.nextOffset;
      const scripts: MidgardNativeScript[] = [];
      for (let i = 0; i < children.length; i += 1) {
        const child = decodeNativeScriptAt(bytes, cursor, depth + 1);
        scripts.push(child.script);
        cursor = child.nextOffset;
      }
      return {
        script: { type: "atLeast", required: required.value, scripts },
        nextOffset: cursor,
      };
    }
    case 4n:
    case 5n: {
      if (header.length !== 2) {
        fail("Native timelock script must have 2 fields");
      }
      const slot = readCborUnsigned(bytes, cursor, "native_script.slot");
      cursor = slot.nextOffset;
      return {
        script: { type: tag.value === 4n ? "after" : "before", slot: slot.value },
        nextOffset: cursor,
      };
    }
    default:
      return fail("Unsupported native script tag", tag.value.toString());
  }
};

export const decodeMidgardNativeScript = (
  bytes: Uint8Array,
): DecodedMidgardNativeScript => {
  const decoded = decodeNativeScriptAt(bytes, 0, 0);
  if (decoded.nextOffset !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after native script",
      `offset=${decoded.nextOffset}`,
    );
  }
  const cbor = encodeMidgardNativeScript(decoded.script);
  if (!cbor.equals(Buffer.from(bytes))) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Native script CBOR is not canonical",
    );
  }
  return { script: decoded.script, cbor };
};

export const hashMidgardNativeScript = (
  script: MidgardNativeScript | Uint8Array,
): string => {
  const cbor =
    script instanceof Uint8Array
      ? decodeMidgardNativeScript(script).cbor
      : encodeMidgardNativeScript(script);
  return Buffer.from(
    blake2b(Buffer.concat([Buffer.from([0x00]), cbor]), { dkLen: 28 }),
  ).toString("hex");
};

export const verifyMidgardNativeScript = (
  script: MidgardNativeScript,
  input: MidgardNativeScriptVerifierInput,
): boolean => {
  switch (script.type) {
    case "sig":
      return input.witnessSigners.has(script.keyHash.toString("hex"));
    case "all":
      return script.scripts.every((child) =>
        verifyMidgardNativeScript(child, input),
      );
    case "any":
      return script.scripts.some((child) =>
        verifyMidgardNativeScript(child, input),
      );
    case "atLeast": {
      if (script.required === 0n) {
        return true;
      }
      let count = 0n;
      for (const child of script.scripts) {
        if (verifyMidgardNativeScript(child, input)) {
          count += 1n;
          if (count >= script.required) {
            return true;
          }
        }
      }
      return false;
    }
    case "after":
      return (
        input.validityIntervalStart !== undefined &&
        input.validityIntervalStart >= script.slot
      );
    case "before":
      return (
        input.validityIntervalEnd !== undefined &&
        input.validityIntervalEnd < script.slot
      );
  }
};
