/**
 * Native-script token and frame scanning bounded by the scan node and depth limits.
 */

import { encodeCbor } from "@al-ft/midgard-core";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";

import { hash32 } from "./input-resolution.js";

const NATIVE_SCRIPT_SCAN_FRAME_DOMAIN = Buffer.from(
  "MidgardNativeScriptScanFrameV1",
  "ascii",
);

export const MAX_NATIVE_SCRIPT_SCAN_NODES = 16_384;

export const MAX_NATIVE_SCRIPT_SCAN_DEPTH = 16_384;

export type ValidationMachineNativeScriptToken = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly nextOffset: number;
  readonly childCount: number;
  readonly required: bigint;
  readonly keyHash: Buffer;
  readonly slot: bigint;
};

export type ValidationMachineNativeScriptTokenHead = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly payloadOffset: number;
};

export type ValidationMachineNativeScriptFrame = {
  readonly tail: Buffer;
  readonly kind: 1 | 2 | 3;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

export type ValidationMachineVersionedScriptHeader = {
  readonly languageTag: 0 | 3 | 128;
  readonly payloadOffset: number;
  readonly payloadLength: number;
};

export const readValidationMachineVersionedScriptHeader = (
  item: Buffer,
): ValidationMachineVersionedScriptHeader => {
  const outer = readCborArrayHeader(item, 0, "versioned_script");
  if (outer.length !== 2) {
    throw new Error("versioned script must contain exactly two fields");
  }
  const language = readCborUnsigned(
    item,
    outer.nextOffset,
    "versioned_script.language",
  );
  const payload = readCborBytes(
    item,
    language.nextOffset,
    "versioned_script.payload",
  );
  if (
    (language.value !== 0n &&
      language.value !== 3n &&
      language.value !== 128n) ||
    payload.nextOffset !== item.length
  ) {
    throw new Error("versioned script has an invalid language or length");
  }
  return {
    languageTag: Number(language.value) as 0 | 3 | 128,
    payloadOffset: payload.nextOffset - payload.value.length,
    payloadLength: payload.value.length,
  };
};

export const readValidationMachineNativeScriptTokenHead = (
  item: Buffer,
  offset: number,
): ValidationMachineNativeScriptTokenHead => {
  const outer = readCborArrayHeader(item, offset, "native_script");
  const tag = readCborUnsigned(item, outer.nextOffset, "native_script.tag");
  if (tag.value < 0n || tag.value > 5n) {
    throw new Error("native script has an unsupported tag");
  }
  const kind = Number(tag.value) as 0 | 1 | 2 | 3 | 4 | 5;
  if (
    (kind === 3 && outer.length !== 3) ||
    (kind !== 3 && outer.length !== 2)
  ) {
    throw new Error("native script has an invalid outer shape");
  }
  return { kind, payloadOffset: tag.nextOffset };
};

export const readValidationMachineNativeScriptPayload = (
  item: Buffer,
  offset: number,
  kind: 0 | 1 | 2 | 3 | 4 | 5,
): ValidationMachineNativeScriptToken => {
  if (kind === 0) {
    const keyHash = readCborBytes(item, offset, "native_script.key_hash");
    if (keyHash.value.length !== 28) {
      throw new Error("native signature script has an invalid shape");
    }
    return {
      kind: 0,
      nextOffset: keyHash.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: keyHash.value,
      slot: 0n,
    };
  }
  if (kind === 1 || kind === 2) {
    const children = readCborArrayHeader(
      item,
      offset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES) {
      throw new Error("native all/any script has an invalid shape");
    }
    return {
      kind,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 3) {
    const required = readCborUnsigned(item, offset, "native_script.required");
    const children = readCborArrayHeader(
      item,
      required.nextOffset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES) {
      throw new Error("native at-least script has an invalid shape");
    }
    return {
      kind: 3,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: required.value,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 4 || kind === 5) {
    const slot = readCborUnsigned(item, offset, "native_script.slot");
    return {
      kind,
      nextOffset: slot.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: slot.value,
    };
  }
  throw new Error("native script payload has an unsupported tag");
};

const validationMachineNativeScriptFrameIsWellFormed = (
  frame: ValidationMachineNativeScriptFrame,
): boolean => {
  const processed = frame.childCount - frame.remaining;
  return (
    (frame.tail.length === 0 || frame.tail.length === 32) &&
    frame.childCount > 0 &&
    frame.childCount <= MAX_NATIVE_SCRIPT_SCAN_NODES &&
    frame.remaining > 0 &&
    frame.remaining <= frame.childCount &&
    frame.validCount >= 0 &&
    frame.validCount <= processed &&
    (frame.kind === 3 ? frame.required >= 0n : frame.required === 0n)
  );
};

export const hashValidationMachineNativeScriptFrame = (
  frame: ValidationMachineNativeScriptFrame,
): Buffer => {
  if (!validationMachineNativeScriptFrameIsWellFormed(frame)) {
    throw new Error("cannot hash a malformed native-script frame");
  }
  return hash32(
    Buffer.concat([
      NATIVE_SCRIPT_SCAN_FRAME_DOMAIN,
      encodeCbor([
        frame.tail,
        BigInt(frame.kind),
        BigInt(frame.childCount),
        BigInt(frame.remaining),
        BigInt(frame.validCount),
        frame.required,
      ]),
    ]),
  );
};
