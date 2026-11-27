import {
  assertCanonicalCborRoundTrip,
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

type MidgardCompositeNativeScript = Extract<
  MidgardNativeScript,
  { readonly scripts: readonly MidgardNativeScript[] }
>;

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const KEY_HASH_LENGTH = 28;

/**
 * V1's native-script depth capability floor is intentionally derived from the
 * complete Cardano transaction-size floor rather than a host stack limit.
 * Every traversal below is iterative so this protocol bound is executable.
 */
export const MIDGARD_NATIVE_SCRIPT_MAX_DEPTH = 16_384;
export const MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT = 16_384;

const assertSupportedNativeScriptDepth = (depth: number): void => {
  if (depth > MIDGARD_NATIVE_SCRIPT_MAX_DEPTH) {
    fail(
      "Native script nesting exceeds the V1 maximum",
      `${depth.toString()} > ${MIDGARD_NATIVE_SCRIPT_MAX_DEPTH.toString()}`,
    );
  }
};

const encodeCborArrayHeader = (length: number): Buffer => {
  if (!Number.isSafeInteger(length) || length < 0) {
    fail("Native script child count must be a safe non-negative integer");
  }
  if (length < 24) {
    return Buffer.from([0x80 | length]);
  }
  if (length <= 0xff) {
    return Buffer.from([0x98, length]);
  }
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = 0x99;
    header.writeUInt16BE(length, 1);
    return header;
  }
  if (length <= 0xffff_ffff) {
    const header = Buffer.alloc(5);
    header[0] = 0x9a;
    header.writeUInt32BE(length, 1);
    return header;
  }
  const header = Buffer.alloc(9);
  header[0] = 0x9b;
  header.writeBigUInt64BE(BigInt(length), 1);
  return header;
};

export const encodeMidgardNativeScript = (
  script: MidgardNativeScript,
): Buffer => {
  const chunks: Buffer[] = [];
  let nodeCount = 0;
  const pending: {
    readonly script: MidgardNativeScript;
    readonly depth: number;
  }[] = [{ script, depth: 1 }];

  while (pending.length > 0) {
    const current = pending.pop()!;
    assertSupportedNativeScriptDepth(current.depth);
    nodeCount += 1;
    if (nodeCount > MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT) {
      fail(
        "Native script node count exceeds the V1 maximum",
        `${nodeCount.toString()} > ${MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT.toString()}`,
      );
    }

    switch (current.script.type) {
      case "sig":
        if (current.script.keyHash.length !== KEY_HASH_LENGTH) {
          fail("Native script key hash must be 28 bytes");
        }
        chunks.push(
          encodeCborArrayRaw([
            encodeCborUnsigned(0n),
            encodeCborBytes(current.script.keyHash),
          ]),
        );
        break;
      case "all":
      case "any": {
        chunks.push(
          encodeCborArrayHeader(2),
          encodeCborUnsigned(current.script.type === "all" ? 1n : 2n),
          encodeCborArrayHeader(current.script.scripts.length),
        );
        for (
          let index = current.script.scripts.length - 1;
          index >= 0;
          index -= 1
        ) {
          pending.push({
            script: current.script.scripts[index]!,
            depth: current.depth + 1,
          });
        }
        break;
      }
      case "atLeast":
        chunks.push(
          encodeCborArrayHeader(3),
          encodeCborUnsigned(3n),
          encodeCborUnsigned(current.script.required),
          encodeCborArrayHeader(current.script.scripts.length),
        );
        for (
          let index = current.script.scripts.length - 1;
          index >= 0;
          index -= 1
        ) {
          pending.push({
            script: current.script.scripts[index]!,
            depth: current.depth + 1,
          });
        }
        break;
      case "after":
      case "before":
        chunks.push(
          encodeCborArrayRaw([
            encodeCborUnsigned(current.script.type === "after" ? 4n : 5n),
            encodeCborUnsigned(current.script.slot),
          ]),
        );
        break;
    }
  }

  return Buffer.concat(chunks);
};

type NativeScriptDecodeFrame =
  | {
      readonly type: "all" | "any";
      readonly scripts: MidgardNativeScript[];
      remaining: number;
    }
  | {
      readonly type: "atLeast";
      readonly required: bigint;
      readonly scripts: MidgardNativeScript[];
      remaining: number;
    };

const decodeNativeScript = (
  bytes: Uint8Array,
): { readonly script: MidgardNativeScript; readonly nextOffset: number } => {
  const parents: NativeScriptDecodeFrame[] = [];
  let cursor = 0;
  let nodeCount = 0;
  let completed: MidgardNativeScript | undefined;

  while (true) {
    if (completed !== undefined) {
      const parent = parents.at(-1);
      if (parent === undefined) {
        return { script: completed, nextOffset: cursor };
      }
      parent.scripts.push(completed);
      parent.remaining -= 1;
      if (parent.remaining > 0) {
        completed = undefined;
        continue;
      }
      parents.pop();
      completed =
        parent.type === "atLeast"
          ? {
              type: "atLeast",
              required: parent.required,
              scripts: parent.scripts,
            }
          : { type: parent.type, scripts: parent.scripts };
      continue;
    }

    assertSupportedNativeScriptDepth(parents.length + 1);
    nodeCount += 1;
    if (nodeCount > MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT) {
      fail(
        "Native script node count exceeds the V1 maximum",
        `${nodeCount.toString()} > ${MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT.toString()}`,
      );
    }
    const header = readCborArrayHeader(bytes, cursor, "native_script");
    cursor = header.nextOffset;
    const tag = readCborUnsigned(bytes, cursor, "native_script.tag");
    cursor = tag.nextOffset;

    switch (tag.value) {
      case 0n: {
        if (header.length !== 2) {
          fail("Native sig script must have 2 fields");
        }
        const keyHash = readCborBytes(
          bytes,
          cursor,
          "native_script.sig.key_hash",
        );
        cursor = keyHash.nextOffset;
        if (keyHash.value.length !== KEY_HASH_LENGTH) {
          fail("Native script key hash must be 28 bytes");
        }
        completed = { type: "sig", keyHash: keyHash.value };
        break;
      }
      case 1n:
      case 2n: {
        if (header.length !== 2) {
          fail("Native all/any script must have 2 fields");
        }
        const children = readCborArrayHeader(
          bytes,
          cursor,
          "native_script.children",
        );
        cursor = children.nextOffset;
        const type = tag.value === 1n ? "all" : "any";
        if (children.length === 0) {
          completed = { type, scripts: [] };
        } else {
          parents.push({
            type,
            scripts: [],
            remaining: children.length,
          });
        }
        break;
      }
      case 3n: {
        if (header.length !== 3) {
          fail("Native atLeast script must have 3 fields");
        }
        const required = readCborUnsigned(
          bytes,
          cursor,
          "native_script.atLeast.required",
        );
        cursor = required.nextOffset;
        const children = readCborArrayHeader(
          bytes,
          cursor,
          "native_script.atLeast.children",
        );
        cursor = children.nextOffset;
        if (children.length === 0) {
          completed = {
            type: "atLeast",
            required: required.value,
            scripts: [],
          };
        } else {
          parents.push({
            type: "atLeast",
            required: required.value,
            scripts: [],
            remaining: children.length,
          });
        }
        break;
      }
      case 4n:
      case 5n: {
        if (header.length !== 2) {
          fail("Native timelock script must have 2 fields");
        }
        const slot = readCborUnsigned(bytes, cursor, "native_script.slot");
        cursor = slot.nextOffset;
        completed = {
          type: tag.value === 4n ? "after" : "before",
          slot: slot.value,
        };
        break;
      }
      default:
        return fail("Unsupported native script tag", tag.value.toString());
    }
  }
};

export const decodeMidgardNativeScript = (
  bytes: Uint8Array,
): DecodedMidgardNativeScript => {
  const decoded = decodeNativeScript(bytes);
  if (decoded.nextOffset !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after native script",
      `offset=${decoded.nextOffset}`,
    );
  }
  const cbor = assertCanonicalCborRoundTrip(
    bytes,
    decoded.script,
    encodeMidgardNativeScript,
    "Native script CBOR is not canonical",
  );
  return { script: decoded.script, cbor };
};

export const verifyMidgardNativeScript = (
  script: MidgardNativeScript,
  input: MidgardNativeScriptVerifierInput,
): boolean => {
  const pending: (
    | {
        readonly script: MidgardNativeScript;
        readonly depth: number;
        readonly expanded: false;
      }
    | {
        readonly script: MidgardCompositeNativeScript;
        readonly depth: number;
        readonly expanded: true;
      }
  )[] = [{ script, depth: 1, expanded: false }];
  const results: boolean[] = [];
  let nodeCount = 0;

  while (pending.length > 0) {
    const current = pending.pop()!;
    if (
      current.depth > MIDGARD_NATIVE_SCRIPT_MAX_DEPTH ||
      (!current.expanded &&
        (nodeCount += 1) > MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT)
    ) {
      return false;
    }
    if (current.expanded) {
      const childResults = results.splice(
        results.length - current.script.scripts.length,
      );
      switch (current.script.type) {
        case "all":
          results.push(childResults.every(Boolean));
          break;
        case "any":
          results.push(childResults.some(Boolean));
          break;
        case "atLeast":
          results.push(
            BigInt(childResults.filter(Boolean).length) >=
              current.script.required,
          );
          break;
      }
      continue;
    }

    switch (current.script.type) {
      case "sig":
        results.push(
          input.witnessSigners.has(current.script.keyHash.toString("hex")),
        );
        break;
      case "after":
        results.push(
          input.validityIntervalStart !== undefined &&
            input.validityIntervalStart >= current.script.slot,
        );
        break;
      case "before":
        results.push(
          input.validityIntervalEnd !== undefined &&
            input.validityIntervalEnd <= current.script.slot,
        );
        break;
      case "all":
      case "any":
      case "atLeast":
        pending.push({
          script: current.script,
          depth: current.depth,
          expanded: true,
        });
        for (
          let index = current.script.scripts.length - 1;
          index >= 0;
          index -= 1
        ) {
          pending.push({
            script: current.script.scripts[index]!,
            depth: current.depth + 1,
            expanded: false,
          });
        }
        break;
    }
  }

  return results.length === 1 && results[0] === true;
};
