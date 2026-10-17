import {
  encodeCborNodeWithDefiniteMaps,
  isMidgardPlutusDataConstrTag,
  MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK,
  type MidgardPlutusDataHead,
} from "./plutus-data-cbor.encode-cbor-node-with-definite-maps.js";
import { parseCborNode } from "./plutus-data-cbor.parse-cbor-node.js";

const readMidgardPlutusDataHead = (
  bytes: Uint8Array,
  offset: number,
): MidgardPlutusDataHead => {
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of PlutusData CBOR");
  }
  const major = initial >> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return { major, value: BigInt(additional), offset: offset + 1 };
  }
  if (additional === 31) {
    return { major, value: null, offset: offset + 1 };
  }
  const width =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : null;
  if (width === null) {
    throw new Error(
      `Unsupported PlutusData CBOR additional information ${additional.toString()}`,
    );
  }
  if (offset + 1 + width > bytes.length) {
    throw new Error("Truncated PlutusData CBOR head");
  }
  let value = 0n;
  for (let index = 0; index < width; index += 1) {
    value = (value << 8n) | BigInt(bytes[offset + 1 + index]!);
  }
  return { major, value, offset: offset + 1 + width };
};

const consumeMidgardPlutusDataByteString = (
  bytes: Uint8Array,
  head: MidgardPlutusDataHead,
): number => {
  if (head.value === null) {
    let cursor = head.offset;
    for (;;) {
      const marker = bytes[cursor];
      if (marker === undefined) {
        throw new Error("Unterminated indefinite PlutusData bytes");
      }
      if (marker === 0xff) {
        return cursor + 1;
      }
      const chunk = readMidgardPlutusDataHead(bytes, cursor);
      if (chunk.major !== 2 || chunk.value === null) {
        throw new Error(
          "Indefinite PlutusData bytes must contain only definite byte chunks",
        );
      }
      if (chunk.value > BigInt(MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK)) {
        throw new Error("PlutusData byte chunk exceeds 64 bytes");
      }
      const end = chunk.offset + Number(chunk.value);
      if (end > bytes.length) {
        throw new Error("Truncated PlutusData bytes chunk");
      }
      cursor = end;
    }
  }
  if (head.value > BigInt(MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK)) {
    throw new Error(
      "Definite PlutusData bytes exceed 64 bytes and must be chunked",
    );
  }
  const end = head.offset + Number(head.value);
  if (end > bytes.length) {
    throw new Error("Truncated PlutusData bytes");
  }
  return end;
};

type MidgardPlutusDataFrame =
  | { readonly kind: "items"; remaining: bigint }
  | { readonly kind: "itemsIndefinite" }
  | { readonly kind: "pairs"; remaining: bigint; awaitingValue: boolean }
  | { readonly kind: "pairsIndefinite"; awaitingValue: boolean };

/**
 * Validates that `bytes` is exactly one well-formed Plutus `Data` value with
 * no trailing content, in a single pass over an explicit frame stack.
 *
 * This is the recursion-free replacement for probing decodability through
 * the CML/lucid `Data.from` parser, whose wasm build overflows its fixed
 * 1 MiB shadow stack near 1,522 nested nodes (far below the depth-4,043
 * value a maximal 16,384-byte Cardano transaction carries) and whose
 * host-side walk is quadratic in depth. Depth here is bounded only by the
 * bytes that carry the value, so the admitted maximum keeps deriving from
 * Cardano transaction capacity rather than a recursion cap.
 *
 * The accepted grammar is differentially locked against
 * `CML.PlutusData.from_cbor_*`: integers use definite heads; byte strings
 * are definite up to 64 bytes or indefinite sequences of definite chunks of
 * at most 64 bytes (zero chunks allowed, as in CML); lists and maps may use
 * either framing; constructor tags are 121..127 and 1280..1400 with an array
 * of fields, or the general tag 102 wrapping a definite two-item array of an
 * unsigned alternative and a fields array; bignum tags 2 and 3 wrap a byte
 * string under the same chunk rule. Everywhere this check is stricter than
 * CML (trailing bytes, indefinite-outer general constructors, definite byte
 * strings above 64 bytes), the exact Aiken `serialiseData` canonicity gate
 * that callers apply next independently rejects the same encodings, so the
 * composite accept/reject verdict is unchanged.
 */
export const assertMidgardPlutusDataWellFormed = (bytes: Uint8Array): void => {
  const stack: MidgardPlutusDataFrame[] = [];
  let cursor = 0;
  let valueComplete = false;

  const completeValue = (): void => {
    for (;;) {
      const frame = stack.at(-1);
      if (frame === undefined) {
        valueComplete = true;
        return;
      }
      if (frame.kind === "items") {
        frame.remaining -= 1n;
        if (frame.remaining > 0n) {
          return;
        }
        stack.pop();
        continue;
      }
      if (frame.kind === "itemsIndefinite") {
        return;
      }
      if (!frame.awaitingValue) {
        frame.awaitingValue = true;
        return;
      }
      frame.awaitingValue = false;
      if (frame.kind === "pairs") {
        frame.remaining -= 1n;
        if (frame.remaining > 0n) {
          return;
        }
        stack.pop();
        continue;
      }
      return;
    }
  };

  const openItems = (count: bigint | null): void => {
    if (count === null) {
      stack.push({ kind: "itemsIndefinite" });
      return;
    }
    if (count === 0n) {
      completeValue();
      return;
    }
    stack.push({ kind: "items", remaining: count });
  };

  while (!valueComplete) {
    const frame = stack.at(-1);
    if (bytes[cursor] === 0xff) {
      if (frame?.kind === "itemsIndefinite") {
        stack.pop();
        cursor += 1;
        completeValue();
        continue;
      }
      if (frame?.kind === "pairsIndefinite") {
        if (frame.awaitingValue) {
          throw new Error("Indefinite PlutusData map is missing a value");
        }
        stack.pop();
        cursor += 1;
        completeValue();
        continue;
      }
      throw new Error("Unexpected PlutusData break marker");
    }

    const head = readMidgardPlutusDataHead(bytes, cursor);
    cursor = head.offset;

    if (head.major === 0 || head.major === 1) {
      if (head.value === null) {
        throw new Error("PlutusData integer must use a definite head");
      }
      completeValue();
      continue;
    }

    if (head.major === 2) {
      cursor = consumeMidgardPlutusDataByteString(bytes, head);
      completeValue();
      continue;
    }

    if (head.major === 4) {
      openItems(head.value);
      continue;
    }

    if (head.major === 5) {
      if (head.value === null) {
        stack.push({ kind: "pairsIndefinite", awaitingValue: false });
      } else if (head.value === 0n) {
        completeValue();
      } else {
        stack.push({
          kind: "pairs",
          remaining: head.value,
          awaitingValue: false,
        });
      }
      continue;
    }

    if (head.major === 6) {
      if (head.value === null) {
        throw new Error("PlutusData tag must use a definite head");
      }
      const tag = head.value;
      if (isMidgardPlutusDataConstrTag(tag)) {
        const fields = readMidgardPlutusDataHead(bytes, cursor);
        if (fields.major !== 4) {
          throw new Error("PlutusData constructor fields must be an array");
        }
        cursor = fields.offset;
        openItems(fields.value);
        continue;
      }
      if (tag === 102n) {
        const outer = readMidgardPlutusDataHead(bytes, cursor);
        if (outer.major !== 4 || outer.value !== 2n) {
          throw new Error(
            "General PlutusData constructor must be a definite two-item array",
          );
        }
        const alternative = readMidgardPlutusDataHead(bytes, outer.offset);
        if (alternative.major !== 0 || alternative.value === null) {
          throw new Error(
            "General PlutusData constructor alternative must be an unsigned integer",
          );
        }
        const fields = readMidgardPlutusDataHead(bytes, alternative.offset);
        if (fields.major !== 4) {
          throw new Error("PlutusData constructor fields must be an array");
        }
        cursor = fields.offset;
        openItems(fields.value);
        continue;
      }
      if (tag === 2n || tag === 3n) {
        const payload = readMidgardPlutusDataHead(bytes, cursor);
        if (payload.major !== 2) {
          throw new Error("PlutusData bignum must wrap a byte string");
        }
        cursor = consumeMidgardPlutusDataByteString(bytes, payload);
        completeValue();
        continue;
      }
      throw new Error(
        `Unsupported PlutusData constructor tag ${tag.toString()}`,
      );
    }

    throw new Error(
      `Unsupported PlutusData CBOR major type ${head.major.toString()}`,
    );
  }

  if (cursor !== bytes.length) {
    throw new Error("Trailing bytes after PlutusData value");
  }
};

/**
 * Transport spelling for a proof carried in reference byte chunks. Definite
 * byte strings avoid quadratic concatenation in a Data decoder. Map order
 * and every contained byte string remain unchanged. This is not a ledger
 * serialiseData encoding and must never replace a consensus preimage.
 */
export const compactPlutusDataCarriageCbor = (cbor: string): string => {
  const input = Buffer.from(cbor, "hex");
  const parsed = parseCborNode(input, 0);
  if (parsed.offset !== input.length)
    throw new Error("Trailing proof carriage CBOR");
  return encodeCborNodeWithDefiniteMaps(parsed.node, false, false).toString(
    "hex",
  );
};
