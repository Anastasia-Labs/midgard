import { CML } from "@lucid-evolution/lucid";

type CborNode =
  | { readonly kind: "uint"; readonly value: bigint }
  | { readonly kind: "nint"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly value: Buffer }
  | {
      readonly kind: "array";
      readonly items: readonly CborNode[];
      readonly indefinite: boolean;
    }
  | {
      readonly kind: "map";
      readonly entries: readonly (readonly [CborNode, CborNode])[];
    }
  | { readonly kind: "tag"; readonly tag: bigint; readonly value: CborNode };

type CborRange = {
  readonly start: number;
  readonly end: number;
};

const readCborLength = (
  bytes: Buffer,
  offset: number,
  additional: number,
): { readonly value: bigint | null; readonly offset: number } => {
  if (additional < 24) {
    return { value: BigInt(additional), offset };
  }
  if (additional === 24) {
    return { value: BigInt(bytes[offset]!), offset: offset + 1 };
  }
  if (additional === 25) {
    return { value: BigInt(bytes.readUInt16BE(offset)), offset: offset + 2 };
  }
  if (additional === 26) {
    return { value: BigInt(bytes.readUInt32BE(offset)), offset: offset + 4 };
  }
  if (additional === 27) {
    return { value: bytes.readBigUInt64BE(offset), offset: offset + 8 };
  }
  if (additional === 31) {
    return { value: null, offset };
  }
  throw new Error(`Unsupported CBOR additional information ${additional}`);
};

const expectCborLength = (length: bigint | null, context: string): bigint => {
  if (length === null) {
    throw new Error(`${context} must use a definite length`);
  }
  return length;
};

const parseCborNode = (
  bytes: Buffer,
  offset: number,
): { readonly node: CborNode; readonly offset: number } => {
  type ParseFrame =
    | {
        readonly kind: "array";
        readonly indefinite: boolean;
        remaining: bigint | null;
        readonly items: CborNode[];
      }
    | {
        readonly kind: "map";
        remaining: bigint | null;
        pendingKey: CborNode | null;
        readonly entries: (readonly [CborNode, CborNode])[];
      }
    | {
        readonly kind: "tag";
        readonly tag: bigint;
      };

  const frames: ParseFrame[] = [];
  let cursor = offset;
  let completed: CborNode | null = null;

  const attach = (initialNode: CborNode): CborNode | null => {
    let node = initialNode;
    while (frames.length > 0) {
      const frame = frames.at(-1)!;
      if (frame.kind === "tag") {
        frames.pop();
        node = { kind: "tag", tag: frame.tag, value: node };
        continue;
      }
      if (frame.kind === "array") {
        frame.items.push(node);
        if (frame.remaining !== null) {
          frame.remaining -= 1n;
          if (frame.remaining === 0n) {
            frames.pop();
            node = {
              kind: "array",
              items: frame.items,
              indefinite: false,
            };
            continue;
          }
        }
        return null;
      }
      if (frame.pendingKey === null) {
        frame.pendingKey = node;
        return null;
      }
      frame.entries.push([frame.pendingKey, node]);
      frame.pendingKey = null;
      if (frame.remaining !== null) {
        frame.remaining -= 1n;
        if (frame.remaining === 0n) {
          frames.pop();
          node = { kind: "map", entries: frame.entries };
          continue;
        }
      }
      return null;
    }
    return node;
  };

  while (completed === null) {
    const frame = frames.at(-1);
    if (bytes[cursor] === 0xff) {
      if (
        frame === undefined ||
        frame.kind === "tag" ||
        frame.remaining !== null
      ) {
        throw new Error("Unexpected CBOR break marker");
      }
      if (frame.kind === "map" && frame.pendingKey !== null) {
        throw new Error(
          "Indefinite CBOR map is missing a value before its break",
        );
      }
      frames.pop();
      cursor += 1;
      completed = attach(
        frame.kind === "array"
          ? {
              kind: "array",
              items: frame.items,
              indefinite: true,
            }
          : { kind: "map", entries: frame.entries },
      );
      continue;
    }

    const initial = bytes[cursor];
    if (initial === undefined) {
      throw new Error("Unexpected end of CBOR input");
    }
    const major = initial >> 5;
    const additional = initial & 0x1f;
    const length = readCborLength(bytes, cursor + 1, additional);
    cursor = length.offset;

    if (major === 0 || major === 1) {
      const value = expectCborLength(
        length.value,
        major === 0 ? "uint" : "nint",
      );
      completed = attach(
        major === 0 ? { kind: "uint", value } : { kind: "nint", value },
      );
      continue;
    }

    if (major === 2) {
      if (length.value === null) {
        const chunks: Buffer[] = [];
        while (bytes[cursor] !== 0xff) {
          const chunkInitial = bytes[cursor];
          if (chunkInitial === undefined || chunkInitial >> 5 !== 2) {
            throw new Error(
              "Indefinite CBOR bytes must contain only definite byte chunks",
            );
          }
          const chunkLength = readCborLength(
            bytes,
            cursor + 1,
            chunkInitial & 0x1f,
          );
          const definiteChunkLength = Number(
            expectCborLength(chunkLength.value, "indefinite bytes chunk"),
          );
          const end = chunkLength.offset + definiteChunkLength;
          if (end > bytes.length) {
            throw new Error("Unexpected end of indefinite CBOR bytes");
          }
          chunks.push(bytes.subarray(chunkLength.offset, end));
          cursor = end;
        }
        if (bytes[cursor] !== 0xff) {
          throw new Error("Unterminated indefinite CBOR bytes");
        }
        cursor += 1;
        completed = attach({
          kind: "bytes",
          value: Buffer.concat(chunks),
        });
        continue;
      }
      const byteLength = Number(expectCborLength(length.value, "bytes"));
      const end = cursor + byteLength;
      if (end > bytes.length) {
        throw new Error("Unexpected end of CBOR bytes");
      }
      completed = attach({
        kind: "bytes",
        value: bytes.subarray(cursor, end),
      });
      cursor = end;
      continue;
    }

    if (major === 4) {
      if (length.value === 0n) {
        completed = attach({
          kind: "array",
          items: [],
          indefinite: false,
        });
      } else {
        frames.push({
          kind: "array",
          indefinite: length.value === null,
          remaining: length.value,
          items: [],
        });
      }
      continue;
    }

    if (major === 5) {
      if (length.value === 0n) {
        completed = attach({ kind: "map", entries: [] });
      } else {
        frames.push({
          kind: "map",
          remaining: length.value,
          pendingKey: null,
          entries: [],
        });
      }
      continue;
    }

    if (major === 6) {
      frames.push({
        kind: "tag",
        tag: expectCborLength(length.value, "tag"),
      });
      continue;
    }

    throw new Error(`Unsupported PlutusData CBOR major type ${major}`);
  }

  return { node: completed, offset: cursor };
};

const parseCborRange = (bytes: Buffer, offset: number): CborRange => ({
  start: offset,
  end: parseCborNode(bytes, offset).offset,
});

const parseArrayItemRanges = (
  bytes: Buffer,
  offset: number,
): { readonly items: readonly CborRange[]; readonly offset: number } => {
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of CBOR input");
  }
  const major = initial >> 5;
  if (major !== 4) {
    throw new Error("Expected constructor fields to be a CBOR array");
  }

  const additional = initial & 0x1f;
  const length = readCborLength(bytes, offset + 1, additional);
  const items: CborRange[] = [];
  let cursor = length.offset;

  if (length.value === null) {
    while (bytes[cursor] !== 0xff) {
      const range = parseCborRange(bytes, cursor);
      items.push(range);
      cursor = range.end;
    }
    if (bytes[cursor] !== 0xff) {
      throw new Error("Unterminated indefinite CBOR array");
    }
    return { items, offset: cursor + 1 };
  }

  for (let i = 0n; i < length.value; i += 1n) {
    const range = parseCborRange(bytes, cursor);
    items.push(range);
    cursor = range.end;
  }
  return { items, offset: cursor };
};

const constrFieldRanges = (
  bytes: Buffer,
): { readonly items: readonly CborRange[]; readonly offset: number } => {
  const initial = bytes[0];
  if (initial === undefined) {
    throw new Error("Unexpected empty PlutusData CBOR input");
  }
  if (initial >> 5 !== 6) {
    throw new Error("Expected constructor PlutusData CBOR");
  }

  const tag = readCborLength(bytes, 1, initial & 0x1f);
  const tagValue = expectCborLength(tag.value, "constructor tag");
  if (
    (121n <= tagValue && tagValue <= 127n) ||
    (1280n <= tagValue && tagValue <= 1400n)
  ) {
    return parseArrayItemRanges(bytes, tag.offset);
  }

  if (tagValue === 102n) {
    const constructorItems = parseArrayItemRanges(bytes, tag.offset);
    const fieldsRange = constructorItems.items[1];
    if (fieldsRange === undefined) {
      throw new Error("General constructor is missing its fields array");
    }
    return parseArrayItemRanges(
      bytes.subarray(fieldsRange.start, fieldsRange.end),
      0,
    );
  }

  throw new Error(`Unsupported PlutusData constructor tag ${tagValue}`);
};

const encodeCborHeader = (major: number, value: bigint | null): Buffer => {
  if (value === null) {
    return Buffer.from([(major << 5) | 31]);
  }
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  if (value <= 0xffn) {
    return Buffer.from([(major << 5) | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const out = Buffer.alloc(3);
    out[0] = (major << 5) | 25;
    out.writeUInt16BE(Number(value), 1);
    return out;
  }
  if (value <= 0xffffffffn) {
    const out = Buffer.alloc(5);
    out[0] = (major << 5) | 26;
    out.writeUInt32BE(Number(value), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = (major << 5) | 27;
  out.writeBigUInt64BE(value, 1);
  return out;
};

const encodeCborNodeWithDefiniteMaps = (
  node: CborNode,
  sortMaps: boolean,
): Buffer => {
  type EncodeVisit = {
    readonly node: CborNode;
    readonly expanded: boolean;
  };
  const encoded = new Map<CborNode, Buffer>();
  const stack: EncodeVisit[] = [{ node, expanded: false }];

  while (stack.length > 0) {
    const visit = stack.pop()!;
    const current = visit.node;
    if (!visit.expanded) {
      stack.push({ node: current, expanded: true });
      if (current.kind === "array") {
        for (let index = current.items.length - 1; index >= 0; index -= 1) {
          stack.push({
            node: current.items[index]!,
            expanded: false,
          });
        }
      } else if (current.kind === "map") {
        for (let index = current.entries.length - 1; index >= 0; index -= 1) {
          const [key, value] = current.entries[index]!;
          stack.push({ node: value, expanded: false });
          stack.push({ node: key, expanded: false });
        }
      } else if (current.kind === "tag") {
        stack.push({ node: current.value, expanded: false });
      }
      continue;
    }

    switch (current.kind) {
      case "uint":
        encoded.set(current, encodeCborHeader(0, current.value));
        break;
      case "nint":
        encoded.set(current, encodeCborHeader(1, current.value));
        break;
      case "bytes": {
        if (current.value.length <= 64) {
          encoded.set(
            current,
            Buffer.concat([
              encodeCborHeader(2, BigInt(current.value.length)),
              current.value,
            ]),
          );
          break;
        }
        const chunks: Buffer[] = [];
        for (
          let chunkOffset = 0;
          chunkOffset < current.value.length;
          chunkOffset += 64
        ) {
          const chunk = current.value.subarray(chunkOffset, chunkOffset + 64);
          chunks.push(encodeCborHeader(2, BigInt(chunk.length)), chunk);
        }
        encoded.set(
          current,
          Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]),
        );
        break;
      }
      case "array":
        encoded.set(
          current,
          current.items.length === 0
            ? Buffer.from([0x80])
            : Buffer.concat([
                Buffer.from([0x9f]),
                ...current.items.map((item) => encoded.get(item)!),
                Buffer.from([0xff]),
              ]),
        );
        break;
      case "map": {
        const entries = current.entries.map(([key, value]) => ({
          encodedKey: encoded.get(key)!,
          encodedValue: encoded.get(value)!,
        }));
        if (sortMaps) {
          entries.sort((left, right) =>
            Buffer.compare(left.encodedKey, right.encodedKey),
          );
        }
        encoded.set(
          current,
          Buffer.concat([
            encodeCborHeader(5, BigInt(entries.length)),
            ...entries.flatMap(({ encodedKey, encodedValue }) => [
              encodedKey,
              encodedValue,
            ]),
          ]),
        );
        break;
      }
      case "tag":
        encoded.set(
          current,
          current.tag === 102n &&
            current.value.kind === "array" &&
            current.value.items.length === 2
            ? Buffer.concat([
                encodeCborHeader(6, current.tag),
                Buffer.from([0x82]),
                ...current.value.items.map((item) => encoded.get(item)!),
              ])
            : Buffer.concat([
                encodeCborHeader(6, current.tag),
                encoded.get(current.value)!,
              ]),
        );
        break;
    }
  }

  return encoded.get(node)!;
};

/**
 * Mirrors `cbor.serialise(builtin.b_data(bytes))`. Plutus Data byte strings
 * longer than 64 bytes use an indefinite byte string containing definite
 * chunks of at most 64 bytes.
 */
export const aikenSerialisedPlutusDataBytes = (bytes: Uint8Array): Buffer =>
  encodeCborNodeWithDefiniteMaps(
    { kind: "bytes", value: Buffer.from(bytes) },
    true,
  );

const aikenSerialisedPlutusDataCborWithMapOrder = (
  cbor: string,
  sortMaps: boolean,
): string => {
  const input = Buffer.from(cbor, "hex");
  const parsed = parseCborNode(input, 0);
  if (parsed.offset !== input.length) {
    throw new Error("Unexpected trailing bytes in PlutusData CBOR");
  }
  return encodeCborNodeWithDefiniteMaps(parsed.node, sortMaps).toString("hex");
};

export const aikenSerialisedPlutusDataCbor = (cbor: string): string =>
  aikenSerialisedPlutusDataCborWithMapOrder(cbor, true);

/**
 * Mirrors `serialiseData` for an already constructed Data value. Unlike
 * typed SDK encoders, a raw Plutus Data map retains its explicit pair order,
 * and that order is observable through both `unMapData` and serialization.
 */
export const aikenSerialisedPlutusDataCborPreservingMapOrder = (
  cbor: string,
): string => aikenSerialisedPlutusDataCborWithMapOrder(cbor, false);

export const plutusConstrFieldCbor = (
  cbor: string,
  fieldPath: readonly number[],
): string => {
  let current = Buffer.from(cbor, "hex");
  for (const index of fieldPath) {
    const fields = constrFieldRanges(current);
    const field = fields.items[index];
    if (field === undefined) {
      throw new Error(`Constructor field ${index.toString()} is missing`);
    }
    current = current.subarray(field.start, field.end);
  }
  return current.toString("hex");
};

export const aikenSerialisedPlutusConstrFieldCbor = (
  cbor: string,
  fieldPath: readonly number[],
): string =>
  aikenSerialisedPlutusDataCbor(plutusConstrFieldCbor(cbor, fieldPath));

export const canonicalPlutusDataCbor = (cbor: string): string =>
  CML.PlutusData.from_cbor_hex(cbor).to_canonical_cbor_hex();

const MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK_V1 = 64;

const isMidgardPlutusDataConstrTagV1 = (tag: bigint): boolean =>
  (tag >= 121n && tag <= 127n) || (tag >= 1_280n && tag <= 1_400n);

type MidgardPlutusDataHeadV1 = {
  readonly major: number;
  readonly value: bigint | null;
  readonly offset: number;
};

const readMidgardPlutusDataHeadV1 = (
  bytes: Uint8Array,
  offset: number,
): MidgardPlutusDataHeadV1 => {
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

const consumeMidgardPlutusDataByteStringV1 = (
  bytes: Uint8Array,
  head: MidgardPlutusDataHeadV1,
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
      const chunk = readMidgardPlutusDataHeadV1(bytes, cursor);
      if (chunk.major !== 2 || chunk.value === null) {
        throw new Error(
          "Indefinite PlutusData bytes must contain only definite byte chunks",
        );
      }
      if (chunk.value > BigInt(MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK_V1)) {
        throw new Error("PlutusData byte chunk exceeds 64 bytes");
      }
      const end = chunk.offset + Number(chunk.value);
      if (end > bytes.length) {
        throw new Error("Truncated PlutusData bytes chunk");
      }
      cursor = end;
    }
  }
  if (head.value > BigInt(MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK_V1)) {
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

type MidgardPlutusDataFrameV1 =
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
export const assertMidgardPlutusDataWellFormedV1 = (
  bytes: Uint8Array,
): void => {
  const stack: MidgardPlutusDataFrameV1[] = [];
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

    const head = readMidgardPlutusDataHeadV1(bytes, cursor);
    cursor = head.offset;

    if (head.major === 0 || head.major === 1) {
      if (head.value === null) {
        throw new Error("PlutusData integer must use a definite head");
      }
      completeValue();
      continue;
    }

    if (head.major === 2) {
      cursor = consumeMidgardPlutusDataByteStringV1(bytes, head);
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
      if (isMidgardPlutusDataConstrTagV1(tag)) {
        const fields = readMidgardPlutusDataHeadV1(bytes, cursor);
        if (fields.major !== 4) {
          throw new Error("PlutusData constructor fields must be an array");
        }
        cursor = fields.offset;
        openItems(fields.value);
        continue;
      }
      if (tag === 102n) {
        const outer = readMidgardPlutusDataHeadV1(bytes, cursor);
        if (outer.major !== 4 || outer.value !== 2n) {
          throw new Error(
            "General PlutusData constructor must be a definite two-item array",
          );
        }
        const alternative = readMidgardPlutusDataHeadV1(bytes, outer.offset);
        if (alternative.major !== 0 || alternative.value === null) {
          throw new Error(
            "General PlutusData constructor alternative must be an unsigned integer",
          );
        }
        const fields = readMidgardPlutusDataHeadV1(bytes, alternative.offset);
        if (fields.major !== 4) {
          throw new Error("PlutusData constructor fields must be an array");
        }
        cursor = fields.offset;
        openItems(fields.value);
        continue;
      }
      if (tag === 2n || tag === 3n) {
        const payload = readMidgardPlutusDataHeadV1(bytes, cursor);
        if (payload.major !== 2) {
          throw new Error("PlutusData bignum must wrap a byte string");
        }
        cursor = consumeMidgardPlutusDataByteStringV1(bytes, payload);
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
