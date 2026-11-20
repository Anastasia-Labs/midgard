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
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of CBOR input");
  }
  if (initial === 0xff) {
    throw new Error("Unexpected CBOR break marker");
  }

  const major = initial >> 5;
  const additional = initial & 0x1f;
  const length = readCborLength(bytes, offset + 1, additional);

  if (major === 0) {
    return {
      node: { kind: "uint", value: expectCborLength(length.value, "uint") },
      offset: length.offset,
    };
  }
  if (major === 1) {
    return {
      node: { kind: "nint", value: expectCborLength(length.value, "nint") },
      offset: length.offset,
    };
  }
  if (major === 2) {
    const byteLength = Number(expectCborLength(length.value, "bytes"));
    const end = length.offset + byteLength;
    return {
      node: { kind: "bytes", value: bytes.subarray(length.offset, end) },
      offset: end,
    };
  }
  if (major === 4) {
    const items: CborNode[] = [];
    if (length.value === null) {
      let cursor = length.offset;
      while (bytes[cursor] !== 0xff) {
        const parsed = parseCborNode(bytes, cursor);
        items.push(parsed.node);
        cursor = parsed.offset;
      }
      return {
        node: { kind: "array", items, indefinite: true },
        offset: cursor + 1,
      };
    }
    let cursor = length.offset;
    for (let i = 0n; i < length.value; i += 1n) {
      const parsed = parseCborNode(bytes, cursor);
      items.push(parsed.node);
      cursor = parsed.offset;
    }
    return {
      node: { kind: "array", items, indefinite: false },
      offset: cursor,
    };
  }
  if (major === 5) {
    const entries: (readonly [CborNode, CborNode])[] = [];
    if (length.value === null) {
      let cursor = length.offset;
      while (bytes[cursor] !== 0xff) {
        const key = parseCborNode(bytes, cursor);
        const value = parseCborNode(bytes, key.offset);
        entries.push([key.node, value.node]);
        cursor = value.offset;
      }
      return { node: { kind: "map", entries }, offset: cursor + 1 };
    }
    let cursor = length.offset;
    for (let i = 0n; i < length.value; i += 1n) {
      const key = parseCborNode(bytes, cursor);
      const value = parseCborNode(bytes, key.offset);
      entries.push([key.node, value.node]);
      cursor = value.offset;
    }
    return { node: { kind: "map", entries }, offset: cursor };
  }
  if (major === 6) {
    const parsed = parseCborNode(bytes, length.offset);
    return {
      node: {
        kind: "tag",
        tag: expectCborLength(length.value, "tag"),
        value: parsed.node,
      },
      offset: parsed.offset,
    };
  }

  throw new Error(`Unsupported PlutusData CBOR major type ${major}`);
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

const encodeCborNodeWithDefiniteMaps = (node: CborNode): Buffer => {
  switch (node.kind) {
    case "uint":
      return encodeCborHeader(0, node.value);
    case "nint":
      return encodeCborHeader(1, node.value);
    case "bytes":
      return Buffer.concat([
        encodeCborHeader(2, BigInt(node.value.length)),
        node.value,
      ]);
    case "array": {
      const items = node.items.map(encodeCborNodeWithDefiniteMaps);
      if (node.indefinite) {
        return Buffer.concat([
          Buffer.from([0x9f]),
          ...items,
          Buffer.from([0xff]),
        ]);
      }
      return Buffer.concat([
        encodeCborHeader(4, BigInt(node.items.length)),
        ...items,
      ]);
    }
    case "map": {
      const entries = node.entries
        .map(([key, value]) => {
          const encodedKey = encodeCborNodeWithDefiniteMaps(key);
          const encodedValue = encodeCborNodeWithDefiniteMaps(value);
          return { encodedKey, encodedValue };
        })
        .sort((left, right) =>
          Buffer.compare(left.encodedKey, right.encodedKey),
        );
      return Buffer.concat([
        encodeCborHeader(5, BigInt(entries.length)),
        ...entries.flatMap(({ encodedKey, encodedValue }) => [
          encodedKey,
          encodedValue,
        ]),
      ]);
    }
    case "tag":
      return Buffer.concat([
        encodeCborHeader(6, node.tag),
        encodeCborNodeWithDefiniteMaps(node.value),
      ]);
  }
};

export const aikenSerialisedPlutusDataCbor = (cbor: string): string => {
  const input = Buffer.from(cbor, "hex");
  const parsed = parseCborNode(input, 0);
  if (parsed.offset !== input.length) {
    throw new Error("Unexpected trailing bytes in PlutusData CBOR");
  }
  return encodeCborNodeWithDefiniteMaps(parsed.node).toString("hex");
};
