export type CborNode =
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

export type CborRange = {
  readonly start: number;
  readonly end: number;
};

export const readCborLength = (
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

export const expectCborLength = (
  length: bigint | null,
  context: string,
): bigint => {
  if (length === null) {
    throw new Error(`${context} must use a definite length`);
  }
  return length;
};

export const parseCborNode = (
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

export const parseCborRange = (bytes: Buffer, offset: number): CborRange => ({
  start: offset,
  end: parseCborNode(bytes, offset).offset,
});
