import {
  HASH_BYTES,
  type StoredNode,
} from "./service.normalize-owner-options.js";

export const assertStoredNode = (
  value: unknown,
  hashHex: string,
): StoredNode => {
  if (typeof value !== "object" || value === null || !("__kind" in value)) {
    throw new Error(`MPF record ${hashHex} is not a node`);
  }
  const candidate = value as Record<string, unknown>;
  if (
    typeof candidate.prefix !== "string" ||
    !/^[0-9a-f]*$/.test(candidate.prefix)
  ) {
    throw new Error(`MPF record ${hashHex} has a non-canonical prefix`);
  }
  if (candidate.__kind === "Leaf") {
    if (
      typeof candidate.key !== "string" ||
      !/^(?:[0-9a-f]{2})*$/.test(candidate.key) ||
      typeof candidate.value !== "string" ||
      !/^(?:[0-9a-f]{2})*$/.test(candidate.value)
    ) {
      throw new Error(`MPF leaf ${hashHex} has invalid key/value hex`);
    }
    return {
      __kind: "Leaf",
      prefix: candidate.prefix,
      key: candidate.key,
      value: candidate.value,
    };
  }
  if (
    candidate.__kind !== "Branch" ||
    !Array.isArray(candidate.children) ||
    candidate.children.length !== 16 ||
    candidate.children.some(
      (child) => child !== null && !/^[0-9a-f]{64}$/.test(String(child)),
    ) ||
    !Number.isSafeInteger(candidate.size) ||
    Number(candidate.size) < 0
  ) {
    throw new Error(`MPF branch ${hashHex} is invalid`);
  }
  return {
    __kind: "Branch",
    prefix: candidate.prefix,
    children: candidate.children.map((child) =>
      child === null ? null : String(child),
    ),
    size: Number(candidate.size),
  };
};

export const encodeStoredNode = (hashHex: string, value: unknown): Buffer => {
  const node = assertStoredNode(value, hashHex);
  const prefix = Buffer.from(
    [...node.prefix].map((nibble) => Number.parseInt(nibble, 16)),
  );
  if (node.__kind === "Leaf") {
    const key = Buffer.from(node.key, "hex");
    const leafValue = Buffer.from(node.value, "hex");
    const output = Buffer.allocUnsafe(
      1 +
        HASH_BYTES +
        1 +
        prefix.length +
        2 +
        4 +
        key.length +
        leafValue.length,
    );
    let offset = 0;
    output.writeUInt8(1, offset++);
    Buffer.from(hashHex, "hex").copy(output, offset);
    offset += HASH_BYTES;
    output.writeUInt8(prefix.length, offset++);
    prefix.copy(output, offset);
    offset += prefix.length;
    output.writeUInt16LE(key.length, offset);
    offset += 2;
    output.writeUInt32LE(leafValue.length, offset);
    offset += 4;
    key.copy(output, offset);
    offset += key.length;
    leafValue.copy(output, offset);
    return output;
  }
  const children = node.children.filter(
    (child): child is string => child !== null,
  );
  const output = Buffer.allocUnsafe(
    1 + HASH_BYTES + 1 + prefix.length + 8 + 2 + children.length * HASH_BYTES,
  );
  let offset = 0;
  output.writeUInt8(2, offset++);
  Buffer.from(hashHex, "hex").copy(output, offset);
  offset += HASH_BYTES;
  output.writeUInt8(prefix.length, offset++);
  prefix.copy(output, offset);
  offset += prefix.length;
  output.writeBigUInt64LE(BigInt(node.size), offset);
  offset += 8;
  const bitmap = node.children.reduce(
    (bits, child, index) => bits | (child === null ? 0 : 1 << index),
    0,
  );
  output.writeUInt16LE(bitmap, offset);
  offset += 2;
  for (const child of children) {
    Buffer.from(child, "hex").copy(output, offset);
    offset += HASH_BYTES;
  }
  return output;
};
