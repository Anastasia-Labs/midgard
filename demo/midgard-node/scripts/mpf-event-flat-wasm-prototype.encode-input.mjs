import assert from "node:assert/strict";

export const prefixBytes = (prefix) =>
  Buffer.from([...prefix].map((digit) => Number.parseInt(digit, 16)));

export const key = (index) => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value;
};

const recordSize = (value) => {
  const common = 1 + 32 + 1 + value.prefix.length;
  if (value.__kind === "Leaf") {
    return common + 2 + 4 + value.key.length / 2 + value.value.length / 2;
  }
  return (
    common + 8 + 2 + value.children.filter((child) => child != null).length * 32
  );
};

export const opSize = (op) =>
  1 + 2 + 4 + op.key.length + (op.type === "insert" ? op.value.length : 0);

export const encodeInput = ({ baseRoot, records, events }) => {
  const ops = events.flat();
  const bytes =
    72 +
    [...records.values()].reduce(
      (total, value) => total + recordSize(value),
      0,
    ) +
    events.length * 4 +
    ops.reduce((total, op) => total + opSize(op), 0);
  const output = Buffer.allocUnsafe(bytes);
  let offset = 0;
  const put = (value) => {
    Buffer.from(value).copy(output, offset);
    offset += value.length;
  };
  const u8 = (value) => {
    output.writeUInt8(value, offset);
    offset += 1;
  };
  const u16 = (value) => {
    output.writeUInt16LE(value, offset);
    offset += 2;
  };
  const u32 = (value) => {
    output.writeUInt32LE(value, offset);
    offset += 4;
  };
  const u64 = (value) => {
    output.writeBigUInt64LE(BigInt(value), offset);
    offset += 8;
  };
  put(Buffer.from("MEF6"));
  u16(1);
  u16(0);
  u32(1_000_000);
  u32(100_000);
  u32(400_000);
  u32(536_870_912);
  u32(536_870_912);
  u32(records.size);
  u32(events.length);
  u32(ops.length);
  put(baseRoot);
  for (const [hash, value] of records) {
    u8(value.__kind === "Leaf" ? 1 : 2);
    put(Buffer.from(hash, "hex"));
    u8(value.prefix.length);
    put(prefixBytes(value.prefix));
    if (value.__kind === "Leaf") {
      const keyBytes = Buffer.from(value.key, "hex");
      const valueBytes = Buffer.from(value.value, "hex");
      u16(keyBytes.length);
      u32(valueBytes.length);
      put(keyBytes);
      put(valueBytes);
    } else {
      u64(value.size);
      const bitmap = value.children.reduce(
        (bits, child, index) => bits | (child == null ? 0 : 1 << index),
        0,
      );
      u16(bitmap);
      for (const child of value.children) {
        if (child != null) put(Buffer.from(child, "hex"));
      }
    }
  }
  for (const event of events) {
    u32(event.length);
    for (const op of event) {
      u8(op.type === "insert" ? 1 : 2);
      u16(op.key.length);
      u32(op.type === "insert" ? op.value.length : 0);
      put(op.key);
      if (op.type === "insert") put(op.value);
    }
  }
  assert.equal(offset, output.length);
  return output;
};

export const commonPrefix = (left, right) => {
  let index = 0;
  while (index < left.length && left[index] === right[index]) index += 1;
  return left.slice(0, index);
};

export const makeProbeEvents = (initialUtxos, transactions) =>
  Array.from({ length: transactions }, (_, index) => [
    { type: "delete", key: key(index) },
    {
      type: "insert",
      key: key(initialUtxos + index),
      value: Buffer.alloc(64, (index + 1) % 251),
    },
  ]);
