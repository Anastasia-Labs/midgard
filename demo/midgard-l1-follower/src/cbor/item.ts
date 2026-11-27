import { readHead } from "./reader.js";

/** A decoded CBOR item. Tag 258 (a set) is unwrapped to its array. */
export type CborItem =
  | Readonly<{ kind: "int"; value: bigint }>
  | Readonly<{ kind: "bytes"; value: Buffer }>
  | Readonly<{ kind: "text"; value: string }>
  | Readonly<{ kind: "array"; items: readonly CborItem[] }>
  | Readonly<{
      kind: "map";
      entries: readonly (readonly [CborItem, CborItem])[];
    }>
  | Readonly<{ kind: "tag"; tag: bigint; item: CborItem }>
  | Readonly<{ kind: "simple"; text: string }>;

const MAX_DEPTH = 48;
const BREAK = 0xff;
const SET_TAG = 258n;

/**
 * Decodes the whole item at `offset`, for small diagnostic payloads (a ledger
 * rejection) where every value is wanted rather than one span. Depth is
 * bounded, so hostile nesting throws instead of exhausting the stack.
 */
export const decodeCborItem = (
  bytes: Uint8Array,
  offset: number,
  depth = 0,
): { item: CborItem; next: number } => {
  if (depth > MAX_DEPTH) throw new Error(`nesting deeper than ${MAX_DEPTH}`);
  const head = readHead(bytes, offset);
  switch (head.major) {
    case 0:
      return { item: { kind: "int", value: head.value }, next: head.next };
    case 1:
      return {
        item: { kind: "int", value: -1n - head.value },
        next: head.next,
      };
    case 2:
    case 3: {
      const { chunks, next } = stringChunks(bytes, offset, head);
      const value = Buffer.concat(chunks);
      return {
        item:
          head.major === 2
            ? { kind: "bytes", value }
            : { kind: "text", value: value.toString("utf8") },
        next,
      };
    }
    case 4: {
      const items: CborItem[] = [];
      const next = forEachChild(bytes, head, (position) => {
        const child = decodeCborItem(bytes, position, depth + 1);
        items.push(child.item);
        return child.next;
      });
      return { item: { kind: "array", items }, next };
    }
    case 5: {
      const entries: (readonly [CborItem, CborItem])[] = [];
      const next = forEachChild(bytes, head, (position) => {
        const key = decodeCborItem(bytes, position, depth + 1);
        const value = decodeCborItem(bytes, key.next, depth + 1);
        entries.push([key.item, value.item]);
        return value.next;
      });
      return { item: { kind: "map", entries }, next };
    }
    case 6: {
      const inner = decodeCborItem(bytes, head.next, depth + 1);
      return {
        item:
          head.value === SET_TAG
            ? inner.item
            : { kind: "tag", tag: head.value, item: inner.item },
        next: inner.next,
      };
    }
    default: {
      const info = (bytes[offset] as number) & 0x1f;
      const text =
        info === 20
          ? "false"
          : info === 21
            ? "true"
            : info === 22
              ? "null"
              : `simple(${String(head.value)})`;
      return { item: { kind: "simple", text }, next: head.next };
    }
  }
};

type Head = ReturnType<typeof readHead>;

const stringChunks = (
  bytes: Uint8Array,
  offset: number,
  head: Head,
): { chunks: Buffer[]; next: number } => {
  if (!head.indefinite) {
    const end = head.next + Number(head.value);
    if (head.value > BigInt(bytes.length) || end > bytes.length)
      throw new Error(`truncated string at offset ${offset}`);
    return { chunks: [Buffer.from(bytes.subarray(head.next, end))], next: end };
  }
  const chunks: Buffer[] = [];
  let position = head.next;
  while (bytes[position] !== BREAK) {
    if (position >= bytes.length)
      throw new Error(`unterminated string at offset ${offset}`);
    const chunk = readHead(bytes, position);
    if (chunk.major !== head.major || chunk.indefinite)
      throw new Error(`bad string chunk at offset ${position}`);
    const inner = stringChunks(bytes, position, chunk);
    chunks.push(...inner.chunks);
    position = inner.next;
  }
  return { chunks, next: position + 1 };
};

/** Visits each child of a container head; `visit` returns the next offset. */
const forEachChild = (
  bytes: Uint8Array,
  head: Head,
  visit: (position: number) => number,
): number => {
  let position = head.next;
  if (head.indefinite) {
    while (bytes[position] !== BREAK) {
      if (position >= bytes.length) throw new Error("unterminated container");
      position = visit(position);
    }
    return position + 1;
  }
  if (head.value > BigInt(bytes.length))
    throw new Error("container longer than the input");
  for (let index = 0n; index < head.value; index += 1n)
    position = visit(position);
  return position;
};

/** Renders any item on one line; byte strings past 64 bytes are shortened. */
export const renderCborItem = (item: CborItem): string => {
  switch (item.kind) {
    case "int":
      return item.value.toString();
    case "bytes":
      return item.value.length > 64
        ? `${item.value.subarray(0, 32).toString("hex")}…(${item.value.length} bytes)`
        : item.value.toString("hex");
    case "text":
      return JSON.stringify(item.value);
    case "array":
      return `[${item.items.map(renderCborItem).join(", ")}]`;
    case "map":
      return `{${item.entries.map(([k, v]) => `${renderCborItem(k)}: ${renderCborItem(v)}`).join(", ")}}`;
    case "tag":
      return `${item.tag}(${renderCborItem(item.item)})`;
    case "simple":
      return item.text;
  }
};
