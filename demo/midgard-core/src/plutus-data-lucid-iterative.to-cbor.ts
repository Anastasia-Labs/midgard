import { Constr, type Data } from "@lucid-evolution/lucid";

import { PlutusDataStructureIds } from "./plutus-data-lucid-iterative.structure-ids.js";

/**
 * Lucid `Data.to(data)` (no schema, not canonical), without recursion.
 *
 * Lucid builds a CML `PlutusData` and writes it in the cardano-node format:
 *
 * - integers in minimal heads within 64 bits, otherwise as bignums (tags 2
 *   and 3) over their big-endian magnitude;
 * - byte strings of at most 64 bytes as one string, longer ones as an
 *   indefinite string of 64-byte chunks; hex is read case-insensitively;
 * - non-empty lists, maps and constructor fields indefinite, empty ones as
 *   `80` / `a0`;
 * - constructors under tags 121 to 127 (alternatives 0 to 6), 1280 to 1400
 *   (7 to 127), or 102 over an indefinite `[alternative, fields]` array;
 * - map entries set one by one as CML's `PlutusMap.set` does (an entry whose
 *   key equals an earlier one replaces it; maps inside keys compare in their
 *   insertion order), then written sorted by CML's order on keys;
 * - arrays and constructor fields read with `forEach` (holes skipped), and a
 *   constructor index parsed from `index.toString()` as an unsigned 64-bit
 *   decimal, as CML's `BigInteger.from_str(...).as_u64()` parses it.
 *
 * Anything else Lucid refuses is refused here too.
 */

type PlannedNode =
  | { readonly kind: "int"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly bytes: Buffer }
  | { readonly kind: "list"; readonly children: readonly number[] }
  | { readonly kind: "map"; readonly children: readonly number[] }
  | {
      readonly kind: "constr";
      readonly alternative: bigint;
      readonly children: readonly number[];
    };

type PlanFrame = {
  readonly node: number;
  readonly value: object;
  readonly kind: "list" | "map" | "constr";
  /** The children to plan, as values; for maps, keys and values alternate. */
  readonly pending: readonly unknown[];
  next: number;
  readonly children: number[];
};

const U64_MAX = (1n << 64n) - 1n;

const refuse = (reason: string): never => {
  throw new Error(`Could not serialize the data: ${reason}`);
};

/** noble `hexToBytes`: even length, hex digits only, either case. */
const bytesFromHex = (hex: string): Buffer => {
  if (hex.length % 2 !== 0 || !/^[0-9a-fA-F]*$/.test(hex)) {
    refuse("hex string expected");
  }
  return Buffer.from(hex, "hex");
};

/** num-bigint `BigInt::from_str` followed by `as_u64`. */
const parseAlternative = (index: unknown): bigint => {
  const text: unknown = (index as { toString(): unknown }).toString();
  if (typeof text !== "string") return refuse("invalid constructor index");
  let digits = text;
  let negative = false;
  if (digits.startsWith("-")) {
    negative = true;
    if (!digits.startsWith("-+")) digits = digits.slice(1);
  }
  if (digits.startsWith("+") && !digits.startsWith("++")) {
    digits = digits.slice(1);
  }
  if (
    digits.length === 0 ||
    digits.startsWith("_") ||
    !/^[0-9_]+$/.test(digits)
  ) {
    return refuse("invalid constructor index");
  }
  const magnitude = BigInt(digits.replace(/_/g, ""));
  const value = negative ? -magnitude : magnitude;
  if (value < 0n || value > U64_MAX) {
    return refuse("constructor index is not an unsigned 64-bit integer");
  }
  return value;
};

const collect = (items: { forEach?: unknown }): unknown[] => {
  if (typeof items.forEach !== "function") {
    return refuse("fields.forEach is not a function");
  }
  const collected: unknown[] = [];
  (items.forEach as (visit: (item: unknown) => void) => void).call(
    items,
    (item) => {
      collected.push(item);
    },
  );
  return collected;
};

const KIND_RANK: Readonly<Record<PlannedNode["kind"], number>> = {
  constr: 0,
  map: 1,
  list: 2,
  int: 3,
  bytes: 4,
};

/**
 * CML's order on `PlutusData`: constructors, maps, lists, integers, then
 * byte strings; integers by value, byte strings bytewise, and constructors
 * (alternative first), lists and maps (entries, key then value, in sorted
 * order) lexicographically. The walk is depth-first with an explicit stack.
 */
const comparePlanned = (
  nodes: readonly PlannedNode[],
  sortedIds: PlutusDataStructureIds,
  left: number,
  right: number,
): number => {
  const stack: (readonly [number, number, boolean])[] = [[left, right, false]];
  for (let task = stack.pop(); task !== undefined; task = stack.pop()) {
    const [x, y, isLength] = task;
    if (isLength) {
      if (x !== y) return x < y ? -1 : 1;
      continue;
    }
    if (sortedIds.of(x) === sortedIds.of(y)) continue;
    const nx = nodes[x]!;
    const ny = nodes[y]!;
    if (nx.kind !== ny.kind) {
      return KIND_RANK[nx.kind] < KIND_RANK[ny.kind] ? -1 : 1;
    }
    if (nx.kind === "int" && ny.kind === "int") {
      return nx.value < ny.value ? -1 : 1;
    }
    if (nx.kind === "bytes" && ny.kind === "bytes") {
      return Buffer.compare(nx.bytes, ny.bytes);
    }
    if (
      nx.kind === "constr" &&
      ny.kind === "constr" &&
      nx.alternative !== ny.alternative
    ) {
      return nx.alternative < ny.alternative ? -1 : 1;
    }
    const cx = (nx as { readonly children: readonly number[] }).children;
    const cy = (ny as { readonly children: readonly number[] }).children;
    stack.push([cx.length, cy.length, true]);
    for (
      let index = Math.min(cx.length, cy.length) - 1;
      index >= 0;
      index -= 1
    ) {
      stack.push([cx[index]!, cy[index]!, false]);
    }
  }
  return 0;
};

const planData = (root: unknown): PlannedNode[] => {
  const nodes: PlannedNode[] = [];
  // Equality as CML's `PlutusMap.set` sees it (maps in insertion order), and
  // equality of the written form (maps sorted), which the order relies on.
  const rawIds = new PlutusDataStructureIds(0);
  const sortedIds = new PlutusDataStructureIds(0);
  const onPath = new Set<object>();
  const stack: PlanFrame[] = [];
  const open = (value: unknown): void => {
    const node = nodes.length;
    if (typeof value === "bigint") {
      nodes.push({ kind: "int", value });
      rawIds.integer(node, value);
      sortedIds.integer(node, value);
      return;
    }
    if (typeof value === "string") {
      const bytes = bytesFromHex(value);
      nodes.push({ kind: "bytes", bytes });
      rawIds.bytes(node, bytes.toString("hex"));
      sortedIds.bytes(node, bytes.toString("hex"));
      return;
    }
    let kind: PlanFrame["kind"];
    let pending: unknown[];
    if (value instanceof Constr) {
      kind = "constr";
      pending = collect(value.fields as { forEach?: unknown });
    } else if (value instanceof Array) {
      kind = "list";
      pending = collect(value);
    } else if (value instanceof Map) {
      kind = "map";
      pending = [...(value as Map<unknown, unknown>).entries()].flat();
    } else {
      return refuse("Unsupported type");
    }
    if (onPath.has(value)) return refuse("Data contains itself");
    onPath.add(value);
    nodes.push({ kind: "list", children: [] });
    stack.push({ node, value, kind, pending, next: 0, children: [] });
  };
  const close = (frame: PlanFrame): void => {
    onPath.delete(frame.value);
    const children = frame.children;
    if (frame.kind === "list") {
      nodes[frame.node] = { kind: "list", children };
      rawIds.list(frame.node, children);
      sortedIds.list(frame.node, children);
      return;
    }
    if (frame.kind === "constr") {
      const alternative = parseAlternative((frame.value as Constr<Data>).index);
      nodes[frame.node] = { kind: "constr", alternative, children };
      rawIds.constr(frame.node, alternative, children);
      sortedIds.constr(frame.node, alternative, children);
      return;
    }
    // `set` drops any entry with an equal key and appends the new one.
    const pairs: (readonly [number, number] | undefined)[] = [];
    const pairByKey = new Map<number, number>();
    for (let entry = 0; entry < children.length; entry += 2) {
      const keyId = rawIds.of(children[entry]!);
      const previous = pairByKey.get(keyId);
      if (previous !== undefined) pairs[previous] = undefined;
      pairByKey.set(keyId, pairs.length);
      pairs.push([children[entry]!, children[entry + 1]!]);
    }
    const live = pairs.filter((pair) => pair !== undefined);
    rawIds.map(frame.node, live.flat());
    // Written sorted by key (a stable sort, so equal keys keep their order).
    const sorted = [...live]
      .sort(([leftKey], [rightKey]) =>
        comparePlanned(nodes, sortedIds, leftKey, rightKey),
      )
      .flat();
    nodes[frame.node] = { kind: "map", children: sorted };
    sortedIds.map(frame.node, sorted);
  };

  open(root);
  for (;;) {
    const top = stack[stack.length - 1];
    if (top === undefined) return nodes;
    if (top.next < top.pending.length) {
      top.children.push(nodes.length);
      open(top.pending[top.next]);
      top.next += 1;
      continue;
    }
    stack.pop();
    close(top);
  }
};

const head = (major: number, value: bigint): Buffer => {
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  const width =
    value <= 0xffn ? 1 : value <= 0xffffn ? 2 : value <= 0xffffffffn ? 4 : 8;
  const out = Buffer.alloc(1 + width);
  out[0] = prefix | (24 + Math.log2(width));
  for (let index = width; index >= 1; index -= 1) {
    out[index] = Number((value >> BigInt((width - index) * 8)) & 0xffn);
  }
  return out;
};

const chunkedBytes = (bytes: Buffer): Buffer[] => {
  if (bytes.length <= 64) return [head(2, BigInt(bytes.length)), bytes];
  const parts: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    const chunk = bytes.subarray(offset, offset + 64);
    parts.push(head(2, BigInt(chunk.length)), chunk);
  }
  parts.push(Buffer.from([0xff]));
  return parts;
};

const integerParts = (value: bigint): Buffer[] => {
  if (value >= 0n && value <= U64_MAX) return [head(0, value)];
  if (value < 0n && -1n - value <= U64_MAX) return [head(1, -1n - value)];
  const magnitude = value < 0n ? -1n - value : value;
  const hex = magnitude.toString(16);
  return [
    head(6, value < 0n ? 3n : 2n),
    ...chunkedBytes(Buffer.from(hex.length % 2 === 0 ? hex : `0${hex}`, "hex")),
  ];
};

const BREAK = Buffer.from([0xff]);

/** Lucid `Data.to(data)` without a schema, as bytes. */
export const lucidDataToCborIterative = (data: Data): Buffer => {
  const nodes = planData(data);
  const out: Buffer[] = [];
  const tasks: (number | Buffer)[] = [0];
  const pushContainer = (
    children: readonly number[],
    empty: number,
    indefinite: number,
  ): void => {
    if (children.length === 0) {
      out.push(Buffer.from([empty]));
      return;
    }
    out.push(Buffer.from([indefinite]));
    tasks.push(BREAK);
    for (let index = children.length - 1; index >= 0; index -= 1) {
      tasks.push(children[index]!);
    }
  };
  for (let task = tasks.pop(); task !== undefined; task = tasks.pop()) {
    if (Buffer.isBuffer(task)) {
      out.push(task);
      continue;
    }
    const node = nodes[task]!;
    if (node.kind === "int") {
      out.push(...integerParts(node.value));
    } else if (node.kind === "bytes") {
      out.push(...chunkedBytes(node.bytes));
    } else if (node.kind === "list") {
      pushContainer(node.children, 0x80, 0x9f);
    } else if (node.kind === "map") {
      pushContainer(node.children, 0xa0, 0xbf);
    } else if (node.alternative <= 6n) {
      out.push(head(6, 121n + node.alternative));
      pushContainer(node.children, 0x80, 0x9f);
    } else if (node.alternative <= 127n) {
      out.push(head(6, 1280n + node.alternative - 7n));
      pushContainer(node.children, 0x80, 0x9f);
    } else {
      out.push(head(6, 102n), Buffer.from([0x9f]), head(0, node.alternative));
      tasks.push(BREAK);
      pushContainer(node.children, 0x80, 0x9f);
    }
  }
  return Buffer.concat(out);
};
