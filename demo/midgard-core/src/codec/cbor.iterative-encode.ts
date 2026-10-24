/**
 * Midgard's generic CBOR encoder, written with explicit stacks so that nesting
 * depth is bounded only by memory.
 *
 * It reproduces, byte for byte and error for error, the RFC 8949 encoding the
 * codec previously delegated to `cborg` (`rfc8949EncodeOptions`): integers
 * and lengths in their shortest form, non-integer or unsafe numbers as 64-bit
 * floats, strings as UTF-8 (lone surrogates become U+FFFD), byte-like values
 * as byte strings, and map entries sorted by their encoded key bytes.
 *
 * The encoding is produced in two modes, as before:
 * - arrays and scalars are written as they are visited (a too-large bigint
 *   fails at once);
 * - a map or plain object is first turned into a token tree for its whole
 *   subtree, each map sorted when it completes, and only then written. Only
 *   scalar keys (and empty containers, whose sort key is `0x00`) can be
 *   sorted; any other key fails the sort.
 */

import {
  assertScalarType,
  ByteWriter,
  CIRCULAR_ERROR_MESSAGE,
  classify,
  COMPLEX_KEY_ERROR_MESSAGE,
  isContainerType,
  MAJOR_ARRAY,
  MAJOR_MAP,
  writeScalar,
} from "./cbor.encode-scalar.js";

type TokenNode =
  | {
      readonly kind: "leaf";
      readonly value: unknown;
      readonly type: string;
      keyBytes?: Uint8Array;
    }
  | { readonly kind: "empty"; readonly byte: number }
  | { readonly kind: "array"; readonly items: TokenNode[] }
  | { readonly kind: "map"; readonly entries: [TokenNode, TokenNode][] };

const EMPTY_ARRAY_TOKEN: TokenNode = { kind: "empty", byte: MAJOR_ARRAY };
const EMPTY_MAP_TOKEN: TokenNode = { kind: "empty", byte: MAJOR_MAP };
const EMPTY_KEY_BYTES = new Uint8Array([0]);

const sortKeyBytes = (node: TokenNode): Uint8Array => {
  if (node.kind === "empty") {
    return EMPTY_KEY_BYTES;
  }
  if (node.kind !== "leaf") {
    throw new Error(COMPLEX_KEY_ERROR_MESSAGE);
  }
  if (node.keyBytes === undefined) {
    const writer = new ByteWriter();
    writeScalar(writer, node.value, node.type);
    node.keyBytes = writer.toBuffer();
  }
  return node.keyBytes;
};

/** Byte comparison bounded by the left operand, as the reference sorter did. */
const compareSortKeys = (left: Uint8Array, right: Uint8Array): number => {
  for (let i = 0; i < left.length; i += 1) {
    if (left[i] === right[i]) {
      continue;
    }
    return right[i] === undefined || left[i] > right[i] ? 1 : -1;
  }
  return 0;
};

const compareEntries = (
  left: readonly [TokenNode, TokenNode],
  right: readonly [TokenNode, TokenNode],
): number => {
  const leftKey = left[0];
  const rightKey = right[0];
  const scalar = (node: TokenNode): boolean =>
    node.kind === "leaf" || node.kind === "empty";
  if (!scalar(leftKey) || !scalar(rightKey)) {
    throw new Error(COMPLEX_KEY_ERROR_MESSAGE);
  }
  return compareSortKeys(sortKeyBytes(leftKey), sortKeyBytes(rightKey));
};

type TokenFrame =
  | {
      readonly kind: "array";
      readonly source: readonly unknown[];
      readonly owner: object;
      index: number;
      readonly items: TokenNode[];
    }
  | {
      readonly kind: "map";
      readonly owner: object;
      readonly isMap: boolean;
      readonly keys: Iterator<unknown>;
      readonly entries: [TokenNode, TokenNode][];
      stage: "key" | "key-done" | "value" | "value-done";
      pendingValue: unknown;
      pendingKey: TokenNode | undefined;
    };

const enterGuard = (ancestors: Set<object>, value: object): void => {
  if (ancestors.has(value)) {
    throw new Error(CIRCULAR_ERROR_MESSAGE);
  }
  ancestors.add(value);
};

const openMapFrame = (
  value: object,
  type: string,
  ancestors: Set<object>,
): TokenFrame | undefined => {
  const isMap = type !== "Object";
  let keys: Iterator<unknown>;
  let size: number;
  if (isMap) {
    keys = (value as Map<unknown, unknown>).keys();
    size = (value as Map<unknown, unknown>).size;
  } else {
    const names = Object.keys(value);
    keys = names[Symbol.iterator]();
    size = names.length;
  }
  if (size === 0) {
    return undefined;
  }
  enterGuard(ancestors, value);
  return {
    kind: "map",
    owner: value,
    isMap,
    keys,
    entries: [],
    stage: "key",
    pendingValue: undefined,
    pendingKey: undefined,
  };
};

/** Next child value of a token frame, or `done` when the frame is complete. */
const nextTokenChild = (
  frame: TokenFrame,
): { readonly done: boolean; readonly value?: unknown } => {
  if (frame.kind === "array") {
    if (frame.index < frame.source.length) {
      const value = frame.source[frame.index];
      frame.index += 1;
      return { done: false, value };
    }
    return { done: true };
  }
  if (frame.stage === "value") {
    frame.stage = "value-done";
    return { done: false, value: frame.pendingValue };
  }
  const step = frame.keys.next();
  if (step.done === true) {
    return { done: true };
  }
  const key = step.value;
  frame.pendingValue = frame.isMap
    ? (frame.owner as Map<unknown, unknown>).get(key)
    : (frame.owner as Record<string, unknown>)[key as string];
  frame.stage = "key-done";
  return { done: false, value: key };
};

const attachToken = (frame: TokenFrame, node: TokenNode): void => {
  if (frame.kind === "array") {
    frame.items.push(node);
  } else if (frame.stage === "key-done") {
    frame.pendingKey = node;
    frame.stage = "value";
  } else {
    frame.entries.push([frame.pendingKey!, node]);
    frame.pendingKey = undefined;
    frame.pendingValue = undefined;
    frame.stage = "key";
  }
};

const closeTokenFrame = (frame: TokenFrame): TokenNode => {
  if (frame.kind === "array") {
    return { kind: "array", items: frame.items };
  }
  if (frame.entries.length === 0) {
    return EMPTY_MAP_TOKEN;
  }
  frame.entries.sort(compareEntries);
  return { kind: "map", entries: frame.entries };
};

/**
 * Turns a Map/Object subtree into a token tree with every map sorted.
 * `ancestors` holds the containers on the path from the encode root.
 */
const tokenizeMapSubtree = (
  root: object,
  rootType: string,
  ancestors: Set<object>,
): TokenNode => {
  const rootFrame = openMapFrame(root, rootType, ancestors);
  if (rootFrame === undefined) {
    return EMPTY_MAP_TOKEN;
  }
  const stack: TokenFrame[] = [rootFrame];
  for (;;) {
    const frame = stack[stack.length - 1];
    const child = nextTokenChild(frame);
    let completed: TokenNode | undefined;
    if (child.done) {
      stack.pop();
      ancestors.delete(frame.owner);
      completed = closeTokenFrame(frame);
    } else {
      const value = child.value;
      const type = classify(value);
      if (!isContainerType(type)) {
        assertScalarType(type);
        completed = { kind: "leaf", value, type };
      } else if (type === "Array") {
        const array = value as unknown[];
        if (array.length === 0) {
          completed = EMPTY_ARRAY_TOKEN;
        } else {
          enterGuard(ancestors, array);
          stack.push({
            kind: "array",
            source: array,
            owner: array,
            index: 0,
            items: [],
          });
          continue;
        }
      } else {
        const opened = openMapFrame(value as object, type, ancestors);
        if (opened === undefined) {
          completed = EMPTY_MAP_TOKEN;
        } else {
          stack.push(opened);
          continue;
        }
      }
    }
    const parent = stack[stack.length - 1];
    if (parent === undefined) {
      return completed;
    }
    attachToken(parent, completed);
  }
};

const writeTokenTree = (writer: ByteWriter, root: TokenNode): void => {
  type WriteFrame = {
    readonly children: readonly TokenNode[];
    index: number;
  };
  const stack: WriteFrame[] = [];
  let node: TokenNode | undefined = root;
  for (;;) {
    if (node !== undefined) {
      switch (node.kind) {
        case "leaf":
          writeScalar(writer, node.value, node.type);
          break;
        case "empty":
          writer.byte(node.byte);
          break;
        case "array":
          writer.head(MAJOR_ARRAY, node.items.length);
          stack.push({ children: node.items, index: 0 });
          break;
        case "map":
          writer.head(MAJOR_MAP, node.entries.length);
          stack.push({ children: node.entries.flat(), index: 0 });
          break;
      }
    }
    const top = stack[stack.length - 1];
    if (top === undefined) {
      return;
    }
    if (top.index < top.children.length) {
      node = top.children[top.index];
      top.index += 1;
    } else {
      stack.pop();
      node = undefined;
    }
  }
};

type DirectFrame = {
  readonly source: readonly unknown[];
  index: number;
};

/**
 * Encodes `value` as CBOR, throwing a plain `Error` with the reference
 * encoder's message for unsupported types, circular references, unsortable
 * map keys and integers outside the 64-bit CBOR range.
 */
export const encodeCborIteratively = (value: unknown): Buffer => {
  const writer = new ByteWriter();
  const ancestors = new Set<object>();
  const stack: DirectFrame[] = [];
  let pending: unknown = value;
  let hasPending = true;
  for (;;) {
    if (hasPending) {
      hasPending = false;
      const type = classify(pending);
      if (!writeScalar(writer, pending, type)) {
        if (type === "Array") {
          const array = pending as unknown[];
          if (array.length === 0) {
            writer.byte(MAJOR_ARRAY);
          } else {
            enterGuard(ancestors, array);
            writer.head(MAJOR_ARRAY, array.length);
            stack.push({ source: array, index: 0 });
          }
        } else {
          writeTokenTree(
            writer,
            tokenizeMapSubtree(pending as object, type, ancestors),
          );
        }
      }
    }
    const top = stack[stack.length - 1];
    if (top === undefined) {
      return writer.toBuffer();
    }
    if (top.index < top.source.length) {
      pending = top.source[top.index];
      top.index += 1;
      hasPending = true;
    } else {
      stack.pop();
      ancestors.delete(top.source);
    }
  }
};
