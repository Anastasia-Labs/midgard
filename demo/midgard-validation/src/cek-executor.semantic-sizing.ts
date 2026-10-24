import {
  MIDGARD_CEK_BLS_G1_MEMORY,
  MIDGARD_CEK_BLS_G2_MEMORY,
  MIDGARD_CEK_BLS_ML_RESULT_MEMORY,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  midgardCekStringMemoryWords,
} from "@al-ft/midgard-core";

import { type MidgardCekConstantType } from "./cek-constant.js";
import {
  type Bytes,
  rootHex,
} from "./cek-executor.build-midgard-cek-execution-graph.js";

/**
 * A constant's size under a type: its ExMemory (null where sizing it under
 * that type is refused) and whether its payload has exactly that type.
 */
export type MidgardCekTypedSize = {
  readonly memory: bigint | null;
  readonly exact: boolean;
};

/** The installed semantic nodes sizing reads. */
export type MidgardCekSizingSource = {
  readonly node: (root: Bytes) => MidgardCekDataNode;
  readonly link: (root: Bytes) => MidgardCekDataListNode;
  readonly blob: (root: Bytes) => Buffer;
};

const REFUSED: MidgardCekTypedSize = Object.freeze({
  memory: null,
  exact: false,
});

const addSizes = (
  left: MidgardCekTypedSize,
  right: MidgardCekTypedSize,
): MidgardCekTypedSize => ({
  memory:
    left.memory === null || right.memory === null
      ? null
      : left.memory + right.memory,
  exact: left.exact && right.exact,
});

/** A stable key naming a constant type. */
export const midgardCekConstantTypeKey = (
  type: MidgardCekConstantType,
): string => {
  switch (type.kind) {
    case "list":
      return `list(${midgardCekConstantTypeKey(type.element)})`;
    case "array":
      return `array(${midgardCekConstantTypeKey(type.element)})`;
    case "pair":
      return `pair(${midgardCekConstantTypeKey(type.first)},${midgardCekConstantTypeKey(type.second)})`;
    case "integer":
    case "bytes":
    case "string":
    case "unit":
    case "boolean":
    case "data":
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
    case "value":
      return type.kind;
  }
};

const isUtf8 = (bytes: Buffer): boolean => {
  try {
    return Buffer.from(
      new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      "utf8",
    ).equals(bytes);
  } catch {
    return false;
  }
};

/**
 * Typed sizes of installed Data payloads, memoized per type and root, so a
 * value built from sized parts is sized by its new nodes only.
 */
export class MidgardCekSemanticSizing {
  private readonly sizes = new Map<string, MidgardCekTypedSize>();

  constructor(private readonly source: MidgardCekSizingSource) {}

  private node(root: Bytes): MidgardCekDataNode {
    return this.source.node(root);
  }

  private link(root: Bytes): MidgardCekDataListNode {
    return this.source.link(root);
  }

  private blob(root: Bytes): Buffer {
    return this.source.blob(root);
  }

  /** Records a size already computed from the decoded payload. */
  public seed(
    type: MidgardCekConstantType,
    root: Bytes,
    size: MidgardCekTypedSize,
  ): void {
    this.sizes.set(`${midgardCekConstantTypeKey(type)}|${rootHex(root)}`, size);
  }

  /**
   * The size of the payload at `root` read under `type`, mirroring the
   * constant memory rule and the exact payload-type rule. Each node is sized
   * once per type; a list is sized link by link, so a list that grew by one
   * link costs one new entry.
   */
  public sized(type: MidgardCekConstantType, root: Bytes): MidgardCekTypedSize {
    const node = this.node(root);
    switch (type.kind) {
      case "data":
        return { memory: node.memory, exact: true };
      case "integer":
        return node.kind === "integer"
          ? { memory: node.memory - 4n, exact: true }
          : REFUSED;
      case "bytes":
        return node.kind === "bytes"
          ? { memory: node.memory - 4n, exact: true }
          : REFUSED;
      case "unit":
      case "boolean":
        if (node.kind !== "constrSmall" && node.kind !== "constrLarge") {
          return REFUSED;
        }
        return {
          memory: 1n,
          exact:
            node.kind === "constrSmall" &&
            node.fieldsCount === 0n &&
            (node.constructor === 0n ||
              (type.kind === "boolean" && node.constructor === 1n)),
        };
      case "blsG1":
      case "blsG2":
        return {
          memory:
            type.kind === "blsG1"
              ? MIDGARD_CEK_BLS_G1_MEMORY
              : MIDGARD_CEK_BLS_G2_MEMORY,
          exact:
            node.kind === "bytes" &&
            node.bytesLength === (type.kind === "blsG1" ? 48n : 96n),
        };
      case "blsMillerLoopResult":
        return { memory: MIDGARD_CEK_BLS_ML_RESULT_MEMORY, exact: false };
      // The PV 11 kinds are not sized from installed nodes.
      case "array":
      case "value":
        return REFUSED;
      case "string":
      case "list":
      case "pair":
        break;
    }
    const key = `${midgardCekConstantTypeKey(type)}|${rootHex(root)}`;
    const known = this.sizes.get(key);
    if (known !== undefined) return known;
    let size: MidgardCekTypedSize;
    if (type.kind === "string") {
      size =
        node.kind === "bytes"
          ? {
              memory: midgardCekStringMemoryWords(node.bytesLength),
              exact: isUtf8(this.blob(node.bytesRoot)),
            }
          : REFUSED;
    } else if (type.kind === "list") {
      size =
        node.kind === "list"
          ? this.sizedItems(type.element, node.itemsRoot, node.itemsCount)
          : REFUSED;
    } else if (
      type.kind === "pair" &&
      node.kind === "constrSmall" &&
      node.constructor === 0n &&
      node.fieldsCount === 2n
    ) {
      const first = this.link(node.fieldsRoot);
      const second = this.link(first.tail);
      size = addSizes(
        this.sized(type.first, first.head),
        this.sized(type.second, second.head),
      );
    } else {
      size = REFUSED;
    }
    this.sizes.set(key, size);
    return size;
  }

  private sizedItems(
    element: MidgardCekConstantType,
    root: Bytes,
    count: bigint,
  ): MidgardCekTypedSize {
    if (count === 0n) return { memory: 0n, exact: true };
    if (element.kind === "data") {
      return { memory: this.link(root).memory, exact: true };
    }
    const prefix = `${midgardCekConstantTypeKey(element)}|L|`;
    const pending: MidgardCekDataListNode[] = [];
    let cursor = root;
    let remaining = count;
    let size: MidgardCekTypedSize = { memory: 0n, exact: true };
    while (remaining > 0n) {
      const known = this.sizes.get(prefix + rootHex(cursor));
      if (known !== undefined) {
        size = known;
        break;
      }
      const link = this.link(cursor);
      pending.push(link);
      cursor = link.tail;
      remaining -= 1n;
    }
    for (let index = pending.length - 1; index >= 0; index -= 1) {
      const link = pending[index]!;
      size = addSizes(this.sized(element, link.head), size);
      this.sizes.set(
        prefix + rootHex(index === 0 ? root : pending[index - 1]!.tail),
        size,
      );
    }
    return size;
  }
}
