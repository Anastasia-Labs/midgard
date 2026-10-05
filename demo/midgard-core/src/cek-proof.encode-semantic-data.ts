import { Data as LucidData } from "@lucid-evolution/lucid";

import {
  encodeSemanticBytes,
  isSemanticConstr,
  isSemanticList,
  isSemanticMap,
  semanticCborHeader,
  type SemanticDataValue,
  semanticMapChildren,
} from "./cek-proof.program-material-task.js";

export type SemanticConstrValue = Extract<
  SemanticDataValue,
  { readonly kind: "constr" }
>;

/** The tag (and, for tag 102, the pair head and index) before the fields. */
export const semanticConstrHeader = (value: SemanticConstrValue): Buffer => {
  if (value.constructor <= 6n) {
    return semanticCborHeader(6, 121n + value.constructor);
  }
  if (value.constructor <= 127n) {
    return semanticCborHeader(6, 1280n + value.constructor - 7n);
  }
  return Buffer.concat([
    semanticCborHeader(6, 102n),
    Buffer.from([0x82]),
    Buffer.from(LucidData.to(value.constructor), "hex"),
  ]);
};

type EncodeFrame = {
  readonly owner: object;
  readonly children: readonly SemanticDataValue[];
  index: number;
  /** Whether the frame is an indefinite list that must be closed. */
  readonly indefinite: boolean;
  /** For constructor fields: the constructor and its reserved header slot. */
  readonly constr: SemanticConstrValue | undefined;
  readonly headerSlot: number;
};

const INDEFINITE_LIST = Buffer.from([0x9f]);
const BREAK = Buffer.from([0xff]);
const EMPTY_LIST = Buffer.from([0x80]);
const NO_BYTES = Buffer.alloc(0);

const UNKNOWN_DATA_MESSAGE = "CEK constant contains unknown semantic Data";

/**
 * One walk for both the encoder and the encodability check. With `chunks` it
 * appends the canonical CBOR; without, it only runs the checks, and a subtree
 * reached twice (a shared value) is checked once.
 *
 * The walk uses an explicit stack and checks the tree in the recursive
 * encoder's order: items and map entries left to right, and a constructor's
 * fields before its header. A value that contains itself is refused.
 */
const walkSemanticData = (
  value: SemanticDataValue,
  chunks: Buffer[] | undefined,
): void => {
  const checked = new Set<object>();
  const open = new Set<object>();
  const stack: EncodeFrame[] = [];
  const push = (chunk: Buffer): void => {
    chunks?.push(chunk);
  };
  const openFrame = (
    owner: object,
    items: readonly SemanticDataValue[],
    indefinite: boolean,
    constr: SemanticConstrValue | undefined,
    headerSlot: number,
  ): void => {
    if (open.has(owner)) {
      throw new Error("CEK semantic Data contains a cycle");
    }
    open.add(owner);
    stack.push({
      owner,
      children: items,
      index: 0,
      indefinite,
      constr,
      headerSlot,
    });
  };
  const openList = (
    owner: object,
    items: readonly SemanticDataValue[],
    constr: SemanticConstrValue | undefined,
    headerSlot: number,
  ): void => {
    push(items.length === 0 ? EMPTY_LIST : INDEFINITE_LIST);
    openFrame(owner, items, items.length > 0, constr, headerSlot);
  };
  // `hasPending` rather than `pending !== undefined`: an `undefined` item (or a
  // hole) is a value to refuse, not the absence of one.
  let pending: SemanticDataValue = value;
  let hasPending = true;
  for (;;) {
    if (hasPending) {
      const current: SemanticDataValue = pending;
      hasPending = false;
      if (typeof current === "bigint") {
        if (chunks !== undefined) {
          chunks.push(Buffer.from(LucidData.to(current), "hex"));
        }
      } else if (typeof current === "string") {
        if (chunks !== undefined) {
          chunks.push(encodeSemanticBytes(Buffer.from(current, "hex")));
        }
      } else if (chunks === undefined && checked.has(current)) {
        // Already checked through another parent.
      } else if (isSemanticList(current)) {
        openList(current, current, undefined, -1);
      } else if (isSemanticMap(current)) {
        const children = semanticMapChildren(current);
        push(semanticCborHeader(5, BigInt(children.length / 2)));
        openFrame(current, children, false, undefined, -1);
      } else if (isSemanticConstr(current)) {
        const headerSlot = chunks?.length ?? -1;
        push(NO_BYTES);
        openList(current, current.fields, current, headerSlot);
      } else {
        throw new Error(UNKNOWN_DATA_MESSAGE);
      }
    }
    const top = stack[stack.length - 1];
    if (top === undefined) {
      return;
    }
    if (top.index < top.children.length) {
      pending = top.children[top.index]!;
      hasPending = true;
      top.index += 1;
      continue;
    }
    stack.pop();
    open.delete(top.owner);
    checked.add(top.owner);
    if (top.indefinite) {
      push(BREAK);
    }
    if (top.constr !== undefined) {
      const header = semanticConstrHeader(top.constr);
      if (chunks !== undefined) {
        chunks[top.headerSlot] = header;
      }
    }
  }
};

/**
 * The canonical CBOR of a semantic Data value: integers as Lucid writes them,
 * bytes chunked at 64, lists indefinite (empty as `80`), maps definite with
 * every entry in order (duplicate keys included), and constructors in compact
 * or tag-102 form.
 */
export const encodeSemanticData = (value: SemanticDataValue): Buffer => {
  const chunks: Buffer[] = [];
  walkSemanticData(value, chunks);
  return Buffer.concat(chunks);
};

/**
 * Throws exactly what {@link encodeSemanticData} would throw on `value`,
 * without building the bytes and in time linear in the distinct subtrees.
 */
export const assertSemanticDataEncodable = (value: SemanticDataValue): void => {
  walkSemanticData(value, undefined);
};
