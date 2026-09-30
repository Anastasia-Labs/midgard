import { Data as LucidData } from "@lucid-evolution/lucid";

import { commitMidgardCekBlob } from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import { MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES } from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  assertSemanticDataEncodable,
  encodeSemanticData,
  semanticConstrHeader,
  type SemanticConstrValue,
} from "./cek-proof.encode-semantic-data.js";
import {
  encodeSemanticBytes,
  isSemanticConstr,
  isSemanticList,
  isSemanticMap,
  semanticCborHeader,
  type SemanticConstantType,
  type SemanticDataValue,
  semanticMapChildren,
} from "./cek-proof.program-material-task.js";
import {
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "./cek-semantic.js";
import { type Hash32 } from "./codec/hash.js";

export const decodeSemanticConstantType = (
  typeCbor: Uint8Array,
): SemanticConstantType => {
  if (typeCbor.length > MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES) {
    throw new Error(
      `CEK constant type exceeds the ${MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES.toString()}-byte L1 bound`,
    );
  }
  const decoded = LucidData.from(Buffer.from(typeCbor).toString("hex"));
  const canonical = encodeSemanticData(decoded as SemanticDataValue);
  if (!canonical.equals(Buffer.from(typeCbor))) {
    throw new Error("CEK constant type CBOR is not canonical");
  }
  if (
    !Array.isArray(decoded) ||
    !decoded.every((tag) => typeof tag === "bigint")
  ) {
    throw new Error("CEK constant type payload is not an integer list");
  }
  const stack: SemanticConstantType[] = [];
  for (let offset = decoded.length - 1; offset >= 0; offset -= 1) {
    const tag = decoded[offset];
    if (tag === 0n) stack.push({ kind: "integer" });
    else if (tag === 1n) stack.push({ kind: "bytes" });
    else if (tag === 2n) stack.push({ kind: "string" });
    else if (tag === 3n) stack.push({ kind: "unit" });
    else if (tag === 4n) stack.push({ kind: "boolean" });
    else if (tag === 8n) stack.push({ kind: "data" });
    else if (tag === 9n) stack.push({ kind: "blsG1" });
    else if (tag === 10n) stack.push({ kind: "blsG2" });
    else if (tag === 11n) stack.push({ kind: "blsMillerLoop" });
    else if (tag === 5n) {
      const element = stack.pop();
      if (element === undefined) {
        throw new Error("CEK constant list type is missing its element type");
      }
      stack.push({ kind: "list", element });
    } else if (tag === 6n) {
      const first = stack.pop();
      const second = stack.pop();
      if (first === undefined || second === undefined) {
        throw new Error("CEK constant pair type is missing a child type");
      }
      stack.push({ kind: "pair", first, second });
    } else {
      throw new Error("CEK constant has an unknown semantic type tag");
    }
  }
  if (stack.length !== 1) {
    throw new Error("CEK constant type payload has trailing tags");
  }
  return stack[0]!;
};

export const semanticIntegerMemory = (value: bigint): bigint => {
  const doubled = value < 0n ? (-value - 1n) * 2n : value * 2n;
  if (doubled === 0n) return 1n;
  return BigInt(Math.ceil(doubled.toString(2).length / 8));
};

type SemanticDataSummary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type SemanticListSummary = {
  readonly root: Hash32;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

type CommittedNode = SemanticDataSummary & {
  /** The length of the node's canonical CBOR, from its children's lengths. */
  readonly encodedLength: bigint;
};

type CommitFrame = {
  readonly owner: SemanticDataValue & object;
  /** List items, constructor fields, or map entries flattened key/value. */
  readonly children: readonly SemanticDataValue[];
  readonly kind: "list" | "map" | "constr";
  /** Next child, walking from the last entry to the first. */
  entry: number;
  pendingKey: CommittedNode | undefined;
  summary: SemanticListSummary;
  childrenEncodedLength: bigint;
};

const emptySummary = (root: Hash32): SemanticListSummary => ({
  root,
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
});

const commitLeaf = (value: bigint | string): CommittedNode => {
  let node: MidgardCekDataNode;
  let encodedLength: bigint;
  if (typeof value === "bigint") {
    const canonicalCbor = Buffer.from(LucidData.to(value), "hex");
    encodedLength = BigInt(canonicalCbor.length);
    node = {
      kind: "integer",
      cborRoot: commitMidgardCekBlob(canonicalCbor).root,
      cborLength: BigInt(canonicalCbor.length),
      memory: 4n + semanticIntegerMemory(value),
    };
  } else {
    const bytes = Buffer.from(value, "hex");
    encodedLength = BigInt(encodeSemanticBytes(bytes).length);
    node = {
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(bytes).root,
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
      memory: 4n + BigInt(Math.max(1, bytes.length)),
    };
  }
  return sealNode(node, encodedLength);
};

const sealNode = (
  node: MidgardCekDataNode,
  encodedLength: bigint,
): CommittedNode => {
  if (node.cborLength !== encodedLength) {
    throw new Error("CEK semantic Data CBOR summary is not exact");
  }
  return {
    root: hashMidgardCekDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
    encodedLength,
  };
};

const indefiniteListLength = (count: number, payload: bigint): bigint =>
  count === 0 ? 1n : 2n + payload;

const closeCommitFrame = (frame: CommitFrame): CommittedNode => {
  const summary = frame.summary;
  const payload = frame.childrenEncodedLength;
  if (frame.kind === "list") {
    return sealNode(
      {
        kind: "list",
        itemsCount: summary.length,
        itemsRoot: summary.root,
        cborLength: midgardCekDataListCborLength(
          summary.length,
          summary.payloadCborLength,
        ),
        memory: 4n + summary.memory,
      },
      indefiniteListLength(frame.children.length, payload),
    );
  }
  if (frame.kind === "map") {
    return sealNode(
      {
        kind: "map",
        entriesCount: summary.length,
        entriesRoot: summary.root,
        cborLength: midgardCekDataMapCborLength(
          summary.length,
          summary.payloadCborLength,
        ),
        memory: 4n + summary.memory,
      },
      BigInt(semanticCborHeader(5, BigInt(frame.children.length / 2)).length) +
        payload,
    );
  }
  const constr = frame.owner as SemanticConstrValue;
  const constructor = constr.constructor;
  const encodedLength =
    BigInt(semanticConstrHeader(constr).length) +
    indefiniteListLength(frame.children.length, payload);
  if (constructor <= 127n) {
    return sealNode(
      {
        kind: "constrSmall",
        constructor,
        fieldsCount: summary.length,
        fieldsRoot: summary.root,
        cborLength: midgardCekDataConstrCborLength(
          constructor,
          summary.length,
          summary.payloadCborLength,
        ),
        memory: 4n + summary.memory,
      },
      encodedLength,
    );
  }
  const constructorCbor = Buffer.from(LucidData.to(constructor), "hex");
  return sealNode(
    {
      kind: "constrLarge",
      constructorCborRoot: commitMidgardCekBlob(constructorCbor).root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 4n + semanticIntegerMemory(constructor),
      fieldsCount: summary.length,
      fieldsRoot: summary.root,
      cborLength: midgardCekDataConstrCborLength(
        constructor,
        summary.length,
        summary.payloadCborLength,
      ),
      memory: 4n + summary.memory,
    },
    encodedLength,
  );
};

/** Folds one committed child into its parent's list or pair chain. */
const attachCommitted = (frame: CommitFrame, child: CommittedNode): void => {
  const summary = frame.summary;
  frame.childrenEncodedLength += child.encodedLength;
  if (frame.kind !== "map") {
    const node: MidgardCekDataListNode = {
      head: child.root,
      headCborLength: child.cborLength,
      headMemory: child.memory,
      tail: summary.root,
      length: summary.length + 1n,
      payloadCborLength: child.cborLength + summary.payloadCborLength,
      memory: child.memory + summary.memory,
    };
    frame.summary = {
      root: hashMidgardCekDataListNode(node),
      length: node.length,
      payloadCborLength: node.payloadCborLength,
      memory: node.memory,
    };
    return;
  }
  if (frame.pendingKey === undefined) {
    frame.pendingKey = child;
    return;
  }
  const key = frame.pendingKey;
  frame.pendingKey = undefined;
  const node: MidgardCekDataPairNode = {
    key: key.root,
    keyCborLength: key.cborLength,
    keyMemory: key.memory,
    value: child.root,
    valueCborLength: child.cborLength,
    valueMemory: child.memory,
    tail: summary.root,
    length: summary.length + 1n,
    payloadCborLength:
      key.cborLength + child.cborLength + summary.payloadCborLength,
    memory: key.memory + child.memory + summary.memory,
  };
  frame.summary = {
    root: hashMidgardCekDataPairNode(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

/** The child to commit next: entries from last to first, key before value. */
const nextCommitChild = (frame: CommitFrame): SemanticDataValue | undefined => {
  if (frame.kind === "map") {
    if (frame.pendingKey !== undefined) {
      return frame.children[frame.entry * 2 + 1];
    }
    frame.entry -= 1;
    return frame.entry >= 0 ? frame.children[frame.entry * 2] : undefined;
  }
  frame.entry -= 1;
  return frame.entry >= 0 ? frame.children[frame.entry] : undefined;
};

const openCommitFrame = (value: SemanticDataValue & object): CommitFrame => {
  if (isSemanticList(value)) {
    return {
      owner: value,
      children: value,
      kind: "list",
      entry: value.length,
      pendingKey: undefined,
      summary: emptySummary(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
      childrenEncodedLength: 0n,
    };
  }
  if (isSemanticMap(value)) {
    const children = semanticMapChildren(value);
    return {
      owner: value,
      children,
      kind: "map",
      entry: children.length / 2,
      pendingKey: undefined,
      summary: emptySummary(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
      childrenEncodedLength: 0n,
    };
  }
  if (isSemanticConstr(value)) {
    return {
      owner: value,
      children: value.fields,
      kind: "constr",
      entry: value.fields.length,
      pendingKey: undefined,
      summary: emptySummary(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
      childrenEncodedLength: 0n,
    };
  }
  throw new Error("CEK constant contains unknown Plutus Data");
};

/**
 * The semantic Data commitment of `value`: its root hash, canonical CBOR
 * length and memory.
 *
 * The value is first checked for encodability, so an unencodable value fails
 * exactly as the encoder would. The commitment is then one post-order walk (children last to first, keys
 * before values), with an explicit stack, that checks each node's declared
 * CBOR length against the length summed from its children. A value reached
 * twice (a shared subtree) is committed once.
 */
export const commitSemanticData = (
  value: SemanticDataValue,
): SemanticDataSummary => {
  assertSemanticDataEncodable(value);
  const committed = new Map<object, CommittedNode>();
  const stack: CommitFrame[] = [];
  let pending: SemanticDataValue = value;
  for (;;) {
    let done: CommittedNode | undefined;
    if (typeof pending === "bigint" || typeof pending === "string") {
      done = commitLeaf(pending);
    } else {
      done = committed.get(pending);
      if (done === undefined) {
        stack.push(openCommitFrame(pending));
      }
    }
    // Hand results up until some frame has another child to commit.
    for (;;) {
      const top = stack[stack.length - 1];
      if (done !== undefined) {
        if (top === undefined) {
          return {
            root: done.root,
            cborLength: done.cborLength,
            memory: done.memory,
          };
        }
        attachCommitted(top, done);
        done = undefined;
      }
      const frame = top!;
      const next = nextCommitChild(frame);
      if (next !== undefined) {
        pending = next;
        break;
      }
      stack.pop();
      done = closeCommitFrame(frame);
      committed.set(frame.owner, done);
    }
  }
};
