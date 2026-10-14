import {
  commitMidgardCekBlob,
  decodeMidgardCekProgramBlobPreimage,
  type Hash32,
  hashMidgardCekBlobChunk,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekDecodedProgramBlob,
  type MidgardCekValueNode,
} from "@al-ft/midgard-core";
import { DataI } from "@harmoniclabs/plutus-data";

import {
  type MidgardCekConstantValueWitness,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import {
  commitMidgardCekDataTree,
  type MidgardCekDataLinkSummary,
  midgardCekDataSequenceSummary,
} from "./cek-data-tree.js";
import {
  type Bytes,
  rootHex,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import { materializeMidgardCekDataBytes } from "./cek-executor.semantic-bytes.js";
import {
  midgardCekConstantTypeKey,
  MidgardCekSemanticSizing,
} from "./cek-executor.semantic-sizing.js";
import {
  readMidgardCekSemanticBlob,
  readMidgardCekSemanticData,
} from "./cek-executor.structural-executor-data.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

/** What a constant value is, independent of how its witness reveals it. */
export type MidgardCekConstantInfo = {
  readonly type: MidgardCekConstantType;
  readonly dataRoot: Bytes;
};

const EMPTY_ITEMS: MidgardCekDataLinkSummary = Object.freeze({
  root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
});

const EMPTY_ENTRIES: MidgardCekDataLinkSummary = Object.freeze({
  root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
});

/**
 * The executor-lifetime store of semantic Data material and of what every
 * constant value root is. Each Data node, list link and pair link is keyed by
 * its root; a constant's type, payload root and exact bytes are recorded
 * once, when the constant is first read or created, and every later step
 * reads them by root. Structural builtins therefore build their results from
 * their arguments' roots and pay only for the nodes they create.
 */
export class MidgardCekSemanticStore {
  public readonly dataNodes = new Map<string, MidgardCekDataNode>();
  public readonly dataLists = new Map<string, MidgardCekDataListNode>();
  public readonly dataPairs = new Map<string, MidgardCekDataPairNode>();
  public readonly blobs = new Map<string, MidgardCekDecodedProgramBlob>();
  private readonly payloadBytes = new Map<string, Buffer>();
  private readonly constants = new Map<string, MidgardCekConstantInfo>();
  private readonly witnessRoots = new WeakMap<object, Bytes>();
  private readonly types = new Map<string, MidgardCekConstantType>();
  private readonly typeCbors = new Map<
    string,
    { readonly cbor: Buffer; readonly root: Hash32 }
  >();
  private readonly sizing = new MidgardCekSemanticSizing(this);
  private readonly integers = new Map<string, bigint>();

  constructor(
    private readonly addValue: (node: MidgardCekValueNode) => Hash32,
    private readonly setConstant: (
      key: string,
      witness: MidgardCekConstantValueWitness,
    ) => void,
  ) {}

  public node(root: Bytes): MidgardCekDataNode {
    const node = this.dataNodes.get(rootHex(root));
    if (node === undefined) {
      throw new Error(`missing authenticated CEK Data node ${rootHex(root)}`);
    }
    return node;
  }

  public link(root: Bytes): MidgardCekDataListNode {
    const link = this.dataLists.get(rootHex(root));
    if (link === undefined) {
      throw new Error(
        `missing authenticated CEK Data-list node ${rootHex(root)}`,
      );
    }
    return link;
  }

  public pairLink(root: Bytes): MidgardCekDataPairNode {
    const link = this.dataPairs.get(rootHex(root));
    if (link === undefined) {
      throw new Error(
        `missing authenticated CEK Data-pair node ${rootHex(root)}`,
      );
    }
    return link;
  }

  public blob(root: Bytes): Buffer {
    return readMidgardCekSemanticBlob(this.blobs, root);
  }

  /** The summary of the `count`-item list sequence starting at `root`. */
  public items(root: Bytes, count: bigint): MidgardCekDataLinkSummary {
    return count === 0n
      ? EMPTY_ITEMS
      : midgardCekDataSequenceSummary(root, this.link(root));
  }

  /** The summary of the `count`-entry map sequence starting at `root`. */
  public entries(root: Bytes, count: bigint): MidgardCekDataLinkSummary {
    return count === 0n
      ? EMPTY_ENTRIES
      : midgardCekDataSequenceSummary(root, this.pairLink(root));
  }

  /** The constructor index of a constructor node. */
  public constructorOf(node: MidgardCekDataNode): bigint {
    if (node.kind === "constrSmall") return node.constructor;
    if (node.kind !== "constrLarge") {
      throw new Error("CEK Data node is not a constructor");
    }
    const decoded = plutusDataFromCborIterative(
      this.blob(node.constructorCborRoot),
    );
    if (!(decoded instanceof DataI)) {
      throw new Error("CEK large Data constructor is not an integer");
    }
    return decoded.int;
  }

  public installTree(tree: ReturnType<typeof commitMidgardCekDataTree>): void {
    for (const [key, entry] of tree.dataNodes) {
      this.dataNodes.set(key, entry.node);
    }
    for (const [key, entry] of tree.listNodes) {
      this.dataLists.set(key, entry.node);
    }
    for (const [key, entry] of tree.pairNodes) {
      this.dataPairs.set(key, entry.node);
    }
    for (const [key, entry] of tree.blobNodes) {
      this.blobs.set(
        key,
        decodeMidgardCekProgramBlobPreimage(
          entry.kind === "chunk" ? "blobChunk" : "blobBranch",
          entry.preimage,
        ),
      );
    }
  }

  public readonly addBlob = (bytes: Uint8Array): Uint8Array => {
    const committed = commitMidgardCekBlob(bytes);
    for (const [key, node] of committed.nodes) {
      this.blobs.set(
        key,
        decodeMidgardCekProgramBlobPreimage(
          node.kind === "chunk" ? "blobChunk" : "blobBranch",
          node.preimage,
        ),
      );
    }
    return committed.root;
  };

  public addDataNode(node: MidgardCekDataNode): Hash32 {
    const root = hashMidgardCekDataNode(node);
    this.dataNodes.set(rootHex(root), node);
    return root;
  }

  public addListLink(link: MidgardCekDataListNode): Hash32 {
    const root = hashMidgardCekDataListNode(link);
    this.dataLists.set(rootHex(root), link);
    return root;
  }

  public addPairLink(link: MidgardCekDataPairNode): Hash32 {
    const root = hashMidgardCekDataPairNode(link);
    this.dataPairs.set(rootHex(root), link);
    return root;
  }

  /** Records the value root an authenticated witness object commits to. */
  public registerWitness(witness: object, root: Bytes): void {
    this.witnessRoots.set(witness, root);
  }

  /** The value root of a witness object this store has authenticated. */
  public witnessRoot(witness: object): Bytes | undefined {
    return this.witnessRoots.get(witness);
  }

  /** The decoded type a canonical type CBOR names. */
  public typeOf(typeCbor: Uint8Array): MidgardCekConstantType {
    const key = rootHex(typeCbor);
    let type = this.types.get(key);
    if (type === undefined) {
      type = decodeMidgardCekConstantTypeCbor(typeCbor);
      this.types.set(key, type);
    }
    return type;
  }

  private typeCbor(type: MidgardCekConstantType): {
    readonly cbor: Buffer;
    readonly root: Hash32;
  } {
    const key = midgardCekConstantTypeKey(type);
    let entry = this.typeCbors.get(key);
    if (entry === undefined) {
      const cbor = encodeMidgardCekConstantTypeCbor(type);
      entry = { cbor, root: hashMidgardCekBlobChunk(cbor) };
      this.typeCbors.set(key, entry);
    }
    return entry;
  }

  /**
   * What the constant at value root `root` is. A constant first read here is
   * decoded and committed once: a direct witness from its payload bytes, a
   * semantic one from its authenticated Data material.
   */
  public constant(
    root: Bytes,
    value: MidgardCekDirectValueWitness,
  ): MidgardCekConstantInfo {
    const key = rootHex(root);
    const known = this.constants.get(key);
    if (known !== undefined) return known;
    let info: MidgardCekConstantInfo;
    if (value.kind === "constant") {
      const decoded = decodeMidgardCekConstantWitness(value.witness);
      const tree = commitMidgardCekDataTree(decoded.payload);
      this.installTree(tree);
      info = { type: decoded.type, dataRoot: tree.root };
      this.payloadBytes.set(
        rootHex(tree.root),
        Buffer.from(value.witness.payloadCbor),
      );
      this.sizing.seed(decoded.type, tree.root, {
        memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
        exact: true,
      });
    } else if (value.kind === "semanticConstant") {
      // Reading the tree checks every node under the root once; the nodes
      // it reads are already installed under their own roots.
      readMidgardCekSemanticData(this, value.witness.payload.root);
      info = {
        type: this.typeOf(value.witness.typeCbor),
        dataRoot: Buffer.from(value.witness.payload.root),
      };
    } else {
      throw new Error("CEK semantic builtin requires a constant");
    }
    this.constants.set(key, info);
    return info;
  }

  /** Exact Data CBOR of an installed node. */
  public bytes(root: Bytes): Buffer {
    const key = rootHex(root);
    const known = this.payloadBytes.get(key);
    if (known !== undefined) return known;
    const bytes = materializeMidgardCekDataBytes(
      {
        node: (child) => this.node(child),
        list: (child) => this.link(child),
        pair: (child) => this.pairLink(child),
        blob: (child) => this.blob(child),
        known: (child) => this.payloadBytes.get(child),
      },
      root,
    );
    this.payloadBytes.set(key, bytes);
    return bytes;
  }

  /** Exact bytes of a node, when they are already known. */
  public cachedBytes(root: Bytes): Buffer | undefined {
    return this.payloadBytes.get(rootHex(root));
  }

  /** The integer an integer Data node holds. */
  public integer(root: Bytes): bigint {
    const key = rootHex(root);
    const known = this.integers.get(key);
    if (known !== undefined) return known;
    const node = this.node(root);
    if (node.kind !== "integer") {
      throw new Error("CEK Data node is not an integer");
    }
    const decoded = plutusDataFromCborIterative(this.blob(node.cborRoot));
    if (!(decoded instanceof DataI)) {
      throw new Error("CEK integer Data leaf has a non-integer payload");
    }
    this.integers.set(key, decoded.int);
    return decoded.int;
  }

  /**
   * Adds the constant of `type` whose payload is the installed node at
   * `dataRoot`. Its witness reveals the payload bytes directly when they fit
   * one direct payload, and otherwise commits to the semantic tree. `bytes`
   * supplies the exact payload bytes from the arguments when they are known.
   */
  public addConstant(
    type: MidgardCekConstantType,
    dataRoot: Bytes,
    bytes?: () => Buffer | undefined,
  ): {
    readonly result: MidgardCekDirectValueWitness;
    readonly root: Bytes;
  } {
    const node = this.node(dataRoot);
    const typeCbor = this.typeCbor(type);
    let payloadCbor: Buffer | undefined;
    if (
      node.cborLength <= BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
    ) {
      payloadCbor = bytes?.() ?? this.bytes(dataRoot);
      if (payloadCbor.length > MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES) {
        payloadCbor = undefined;
      }
    }
    const size = this.sizing.sized(type, dataRoot);
    if (payloadCbor !== undefined && !size.exact) {
      throw new Error("V1 constant payload does not match its type");
    }
    if (size.memory === null) {
      throw new Error("V1 constant payload cannot be sized under its type");
    }
    const result: MidgardCekDirectValueWitness =
      payloadCbor === undefined
        ? {
            kind: "semanticConstant",
            witness: {
              typeCbor: Buffer.from(typeCbor.cbor),
              payload: {
                root: dataRoot,
                cborLength: node.cborLength,
                memory: node.memory,
              },
              memory: size.memory,
            },
          }
        : {
            kind: "constant",
            witness: {
              typeCbor: Buffer.from(typeCbor.cbor),
              payloadCbor,
            },
          };
    const root = this.addValue({
      kind: "constant",
      typeRoot: typeCbor.root,
      payloadRoot: dataRoot,
      payloadLength: node.cborLength,
      semanticRoot: dataRoot,
      memory: size.memory,
    });
    const key = rootHex(root);
    if (payloadCbor !== undefined) {
      this.payloadBytes.set(rootHex(dataRoot), payloadCbor);
    }
    this.constants.set(key, { type, dataRoot });
    this.registerWitness(result.witness, root);
    this.setConstant(key, result);
    return { result, root };
  }
}
