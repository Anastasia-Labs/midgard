import { describe, expect, it } from "vitest";

import { commitSemanticData } from "../src/cek-proof.commit-semantic-data.js";
import { encodeSemanticData } from "../src/cek-proof.encode-semantic-data.js";
import {
  commitMidgardCekBlob,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  MidgardCekProgramMaterialMissingRootError,
  verifyMidgardCekProgramMaterial,
} from "../src/cek-proof.js";
import { isSemanticMap } from "../src/cek-proof.program-material-task.js";
import { makeSemanticDataReconstructor } from "../src/cek-proof.reconstruct-semantic-data.js";
import {
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  encodeMidgardCekDataPairNode,
} from "../src/cek-semantic.js";
import { type Hash32 } from "../src/codec/hash.js";
import {
  addRawData,
  emptySemanticMaterial,
  hexRoot,
  materialSource,
  type RawData,
  type SemanticMaterial,
} from "./cek-proof-semantic.material.js";

/**
 * A Plutus Data map is a list of pairs: Cardano keeps every entry, in order,
 * duplicate keys included, and so does the on-chain commitment. CEK constant
 * material with such a map must rebuild to the same entries and commit to the
 * same root, and material that drops an entry must be refused.
 */

/** `Map [(I 1, I 2), (I 1, I 3)]`, the vector shared with the Aiken twin. */
const DUPLICATE_KEY_MAP: RawData = {
  kind: "map",
  entries: [
    [1n, 2n],
    [1n, 3n],
  ],
};
/** The map above with its second entry dropped. */
const DEDUPLICATED_MAP: RawData = { kind: "map", entries: [[1n, 2n]] };

/** Pinned in `onchain/aiken/lib/midgard/cek-data-v1.test.ak`. */
const DUPLICATE_KEY_MAP_ROOT =
  "aef1afbc4af14bed532f136ec0e9db7d1f297752cbe935c6af79d0f49b0817e5";
const DUPLICATE_KEY_MAP_CBOR = "a201020103";

const rebuild = (raw: RawData) => {
  const material = emptySemanticMaterial();
  const summary = addRawData(material, raw);
  const value = makeSemanticDataReconstructor(materialSource(material))(
    summary.root,
  );
  return { summary, value };
};

/** Program material for the one-constant program `(con data <raw>)`. */
const dataConstantProgram = (
  raw: RawData,
  declare: { readonly payloadLength?: bigint } = {},
): {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly entries: MidgardCekProgramMaterialEntry[];
  readonly semantic: SemanticMaterial;
  readonly dataRoot: Hash32;
} => {
  const semantic = emptySemanticMaterial();
  const data = addRawData(semantic, raw);
  const entries: MidgardCekProgramMaterialEntry[] = [];
  const addBlob = (bytes: Buffer): Hash32 => {
    const blob = commitMidgardCekBlob(bytes);
    for (const [rootHex, node] of blob.nodes.entries()) {
      entries.push({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      });
    }
    return blob.root;
  };
  for (const bytes of semantic.blobs.values()) addBlob(bytes);
  for (const [rootHex, node] of semantic.dataNodes) {
    entries.push({
      kind: "dataNode",
      root: Buffer.from(rootHex, "hex") as Hash32,
      preimage: encodeMidgardCekDataNode(node),
    });
  }
  for (const [rootHex, node] of semantic.dataLists) {
    entries.push({
      kind: "dataList",
      root: Buffer.from(rootHex, "hex") as Hash32,
      preimage: encodeMidgardCekDataListNode(node),
    });
  }
  for (const [rootHex, node] of semantic.dataPairs) {
    entries.push({
      kind: "dataPair",
      root: Buffer.from(rootHex, "hex") as Hash32,
      preimage: encodeMidgardCekDataPairNode(node),
    });
  }
  const valueNode = {
    kind: "constant",
    typeRoot: addBlob(Buffer.from("9f08ff", "hex")),
    payloadRoot: data.root,
    payloadLength: declare.payloadLength ?? data.cborLength,
    semanticRoot: data.root,
    memory: data.memory,
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  entries.push({
    kind: "value",
    root: valueRoot,
    preimage: encodeMidgardCekValueNode(valueNode),
  });
  const termNode = { kind: "constant", value: valueRoot } as const;
  const termRoot = hashMidgardCekTermNode(termNode);
  entries.push({
    kind: "term",
    root: termRoot,
    preimage: encodeMidgardCekTermNode(termNode),
  });
  return {
    envelope: {
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: BigInt(entries.length),
      materialByteLength: entries.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    },
    entries,
    semantic,
    dataRoot: data.root,
  };
};

describe("semantic Data maps with duplicate keys", () => {
  it("rebuilds every entry in order and commits to the material root", () => {
    const { summary, value } = rebuild(DUPLICATE_KEY_MAP);
    expect(value).toEqual({
      kind: "map",
      entries: [
        [1n, 2n],
        [1n, 3n],
      ],
    });
    expect(encodeSemanticData(value).toString("hex")).toBe(
      DUPLICATE_KEY_MAP_CBOR,
    );
    const committed = commitSemanticData(value);
    expect(hexRoot(committed.root)).toBe(hexRoot(summary.root));
    expect(hexRoot(committed.root)).toBe(DUPLICATE_KEY_MAP_ROOT);
    expect(committed.cborLength).toBe(5n);
    expect(committed.memory).toBe(24n);
  });

  it("keeps non-adjacent duplicates of a shared structured key", () => {
    const raw: RawData = {
      kind: "map",
      entries: [
        [[1n], 2n],
        [3n, 4n],
        [[1n], 5n],
      ],
    };
    const { summary, value } = rebuild(raw);
    if (!isSemanticMap(value)) throw new Error("expected a map");
    const entries = value.entries;
    expect(entries).toHaveLength(3);
    // Both keys are the one value rebuilt for their shared root.
    expect(entries[0]![0]).toBe(entries[2]![0]);
    expect(encodeSemanticData(value).toString("hex")).toBe(
      "a39f01ff0203049f01ff05",
    );
    expect(hexRoot(commitSemanticData(value).root)).toBe(hexRoot(summary.root));
  });

  it("commits a duplicate-key map apart from its deduplicated form", () => {
    expect(hexRoot(rebuild(DEDUPLICATED_MAP).summary.root)).not.toBe(
      DUPLICATE_KEY_MAP_ROOT,
    );
  });
});

describe("CEK program material with a duplicate-key Data constant", () => {
  it("accepts the honest material and returns the exact payload", () => {
    const program = dataConstantProgram(DUPLICATE_KEY_MAP);
    const verified = verifyMidgardCekProgramMaterial(
      program.envelope,
      program.entries,
    );
    expect(verified.constants).toHaveLength(1);
    const [constant] = verified.constants;
    expect(hexRoot(constant!.semanticRoot)).toBe(DUPLICATE_KEY_MAP_ROOT);
    expect(constant!.payloadCbor.toString("hex")).toBe(DUPLICATE_KEY_MAP_CBOR);
  });

  it("refuses deduplicated material under the honest roots at the preimage hash", () => {
    const honest = dataConstantProgram(DUPLICATE_KEY_MAP);
    const deduplicated = dataConstantProgram(DEDUPLICATED_MAP);
    const dedupMapNode = deduplicated.entries.find(
      (entry) =>
        entry.kind === "dataNode" &&
        hexRoot(entry.root) === hexRoot(deduplicated.dataRoot),
    )!;
    const tampered = honest.entries.map((entry) =>
      hexRoot(entry.root) === hexRoot(honest.dataRoot)
        ? { ...entry, preimage: dedupMapNode.preimage }
        : entry,
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(honest.envelope, tampered),
    ).toThrow("CEK program material root does not match its preimage");
  });

  it("refuses a value node that declares the deduplicated payload length", () => {
    const deduplicatedLength = rebuild(DEDUPLICATED_MAP).summary.cborLength;
    const program = dataConstantProgram(DUPLICATE_KEY_MAP, {
      payloadLength: deduplicatedLength,
    });
    expect(() =>
      verifyMidgardCekProgramMaterial(program.envelope, program.entries),
    ).toThrow(/payload length does not match its semantic tree/u);
  });

  it("refuses a re-rooted deduplicated program against the honest envelope", () => {
    const honest = dataConstantProgram(DUPLICATE_KEY_MAP);
    const deduplicated = dataConstantProgram(DEDUPLICATED_MAP);
    expect(hexRoot(deduplicated.envelope.termRoot)).not.toBe(
      hexRoot(honest.envelope.termRoot),
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(honest.envelope, deduplicated.entries),
    ).toThrow(MidgardCekProgramMaterialMissingRootError);
  });
});
