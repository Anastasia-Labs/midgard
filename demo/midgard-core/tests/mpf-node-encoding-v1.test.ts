import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  mpfSuffix,
  reconstructFraudProofCatalogueMpfNode,
} from "../src/deployment-manifest-identity/catalogue-proof.js";
import { buildMidgardMpfProofFoldTrace } from "../src/mpf-proof-fold.build-midgard-mpf-proof-fold-trace.js";
import {
  parseMidgardMpfProofJson,
  suffix,
} from "../src/mpf-proof-fold.parse-midgard-mpf-proof-json.js";

/**
 * The TypeScript half of the `mpf-node-encoding-v1` channel for this package:
 * the proof fold's `suffix` and fold, and the fraud-proof catalogue's MPF
 * reconstruction, recomputed against values the MPF library produced
 * (`scripts/generate-mpf-node-encoding-v1-goldens.mjs`).
 */

type ProofJson = readonly unknown[];

type Golden = {
  readonly oddLeafMarker: string;
  readonly evenLeafMarker: string;
  readonly suffixes: {
    readonly path: string;
    readonly valueDigest: string;
    readonly rows: readonly {
      readonly cursor: number;
      readonly suffix: string;
      readonly leafHash: string;
    }[];
  };
  readonly trie: {
    readonly root: string;
    readonly membership: readonly {
      readonly index: number;
      readonly key: string;
      readonly value: string;
      readonly proof: ProofJson;
    }[];
    readonly exclusion: readonly {
      readonly index: number;
      readonly key: string;
      readonly root: string;
      readonly proof: ProofJson;
    }[];
  };
  readonly insertDelete: {
    readonly key: string;
    readonly value: string;
    readonly rootWithout: string;
    readonly rootWith: string;
    readonly insertProof: ProofJson;
    readonly deleteProof: ProofJson;
  };
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/mpf-node-encoding-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as Golden;

const bytes = (value: string): Buffer => Buffer.from(value, "hex");
const hash = (value: Uint8Array): Buffer =>
  Buffer.from(blake2b(value, { dkLen: 32 }));
const fold = (key: string, value: string, proof: ProofJson) =>
  buildMidgardMpfProofFoldTrace({
    key: bytes(key),
    value: bytes(value),
    steps: parseMidgardMpfProofJson(proof),
  });

describe("MPF node encoding V1 goldens (midgard-core)", () => {
  it("pins the leaf markers", () => {
    expect(golden.evenLeafMarker).toBe("ff");
    expect(golden.oddLeafMarker).toBe("10");
    expect(golden.suffixes.rows).toHaveLength(65);
  });

  it("spells every suffix as the library's leaf commits it", () => {
    const path = bytes(golden.suffixes.path);
    const valueDigest = bytes(golden.suffixes.valueDigest);
    for (const row of golden.suffixes.rows) {
      for (const encoder of [suffix, mpfSuffix]) {
        const actual = encoder(path, row.cursor);
        expect(actual.toString("hex")).toBe(row.suffix);
        expect(hash(Buffer.concat([actual, valueDigest])).toString("hex")).toBe(
          row.leafHash,
        );
      }
    }
  });

  it("folds every membership and exclusion proof to the library roots", () => {
    for (const { key, value, proof } of golden.trie.membership) {
      expect(
        fold(key, value, proof).terminal.includingRoot.toString("hex"),
      ).toBe(golden.trie.root);
    }
    for (const { key, root, proof } of golden.trie.exclusion) {
      expect(
        fold(key, "00", proof).terminal.excludingRoot.toString("hex"),
      ).toBe(root);
    }
  });

  it("folds the insert/delete pair to the library roots", () => {
    const { key, value, rootWithout, rootWith, insertProof, deleteProof } =
      golden.insertDelete;
    const inserted = fold(key, value, insertProof).terminal;
    expect(inserted.excludingRoot.toString("hex")).toBe(rootWithout);
    expect(inserted.includingRoot.toString("hex")).toBe(rootWith);
    const deleted = buildMidgardMpfProofFoldTrace({
      key: bytes(key),
      value: bytes(value),
      steps: parseMidgardMpfProofJson(deleteProof),
      deletionOpening: Buffer.alloc(0),
    }).terminal;
    expect(deleted.includingRoot.toString("hex")).toBe(rootWith);
    expect(deleted.excludingRoot.toString("hex")).toBe(rootWithout);
  });

  it("reconstructs the library roots from the entries", () => {
    const entries = golden.trie.membership.map(({ index, key, value }) => ({
      index,
      path: hash(bytes(key)),
      valueHash: hash(bytes(value)),
    }));
    expect(
      reconstructFraudProofCatalogueMpfNode(entries, 0).toString("hex"),
    ).toBe(golden.trie.root);
    for (const { index, root } of golden.trie.exclusion) {
      const rest = entries.filter((entry) => entry.index !== index);
      if (rest.length === entries.length) continue;
      expect(
        reconstructFraudProofCatalogueMpfNode(rest, 0).toString("hex"),
      ).toBe(root);
    }
  });
});
