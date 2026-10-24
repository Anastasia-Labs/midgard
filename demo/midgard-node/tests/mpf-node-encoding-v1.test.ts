import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import type * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "../src/mpf/index.js";
import { encodeStoredNode } from "../src/services/mpf-native-owner/service.encode-stored-node.js";
import { parsePromotionRecords } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { computeLeafHash } from "../src/workers/utils/mpf/phas.key-value-phas-non-membership-proof.js";

/**
 * The `midgard-node` half of the `mpf-node-encoding-v1` channel: the PHAS leaf
 * hash and proof walk, and the native owner's stored-node hash, recomputed
 * against values the MPF library produced
 * (`demo/midgard-core/scripts/generate-mpf-node-encoding-v1-goldens.mjs`).
 */

type ProofStepJson =
  | {
      readonly type: "branch";
      readonly skip: number;
      readonly neighbors: string;
    }
  | {
      readonly type: "fork";
      readonly skip: number;
      readonly neighbor: {
        readonly nibble: number;
        readonly prefix: string;
        readonly root: string;
      };
    }
  | {
      readonly type: "leaf";
      readonly skip: number;
      readonly neighbor: { readonly key: string; readonly value: string };
    };

type Golden = {
  readonly suffixes: {
    readonly key: string;
    readonly value: string;
    readonly valueDigest: string;
    readonly rows: readonly {
      readonly cursor: number;
      readonly prefix: string;
      readonly leafHash: string;
    }[];
  };
  readonly trie: {
    readonly root: string;
    readonly membership: readonly {
      readonly key: string;
      readonly value: string;
      readonly proof: readonly ProofStepJson[];
    }[];
    readonly exclusion: readonly {
      readonly key: string;
      readonly root: string;
      readonly proof: readonly ProofStepJson[];
    }[];
  };
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "../../midgard-core/tests/fixtures/mpf-node-encoding-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as Golden;

const sdkProof = (steps: readonly ProofStepJson[]): SDK.Proof =>
  steps.map((step): SDK.ProofStep => {
    switch (step.type) {
      case "branch":
        return {
          Branch: { skip: BigInt(step.skip), neighbors: step.neighbors },
        };
      case "fork":
        return {
          Fork: {
            skip: BigInt(step.skip),
            neighbor: {
              nibble: BigInt(step.neighbor.nibble),
              prefix: step.neighbor.prefix,
              root: step.neighbor.root,
            },
          },
        };
      case "leaf":
        return {
          Leaf: {
            skip: BigInt(step.skip),
            key: step.neighbor.key,
            value: step.neighbor.value,
          },
        };
    }
  });

describe("MPF node encoding V1 goldens (midgard-node)", () => {
  it("hashes a PHAS leaf as the library does at every cursor", () => {
    const valueDigest = Buffer.from(golden.suffixes.valueDigest, "hex");
    expect(golden.suffixes.rows).toHaveLength(65);
    for (const { prefix, leafHash } of golden.suffixes.rows) {
      expect(computeLeafHash(prefix, valueDigest).toString("hex")).toBe(
        leafHash,
      );
    }
  });

  it("hashes a stored native-owner leaf as the library does at every cursor", () => {
    for (const { prefix, leafHash } of golden.suffixes.rows) {
      const records = parsePromotionRecords(
        encodeStoredNode(leafHash, {
          __kind: "Leaf",
          prefix,
          key: golden.suffixes.key,
          value: golden.suffixes.value,
        }),
      );
      expect(records.map(({ hashHex }) => hashHex)).toEqual([leafHash]);
    }
  });

  it.effect("walks every PHAS proof to the library roots", () =>
    Effect.gen(function* () {
      for (const { key, value, proof } of golden.trie.membership) {
        yield* verifyKeyValuePhasMembershipProof({
          root: golden.trie.root,
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
          proof: sdkProof(proof),
        });
      }
      for (const { key, root, proof } of golden.trie.exclusion) {
        yield* verifyKeyValuePhasNonMembershipProof({
          root,
          key: Buffer.from(key, "hex"),
          proof: sdkProof(proof),
        });
      }
    }),
  );
});
