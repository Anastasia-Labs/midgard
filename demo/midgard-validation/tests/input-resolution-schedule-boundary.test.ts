import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  appendMidgardValidationMerkleLeaf,
  commitMidgardValidationMerkleFrontier,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  emptyMidgardValidationMerkleFrontier,
  hashMidgardResolvedContextItemLeaf,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardResolvedInputsAccumulator,
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  emptyMidgardInputResolutionSchedule,
  initialMidgardResolvedInputsAccumulator,
  prependMidgardInputResolutionSchedule,
} from "../src/index.js";
import { makeOutput } from "./validation-fixtures.js";

type BoundaryCorpus = {
  readonly schema: string;
  readonly entries: readonly {
    readonly label: string;
    readonly canonicalCborHex: string;
  }[];
};

type ResolutionNode = {
  readonly sourceKind: "spend" | "reference";
  readonly itemIndex: number;
  readonly key: Buffer;
  readonly value: Buffer;
  readonly nextScheduleHash: Buffer;
  readonly scheduleHash: Buffer;
};

type ResolutionTerminalVector = {
  readonly label: string;
  readonly spendCount: number;
  readonly referenceCount: number;
  readonly originalScheduleHashHex: string;
  readonly terminalAccumulatorHex: string;
  readonly terminalFrontierCommitmentHex: string;
  readonly penultimate: {
    readonly cursor: number;
    readonly accumulatorHex: string;
    readonly remainingScheduleHashHex: string;
    readonly spendIndex: number;
    readonly resolvedItemPeaks: readonly {
      readonly height: number;
      readonly hashHex: string;
    }[];
  };
  readonly terminal: {
    readonly cursor: number;
    readonly spendIndex: number;
    readonly resolvedItemPeaks: readonly {
      readonly height: number;
      readonly hashHex: string;
    }[];
  };
  readonly lastNode: {
    readonly sourceKind: "spend" | "reference";
    readonly itemIndex: number;
    readonly keyHex: string;
    readonly valueHex: string;
    readonly nextScheduleHashHex: string;
  };
};

const expectedTerminalVectors = {
  "maximum-spend-inputs": {
    label: "maximum-spend-inputs",
    spendCount: 434,
    referenceCount: 0,
    originalScheduleHashHex:
      "37dd5fe0c36256fb12a17ffe1c03c85f0554a83579f4ad5000f9baf2fbdd2877",
    terminalAccumulatorHex:
      "ea27a61b8f396c3354831035b7fe4861195ca8796d1703ae0e89a6229952c98d",
    terminalFrontierCommitmentHex:
      "839634308167c061ce5ebad392b4dba1e9d037d65177dc233a76667a416152a2",
    penultimate: {
      cursor: 433,
      accumulatorHex:
        "c73070dfccee71dfb0bb8775eebcf23fc9967f71620cd0e1fd35a5ccabf29b85",
      remainingScheduleHashHex:
        "05d16c4dddaaec7b5c46fbdc0268b27ef430a0cea60630349adaf9f31fd7ff9b",
      spendIndex: 433,
      resolvedItemPeaks: [
        {
          height: 0,
          hashHex:
            "b9467fa86e247e7a65d15e9c85a9c5f340b3bd2fce982e68c88d15a2348078cc",
        },
        {
          height: 4,
          hashHex:
            "c4a783d21ea9afe3d098a0610f6bf62728f0f58751b282a91ce2de8bd1ad2a5c",
        },
        {
          height: 5,
          hashHex:
            "255f6f71f881c1d692b9f0c2feb325d77fe8b109c195dfc68d68b4c47fbc68bf",
        },
        {
          height: 7,
          hashHex:
            "7249e4ca6ce4b4f45791ead776a41724da58067e9a84c0bf5a4808358ed016d9",
        },
        {
          height: 8,
          hashHex:
            "988dd55e275323ec92dd4eb08ecded7a5812233c927bda85b39598bba5823dea",
        },
      ],
    },
    terminal: {
      cursor: 434,
      spendIndex: 434,
      resolvedItemPeaks: [
        {
          height: 1,
          hashHex:
            "be8cdd3293ea83148f1d803ffa9313f97122421aa729e31026656384474fea6b",
        },
        {
          height: 4,
          hashHex:
            "c4a783d21ea9afe3d098a0610f6bf62728f0f58751b282a91ce2de8bd1ad2a5c",
        },
        {
          height: 5,
          hashHex:
            "255f6f71f881c1d692b9f0c2feb325d77fe8b109c195dfc68d68b4c47fbc68bf",
        },
        {
          height: 7,
          hashHex:
            "7249e4ca6ce4b4f45791ead776a41724da58067e9a84c0bf5a4808358ed016d9",
        },
        {
          height: 8,
          hashHex:
            "988dd55e275323ec92dd4eb08ecded7a5812233c927bda85b39598bba5823dea",
        },
      ],
    },
    lastNode: {
      sourceKind: "spend",
      itemIndex: 433,
      keyHex:
        "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
      valueHex:
        "90011901b11829582075905d5cce7bbf39a2d566bf6f5ee2f9074354777ca4408ebd73e9d6d77cc9fb581d60111111111111111111111111111111111111111111111111111111111a00989680005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358200767446910aa276510642f89978c67b7ab51f1fb1d11b18447591f4a0f826329183c18378358200767446910aa276510642f89978c67b7ab51f1fb1d11b18447591f4a0f826329183c18378358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
      nextScheduleHashHex:
        "96c1acf52ec7e32c975a3c5343d7417257e0eca08c0fda83d77f2172000d0b36",
    },
  },
  "maximum-reference-inputs": {
    label: "maximum-reference-inputs",
    spendCount: 1,
    referenceCount: 433,
    originalScheduleHashHex:
      "ffe1307d29932bb07004f4586071371d7f31bde1d565c0800353e345244e9ce7",
    terminalAccumulatorHex:
      "8c0cb3c900dd4b1526b89b1ed63c46d1621cad53e608f5058de703bb49e5bd46",
    terminalFrontierCommitmentHex:
      "ec0105edd129d2b374936f85aad9f28fe10e32703ddc0f205a301e2b773adf13",
    penultimate: {
      cursor: 433,
      accumulatorHex:
        "f31c120d3d965e078630f7b64155608a0d7550a84583426720e4f98785e04b98",
      remainingScheduleHashHex:
        "9ac79f779bf4a86b8a6c3835a012e644f7ad7ceff000393c453a177137a1b48d",
      spendIndex: 1,
      resolvedItemPeaks: [
        {
          height: 0,
          hashHex:
            "7a92d9d2f5a648f7de6437180a121be3085ecea5baa90548f294313f2f437040",
        },
        {
          height: 4,
          hashHex:
            "d84930fcad5a1be2f0b63c8f653e8fd0f0085c737c66da3c0980bd4c1bcc4bd4",
        },
        {
          height: 5,
          hashHex:
            "9cd20b30015713aefe1507d97408acbf739c87f99fbafe18fa5c27c6c5dfa8e1",
        },
        {
          height: 7,
          hashHex:
            "52d757c7abe509358ebb79d685da19705cb68ac1e539cb208369e5f3b41ab257",
        },
        {
          height: 8,
          hashHex:
            "faa0f448c6c8a9b2a2477e8de9754c47d29673e15ff284490cfb2b1fc5f61f66",
        },
      ],
    },
    terminal: {
      cursor: 434,
      spendIndex: 1,
      resolvedItemPeaks: [
        {
          height: 1,
          hashHex:
            "044e443bc41379b7b62047df92c0d699be61172d191942d44fe51d4f180df200",
        },
        {
          height: 4,
          hashHex:
            "d84930fcad5a1be2f0b63c8f653e8fd0f0085c737c66da3c0980bd4c1bcc4bd4",
        },
        {
          height: 5,
          hashHex:
            "9cd20b30015713aefe1507d97408acbf739c87f99fbafe18fa5c27c6c5dfa8e1",
        },
        {
          height: 7,
          hashHex:
            "52d757c7abe509358ebb79d685da19705cb68ac1e539cb208369e5f3b41ab257",
        },
        {
          height: 8,
          hashHex:
            "faa0f448c6c8a9b2a2477e8de9754c47d29673e15ff284490cfb2b1fc5f61f66",
        },
      ],
    },
    lastNode: {
      sourceKind: "reference",
      itemIndex: 432,
      keyHex:
        "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
      valueHex:
        "90011901b11829582075905d5cce7bbf39a2d566bf6f5ee2f9074354777ca4408ebd73e9d6d77cc9fb581d60111111111111111111111111111111111111111111111111111111111a00989680005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358200767446910aa276510642f89978c67b7ab51f1fb1d11b18447591f4a0f826329183c18378358200767446910aa276510642f89978c67b7ab51f1fb1d11b18447591f4a0f826329183c18378358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
      nextScheduleHashHex:
        "96c1acf52ec7e32c975a3c5343d7417257e0eca08c0fda83d77f2172000d0b36",
    },
  },
} as const satisfies Record<string, ResolutionTerminalVector>;

const corpus = JSON.parse(
  readFileSync(
    resolve(
      process.cwd(),
      "../midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json",
    ),
    "utf8",
  ),
) as BoundaryCorpus;

const boundaryEntry = (label: string): Buffer => {
  expect(corpus.schema).toBe(
    "midgard-cardano-capability-p2-boundary-corpus-v1",
  );
  const entry = corpus.entries.find((candidate) => candidate.label === label);
  expect(entry, `missing checked boundary corpus entry ${label}`).toBeDefined();
  return Buffer.from(entry!.canonicalCborHex, "hex");
};

const buildResolutionNodes = (
  label: string,
): {
  readonly spendCount: number;
  readonly referenceCount: number;
  readonly nodes: readonly ResolutionNode[];
  readonly originalScheduleHash: Buffer;
} => {
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(boundaryEntry(label));
  const spendKeys = decodeMidgardNativeByteListPreimage(
    tx.body.spendInputsPreimageCbor,
    `${label}.spend_inputs`,
  );
  const referenceKeys = decodeMidgardNativeByteListPreimage(
    tx.body.referenceInputsPreimageCbor,
    `${label}.reference_inputs`,
  );
  const ordered = [
    ...spendKeys.map((key, itemIndex) => ({
      sourceKind: "spend" as const,
      itemIndex,
      key: Buffer.from(key),
    })),
    ...referenceKeys.map((key, itemIndex) => ({
      sourceKind: "reference" as const,
      itemIndex,
      key: Buffer.from(key),
    })),
  ].sort((left, right) => Buffer.compare(left.key, right.key));
  for (let index = 1; index < ordered.length; index += 1) {
    if (ordered[index - 1]!.key.equals(ordered[index]!.key)) {
      throw new Error(`${label} contains a cross-field duplicate input`);
    }
  }

  const outputCbor = makeOutput(
    10_000_000n,
    Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
  );
  const nodes = new Array<ResolutionNode>(ordered.length);
  let scheduleHash = emptyMidgardInputResolutionSchedule();
  for (let index = ordered.length - 1; index >= 0; index -= 1) {
    const item = ordered[index]!;
    const nextScheduleHash = scheduleHash;
    scheduleHash = prependMidgardInputResolutionSchedule({
      sourceKind: item.sourceKind,
      itemIndex: item.itemIndex,
      key: item.key,
      nextHash: nextScheduleHash,
    });
    nodes[index] = {
      ...item,
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: item.key,
        outputCbor,
      }).descriptorCbor,
      nextScheduleHash,
      scheduleHash,
    };
  }
  return {
    spendCount: spendKeys.length,
    referenceCount: referenceKeys.length,
    nodes,
    originalScheduleHash: scheduleHash,
  };
};

const toPeakVector = (
  frontier: MidgardValidationMerkleFrontier,
): ResolutionTerminalVector["terminal"]["resolvedItemPeaks"] =>
  frontier.peaks.map((peak) => ({
    height: peak.height,
    hashHex: Buffer.from(peak.hash).toString("hex"),
  }));

const deriveTerminalVector = (label: string): ResolutionTerminalVector => {
  const { spendCount, referenceCount, nodes, originalScheduleHash } =
    buildResolutionNodes(label);
  expect(nodes.length).toBeGreaterThan(0);
  let remainingScheduleHash = originalScheduleHash;
  let accumulator = initialMidgardResolvedInputsAccumulator();
  let resolvedItemFrontier = emptyMidgardValidationMerkleFrontier();
  let spendIndex = 0;
  let penultimate: ResolutionTerminalVector["penultimate"] | undefined;

  for (const [cursor, node] of nodes.entries()) {
    expect(remainingScheduleHash).toEqual(node.scheduleHash);
    if (cursor + 1 === nodes.length) {
      penultimate = {
        cursor,
        accumulatorHex: accumulator.toString("hex"),
        remainingScheduleHashHex: remainingScheduleHash.toString("hex"),
        spendIndex,
        resolvedItemPeaks: toPeakVector(resolvedItemFrontier),
      };
    }
    accumulator = advanceMidgardResolvedInputsAccumulator({
      accumulator,
      sourceKind: node.sourceKind,
      key: node.key,
      value: node.value,
    });
    resolvedItemFrontier = appendMidgardValidationMerkleLeaf(
      resolvedItemFrontier,
      hashMidgardResolvedContextItemLeaf({
        sourceKind: node.sourceKind,
        itemIndex: cursor,
        key: node.key,
        outputCbor: node.value,
      }),
    );
    remainingScheduleHash = node.nextScheduleHash;
    if (node.sourceKind === "spend") spendIndex += 1;
  }

  const lastNode = nodes.at(-1)!;
  expect(penultimate).toBeDefined();
  expect(remainingScheduleHash).toEqual(emptyMidgardInputResolutionSchedule());
  expect(spendIndex).toBe(spendCount);
  return {
    label,
    spendCount,
    referenceCount,
    originalScheduleHashHex: originalScheduleHash.toString("hex"),
    terminalAccumulatorHex: accumulator.toString("hex"),
    terminalFrontierCommitmentHex:
      commitMidgardValidationMerkleFrontier(resolvedItemFrontier).toString(
        "hex",
      ),
    penultimate: penultimate!,
    terminal: {
      cursor: nodes.length,
      spendIndex,
      resolvedItemPeaks: toPeakVector(resolvedItemFrontier),
    },
    lastNode: {
      sourceKind: lastNode.sourceKind,
      itemIndex: lastNode.itemIndex,
      keyHex: lastNode.key.toString("hex"),
      valueHex: lastNode.value.toString("hex"),
      nextScheduleHashHex: lastNode.nextScheduleHash.toString("hex"),
    },
  };
};

const assertAdjacentMutationsReject = (
  vector: ResolutionTerminalVector,
): void => {
  const last = vector.lastNode;
  const remaining = Buffer.from(
    vector.penultimate.remainingScheduleHashHex,
    "hex",
  );
  const next = Buffer.from(last.nextScheduleHashHex, "hex");
  const key = Buffer.from(last.keyHex, "hex");
  const exact = prependMidgardInputResolutionSchedule({
    sourceKind: last.sourceKind,
    itemIndex: last.itemIndex,
    key,
    nextHash: next,
  });
  expect(exact).toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind === "spend" ? "reference" : "spend",
      itemIndex: last.itemIndex,
      key,
      nextHash: next,
    }),
  ).not.toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind,
      itemIndex: last.itemIndex + 1,
      key,
      nextHash: next,
    }),
  ).not.toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind,
      itemIndex: last.itemIndex,
      key: Buffer.concat([key.subarray(0, -1), Buffer.from([0xff])]),
      nextHash: next,
    }),
  ).not.toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind,
      itemIndex: last.itemIndex,
      key,
      nextHash: Buffer.alloc(32, 0xff),
    }),
  ).not.toEqual(remaining);
};

describe("retained input-resolution schedule boundary", () => {
  it("exhaustively folds the 434-spend maximum with no reference inputs", () => {
    const vector = deriveTerminalVector("maximum-spend-inputs");
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(JSON.stringify(vector, null, 2));
    }
    expect(vector).toEqual(expectedTerminalVectors["maximum-spend-inputs"]);
    assertAdjacentMutationsReject(vector);
  });

  it("exhaustively folds the mixed one-spend/433-reference maximum", () => {
    const vector = deriveTerminalVector("maximum-reference-inputs");
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(JSON.stringify(vector, null, 2));
    }
    expect(vector).toEqual(expectedTerminalVectors["maximum-reference-inputs"]);
    assertAdjacentMutationsReject(vector);
  });
});
