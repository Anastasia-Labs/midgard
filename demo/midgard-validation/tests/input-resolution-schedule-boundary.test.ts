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
      "b1276de655cef9d7a3bc33abb49024dd36462f2cd2486dd25d9f7347563eab35",
    terminalAccumulatorHex:
      "fb2154b7f046126c8cda87674c2822c3dad1ac294531845e6e6b49c0035e4858",
    terminalFrontierCommitmentHex:
      "8d0f7813e70f9bf63f84b78365a38e5c00efb807ac4f5ed16d21f0eff4b5e5b7",
    penultimate: {
      cursor: 433,
      accumulatorHex:
        "7f4f9eb3bfca6b31c4c6781cc36b58f0305c7cafbfbbe4dbfef9ad2370f8cbdc",
      remainingScheduleHashHex:
        "6d5383cff6142a4e20298cf82b71837886eeb853245d068a8cf43a540e1d2b3d",
      spendIndex: 433,
      resolvedItemPeaks: [
        {
          height: 0,
          hashHex:
            "490d3ccf112900b8af68cdf9873ea682bc41f5e3a18ce80e8cbaad97c7afe1f0",
        },
        {
          height: 4,
          hashHex:
            "d01cd11a1af0cc1270e6f3f3a7dfff15c52f7710c1c12bdd3245c068f328c53b",
        },
        {
          height: 5,
          hashHex:
            "4bd02c803151903ed7bb3a09e8c7823ba0391d78edfc6dacaf66badc3950d9bf",
        },
        {
          height: 7,
          hashHex:
            "66b7b616caa9588056af4d6191ecd23ba077e9c7644e70bb2666fb08f759a0a6",
        },
        {
          height: 8,
          hashHex:
            "998486423647076a38bdd62961e92f2c404cd7c9f3ce8f862890f806e82eb543",
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
            "45e3f84f8c35714d5ce696474f5b300c7860a7ebc7f3eb30cc0c1fc8390c3e20",
        },
        {
          height: 4,
          hashHex:
            "d01cd11a1af0cc1270e6f3f3a7dfff15c52f7710c1c12bdd3245c068f328c53b",
        },
        {
          height: 5,
          hashHex:
            "4bd02c803151903ed7bb3a09e8c7823ba0391d78edfc6dacaf66badc3950d9bf",
        },
        {
          height: 7,
          hashHex:
            "66b7b616caa9588056af4d6191ecd23ba077e9c7644e70bb2666fb08f759a0a6",
        },
        {
          height: 8,
          hashHex:
            "998486423647076a38bdd62961e92f2c404cd7c9f3ce8f862890f806e82eb543",
        },
      ],
    },
    lastNode: {
      sourceKind: "spend",
      keyHex:
        "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
      valueHex:
        "90011901b11829582075905d5cce7bbf39a2d566bf6f5ee2f9074354777ca4408ebd73e9d6d77cc9fb581d60111111111111111111111111111111111111111111111111111111111a00989680005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
      nextScheduleHashHex:
        "96c1acf52ec7e32c975a3c5343d7417257e0eca08c0fda83d77f2172000d0b36",
    },
  },
  "maximum-reference-inputs": {
    label: "maximum-reference-inputs",
    spendCount: 1,
    referenceCount: 433,
    originalScheduleHashHex:
      "320bc3d5140983c35d48db52cbf4e722de03b8ea618ef56b6a7f8b6fc6162e56",
    terminalAccumulatorHex:
      "e0254c2da094b13f9857dd7f442bd346f74ccf0f4a1337bd1328983d80156421",
    terminalFrontierCommitmentHex:
      "b4757e20d40f58687e5421545c69f0399d6de0026a8e9728ec6e9bb13b510a46",
    penultimate: {
      cursor: 433,
      accumulatorHex:
        "cef5b3dc9392a0a6f359309a1b6ef88bc48196c7d85e4b2ab2b539a778fec226",
      remainingScheduleHashHex:
        "11e90c7b9d22faa10a757fe7cbcfc12f5784cb6c546408583d36ce0dd5d49329",
      spendIndex: 1,
      resolvedItemPeaks: [
        {
          height: 0,
          hashHex:
            "c830521b7ad8c959ae55b31633e207388fae52161373666e11f0cfb8f2636258",
        },
        {
          height: 4,
          hashHex:
            "fb7125a9b42801c60668ae85fbb63c9688ae6f1db51b66e808f887beb33ad939",
        },
        {
          height: 5,
          hashHex:
            "d434d9b86a8a039067633f60db0b7028cd8c0e20d7738f136c71ba4ebb17f600",
        },
        {
          height: 7,
          hashHex:
            "a0b8592e98857754eb7c710a2b73418391aef2bda4fcbd1e1bef9db50945bed7",
        },
        {
          height: 8,
          hashHex:
            "7d334c4861036c40c26095c4cb1d9db17b736a5a4108beb136aa4988ce854fe7",
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
            "8ef1a237aa9edee558dfd3d186967ef1d9fcb2359dbc03db2baab40d0e0a957e",
        },
        {
          height: 4,
          hashHex:
            "fb7125a9b42801c60668ae85fbb63c9688ae6f1db51b66e808f887beb33ad939",
        },
        {
          height: 5,
          hashHex:
            "d434d9b86a8a039067633f60db0b7028cd8c0e20d7738f136c71ba4ebb17f600",
        },
        {
          height: 7,
          hashHex:
            "a0b8592e98857754eb7c710a2b73418391aef2bda4fcbd1e1bef9db50945bed7",
        },
        {
          height: 8,
          hashHex:
            "7d334c4861036c40c26095c4cb1d9db17b736a5a4108beb136aa4988ce854fe7",
        },
      ],
    },
    lastNode: {
      sourceKind: "reference",
      keyHex:
        "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
      valueHex:
        "90011901b11829582075905d5cce7bbf39a2d566bf6f5ee2f9074354777ca4408ebd73e9d6d77cc9fb581d60111111111111111111111111111111111111111111111111111111111a00989680005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358208e037f86edc06fd63fbc01bf5e587d66127e73513943f7d1c294a3b6649b72b7183c18528358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
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
    ...spendKeys.map((key) => ({
      sourceKind: "spend" as const,
      key: Buffer.from(key),
    })),
    ...referenceKeys.map((key) => ({
      sourceKind: "reference" as const,
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
  const nodes: ResolutionNode[] = new Array(ordered.length);
  let scheduleHash = emptyMidgardInputResolutionSchedule();
  for (let index = ordered.length - 1; index >= 0; index -= 1) {
    const item = ordered[index]!;
    const nextScheduleHash = scheduleHash;
    scheduleHash = prependMidgardInputResolutionSchedule({
      sourceKind: item.sourceKind,
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
    key,
    nextHash: next,
  });
  expect(exact).toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind === "spend" ? "reference" : "spend",
      key,
      nextHash: next,
    }),
  ).not.toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind,
      key: Buffer.concat([key.subarray(0, -1), Buffer.from([0xff])]),
      nextHash: next,
    }),
  ).not.toEqual(remaining);
  expect(
    prependMidgardInputResolutionSchedule({
      sourceKind: last.sourceKind,
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
