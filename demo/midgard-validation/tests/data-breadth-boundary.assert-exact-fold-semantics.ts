import { readFileSync } from "node:fs";

import {
  advanceMidgardCekDataTraverse,
  encodeMidgardCekDataFrame,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

export type DataBreadthKind = "constructor" | "list" | "map";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly BlueprintValidator[];
};

export const alwaysSucceedsCompiledCode =
  alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === "midgard.deposit_spend.else",
  )?.compiledCode;

const cborUnsignedHex = (value: number): string => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("Data breadth integer must be non-negative");
  }
  if (value < 24) return value.toString(16).padStart(2, "0");
  if (value <= 0xff) {
    return `18${value.toString(16).padStart(2, "0")}`;
  }
  if (value <= 0xffff) {
    return `19${value.toString(16).padStart(4, "0")}`;
  }
  return `1a${value.toString(16).padStart(8, "0")}`;
};

const cborMapHeaderHex = (pairCount: number): string => {
  if (!Number.isSafeInteger(pairCount) || pairCount <= 0) {
    throw new Error("Data map breadth must be positive");
  }
  if (pairCount < 24) {
    return (0xa0 + pairCount).toString(16);
  }
  if (pairCount <= 0xff) {
    return `b8${pairCount.toString(16).padStart(2, "0")}`;
  }
  if (pairCount <= 0xffff) {
    return `b9${pairCount.toString(16).padStart(4, "0")}`;
  }
  return `ba${pairCount.toString(16).padStart(8, "0")}`;
};

export const cardanoBreadthDataCbor = (
  kind: DataBreadthKind,
  breadth: number,
): string => {
  if (!Number.isSafeInteger(breadth) || breadth <= 0) {
    throw new Error("Cardano Data breadth must be positive");
  }
  if (kind === "list") {
    return `9f${"00".repeat(breadth)}ff`;
  }
  if (kind === "constructor") {
    return `d8668218809f${"00".repeat(breadth)}ff`;
  }
  const entries = Array.from(
    { length: breadth },
    (_, index) => `${cborUnsignedHex(index)}00`,
  ).join("");
  return `${cborMapHeaderHex(breadth)}${entries}`;
};

export const dataNodeCount = (
  kind: DataBreadthKind,
  breadth: number,
): number => (kind === "map" ? breadth * 2 + 1 : breadth + 1);

export type DataTraverseStep = {
  readonly control: Parameters<
    typeof advanceMidgardCekDataTraverse
  >[0]["control"];
  readonly action: Parameters<
    typeof advanceMidgardCekDataTraverse
  >[0]["action"];
  readonly next: Parameters<typeof advanceMidgardCekDataTraverse>[0]["control"];
};

const unsignedByteLength = (value: number): number => {
  let size = 1;
  let remaining = value;
  while (remaining >= 256) {
    size += 1;
    remaining = Math.floor(remaining / 256);
  }
  return size;
};

const integerDataMemory = (value: number): bigint =>
  BigInt(4 + unsignedByteLength(value * 2));

const exactBreadthMemory = (kind: DataBreadthKind, breadth: number): bigint => {
  if (kind !== "map") return 4n + BigInt(breadth) * 5n;
  let memory = 4n;
  for (let index = 0; index < breadth; index += 1) {
    memory += integerDataMemory(index) + 5n;
  }
  return memory;
};

export const assertExactBreadthSemantics = (
  kind: DataBreadthKind,
  breadth: number,
  cborHex: string,
): void => {
  const data = CML.PlutusData.from_cbor_hex(cborHex);
  expect(data.to_cbor_hex()).toBe(cborHex);
  if (kind === "constructor") {
    const constructor = data.as_constr_plutus_data();
    expect(constructor?.alternative()).toBe(128n);
    const fields = constructor?.fields();
    expect(fields?.len()).toBe(breadth);
    for (let index = 0; index < breadth; index += 1) {
      expect(fields!.get(index).to_cbor_hex()).toBe("00");
    }
    return;
  }
  if (kind === "list") {
    const list = data.as_list();
    expect(list?.len()).toBe(breadth);
    for (let index = 0; index < breadth; index += 1) {
      expect(list!.get(index).to_cbor_hex()).toBe("00");
    }
    return;
  }
  const map = data.as_map();
  expect(map?.len()).toBe(breadth);
  const keys = map!.keys();
  expect(keys.len()).toBe(breadth);
  for (let index = 0; index < breadth; index += 1) {
    const key = keys.get(index);
    expect(key.to_cbor_hex()).toBe(cborUnsignedHex(index));
    expect(key.as_integer()?.as_u64()).toBe(BigInt(index));
    const values = map!.get_all(key);
    expect(values?.len()).toBe(1);
    expect(values!.get(0).to_cbor_hex()).toBe("00");
  }
};

export const assertExactFoldSemantics = ({
  kind,
  breadth,
  steps,
}: {
  readonly kind: DataBreadthKind;
  readonly breadth: number;
  readonly steps: readonly DataTraverseStep[];
}): void => {
  const folds = steps.flatMap(({ action }) =>
    action?.kind === "foldList" || action?.kind === "foldMap" ? [action] : [],
  );
  if (folds.length !== breadth) {
    throw new Error(
      `${kind} production fold count ${folds.length.toString()} != ${breadth.toString()}`,
    );
  }
  const zeroRoots = new Set<string>();
  const keyRoots = new Set<string>();
  for (let position = 0; position < folds.length; position += 1) {
    const action = folds[position]!;
    const expectedIndex = breadth - position - 1;
    // Only a map frame carries its header count; constructor and list frames
    // are open-ended and closed by the authenticated source.
    const childCount = kind === "map" ? breadth * 2 : breadth;
    const expectedChildren = kind === "map" ? childCount : 0;
    if (
      action.frame.expectedChildren !== expectedChildren ||
      action.frame.childCount !== childCount ||
      action.frame.foldCursor !== position
    ) {
      throw new Error(
        `${kind} production frame lost exact child count or cursor at ${position.toString()}`,
      );
    }
    if (kind === "map") {
      if (
        action.kind !== "foldMap" ||
        action.pairIndex !== expectedIndex ||
        action.key.cborLength !==
          BigInt(cborUnsignedHex(expectedIndex).length / 2) ||
        action.key.memory !== integerDataMemory(expectedIndex) ||
        action.value.cborLength !== 1n ||
        action.value.memory !== 5n
      ) {
        throw new Error(
          `map production fold lost pair/key/value identity at ${expectedIndex.toString()}`,
        );
      }
      keyRoots.add(Buffer.from(action.key.root).toString("hex"));
      zeroRoots.add(Buffer.from(action.value.root).toString("hex"));
    } else {
      if (
        action.kind !== "foldList" ||
        action.childIndex !== expectedIndex ||
        action.child.cborLength !== 1n ||
        action.child.memory !== 5n
      ) {
        throw new Error(
          `${kind} production fold lost child identity at ${expectedIndex.toString()}`,
        );
      }
      zeroRoots.add(Buffer.from(action.child.root).toString("hex"));
    }
  }
  if (zeroRoots.size !== 1 || (kind === "map" && keyRoots.size !== breadth)) {
    throw new Error(`${kind} production fold lost exact scalar identities`);
  }
};

export const assertExactTerminalSummary = ({
  kind,
  breadth,
  dataCborHex,
  summary,
}: {
  readonly kind: DataBreadthKind;
  readonly breadth: number;
  readonly dataCborHex: string;
  readonly summary: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  };
}): void => {
  expect(Buffer.from(summary.root)).toHaveLength(32);
  expect(summary.cborLength).toBe(BigInt(dataCborHex.length / 2));
  expect(summary.memory).toBe(exactBreadthMemory(kind, breadth));
};

export const jsonSummary = (summary: {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}) => ({
  rootHex: Buffer.from(summary.root).toString("hex"),
  cborLength: summary.cborLength.toString(),
  memory: summary.memory.toString(),
});

export const jsonFrame = (
  frame: Parameters<typeof encodeMidgardCekDataFrame>[0],
) => ({
  cborHex: encodeMidgardCekDataFrame(frame).toString("hex"),
  kind: frame.kind,
  ...(frame.kind === "constrSmall"
    ? { constructor: frame.constructor.toString() }
    : frame.kind === "constrLarge"
      ? {
          constructorCborRootHex: Buffer.from(
            frame.constructorCborRoot,
          ).toString("hex"),
          constructorCborLength: frame.constructorCborLength.toString(),
          constructorMemory: frame.constructorMemory.toString(),
        }
      : {}),
  tailHex: Buffer.from(frame.tail).toString("hex"),
  expectedChildren: frame.expectedChildren,
  childCount: frame.childCount,
  childPeaks: frame.childFrontier.peaks.map(({ height, hash }) => ({
    height,
    hashHex: Buffer.from(hash).toString("hex"),
  })),
  foldCursor: frame.foldCursor,
  sequence: {
    rootHex: Buffer.from(frame.sequence.root).toString("hex"),
    length: frame.sequence.length.toString(),
    payloadCborLength: frame.sequence.payloadCborLength.toString(),
    memory: frame.sequence.memory.toString(),
  },
});
