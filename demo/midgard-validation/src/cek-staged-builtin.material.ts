/**
 * The material of a staged builtin step (arm 31, semantics groups 7 and 8):
 * the TypeScript twin of `cek_staged_arm_v1.StagedMaterialV1`.
 *
 * Each builtin argument has one source: its bytes inline, a staged operand
 * (bytes read from staging outputs among the transaction's reference
 * inputs), its leaf preimage alone, or the value of a small integer. A step
 * is emitted with its staged operands named by index and no staging outputs
 * yet; staging them gives the output references, and
 * `stageMidgardCekStagedBuiltinMaterial` fills them in.
 */
import {
  groupMidgardCekStagedRuns,
  type MidgardCekStagedOutRef,
  type MidgardCekStagedReferenceInput,
  orderMidgardCekStagedRuns,
  splitMidgardCekStagedChunks,
} from "@al-ft/midgard-core";
import { Constr } from "@lucid-evolution/lucid";

import type { Bytes } from "./cek-builtin.argument-kinds.js";

export type MidgardCekStagedOperandSource =
  | { readonly kind: "inline"; readonly bytes: Bytes }
  | { readonly kind: "staged"; readonly operand: number }
  | {
      readonly kind: "bytesLeaf";
      readonly root: Bytes;
      readonly length: bigint;
    }
  | {
      readonly kind: "integerLeaf";
      readonly cborRoot: Bytes;
      readonly cborLength: bigint;
      readonly words: bigint;
    }
  | { readonly kind: "smallInteger"; readonly value: bigint };

/**
 * The staging outputs a step reads, in ledger order, and for each staged
 * operand the indices of its runs among them, in chunk order.
 */
export type MidgardCekStagedRuns = {
  readonly refs: readonly MidgardCekStagedOutRef[];
  readonly operands: readonly (readonly number[])[];
};

export type MidgardCekStagedBuiltinMaterial = {
  readonly runs: MidgardCekStagedRuns;
  readonly sources: readonly MidgardCekStagedOperandSource[];
};

/**
 * Where each staged operand was staged: the output reference of each of its
 * runs, in run order, indexed as the sources name the operands.
 */
export type MidgardCekStagingResult =
  readonly (readonly MidgardCekStagedOutRef[])[];

const hex = (bytes: Bytes): string => Buffer.from(bytes).toString("hex");

const sourceData = (source: MidgardCekStagedOperandSource): Constr<unknown> => {
  switch (source.kind) {
    case "inline":
      return new Constr(0, [hex(source.bytes)]);
    case "staged":
      return new Constr(1, [BigInt(source.operand)]);
    case "bytesLeaf":
      return new Constr(2, [hex(source.root), source.length]);
    case "integerLeaf":
      return new Constr(3, [
        hex(source.cborRoot),
        source.cborLength,
        source.words,
      ]);
    case "smallInteger":
      return new Constr(4, [source.value]);
  }
};

/** `StagedMaterialV1` as Data. */
export const midgardCekStagedBuiltinMaterialData = (
  material: MidgardCekStagedBuiltinMaterial,
): Constr<unknown> =>
  new Constr(0, [
    new Constr(0, [
      material.runs.refs.map(
        (ref) =>
          new Constr(0, [hex(ref.transactionId), BigInt(ref.outputIndex)]),
      ),
      material.runs.operands.map((indices) => indices.map(BigInt)),
    ]),
    material.sources.map(sourceData),
  ]);

/** The runs each staged operand is staged as: 4,095-byte chunks, three a run. */
export const midgardCekStagedOperandRuns = (
  stagedOperands: readonly Bytes[],
): Buffer[][][] =>
  stagedOperands.map((bytes) =>
    groupMidgardCekStagedRuns(splitMidgardCekStagedChunks(bytes)),
  );

const refKey = (ref: MidgardCekStagedOutRef): string =>
  `${hex(ref.transactionId)}#${ref.outputIndex}`;

/**
 * The material with its staging outputs filled in from `staging`: the
 * outputs in ledger order, each named once, and each staged operand by the
 * indices of its runs. Two operands may share an output only when it holds
 * the same run for both.
 */
export const stageMidgardCekStagedBuiltinMaterial = (
  material: MidgardCekStagedBuiltinMaterial,
  stagedOperands: readonly Bytes[],
  staging: MidgardCekStagingResult,
): MidgardCekStagedBuiltinMaterial => {
  const runs = midgardCekStagedOperandRuns(stagedOperands);
  if (staging.length !== runs.length) {
    throw new Error("staging does not cover every staged operand");
  }
  const outputs = new Map<
    string,
    { readonly outRef: MidgardCekStagedOutRef; readonly chunks: Buffer[] }
  >();
  staging.forEach((refs, operand) => {
    const operandRuns = runs[operand]!;
    if (refs.length !== operandRuns.length) {
      throw new Error("staging does not name one output per run");
    }
    refs.forEach((outRef, index) => {
      const chunks = operandRuns[index]!;
      const known = outputs.get(refKey(outRef));
      if (known === undefined) {
        outputs.set(refKey(outRef), { outRef, chunks });
      } else if (
        known.chunks.length !== chunks.length ||
        known.chunks.some((chunk, at) => !chunk.equals(chunks[at]!))
      ) {
        throw new Error("a staging output holds two different runs");
      }
    });
  });
  const ordered = orderMidgardCekStagedRuns([...outputs.values()]);
  const position = new Map(
    ordered.refs.map((ref, index) => [refKey(ref), index] as const),
  );
  return Object.freeze({
    runs: Object.freeze({
      refs: ordered.refs,
      operands: staging.map((refs) =>
        refs.map((ref) => position.get(refKey(ref))!),
      ),
    }),
    sources: material.sources,
  });
};

/**
 * A staging of every staged operand on outputs of one placeholder
 * transaction: the step as it reads once staged, for checking it before the
 * real staging outputs exist.
 */
export const midgardCekPlaceholderStaging = (
  stagedOperands: readonly Bytes[],
): MidgardCekStagingResult => {
  let next = 0;
  return midgardCekStagedOperandRuns(stagedOperands).map((runs) =>
    runs.map(() => ({ transactionId: Buffer.alloc(32), outputIndex: next++ })),
  );
};

/**
 * The reference inputs a staged step reads: its staging outputs, in ledger
 * order, each holding the run of the staged operand that names it.
 */
export const midgardCekStagedReferenceInputs = (
  material: MidgardCekStagedBuiltinMaterial,
  stagedOperands: readonly Bytes[],
): MidgardCekStagedReferenceInput[] => {
  const chunks: (Buffer[] | null)[] = material.runs.refs.map(() => null);
  const runs = midgardCekStagedOperandRuns(stagedOperands);
  material.runs.operands.forEach((indices, operand) => {
    const operandRuns = runs[operand];
    if (operandRuns === undefined || operandRuns.length !== indices.length) {
      throw new Error("staged operand runs do not match its bytes");
    }
    indices.forEach((index, at) => {
      if (index < 0 || index >= chunks.length) {
        throw new Error("staged operand names a run outside the outputs");
      }
      chunks[index] ??= operandRuns[at]!;
    });
  });
  return material.runs.refs.map((outRef, index) => ({
    outRef,
    chunks: chunks[index] ?? null,
  }));
};
