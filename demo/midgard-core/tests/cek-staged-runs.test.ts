import { describe, expect, it } from "vitest";

import { commitMidgardCekBlob } from "../src/cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  bindMidgardCekStagedOperand,
  collectMidgardCekStagedRuns,
  compareMidgardCekStagedOutRefs,
  groupMidgardCekStagedRuns,
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  midgardCekFlatRoot,
  midgardCekFlatRootOfPieces,
  type MidgardCekStagedOutRef,
  type MidgardCekStagedReferenceInput,
  orderMidgardCekStagedRuns,
  splitMidgardCekStagedChunks,
} from "../src/cek-staged-runs.js";

const pattern = (length: number, shift: number): Buffer =>
  Buffer.from(Array.from({ length }, (_, index) => (index + shift) % 256));

const outRef = (seed: number): MidgardCekStagedOutRef => ({
  transactionId: Buffer.alloc(32, seed),
  outputIndex: seed,
});

/** Stages `bytes` as runs, interleaved with other reference inputs. */
const stage = (runs: readonly Buffer[][]) => {
  const staged = runs.map((chunks, index) => ({
    outRef: outRef(10 + index),
    chunks,
  }));
  const ordered = orderMidgardCekStagedRuns(staged);
  const referenceInputs: MidgardCekStagedReferenceInput[] = [
    { outRef: outRef(1), chunks: null },
    ...staged.flatMap((entry) => [
      entry,
      { outRef: outRef(100 + entry.outRef.outputIndex), chunks: null },
    ]),
    { outRef: outRef(200), chunks: null },
  ].sort((left, right) =>
    compareMidgardCekStagedOutRefs(left.outRef, right.outRef),
  );
  return { referenceInputs, refs: ordered.refs };
};

const bindStaged = (runs: readonly Buffer[][], indices: readonly number[]) => {
  const { referenceInputs, refs } = stage(runs);
  return bindMidgardCekStagedOperand(
    collectMidgardCekStagedRuns(referenceInputs, refs),
    indices,
  );
};

const range = (count: number) => Array.from({ length: count }, (_, i) => i);

describe("staged operand binding", () => {
  it.each([0, 1, 4_095, 4_096, 9_216, 65_536, 102_400, 131_072])(
    "binds an honest %i-byte operand to its flat root",
    (length) => {
      const bytes = pattern(length, 3);
      const runs = groupMidgardCekStagedRuns(
        splitMidgardCekStagedChunks(bytes),
      );
      const bound = bindStaged(runs, range(runs.length));
      expect(Buffer.from(bound.root)).toEqual(
        Buffer.from(commitMidgardCekBlob(bytes).root),
      );
      expect(bound.length).toBe(length);
      expect(Buffer.concat(bound.chunks)).toEqual(bytes);
    },
  );

  it("gives a staged and a direct operand of the same value the same root", () => {
    const bytes = pattern(9_215, 4);
    const runs = groupMidgardCekStagedRuns(splitMidgardCekStagedChunks(bytes));
    expect(Buffer.from(bindStaged(runs, [0]).root)).toEqual(
      Buffer.from(midgardCekFlatRoot(bytes)),
    );
  });

  it("roots pieces as their concatenation", () => {
    const bytes = pattern(20_000, 5);
    expect(
      Buffer.from(
        midgardCekFlatRootOfPieces([
          bytes.subarray(0, 1),
          bytes.subarray(1, 4_096),
          bytes.subarray(4_096, 20_000),
        ]),
      ),
    ).toEqual(Buffer.from(midgardCekFlatRoot(bytes)));
  });

  const refusedLength = 6 * MIDGARD_CEK_STAGED_CHUNK_BYTES + 100;
  const refusedBytes = pattern(refusedLength, 13);
  const refusedRuns = groupMidgardCekStagedRuns(
    splitMidgardCekStagedChunks(refusedBytes),
  );
  const honestRoot = Buffer.from(midgardCekFlatRoot(refusedBytes));

  it("binds reordered runs to another root", () => {
    expect(Buffer.from(bindStaged(refusedRuns, [1, 0, 2]).root)).not.toEqual(
      honestRoot,
    );
  });

  it("binds a missing run to another root", () => {
    expect(Buffer.from(bindStaged(refusedRuns, [0, 1]).root)).not.toEqual(
      honestRoot,
    );
  });

  it("binds a duplicated run to another root", () => {
    expect(Buffer.from(bindStaged(refusedRuns, [0, 0, 1, 2]).root)).not.toEqual(
      honestRoot,
    );
  });

  it("binds a tampered chunk to another root", () => {
    const bytes = pattern(9_216, 14);
    const [first, second, third] = splitMidgardCekStagedChunks(bytes);
    const tampered = Buffer.from(second!);
    tampered[tampered.length - 1] = (tampered[tampered.length - 1]! + 1) % 256;
    expect(
      Buffer.from(bindStaged([[first!, tampered, third!]], [0]).root),
    ).not.toEqual(Buffer.from(midgardCekFlatRoot(bytes)));
  });

  it("refuses an extra run after a short last chunk", () => {
    expect(() =>
      bindStaged([...refusedRuns, [pattern(4_095, 0)]], [0, 1, 2, 3]),
    ).toThrow(/before the last/u);
  });

  it("refuses a short chunk before the last", () => {
    const bytes = pattern(8_190, 15);
    expect(() =>
      bindStaged(
        [
          [
            bytes.subarray(0, 4_094),
            bytes.subarray(4_094, 8_189),
            bytes.subarray(8_189),
          ],
        ],
        [0],
      ),
    ).toThrow(/before the last/u);
  });

  it("refuses a trailing empty chunk", () => {
    expect(() =>
      bindStaged([[pattern(4_095, 16), Buffer.alloc(0)]], [0]),
    ).toThrow(/last chunk/u);
  });

  it("refuses a run of four chunks and an empty run", () => {
    const four = splitMidgardCekStagedChunks(pattern(4 * 4_095, 17));
    expect(() => bindStaged([four], [0])).toThrow(/1 to 3 chunks/u);
    expect(() => bindStaged([[]], [0])).toThrow(/1 to 3 chunks/u);
  });

  it("refuses a reference input that carries no run", () => {
    expect(() =>
      collectMidgardCekStagedRuns(
        [{ outRef: outRef(10), chunks: null }],
        [outRef(10)],
      ),
    ).toThrow(/1 to 3 chunks/u);
  });

  it("refuses a reference missing from the reference inputs", () => {
    const { referenceInputs } = stage(refusedRuns);
    expect(() =>
      collectMidgardCekStagedRuns(referenceInputs, [outRef(99)]),
    ).toThrow(/not among/u);
  });

  it("refuses references out of ledger order", () => {
    const { referenceInputs, refs } = stage(refusedRuns);
    expect(() =>
      collectMidgardCekStagedRuns(referenceInputs, [
        refs[1]!,
        refs[0]!,
        refs[2]!,
      ]),
    ).toThrow(/not among/u);
  });

  it("refuses an index outside the runs and an operand with no runs", () => {
    expect(() => bindStaged([[pattern(100, 20)]], [1])).toThrow(/outside/u);
    expect(() => bindStaged([[pattern(100, 21)]], [])).toThrow(/no chunks/u);
  });

  it("refuses a repeated staging output reference", () => {
    expect(() =>
      orderMidgardCekStagedRuns([
        { outRef: outRef(3), chunks: [pattern(1, 0)] },
        { outRef: outRef(3), chunks: [pattern(1, 1)] },
      ]),
    ).toThrow(/repeated/u);
  });

  it("orders output references by transaction id, then index", () => {
    const low = { transactionId: Buffer.alloc(32, 1), outputIndex: 9 };
    const high = { transactionId: Buffer.alloc(32, 2), outputIndex: 0 };
    const highNext = { transactionId: Buffer.alloc(32, 2), outputIndex: 1 };
    expect(
      orderMidgardCekStagedRuns([
        { outRef: highNext, chunks: [] },
        { outRef: low, chunks: [] },
        { outRef: high, chunks: [] },
      ]).refs,
    ).toEqual([low, high, highNext]);
  });
});
