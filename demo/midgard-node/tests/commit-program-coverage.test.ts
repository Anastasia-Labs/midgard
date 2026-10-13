import {
  assertMidgardCekProgramMaterialBundle,
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import type { PhaseAValidatedTx } from "@al-ft/midgard-validation";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as LedgerUtils from "../src/database/utils/ledger.js";
import {
  evaluateEvictingUncommittable,
  makeCommitStageRejectionBreaker,
} from "../src/mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import { COMMIT_REJECT_CODE_PROGRAM_COVERAGE } from "../src/mpf/commit-rejection.js";
import { normalProgramCoverageCheck } from "../src/mpf/commit-rejection.normal-program-coverage-check.js";
import {
  lambdaChain,
  programVariants,
} from "./helpers/commit-stage-eviction.fixture.js";

/**
 * The block-wide program-material check over the transactions an evaluation
 * accepts: it keeps them in block order while the block's merged material
 * fits, leaves the rest pending with their in-block descendants for later
 * blocks, and evaluates the block once.
 */

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

// The test transactions are opaque bytes; their programs are the test's.
const programs = vi.hoisted(() => new Map<string, readonly unknown[]>());
vi.mock("@al-ft/midgard-core/codec", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-core/codec")
  >("@al-ft/midgard-core/codec");
  return {
    ...actual,
    decodeMidgardNativeTxFullFromCanonicalCbor: (cbor: Uint8Array) => ({
      testCbor: Buffer.from(cbor).toString("hex"),
    }),
  };
});
vi.mock("@al-ft/midgard-core/script-proof", async () => {
  const actual = await vi.importActual<
    typeof import("@al-ft/midgard-core/script-proof")
  >("@al-ft/midgard-core/script-proof");
  return {
    ...actual,
    collectMidgardEventProgramEnvelopes: (tx: { testCbor: string }) =>
      programs.get(tx.testCbor) ?? [],
  };
});

const outRef = (seed: number) => Buffer.alloc(36, seed);

/** A transaction with one program of `nodeCount` nodes over its own leaf,
 * and its material, spending `spends` when given. */
const scriptTx = (
  seed: number,
  nodeCount: number,
  spends?: PhaseAValidatedTx,
): PhaseAValidatedTx => {
  const txCbor = Buffer.from([0xfe, seed]);
  const chain =
    nodeCount === 0
      ? { material: [], envelopes: [] }
      : lambdaChain(nodeCount, { kind: "builtin", tag: BigInt(seed) });
  return withPrograms(seed, txCbor, chain.envelopes.slice(-1), {
    sidecar: encodeMidgardCekProgramMaterialSidecar(chain.material),
    spends,
  });
};

const withPrograms = (
  seed: number,
  txCbor: Buffer,
  envelopes: readonly MidgardCekProgramEnvelope[],
  {
    sidecar,
    spends,
  }: { readonly sidecar: Buffer; readonly spends?: PhaseAValidatedTx },
): PhaseAValidatedTx => {
  programs.set(hex(txCbor), envelopes);
  return {
    ledgerTx: { txId: Buffer.alloc(32, seed) },
    submission: { txCbor, programMaterialSidecarCbor: sidecar },
    graph: {
      spentOutRefHexes:
        spends === undefined
          ? [hex(outRef(0x80 + seed))]
          : [hex(spends.graph.produced[0]![LedgerUtils.Columns.OUTREF])],
      referenceOutRefHexes: [],
      produced: [
        {
          [LedgerUtils.Columns.OUTREF]: outRef(seed),
          [LedgerUtils.Columns.OUTPUT]: Buffer.from([seed]),
        },
      ],
    },
  } as unknown as PhaseAValidatedTx;
};

/**
 * One block over `candidates`, with an evaluation that accepts every one:
 * what the block keeps and rejects, and how often it evaluated.
 */
const block = async (
  candidates: readonly PhaseAValidatedTx[],
  limits?: Parameters<typeof normalProgramCoverageCheck>[1],
) => {
  let evaluations = 0;
  const result = await Effect.runPromise(
    evaluateEvictingUncommittable({
      candidates,
      stage: "Phase B",
      breaker: makeCommitStageRejectionBreaker(),
      evaluate: (evaluated) =>
        Effect.sync(() => {
          evaluations += 1;
          return { accepted: evaluated, rejected: [] };
        }),
      blockCheck: normalProgramCoverageCheck(new Map(), limits),
    }).pipe(Effect.provide(Logger.remove(Logger.defaultLogger))),
  );
  return {
    kept: result.accepted,
    rejected: result.rejected.map(({ txId, code }) => [hex(txId), code]),
    evaluations,
  };
};

const LIMITS = { nodeCount: 10n, materialBytes: 1_000_000n };

describe("block-wide program-material coverage of normal transactions", () => {
  it("keeps what fits after a heavy transaction in one pass and leaves the rest pending, each in a later block", async () => {
    const heavy = scriptTx(1, 8);
    const honest = [2, 3, 4, 5].map((seed) => scriptTx(seed, 3));
    // Spends the first honest transaction's output; no programs of its own.
    const child = scriptTx(6, 0, honest[0]);
    const small = [10, 11].map((seed) => scriptTx(seed, 1));
    let pending = [heavy, honest[0]!, child, ...honest.slice(1), ...small];
    const blocks = [];
    while (pending.length > 0) {
      const built = await block(pending, LIMITS);
      expect(built.evaluations).toBe(1);
      expect(built.rejected).toEqual([]);
      blocks.push(built.kept);
      pending = pending.filter((tx) => !built.kept.includes(tx));
    }
    expect(blocks).toEqual([
      [heavy, ...small],
      [honest[0], child, honest[1], honest[2]],
      [honest[3]],
    ]);
  });

  it("commits every transaction whose programs pass alone and whose merged material fits, past the per-bundle aggregate of the whole block", async () => {
    const { variantA, variantB, material, sidecarA, sidecarB } =
      programVariants();
    expect(variantA.nodeCount + variantB.nodeCount).toBeGreaterThan(
      MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS,
    );
    expect(() =>
      assertMidgardCekProgramMaterialBundle([variantA, variantB], material),
    ).toThrow();
    const first = withPrograms(20, Buffer.from([0xfd, 20]), [variantA], {
      sidecar: sidecarA,
    });
    const second = withPrograms(21, Buffer.from([0xfd, 21]), [variantB], {
      sidecar: sidecarB,
    });
    const built = await block([first, second]);
    expect(built.kept).toEqual([first, second]);
    expect(built.rejected).toEqual([]);
  }, 240_000);

  it("rejects a transaction whose programs fail alone, with no deferral", async () => {
    const own = scriptTx(30, 3);
    const missing = withPrograms(
      31,
      Buffer.from([0xfd, 31]),
      lambdaChain(3, { kind: "builtin", tag: 31n }).envelopes.slice(-1),
      { sidecar: encodeMidgardCekProgramMaterialSidecar([]) },
    );
    const built = await block([missing, own], LIMITS);
    expect(built.kept).toEqual([own]);
    expect(built.rejected).toEqual([
      [hex(missing.ledgerTx.txId), COMMIT_REJECT_CODE_PROGRAM_COVERAGE],
    ]);
  });
});
