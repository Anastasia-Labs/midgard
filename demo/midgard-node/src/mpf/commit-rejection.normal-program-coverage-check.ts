/**
 * The CEK program-material check over a build's normal transactions, as the
 * block check of `evaluateEvictingUncommittable`.
 */

import {
  assertMidgardCekProgramMaterialBundle,
  decodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type { PhaseAValidatedTx } from "@al-ft/midgard-validation";
import { Cause } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import {
  type BlockCheck,
  type BlockCheckVerdict,
  commitStageFailureDetail,
  isNodeFault,
} from "./commit-rejection.evict-uncommittable-normal-txs.js";
import { COMMIT_REJECT_CODE_PROGRAM_COVERAGE } from "./commit-rejection.js";

export const PROGRAM_COVERAGE_STAGE = "Program material coverage";

/** The bounds on a block's merged program material. */
export type BlockProgramMaterialLimits = {
  readonly nodeCount: bigint;
  readonly materialBytes: bigint;
};

const BLOCK_PROGRAM_MATERIAL_LIMITS: BlockProgramMaterialLimits = {
  nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
  materialBytes: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
};

/**
 * Exactly the program-material checks a verified-block import makes on a
 * block's normal transactions:
 *
 * - per transaction, its programs, collected against the state immediately
 *   before it, verify against the block material within the per-bundle
 *   node-visit and byte-work bounds
 *   (`verified-block-import.program-material.ts:41`). Here they verify
 *   against the transaction's own sidecar alone, with every entry reached,
 *   so a failure is the transaction's own and it is rejected;
 * - block-wide, the merged material stays within the node-count and byte
 *   bounds of `normalizeProgramMaterial`, which the same call applies to the
 *   whole block material, and every entry is reached
 *   (`verified-block-import.replay-events.ts:296`), which every kept
 *   transaction's own check ensures for its entries. A transaction whose
 *   entries would take the kept ones past the bounds is deferred.
 */
export const normalProgramCoverageCheck = (
  preState: ReadonlyMap<string, Buffer>,
  limits: BlockProgramMaterialLimits = BLOCK_PROGRAM_MATERIAL_LIMITS,
): BlockCheck<PhaseAValidatedTx> => ({
  stage: PROGRAM_COVERAGE_STAGE,
  start: (accepted) => {
    const state = new Map(preState);
    const own = accepted.map(
      (
        candidate,
      ):
        | {
            readonly ok: true;
            readonly material: readonly MidgardCekProgramMaterialEntry[];
          }
        | { readonly ok: false; readonly cause: unknown } => {
        try {
          const envelopes = collectMidgardEventProgramEnvelopes(
            decodeMidgardNativeTxFullFromCanonicalCbor(
              candidate.submission.txCbor,
            ),
            (outRefHex) => state.get(outRefHex),
          );
          const sidecar = candidate.submission.programMaterialSidecarCbor;
          const material =
            sidecar === null
              ? []
              : decodeMidgardCekProgramMaterialSidecar(sidecar);
          assertMidgardCekProgramMaterialBundle(envelopes, material);
          return { ok: true, material };
        } catch (cause) {
          // A node fault is no transaction's: it holds the build.
          if (isNodeFault(Cause.die(cause))) throw cause;
          return { ok: false, cause };
        } finally {
          for (const spent of candidate.graph.spentOutRefHexes)
            state.delete(spent);
          for (const produced of candidate.graph.produced)
            state.set(
              produced[Ledger.Columns.OUTREF].toString("hex"),
              Buffer.from(produced[Ledger.Columns.OUTPUT]),
            );
        }
      },
    );
    // Roots are content hashes, so one root is one entry.
    const roots = new Set<string>();
    let nodeCount = 0n;
    let materialBytes = 0n;
    return {
      admit: (index): BlockCheckVerdict => {
        const checked = own[index]!;
        if (!checked.ok)
          return {
            kind: "reject",
            code: COMMIT_REJECT_CODE_PROGRAM_COVERAGE,
            detail: commitStageFailureDetail(
              PROGRAM_COVERAGE_STAGE,
              checked.cause,
            ),
          };
        const added = new Map<string, bigint>();
        for (const entry of checked.material) {
          const root = Buffer.from(entry.root).toString("hex");
          if (!roots.has(root)) added.set(root, BigInt(entry.preimage.length));
        }
        const addedBytes = [...added.values()].reduce(
          (total, bytes) => total + bytes,
          0n,
        );
        if (
          nodeCount + BigInt(added.size) > limits.nodeCount ||
          materialBytes + addedBytes > limits.materialBytes
        )
          return {
            kind: "defer",
            detail: `${PROGRAM_COVERAGE_STAGE}: the block's merged program material would exceed ${limits.nodeCount.toString()} nodes or ${limits.materialBytes.toString()} bytes`,
          };
        for (const root of added.keys()) roots.add(root);
        nodeCount += BigInt(added.size);
        materialBytes += addedBytes;
        return { kind: "keep" };
      },
    };
  },
});
