import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  finishMidgardLedgerOutputScan,
  type MidgardBoundedItemChunkProof,
  type MidgardLedgerOutputScanControl,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import {
  RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY,
  type ResolvedOutputEvidence,
  resolvedOutputScanControlData,
} from "./resolved-output-non-canonical.js";
import { ResolvedOutputScanControlSchema } from "./schemas.js";

/** `bounded_item_v1.chunk_bytes`: the on-chain output chunk domain. */
export const RESOLVED_OUTPUT_CHUNK_BYTES = 4_095;

const fail = (message: string): never => {
  throw new Error(`${RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY}: ${message}`);
};

export const resolvedOutputChunkProofData = (
  proof: MidgardBoundedItemChunkProof,
) => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_index: BigInt(proof.itemIndex),
  total_length: BigInt(proof.totalLength),
  chunk_index: BigInt(proof.chunkIndex),
  chunk: proof.chunk.toString("hex"),
  frontier: proof.frontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: hash.toString("hex"),
  })),
  siblings: proof.siblings.map((hash) => hash.toString("hex")),
});

export type ResolvedOutputAdvanceAction = {
  readonly Advance: {
    readonly chunk_proof: ReturnType<typeof resolvedOutputChunkProofData>;
    readonly next_chunk_proof: ReturnType<
      typeof resolvedOutputChunkProofData
    > | null;
  };
};

/**
 * The one deterministic step-04 transition from an authenticated checkpoint.
 * `advance` continues the self-loop or, at a structural fault, closes the
 * thread as non-canonical; `finalize` closes it as canonical at the control
 * the engine's zero-byte closing edge accepts. The choice is a pure function
 * of the retained output and the on-chain control: the evidence's own
 * verdict must agree with it, never override it.
 */
export type ResolvedOutputReconstructionTransition =
  | {
      readonly kind: "advance";
      readonly action: ResolvedOutputAdvanceAction;
      readonly terminal: false;
      readonly nextControl: MidgardLedgerOutputScanControl;
    }
  | {
      readonly kind: "advance";
      readonly action: ResolvedOutputAdvanceAction;
      readonly terminal: true;
      readonly outputIsNonCanonical: true;
    }
  | {
      readonly kind: "finalize";
      readonly action: "FinalizeCanonical";
      readonly terminal: true;
      readonly outputIsNonCanonical: false;
    };

const encodeControl = (control: unknown): string =>
  Data.to(control as never, ResolvedOutputScanControlSchema as never);

/** Index of the on-chain control inside the authenticated scan trace. */
export const locateResolvedOutputScanControl = (
  evidence: ResolvedOutputEvidence,
  control: unknown,
): number => {
  const encoded = encodeControl(control);
  return evidence.scanControls.findIndex(
    (candidate) =>
      encodeControl(resolvedOutputScanControlData(candidate)) === encoded,
  );
};

export const planResolvedOutputReconstructionTransition = ({
  evidence,
  control,
}: {
  readonly evidence: ResolvedOutputEvidence;
  /** The scan control the live step-04 thread carries (decoded datum data). */
  readonly control: unknown;
}): ResolvedOutputReconstructionTransition => {
  const index = locateResolvedOutputScanControl(evidence, control);
  if (index < 0)
    return fail("reconstruction checkpoint is outside authenticated trace");
  const current = evidence.scanControls[index]!;
  const outputBytes = Buffer.from(evidence.resolved.outputCborHex, "hex");
  if (
    finishMidgardLedgerOutputScan({
      control: current,
      totalLength: outputBytes.length,
    }) !== null
  ) {
    if (evidence.outputIsNonCanonical)
      return fail(
        "evidence claims a non-canonical output but its reconstruction closes canonical",
      );
    return {
      kind: "finalize",
      action: "FinalizeCanonical",
      terminal: true,
      outputIsNonCanonical: false,
    };
  }
  const item =
    evidence.canonicalTrace?.item ??
    buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: evidence.resolved.outputIndex,
      bytes: outputBytes,
    });
  const chunkIndex = Math.floor(current.cursor / RESOLVED_OUTPUT_CHUNK_BYTES);
  const nextChunkIndex =
    chunkIndex + 1 < item.chunkHashes.length ? chunkIndex + 1 : null;
  const action: ResolvedOutputAdvanceAction = {
    Advance: {
      chunk_proof: resolvedOutputChunkProofData(
        buildMidgardBoundedItemChunkProof(item, chunkIndex),
      ),
      next_chunk_proof:
        nextChunkIndex === null
          ? null
          : resolvedOutputChunkProofData(
              buildMidgardBoundedItemChunkProof(item, nextChunkIndex),
            ),
    },
  };
  const nextControl = evidence.scanControls[index + 1];
  if (nextControl === undefined) {
    if (!evidence.outputIsNonCanonical)
      return fail(
        "evidence claims a canonical output but its reconstruction stops at a structural fault",
      );
    return {
      kind: "advance",
      action,
      terminal: true,
      outputIsNonCanonical: true,
    };
  }
  return { kind: "advance", action, terminal: false, nextControl };
};
