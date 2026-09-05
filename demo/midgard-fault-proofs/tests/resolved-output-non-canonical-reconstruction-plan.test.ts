import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  planResolvedOutputReconstructionTransition,
  prepareResolvedOutputNonCanonicalEvidence,
  RESOLVED_OUTPUT_CHUNK_BYTES,
  type ResolvedOutputEvidence,
  resolvedOutputScanControlData,
} from "../src/resolved-output-non-canonical/index.js";
import {
  descriptorFor,
  maximumCanonicalOutput,
  maximumMalformedOutput,
  smallCanonicalOutput,
  smallMalformedOutput,
} from "./support/resolved-output-non-canonical-emulator.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const priorTxId = "ab".repeat(32);
const priorRoot = "44".repeat(32);

const isCanonical = (output: Buffer): boolean => {
  try {
    buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: output,
    });
    return true;
  } catch {
    return false;
  }
};

const evidenceFor = (output: Buffer): ResolvedOutputEvidence => {
  const outRef = encodeMidgardSpendInputItem({
    txId: Buffer.from(priorTxId, "hex"),
    outputIndex: 0,
  });
  const nativeTx = makeNativeTx({ spendInputCbors: [outRef], fee: 7n });
  const transactionId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const coordinate = { sourceKind: 0, inputIndex: 0 } as const;
  const canonical = isCanonical(output);
  // A canonical output contradicts a forced rejection; a malformed one
  // contradicts an acceptance. Either way the evidence builder admits it.
  const subject = canonical
    ? forcedVerdictSubject({
        transactionId,
        sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
        rejectionReason: {
          InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
        },
      })
    : acceptedVerdictSubject(transactionId);
  const evidence = prepareResolvedOutputNonCanonicalEvidence({
    subject,
    coordinate,
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
    resolved: {
      priorRoot,
      transactionId: priorTxId,
      outputIndex: 0,
      descriptorCborHex: descriptorFor(0, output).toString("hex"),
      outputCborHex: output.toString("hex"),
      membershipProofCborHex: "80",
    },
  });
  expect(evidence.outputIsNonCanonical).toBe(!canonical);
  return evidence;
};

/** Replays the deterministic action selection over every checkpoint. */
const walk = (evidence: ResolvedOutputEvidence) => {
  const kinds: string[] = [];
  let terminal: { readonly outputIsNonCanonical: boolean } | undefined;
  // A canonical trace ends with the engine's finished terminal, which is
  // never a thread state: the finishable control before it closes the walk.
  // A malformed trace ends at the faulting control, which is one.
  for (const control of evidence.scanControls) {
    const transition = planResolvedOutputReconstructionTransition({
      evidence,
      control: resolvedOutputScanControlData(control),
    });
    kinds.push(transition.kind);
    if (transition.terminal) {
      terminal = transition;
      break;
    }
    // The next checkpoint the driver persists is the next authenticated one.
    expect(transition.nextControl).toBe(
      evidence.scanControls[evidence.scanControls.indexOf(control) + 1],
    );
    // The chunk proof always names the chunk that holds the cursor.
    expect(transition.action.Advance.chunk_proof.chunk_index).toBe(
      BigInt(Math.floor(control.cursor / RESOLVED_OUTPUT_CHUNK_BYTES)),
    );
  }
  return { kinds, terminal };
};

describe("resolvedOutputNonCanonical deterministic reconstruction actions", () => {
  it("closes a canonical output with FinalizeCanonical at the finishable control and never advances it", () => {
    for (const output of [smallCanonicalOutput(), maximumCanonicalOutput()]) {
      const evidence = evidenceFor(output);
      const { kinds, terminal } = walk(evidence);
      expect(kinds.at(-1)).toBe("finalize");
      expect(kinds.slice(0, -1).every((kind) => kind === "advance")).toBe(true);
      expect(terminal).toEqual(
        expect.objectContaining({ outputIsNonCanonical: false }),
      );
      // A prover cannot claim the same trace as non-canonical.
      const finishable =
        evidence.scanControls[evidence.scanControls.length - 2]!;
      expect(() =>
        planResolvedOutputReconstructionTransition({
          evidence: { ...evidence, outputIsNonCanonical: true },
          control: resolvedOutputScanControlData(finishable),
        }),
      ).toThrow(/closes canonical/u);
    }
  });

  it("closes a malformed output with Advance at the structural fault and never finalizes it", () => {
    for (const output of [smallMalformedOutput(), maximumMalformedOutput()]) {
      const evidence = evidenceFor(output);
      const { kinds, terminal } = walk(evidence);
      expect(kinds.every((kind) => kind === "advance")).toBe(true);
      expect(terminal).toEqual(
        expect.objectContaining({ outputIsNonCanonical: true }),
      );
      const last = evidence.scanControls.at(-1)!;
      expect(() =>
        planResolvedOutputReconstructionTransition({
          evidence: { ...evidence, outputIsNonCanonical: false },
          control: resolvedOutputScanControlData(last),
        }),
      ).toThrow(/stops at a structural fault/u);
    }
  });

  it("supplies the successor chunk exactly when the cursor's chunk is not the last", () => {
    const evidence = evidenceFor(maximumCanonicalOutput());
    const chunkCount = Math.ceil(16_384 / RESOLVED_OUTPUT_CHUNK_BYTES);
    for (const control of evidence.scanControls) {
      const transition = planResolvedOutputReconstructionTransition({
        evidence,
        control: resolvedOutputScanControlData(control),
      });
      if (transition.kind !== "advance") break;
      const chunkIndex = Math.floor(
        control.cursor / RESOLVED_OUTPUT_CHUNK_BYTES,
      );
      expect(transition.action.Advance.next_chunk_proof === null).toBe(
        chunkIndex + 1 >= chunkCount,
      );
    }
  });

  it("refuses a checkpoint the authenticated trace never produced", () => {
    const evidence = evidenceFor(smallCanonicalOutput());
    const [initial] = evidence.scanControls;
    expect(() =>
      planResolvedOutputReconstructionTransition({
        evidence,
        control: {
          ...resolvedOutputScanControlData(initial!),
          cursor: 1n,
        },
      }),
    ).toThrow(/outside authenticated trace/u);
  });
});
