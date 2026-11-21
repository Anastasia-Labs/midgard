import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardLedgerOutputProof,
  attachMidgardLedgerOutputProofFacts,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardDatum,
  demandedMidgardLedgerOutputProofSpan,
  encodeMidgardLedgerOutputCommitment,
  encodeMidgardLedgerOutputProofControl,
  encodeMidgardTxOutput,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  midgardLedgerOutputAttachWindowLength,
  type MidgardLedgerOutputProofControl,
  midgardLedgerOutputProofFactsAreExact,
  MidgardLedgerOutputProofResultKinds,
  type MidgardLedgerOutputProofWitness,
  midgardLedgerOutputWindowCovers,
  type MidgardTxOutput,
  terminalMidgardLedgerOutputDescriptor,
} from "../src/index.js";

/**
 * One successor per step for the two witness-free-by-derivation step kinds of
 * the ledger output proof: the span attach (its window is the consuming
 * stage's demanded span, admitted only while the recorded window fails to
 * cover it) and the fact attach (its commitments come from the terminal
 * control alone). Candidate witnesses still carry the retired
 * redeemer-chosen fields, which the step must ignore.
 */

const output: MidgardTxOutput = {
  address: Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]),
  value: {
    lovelace: 8_000_000n,
    assets: new Map([["55".repeat(28), new Map([["ff", 7n]])]]),
  },
  datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(5_000)), "hex")),
  script_ref: { language: "PlutusV3", scriptBytes: Buffer.alloc(6_000, 0x6b) },
};

const outputCbor = encodeMidgardTxOutput(output);
const trace = buildMidgardLedgerOutputProofTrace({
  outputIndex: 0,
  outputCbor,
});
const item = buildMidgardBoundedItem({
  fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  itemIndex: 0,
  bytes: outputCbor,
});

/** A span-attach witness aimed at `(start, length)`, the pre-fix shape. */
const aimedAttach = (
  start: number,
  length: number,
): MidgardLedgerOutputProofWitness => {
  const first = Math.floor(start / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
  const last = Math.floor(
    (start + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  return {
    kind: "spanAttach",
    start,
    length,
    chunkProof: buildMidgardBoundedItemChunkProof(item, first),
    nextChunkProof:
      last === first ? null : buildMidgardBoundedItemChunkProof(item, last),
  } as unknown as MidgardLedgerOutputProofWitness;
};

/** Every accepted span-attach successor of `control` over a spread of aims. */
const attachSuccessors = (
  control: MidgardLedgerOutputProofControl,
  around: number,
): Set<string> => {
  const successors = new Set<string>();
  const total = outputCbor.length;
  for (let start = Math.max(0, around - 64); start <= around + 8; start += 1) {
    if (start >= total) break;
    const lengths = new Set([
      midgardLedgerOutputAttachWindowLength(total, start),
      Math.min(64, total - start),
      1,
    ]);
    for (const length of lengths) {
      const result = advanceMidgardLedgerOutputProof({
        control,
        witness: aimedAttach(start, length),
      });
      if (result?.kind === MidgardLedgerOutputProofResultKinds.Advanced) {
        successors.add(
          encodeMidgardLedgerOutputProofControl(result.control).toString("hex"),
        );
      }
    }
  }
  return successors;
};

describe("ledger output proof one successor per step", () => {
  const attachSteps = trace.steps.filter(
    ({ witness }) => witness?.kind === "spanAttach",
  );

  it("span attach admits only the demanded window", () => {
    expect(attachSteps.length).toBeGreaterThanOrEqual(3);
    for (const step of attachSteps) {
      const demanded = demandedMidgardLedgerOutputProofSpan(step.control)!;
      expect(demanded).not.toBeNull();
      expect(step.next.spanWindow!.start).toBe(demanded.absoluteStart);
      expect(step.next.spanWindow!.length).toBe(
        midgardLedgerOutputAttachWindowLength(
          outputCbor.length,
          demanded.absoluteStart,
        ),
      );
      const successors = attachSuccessors(step.control, demanded.absoluteStart);
      expect([...successors]).toEqual([
        encodeMidgardLedgerOutputProofControl(step.next).toString("hex"),
      ]);
    }
  });

  it("span attach is refused while the recorded window covers the demand", () => {
    const covered = trace.steps.filter(({ control, witness }) => {
      if (witness?.kind === "spanAttach") return false;
      const demanded = demandedMidgardLedgerOutputProofSpan(control);
      return (
        demanded !== null &&
        midgardLedgerOutputWindowCovers({
          spanWindow: control.spanWindow,
          ...demanded,
        })
      );
    });
    expect(covered.length).toBeGreaterThan(0);
    for (const step of covered) {
      const demanded = demandedMidgardLedgerOutputProofSpan(step.control)!;
      expect(attachSuccessors(step.control, demanded.absoluteStart).size).toBe(
        0,
      );
    }
  });

  it("fact attach has one successor whatever descriptor is offered", () => {
    const descriptor = terminalMidgardLedgerOutputDescriptor(trace.terminal)!;
    const honestCbor = encodeMidgardLedgerOutputCommitment(descriptor);
    const forgedCbor = encodeMidgardLedgerOutputCommitment({
      ...descriptor,
      lovelace: descriptor.lovelace + 1n,
      referenceScriptTotalLength: descriptor.referenceScriptTotalLength + 1,
    });
    const attach = attachMidgardLedgerOutputProofFacts as (
      control: MidgardLedgerOutputProofControl,
      descriptorCbor?: Uint8Array,
    ) => MidgardLedgerOutputProofControl | null;
    let control = trace.terminal;
    for (let group = 0; group < 3; group += 1) {
      const honest = attach(control, honestCbor)!;
      const forged = attach(control, forgedCbor)!;
      expect(encodeMidgardLedgerOutputProofControl(forged)).toEqual(
        encodeMidgardLedgerOutputProofControl(honest),
      );
      control = honest;
    }
    expect(attach(control, honestCbor)).toBeNull();
    expect(midgardLedgerOutputProofFactsAreExact(control, honestCbor)).toBe(
      true,
    );
    expect(midgardLedgerOutputProofFactsAreExact(control, forgedCbor)).toBe(
      false,
    );
    expect(
      midgardLedgerOutputProofFactsAreExact(trace.terminal, honestCbor),
    ).toBe(false);
  });
});
