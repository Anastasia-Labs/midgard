import {
  hashMidgardValidationWorkWitness,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardLedgerOutputProofFactDigest,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { deriveLedgerOutputProofFinalizeClaims } from "../../../src/ledger-output-proof-plan.js";

/**
 * The dishonest ledger-output-proof successors a challenger can claim from an
 * honest pre-state:
 *
 * - `versionFlip`: the honest successor with its leading small-int item
 *   flipped, a well-encoded continuation no step produces.
 * - `offDemandSpanWindow`: at a span-attach step, the successor records a
 *   window one byte before the span the consuming stage demands, sized and
 *   digested exactly as an honest attach at that start would be. An attach
 *   that took its window from the redeemer admitted it.
 * - `descriptorBoundLeafFacts`: at the leaf-summary fact-attach step, the
 *   successor records the datum and value facts committed together with a
 *   descriptor of the prover's choosing (`#"00"`). A fact attach that took
 *   its descriptor from the redeemer admitted it.
 */
export type LedgerOutputProofSuccessorForgery =
  | "versionFlip"
  | "offDemandSpanWindow"
  | "descriptorBoundLeafFacts";

const NONE = "d87a80";
const FACT_BYTES = 38; // d8799f 5820 <32> ff

const uintAt = (bytes: Buffer, at: number): [bigint, number] | null => {
  const head = bytes[at];
  if (head === undefined || head >> 5 !== 0) return null;
  const info = head & 0x1f;
  if (info < 24) return [BigInt(info), at + 1];
  const width = info === 24 ? 1 : info === 25 ? 2 : info === 26 ? 4 : 0;
  if (width === 0) return null;
  return [
    BigInt(`0x${bytes.subarray(at + 1, at + 1 + width).toString("hex")}`),
    at + 1 + width,
  ];
};

const encodeUint = (value: number): Buffer =>
  Buffer.from(Data.to(BigInt(value)), "hex");

/** The successor control with an off-demand span window in place. */
const offDemandSpanWindow = (control: Buffer, chunk: Buffer): Buffer => {
  const windowEnd = control.length - 4 * (NONE.length / 2);
  if (control.subarray(windowEnd).toString("hex") !== NONE.repeat(4)) {
    throw new Error("span-attach successor must carry no facts");
  }
  for (
    let at = control.indexOf("d8799f83", 0, "hex");
    at >= 0;
    at = control.indexOf("d8799f83", at + 1, "hex")
  ) {
    const start = uintAt(control, at + 4);
    const length = start === null ? null : uintAt(control, start[1]);
    if (length === null || start === null) continue;
    const digestAt = length[1] + 2;
    if (
      control.readUInt16BE(length[1]) !== 0x5820 ||
      digestAt + 33 !== windowEnd ||
      control[windowEnd - 1] !== 0xff
    ) {
      continue;
    }
    const forgedStart = Number(start[0]) - 1;
    const forgedLength = Math.min(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
      chunk.length - forgedStart,
    );
    const head = Buffer.concat([
      encodeUint(forgedStart),
      encodeUint(forgedLength),
    ]);
    if (forgedStart < 0 || head.length !== length[1] - (at + 4)) {
      throw new Error("off-demand span window must keep the frame width");
    }
    const digest = Effect.runSync(
      SDK.hashHexWithBlake2b(
        chunk.subarray(forgedStart, forgedStart + forgedLength).toString("hex"),
        32,
      ),
    );
    const forged = Buffer.from(control);
    head.copy(forged, at + 4);
    Buffer.from(digest, "hex").copy(forged, digestAt);
    return forged;
  }
  throw new Error("span-attach successor records no span window");
};

/** The successor control with descriptor-bound leaf facts in place. */
const descriptorBoundLeafFacts = (control: Buffer): Buffer => {
  const leafAt = control.length - 2 * FACT_BYTES;
  if (control.subarray(leafAt - 6, leafAt).toString("hex") !== NONE.repeat(2)) {
    throw new Error(
      `leaf-summary attach successor must carry only leaf facts: ${control.subarray(-4 * FACT_BYTES).toString("hex")}`,
    );
  }
  const items = Data.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(control.toString("hex")),
  );
  if (!Array.isArray(items))
    throw new Error("output proof control must be a list");
  const { claimedValueSummary, claimedDatumSummary } =
    deriveLedgerOutputProofFinalizeClaims(items);
  const bound = (summary: Data) =>
    midgardLedgerOutputProofFactDigest([
      Buffer.from("4100", "hex"),
      Buffer.from(Data.to(summary), "hex"),
    ]);
  const forged = Buffer.from(control);
  bound(claimedDatumSummary).copy(forged, leafAt + 5);
  bound(claimedValueSummary).copy(forged, leafAt + FACT_BYTES + 5);
  return forged;
};

/**
 * Replace the adjacent successor of `disputedLowIndex` in the operator trace
 * with a dishonest ledger-output-proof continuation and recompute its work
 * root, so the evidence builder's successor gate passes and refusal reaches
 * the on-chain step. The carrier holds constr items the Midgard test codec
 * refuses, so the control is located through Lucid's Data decode and patched
 * at the byte level in place: every forgery keeps the control's width, so
 * every enclosing length header still holds.
 */
export const forgeLedgerOutputProofSuccessor = ({
  trace,
  disputedLowIndex,
  pendingInputCarrier,
  forgery,
  operatorStates,
  operatorWitnesses,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly disputedLowIndex: number;
  /** ResolveInputs carries the control in its pending input (item 9, 4). */
  readonly pendingInputCarrier: boolean;
  readonly forgery: LedgerOutputProofSuccessorForgery;
  readonly operatorStates: DeterministicValidationMachineTrace["states"][number][];
  readonly operatorWitnesses: DeterministicValidationMachineTrace["witnesses"][number][];
}): void => {
  const successorIndex = disputedLowIndex + 1;
  const adjacent = operatorWitnesses[successorIndex]!;
  const carrierHex = adjacent.cbor.toString("hex");
  const carrierItems = Data.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(carrierHex),
  );
  if (!Array.isArray(carrierItems)) {
    throw new Error("output proof successor carrier must be a list");
  }
  const pending = (): Data => {
    const pendingHex = carrierItems[9];
    if (typeof pendingHex !== "string") {
      throw new Error("pending input item must be bytes");
    }
    const pendingItems = Data.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(pendingHex),
    );
    if (!Array.isArray(pendingItems)) {
      throw new Error("pending input must be a list");
    }
    return pendingItems[4]!;
  };
  const controlHex = pendingInputCarrier ? pending() : carrierItems[30];
  if (typeof controlHex !== "string") {
    throw new Error("output proof control item must be bytes");
  }
  let controlAt = carrierHex.indexOf(controlHex);
  while (controlAt >= 0 && controlAt % 2 !== 0) {
    controlAt = carrierHex.indexOf(controlHex, controlAt + 1);
  }
  if (controlAt < 0) {
    throw new Error(
      "output proof control bytes not found in the successor carrier",
    );
  }
  const controlStart = controlAt / 2;
  const control = Buffer.from(controlHex, "hex");
  // The control is a 17-item raw frame (header 0x91) whose first item is a
  // single-byte small integer.
  if (control[0] !== 0x91 || control[1]! > 0x17) {
    throw new Error(
      "output proof control does not open with a 17-item frame and small-int item",
    );
  }
  const forged = (() => {
    switch (forgery) {
      case "versionFlip": {
        const flipped = Buffer.from(control);
        flipped[1] = flipped[1]! ^ 0x01;
        return flipped;
      }
      case "offDemandSpanWindow": {
        const auxiliary = trace.witnesses[disputedLowIndex]!.auxiliary;
        const witness =
          auxiliary?.kind === "ledgerOutputProofStep"
            ? auxiliary.witness
            : null;
        if (
          witness?.kind !== "spanAttach" ||
          witness.chunkProof.chunkIndex !== 0 ||
          witness.chunkProof.chunk.length !== witness.chunkProof.totalLength
        ) {
          throw new Error(
            "off-demand span window needs a single-chunk span-attach step",
          );
        }
        return offDemandSpanWindow(control, witness.chunkProof.chunk);
      }
      case "descriptorBoundLeafFacts":
        return descriptorBoundLeafFacts(control);
    }
  })();
  if (forged.equals(control) || forged.length !== control.length) {
    throw new Error("forged output proof control must differ at equal width");
  }
  const cbor = Buffer.from(adjacent.cbor);
  forged.copy(cbor, controlStart);
  operatorWitnesses[successorIndex] = { ...adjacent, cbor };
  operatorStates[successorIndex] = {
    ...operatorStates[successorIndex]!,
    workRoot: hashMidgardValidationWorkWitness({
      phase: adjacent.phase,
      programCounter: adjacent.programCounter,
      witnessCbor: cbor,
    }),
  };
};
