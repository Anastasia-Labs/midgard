import {
  encodeMidgardCekDataFrame,
  hashMidgardCekDataFrame,
  hashMidgardValidationWorkWitness,
  initialMidgardCekDataListFrame,
  initialMidgardCekDataSmallConstrFrame,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardCekDataFrame,
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
 * - `openFrameChildCount`: at a datum head-sequence step, the successor
 *   pushes the honest open-ended frame with a nonzero `expected_children`.
 *   A head that took the child count from the redeemer admitted it; the
 *   derived head pins it to zero and leaves the close to the authenticated
 *   break.
 */
export type LedgerOutputProofSuccessorForgery =
  | "versionFlip"
  | "offDemandSpanWindow"
  | "descriptorBoundLeafFacts"
  | "openFrameChildCount";

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

const FRAME_DOMAIN_HEX = Buffer.from("MidgardCekDataFrameV1", "ascii").toString(
  "hex",
);

/**
 * The datum traversal control the output proof control carries
 * (`Some(control)`: `d8799f` + the 9-item control `89 01 …` + `ff`): where its
 * frame root sits and what it holds.
 */
const datumFrameRoot = (
  control: Buffer,
): { readonly at: number; readonly root: Buffer } => {
  const marker = Buffer.from("d8799f8901", "hex");
  for (
    let at = control.indexOf(marker);
    at >= 0;
    at = control.indexOf(marker, at + 1)
  ) {
    let cursor: [bigint, number] | null = [0n, at + marker.length];
    for (let field = 0; field < 4 && cursor !== null; field += 1) {
      cursor = uintAt(control, cursor[1]);
    }
    if (cursor === null) continue;
    const rootAt = cursor[1];
    if (control[rootAt] === 0x40) {
      return { at: rootAt + 1, root: Buffer.alloc(0) };
    }
    if (control.readUInt16BE(rootAt) === 0x5820) {
      return {
        at: rootAt + 2,
        root: control.subarray(rootAt + 2, rootAt + 34),
      };
    }
  }
  throw new Error("output proof control carries no datum traversal control");
};

/**
 * The successor control whose pushed open-ended frame records `children`
 * expected children. The honest frame is recovered from the pre-state frame
 * root (its tail); the forged frame differs only in `expected_children`, at
 * equal width, so only the head's derived frame can refuse it.
 */
const openFrameChildCount = (
  preControl: Buffer,
  control: Buffer,
  children: number,
): Buffer => {
  const tail = datumFrameRoot(preControl).root;
  const pushed = datumFrameRoot(control);
  if (pushed.root.length !== 32) {
    throw new Error("head-sequence successor must push a frame");
  }
  const candidates: MidgardCekDataFrame[] = [
    initialMidgardCekDataListFrame({ tail }),
    ...Array.from({ length: 7 }, (_, constructor) =>
      initialMidgardCekDataSmallConstrFrame({
        constructor: BigInt(constructor),
        tail,
      }),
    ),
  ];
  const honest = candidates.find((frame) =>
    Buffer.from(hashMidgardCekDataFrame(frame)).equals(pushed.root),
  );
  if (honest === undefined) {
    throw new Error("head-sequence successor pushes no open-ended frame");
  }
  const encoded = encodeMidgardCekDataFrame(honest);
  const tailCbor = Buffer.from(
    tail.length === 0 ? "40" : `5820${tail.toString("hex")}`,
    "hex",
  );
  // tail, expected_children 0, child_count 0, no peaks, fold cursor 0
  const at = encoded.indexOf(
    Buffer.concat([tailCbor, Buffer.from("00008000", "hex")]),
  );
  if (at < 0 || children <= 0 || children > 0x17) {
    throw new Error("open-ended frame must record a zero child count");
  }
  const forgedFrame = Buffer.from(encoded);
  forgedFrame[at + tailCbor.length] = children;
  const forgedRoot = Effect.runSync(
    SDK.hashHexWithBlake2b(
      `${FRAME_DOMAIN_HEX}${forgedFrame.toString("hex")}`,
      32,
    ),
  );
  const forged = Buffer.from(control);
  Buffer.from(forgedRoot, "hex").copy(forged, pushed.at);
  return forged;
};

/** The output proof control inside a work-witness carrier and its offset. */
const carriedControl = (
  carrier: Buffer,
  pendingInputCarrier: boolean,
): { readonly control: Buffer; readonly start: number } => {
  const carrierHex = carrier.toString("hex");
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
  return { control: Buffer.from(controlHex, "hex"), start: controlAt / 2 };
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
  const { control, start: controlStart } = carriedControl(
    Buffer.from(adjacent.cbor),
    pendingInputCarrier,
  );
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
      case "openFrameChildCount": {
        const auxiliary = trace.witnesses[disputedLowIndex]!.auxiliary;
        if (
          auxiliary?.kind !== "ledgerOutputProofStep" ||
          auxiliary.witness?.kind !== "datum" ||
          auxiliary.witness.action?.kind !== "headSequence"
        ) {
          throw new Error(
            "open-frame child count needs a datum head-sequence step",
          );
        }
        return openFrameChildCount(
          carriedControl(
            Buffer.from(operatorWitnesses[disputedLowIndex]!.cbor),
            pendingInputCarrier,
          ).control,
          control,
          1,
        );
      }
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
