/**
 * The resolved-inputs accumulator and the input-resolution schedule hashes.
 */

import { encodeCbor } from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";

export const ZERO_32 = Buffer.alloc(32);

const RESOLVED_INPUTS_ACCUMULATOR_DOMAIN = Buffer.from(
  "MidgardResolvedInputsAccumulatorV1",
  "ascii",
);

const INPUT_RESOLUTION_SCHEDULE_DOMAIN = Buffer.from(
  "MidgardInputResolutionScheduleV1",
  "ascii",
);

export const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(Buffer.from(bytes), { dkLen: 32 }));

export const initialMidgardResolvedInputsAccumulator = (): Buffer =>
  hash32(RESOLVED_INPUTS_ACCUMULATOR_DOMAIN);

export const emptyMidgardInputResolutionSchedule = (): Buffer =>
  hash32(INPUT_RESOLUTION_SCHEDULE_DOMAIN);

export const prependMidgardInputResolutionSchedule = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly nextHash: Uint8Array;
}): Buffer => {
  if (input.nextHash.length !== 32) {
    throw new Error("input-resolution schedule hash must contain 32 bytes");
  }
  return hash32(
    Buffer.concat([
      INPUT_RESOLUTION_SCHEDULE_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      Buffer.from(input.nextHash),
    ]),
  );
};

export const advanceMidgardResolvedInputsAccumulator = (input: {
  readonly accumulator: Uint8Array;
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly value: Uint8Array;
}): Buffer => {
  if (input.accumulator.length !== 32) {
    throw new Error("resolved-input accumulator must contain exactly 32 bytes");
  }
  return hash32(
    Buffer.concat([
      RESOLVED_INPUTS_ACCUMULATOR_DOMAIN,
      Buffer.from(input.accumulator),
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      encodeCbor(Buffer.from(input.value)),
    ]),
  );
};
