import {
  BoundedBlobFrontierPeakSchema,
  FrontierPeakSchema,
  MinAdaOutputScanControlSchema,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { OutputReferenceOutputControlSchema } from "../src/output-reference-script-decoding/schemas.js";
import { ResolvedOutputScanControlSchema } from "../src/resolved-output-non-canonical/schemas.js";

/** A frontier peak whose hash is 20 bytes, not the 32 this side builds. */
const shortPeak = new Constr(0, [1n, "aa".repeat(20)]);

/**
 * An output scan control, in field order, carrying one short peak: every
 * control below declares the same 23 fields with `asset_peaks` at 15.
 */
const control = new Constr(0, [
  ...[0n, 0n, 0n, 0n, 0n, "", 0n, 0n, 0n, 0n, 0n, "", "", "", 0n],
  [shortPeak],
  ...[0n, 0n, 0n, 0n, 0n, 0n, 0n],
]);

/**
 * Sharing a twin must never tighten a decoder: a loose schema reads chain
 * data and admits any peak hash, a strict one builds 32-byte peaks only.
 */
const CASES = [
  {
    name: "SDK FrontierPeakSchema",
    schema: FrontierPeakSchema,
    value: shortPeak,
    admits: true,
  },
  {
    name: "BoundedBlobFrontierPeakSchema",
    schema: BoundedBlobFrontierPeakSchema,
    value: shortPeak,
    admits: false,
  },
  {
    name: "ResolvedOutputScanControlSchema",
    schema: ResolvedOutputScanControlSchema,
    value: control,
    admits: true,
  },
  {
    name: "OutputReferenceOutputControlSchema",
    schema: OutputReferenceOutputControlSchema,
    value: control,
    admits: true,
  },
  {
    name: "MinAdaOutputScanControlSchema",
    schema: MinAdaOutputScanControlSchema,
    value: control,
    admits: false,
  },
];

describe("frontier peak strictness of the shared wire twins", () => {
  it.each(CASES)(
    "$name admits a 20-byte peak hash: $admits",
    ({ schema, value, admits }) => {
      const decode = () => Data.from(Data.to(value), schema as never);
      if (admits) expect(decode).not.toThrow();
      else expect(decode).toThrow();
    },
  );
});
