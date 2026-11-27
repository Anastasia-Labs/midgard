import { expect } from "vitest";

import { type Measurement } from "./mint-declared-asset-limit-lifecycle.registered-contracts.js";

export const expectPositiveMargins = (
  rows: readonly (readonly [string, Measurement])[],
  publicationOnly: readonly string[] = [],
) => {
  for (const [label, measurement] of rows) {
    expect(measurement.l1ByteMargin, label).toBeGreaterThan(0);
    if (!publicationOnly.includes(label)) {
      expect(measurement.executionMemory, label).toBeGreaterThan(0n);
      expect(measurement.executionSteps, label).toBeGreaterThan(0n);
    }
  }
};
