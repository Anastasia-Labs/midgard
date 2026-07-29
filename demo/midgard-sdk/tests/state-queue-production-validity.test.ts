import { describe, expect, it } from "vitest";

import {
  isProductionCommitValidityInterval,
  PRODUCTION_COMMIT_MAX_VALIDITY_RANGE_MS,
  productionCommitHeaderMatchesValidityUpperBound,
} from "../src/state-queue-production.js";

describe("production commit validity binding", () => {
  it("accepts a bounded interval and binds the header to its inclusive upper bound", () => {
    const validFrom = 1_000_000;
    const validTo = validFrom + PRODUCTION_COMMIT_MAX_VALIDITY_RANGE_MS;

    expect(isProductionCommitValidityInterval({ validFrom, validTo })).toBe(
      true,
    );
    expect(
      productionCommitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(validTo - 1),
        validTo,
      }),
    ).toBe(true);
  });

  it("rejects absent, empty, unsafe, and overlong lower-bound intervals", () => {
    const validFrom = 1_000_000;

    expect(
      isProductionCommitValidityInterval({
        validFrom: Number.NaN,
        validTo: validFrom + 1,
      }),
    ).toBe(false);
    expect(
      isProductionCommitValidityInterval({
        validFrom,
        validTo: validFrom,
      }),
    ).toBe(false);
    expect(
      isProductionCommitValidityInterval({
        validFrom,
        validTo: validFrom + PRODUCTION_COMMIT_MAX_VALIDITY_RANGE_MS + 1,
      }),
    ).toBe(false);
  });

  it("rejects a header end-time that differs from validTo minus one", () => {
    const validTo = 1_480_000;

    expect(
      productionCommitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(validTo),
        validTo,
      }),
    ).toBe(false);
  });
});
