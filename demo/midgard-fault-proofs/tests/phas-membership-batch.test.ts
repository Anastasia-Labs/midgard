import { describe, expect, it } from "vitest";

import { TransitionTraceChallengerError } from "../src/transition-trace/errors.js";
import {
  keyValuePhasMembershipProofs,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";

/**
 * Refusal categories are asserted, not merely "something threw": an absent
 * key/value must be rejected before any proof is built
 * (`missingWitnessData`), while an intact entry set opened against a foreign
 * root must be rejected by the per-opening root check
 * (`proofConstructionFailed`). A bare `rejects.toThrow()` cannot tell those
 * two refusals apart, nor distinguish either from an incidental crash.
 */
const expectPhasRefusal = async (
  operation: Promise<unknown>,
  code: "missingWitnessData" | "proofConstructionFailed",
  message: string,
): Promise<void> => {
  const caught: unknown = await operation.then(
    (value) => value,
    (cause: unknown) => cause,
  );
  expect(caught, "refusal must throw a challenger error").toBeInstanceOf(
    TransitionTraceChallengerError,
  );
  const error = caught as TransitionTraceChallengerError;
  expect(error.code).toBe(code);
  expect(error.message).toBe(message);
};

describe("batched exact PHAS membership", () => {
  it("matches individual openings in requested order, including duplicates", async () => {
    const entries = Array.from({ length: 17 }, (_, index) => ({
      key: Buffer.from([index]),
      value: Buffer.from([index + 32]),
    }));
    const root = await keyValuePhasRootWithCount(entries);
    const requested = [entries[16]!, entries[0]!, entries[8]!, entries[16]!];
    expect(await keyValuePhasMembershipProofs(root, requested)).toEqual(
      await Promise.all(
        requested.map(({ key, value }) => keyValuePhasProof(root, key, value)),
      ),
    );
    await expectPhasRefusal(
      keyValuePhasMembershipProofs(root, [
        { ...entries[0]!, value: Buffer.from([255]) },
      ]),
      "missingWitnessData",
      "Cannot build PHAS membership proof for an absent key/value.",
    );
    await expectPhasRefusal(
      keyValuePhasMembershipProofs(root, [
        { key: Buffer.from([255]), value: entries[0]!.value },
      ]),
      "missingWitnessData",
      "Cannot build PHAS membership proof for an absent key/value.",
    );
    await expectPhasRefusal(
      keyValuePhasMembershipProofs(
        { ...root, root: "ff".repeat(32) },
        requested,
      ),
      "proofConstructionFailed",
      "Generated PHAS membership proof does not open committed root.",
    );
    expect(await keyValuePhasMembershipProofs(root, [])).toEqual([]);
  });
});
