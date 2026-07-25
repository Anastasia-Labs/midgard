import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { readValidationDisputeCborFile } from "../src/validation-dispute/from-files.js";
import {
  validationDisputeTimeoutValidityRange,
  validationDisputeValidityRange,
} from "../src/validation-dispute/submit.js";

describe("validation-dispute transaction validity", () => {
  it("uses a bounded closed range with the validator timestamp at its upper bound", () => {
    expect(validationDisputeValidityRange(1_000_000)).toEqual({
      validFrom: 940_000,
      validTo: 1_060_000,
    });
  });

  it("places timeout lower bound strictly after the response deadline", () => {
    expect(validationDisputeTimeoutValidityRange(1_000_000, 990_000)).toEqual({
      validFrom: 990_001,
      validTo: 1_060_000,
    });
    expect(() =>
      validationDisputeTimeoutValidityRange(1_000_000, 1_000_000),
    ).toThrow(/has not passed/);
  });

  it("reads exact lowercase CBOR files and rejects ambiguous wrappers", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-dispute-cbor-"));
    const rawPath = join(directory, "raw.cbor");
    const wrappedPath = join(directory, "wrapped.json");
    const ambiguousPath = join(directory, "ambiguous.json");
    await Promise.all([
      writeFile(rawPath, "d87980\n"),
      writeFile(wrappedPath, '{"cborHex":"d87980"}\n'),
      writeFile(ambiguousPath, '{"cborHex":"d87980","unexpected":true}\n'),
    ]);
    await expect(
      readValidationDisputeCborFile(rawPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(wrappedPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(ambiguousPath, "fixture"),
    ).rejects.toThrow(/exactly a cborHex field/u);
  });
});
