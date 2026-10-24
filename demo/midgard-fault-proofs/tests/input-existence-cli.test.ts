/** Current stage artifact/reference interface. This separate unit registration
 * keeps the existing CLI suite at its frozen 804-line cap. */
import { describe, expect, it } from "vitest";

import { parseArgs } from "../src/bin.parse-args.js";
import { parseInputExistenceFileReferences } from "../src/input-existence/submit-from-files.js";
// Regressions: the new stage interface must carry a full artifact and the
// complete real reference set. Parser-level cases fit this existing CLI suite.
describe("current input-existence stage CLI", () => {
  it.each(["01", "02", "03", "04"])(
    "parses stage %s artifact and reference files",
    (stage) => {
      const args = parseArgs([
        "node",
        "midgard-fault-proofs",
        `submit-no-reference-input-step-${stage}`,
        "--input-existence-artifact",
        "source.json",
        "--input-existence-references",
        "references.json",
      ]);
      expect(args.inputExistenceArtifactPath).toBe("source.json");
      expect(args.inputExistenceReferencesPath).toBe("references.json");
    },
  );
  it("refuses missing, extra and noncanonical reference material", () => {
    const outref = "11".repeat(32) + "#0";
    const refs = {
      steps: [outref, outref, outref, outref],
      computationThreadMint: outref,
      fraudProofMint: outref,
      phasMembershipWithdraw: outref,
      chunkedVerifyWithdraw: outref,
      pexcludesWithdraw: outref,
      fieldCarriage: [outref],
      certificatePolicyId: null,
    };
    expect(parseInputExistenceFileReferences(refs).steps).toHaveLength(4);
    expect(() =>
      parseInputExistenceFileReferences({
        ...refs,
        steps: refs.steps.slice(1),
      }),
    ).toThrow(/four step references/);
    expect(() =>
      parseInputExistenceFileReferences({ ...refs, extra: outref }),
    ).toThrow();
    expect(() =>
      parseInputExistenceFileReferences({
        ...refs,
        certificatePolicyId: "AB".repeat(28),
      }),
    ).toThrow(/canonical policy id/);
    expect(() =>
      parseInputExistenceFileReferences({
        ...refs,
        fieldCarriage: ["not-an-outref"],
      }),
    ).toThrow();
  });
});
