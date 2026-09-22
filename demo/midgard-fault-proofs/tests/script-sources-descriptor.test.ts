import {
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import {
  redeemerItemControlData,
  redeemerItemProofWitnessData,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { scriptSourcesDescriptorClaim } from "../src/validation-dispute/script-sources-descriptor.js";

const evidence = (
  step: ReturnType<typeof buildMidgardRedeemerItemProofTrace>["steps"][number],
) =>
  new Constr(18, [
    new Constr(1, []),
    redeemerItemControlData(step.control),
    redeemerItemProofWitnessData(step.witness),
  ]);
it.each([1, 4090, 4091, 4092, 4093, 8192, 32750])(
  "derives exact descriptor header/tail claims over %i Data bytes",
  (length) => {
    const trace = buildMidgardRedeemerItemProofTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: encodeCbor([0n, 0n, Buffer.alloc(length), [10n, 20n]]),
      mode: MidgardRedeemerItemProofModes.Descriptor,
    });
    expect(trace.steps).toHaveLength(2);
    for (const step of trace.steps) {
      const claim = scriptSourcesDescriptorClaim(evidence(step));
      const next = Data.from(
        Data.to<unknown>(redeemerItemControlData(step.next)),
      );
      if (!(next instanceof Constr)) throw new Error("Invalid test control");
      expect(claim.fields[4]).toEqual(new Constr(0, next.fields.slice(0, 15)));
      expect(claim.fields[1]).toEqual(
        new Constr(step.witness.action.kind === "openTail" ? 1 : 0, []),
      );
    }
  },
);
it("refuses a substituted authenticated source and missing proof", () => {
  const trace = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([0n, 0n, Buffer.from([0]), [10n, 20n]]),
    mode: MidgardRedeemerItemProofModes.Descriptor,
  });
  const step = trace.steps[0]!;
  const proof = step.witness.chunkProof!;
  const changed = {
    ...step,
    witness: {
      ...step.witness,
      chunkProof: { ...proof, chunk: Buffer.alloc(proof.chunk.length, 255) },
    },
  };
  expect(() => scriptSourcesDescriptorClaim(evidence(changed))).toThrow();
  expect(() =>
    scriptSourcesDescriptorClaim(
      evidence({ ...step, witness: { ...step.witness, chunkProof: null } }),
    ),
  ).toThrow();
});
