/** The redeemer item step our runner plans for a CEK context step of the item
 * bind stage, over item proofs of a data item in each mode: where the item
 * return routes the step at each context stage, the result the route's hash
 * or finalize derives, and the pending step's claimed next control and
 * witness hash; and its refusal of evidence that is no item step. */
import {
  buildMidgardRedeemerItemProofTrace,
  computeHash32,
  encodeCbor,
  finalizeMidgardCekDataTraverse,
  hashMidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofMode,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import {
  redeemerItemControlData,
  redeemerItemProofWitnessData,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { planValidationTraceContextItem } from "../src/validation-trace-dispute/one-step-context-item.js";

const steps = (mode: MidgardRedeemerItemProofMode) =>
  buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([
      0n,
      0n,
      Buffer.from("d8799f0102ff", "hex"),
      [10n, 20n],
    ]),
    mode,
  }).steps;

type Step = ReturnType<typeof steps>[number];

const controlData = (control: Step["control"]): Data =>
  Data.from(Data.to<unknown>(redeemerItemControlData(control)));

const witnessData = (step: Step): Data =>
  Data.from(Data.to<unknown>(redeemerItemProofWitnessData(step.witness)));

/** The item auxiliary of `step`, with `witness` in place of its own. */
const auxiliary = (step: Step, witness: Data = witnessData(step)) =>
  Data.to(
    new Constr(18, [new Constr(1, []), controlData(step.control), witness]),
  );

const plan = (step: Step, contextStage: bigint) =>
  planValidationTraceContextItem({
    auxiliaryCbor: auxiliary(step),
    contextStage,
  });

const terminal = (step: Step) =>
  step.next.stage === MidgardRedeemerItemProofStages.Terminal;

describe("validation-trace one-step CEK context item step plan", () => {
  it("routes a selection scan's step to its hash while the item proof continues, and to the finish when it ends", () => {
    const descriptor = steps(MidgardRedeemerItemProofModes.Descriptor);
    expect(descriptor.filter(terminal)).toHaveLength(1);
    for (const step of descriptor) {
      const planned = plan(step, 0n);
      if (terminal(step)) {
        expect(planned.route).toBe("contextItemSelectionFinish");
        expect(planned.resultCbor).toBeUndefined();
      } else {
        expect(planned.route).toBe("contextItemSelectionHash");
        expect(planned.resultCbor).toBe(
          Data.to(
            hashMidgardRedeemerItemProofControl(step.next).toString("hex"),
          ),
        );
      }
    }
  });

  it("routes a data scan's last step to finish a descriptor or finalize a value, by the item proof's mode", () => {
    const descriptor = steps(MidgardRedeemerItemProofModes.Descriptor);
    const data = steps(MidgardRedeemerItemProofModes.Data);
    expect(plan(descriptor.at(-1)!, 9n).route).toBe(
      "contextItemDataFinishDescriptor",
    );
    expect(plan(descriptor.at(-1)!, 9n).resultCbor).toBeUndefined();
    const last = data.at(-1)!;
    expect(terminal(last)).toBe(true);
    const summary = finalizeMidgardCekDataTraverse(last.next.traversal!)!;
    expect(plan(last, 9n)).toMatchObject({
      route: "contextItemFinalize",
      resultCbor: Data.to(
        new Constr(0, [
          Buffer.from(summary.root).toString("hex"),
          summary.cborLength,
          summary.memory,
        ]),
      ),
    });
    for (const step of data.slice(0, -1))
      expect(plan(step, 9n).route).toBe("contextItemDataHash");
  });

  it("claims the step's canonical next control and binds its witness by hash", () => {
    for (const step of steps(MidgardRedeemerItemProofModes.Data)) {
      const planned = plan(step, 9n);
      expect(planned.claimedNextCbor).toBe(
        Data.to<unknown>(redeemerItemControlData(step.next)),
      );
      expect(planned.witnessHash).toBe(
        computeHash32(Buffer.from(Data.to(witnessData(step)), "hex")).toString(
          "hex",
        ),
      );
    }
  });

  it("refuses evidence that is not an item auxiliary", () => {
    const [step] = steps(MidgardRedeemerItemProofModes.Data);
    const fields = [
      new Constr(1, []),
      controlData(step!.control),
      witnessData(step!),
    ];
    for (const evidence of [
      new Constr(17, fields),
      new Constr(18, fields.slice(0, 2)),
    ])
      expect(() =>
        planValidationTraceContextItem({
          auxiliaryCbor: Data.to(evidence),
          contextStage: 9n,
        }),
      ).toThrow(/is not an item auxiliary/u);
  });

  it("refuses a witness that does not advance the step's control", () => {
    const data = steps(MidgardRedeemerItemProofModes.Data);
    expect(() =>
      planValidationTraceContextItem({
        auxiliaryCbor: auxiliary(data[0]!, witnessData(data.at(-1)!)),
        contextStage: 9n,
      }),
    ).toThrow(/does not advance its control/u);
  });
});
