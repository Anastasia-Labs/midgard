/**
 * The installed mint-authorization runner publishes each step's field carriage
 * through its journaled prerequisite, and the transaction port hands the step
 * builder the publications it resolves from those journaled claims. These
 * cases run the installed runner and show the builder never needs carriage of
 * its own: for every step-02, step-03 (both directions) and step-04 action,
 * the plan the builder opens from chain state needs exactly the slots (keys
 * and count) of the plan the prerequisite required from the artifact, and no
 * builder asks `publishFaultProofFieldCarriage` for anything.
 */
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk/fraud-proof";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { mintAuthorizationStepLabel } from "../src/mint-authorization/submit-common.js";
import {
  carriageNeedsByField,
  carriagePlanRecord,
  resetCarriagePlanRecord,
} from "./support/carriage-plan-parity.js";
import { captureAndRestart } from "./support/mint-authorization-installed-lifecycle-cases.js";

vi.mock("../src/field-opening.js", async (importOriginal) =>
  (await import("./support/carriage-plan-parity.js")).recordingFieldOpening(
    await importOriginal(),
  ),
);
vi.mock(
  "../src/workflow/field-carriage-prerequisite.js",
  async (importOriginal) =>
    (
      await import("./support/carriage-plan-parity.js")
    ).recordingFieldCarriagePrerequisite(await importOriginal()),
);

beforeEach(resetCarriagePlanRecord);

/** The step stage whose builder labelled a plan, or null for another caller. */
const builderStage = (label: string) =>
  ([1, 2, 3] as const)
    .map((stepIndex) => ({
      prefix: mintAuthorizationStepLabel(stepIndex),
      stage: `step_0${(stepIndex + 1).toString()}`,
    }))
    .find(({ prefix }) => label.startsWith(`${prefix} `))?.stage ?? null;

/** Every `stage:field:tier` a builder planned, in a stable order. */
const builderCoverage = () =>
  [
    ...new Set(
      carriagePlanRecord.planned.flatMap(({ label, plan }) => {
        const stage = builderStage(label);
        return stage === null
          ? []
          : [`${stage}:${plan.fieldIndex.toString()}:${plan.tier}`];
      }),
    ),
  ].sort();

const {
  mint,
  scriptWitnesses,
  addressWitnesses,
  spendInputs,
  referenceInputs,
} = MIDGARD_FIELD_INDEX;
const at = (stage: string, fieldIndex: number, tier: string) =>
  `${stage}:${fieldIndex.toString()}:${tier}`;

/** The thread out-refs the runner built step-02 transactions from. */
const step02ThreadOutRefs = () =>
  new Set(
    carriagePlanRecord.required.flatMap(({ input }) =>
      input.stage === "step_02" ? [String(input.threadOutRef)] : [],
    ),
  ).size;

describe("mint-authorization field carriage plan parity", () => {
  it.each([
    {
      name: "absent-mint",
      coverage: [
        at("step_02", mint, "Inline"),
        at("step_03", scriptWitnesses, "Inline"),
        at("step_04", spendInputs, "Inline"),
        at("step_04", referenceInputs, "Inline"),
      ],
      step02Transactions: 1,
    },
    {
      name: "unsatisfied-mint",
      coverage: [
        at("step_02", mint, "Inline"),
        at("step_03", addressWitnesses, "Inline"),
      ],
      step02Transactions: 1,
    },
    {
      name: "reference-absent",
      coverage: [
        at("step_02", mint, "Inline"),
        at("step_03", scriptWitnesses, "Inline"),
        at("step_04", spendInputs, "Inline"),
        at("step_04", referenceInputs, "Inline"),
      ],
      step02Transactions: 1,
    },
    {
      name: "maximum-mint-tail",
      coverage: [
        at("step_02", mint, "Certified"),
        at("step_03", scriptWitnesses, "Inline"),
        at("step_04", spendInputs, "Inline"),
        at("step_04", referenceInputs, "Inline"),
      ],
      step02Transactions: 29,
    },
    {
      name: "maximum-signers",
      coverage: [
        at("step_02", mint, "Inline"),
        at("step_03", addressWitnesses, "Certified"),
      ],
      step02Transactions: 1,
    },
  ])(
    "$name: each step builder plans exactly the carriage slots its journaled prerequisite published",
    async ({ name, coverage, step02Transactions }) => {
      await captureAndRestart(name);
      const signerAddress = carriagePlanRecord.required[0]!.publisherAddress;
      const builderPlans = carriagePlanRecord.planned.flatMap(
        ({ label, plan }) =>
          builderStage(label) === null
            ? []
            : [{ plan, publisherAddress: signerAddress }],
      );
      expect(carriageNeedsByField(builderPlans)).toEqual(
        carriageNeedsByField(carriagePlanRecord.required),
      );
      // The port hands every builder its publications, so none plans one.
      expect(carriagePlanRecord.published).toEqual([]);
      expect(builderCoverage()).toEqual(coverage);
      // A mint scan over several transactions is one step-02 action per
      // checkpoint, each built from the thread output the last one left.
      expect(step02ThreadOutRefs()).toBe(step02Transactions);
    },
    1_800_000,
  );
});
