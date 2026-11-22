/**
 * Records the field carriage a journaled workflow's prerequisite requires and
 * the carriage its step builders plan, so a test can show the two are the same
 * slots: every publication a builder reads is one the prerequisite published
 * before the step, and no builder builds one of its own.
 *
 * A test installs the recorders with `vi.mock` on `src/field-opening.js` and
 * `src/workflow/field-carriage-prerequisite.js`; both pass every call through
 * unchanged.
 */
import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";

import { isPrerequisiteTurn } from "../../src/fault-proof-turn.js";
import type * as FieldOpening from "../../src/field-opening.js";
import { fieldPublicationDatums } from "../../src/field-opening.own-field-carriage.js";
import { fieldCarriagePlanTransactions } from "../../src/field-opening.publish-fault-proof-field-carriage.js";
import type * as FieldCarriagePrerequisite from "../../src/workflow/field-carriage-prerequisite.js";

type RecordedPlan = Readonly<{
  plan: MidgardFieldCarriagePlan;
  publisherAddress: string;
}>;

/** What the calls of one test recorded, in call order. */
export const carriagePlanRecord = {
  /** Each field requirement the prerequisite returned, with its action input. */
  required: [] as (RecordedPlan & {
    input: Readonly<Record<string, unknown>>;
  })[],
  /** Each plan `planFaultProofFieldOpening` returned, with its label. */
  planned: [] as (Pick<RecordedPlan, "plan"> & { label: string })[],
  /** Each `publishFaultProofFieldCarriage` call, and whether it built a turn. */
  published: [] as (RecordedPlan & { label: string; turn: boolean })[],
};

export const resetCarriagePlanRecord = (): void => {
  carriagePlanRecord.required.length = 0;
  carriagePlanRecord.planned.length = 0;
  carriagePlanRecord.published.length = 0;
};

/** `src/field-opening.js` with its planner and publisher recorded. */
export const recordingFieldOpening = (
  actual: typeof FieldOpening,
): typeof FieldOpening => {
  // One wrapper serves both overloads; each call is passed on unchanged.
  const plan = actual.planFaultProofFieldOpening as (input: {
    readonly label: string;
  }) => ReturnType<typeof actual.planFaultProofFieldOpening>;
  return {
    ...actual,
    planFaultProofFieldOpening: ((input: { readonly label: string }) => {
      const planned = plan(input);
      carriagePlanRecord.planned.push({
        label: input.label,
        plan: planned.plan,
      });
      return planned;
    }) as typeof actual.planFaultProofFieldOpening,
    publishFaultProofFieldCarriage: async (input) => {
      const result = await actual.publishFaultProofFieldCarriage(input);
      carriagePlanRecord.published.push({
        label: input.label,
        plan: input.planned.plan,
        publisherAddress: input.publisherAddress,
        turn: isPrerequisiteTurn(result),
      });
      return result;
    },
  };
};

/** `src/workflow/field-carriage-prerequisite.js` with field requirements recorded. */
export const recordingFieldCarriagePrerequisite = (
  actual: typeof FieldCarriagePrerequisite,
): typeof FieldCarriagePrerequisite => ({
  ...actual,
  createAuthenticatedFieldCarriagePrerequisitePort: (config) =>
    actual.createAuthenticatedFieldCarriagePrerequisitePort({
      ...config,
      requirementForAction: async (input) => {
        const required = await config.requirementForAction(input);
        // A raw-datum requirement names its kind; a field
        // requirement does not.
        if (required !== null && !("kind" in required))
          carriagePlanRecord.required.push({
            input: input.action.input,
            plan: required.planned.plan,
            publisherAddress: config.signer.proofMaterialsAddress,
          });
        return required;
      },
    }),
});

/**
 * What a carriage plan needs on chain before its step: the plan's identity,
 * its tier, how many transactions it takes, and the datum of each
 * publication, in plan order. Empty for a plan that rides inline.
 */
export const carriageNeed = ({ plan }: RecordedPlan) => {
  const { key, transactions } = fieldCarriagePlanTransactions(plan);
  return transactions === 0
    ? null
    : {
        fieldIndex: plan.fieldIndex,
        key,
        tier: plan.tier,
        transactions,
        slots: fieldPublicationDatums(plan),
      };
};

type CarriageNeed = NonNullable<ReturnType<typeof carriageNeed>>;

/**
 * The distinct needs of some plans, by field index, in a stable order. A field
 * whose plans all ride inline needs nothing and has no entry.
 */
export const carriageNeedsByField = (
  plans: readonly RecordedPlan[],
): Readonly<Record<number, readonly CarriageNeed[]>> => {
  const byField: Record<number, Map<string, CarriageNeed>> = {};
  for (const recorded of plans) {
    const need = carriageNeed(recorded);
    if (need === null) continue;
    (byField[need.fieldIndex] ??= new Map()).set(JSON.stringify(need), need);
  }
  return Object.fromEntries(
    Object.entries(byField).map(([fieldIndex, needs]) => [
      fieldIndex,
      [...needs.entries()]
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([, need]) => need),
    ]),
  );
};

/** Every `field:tier` a builder asked `publishFaultProofFieldCarriage` for. */
export const publishedCoverage = (): readonly string[] =>
  [
    ...new Set(
      carriagePlanRecord.published.map(
        ({ plan }) => `${plan.fieldIndex.toString()}:${plan.tier}`,
      ),
    ),
  ].sort();
