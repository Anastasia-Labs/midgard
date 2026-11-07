/** Both authenticated source fields needed by one physical selection step. */
import { planMidgardFieldCarriage } from "@al-ft/midgard-core";
import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import type { UTxO } from "@lucid-evolution/lucid";

import type { FaultProofFieldOpeningPlan } from "../field-opening.js";
import type { PreparedInputExistence } from "./prepare.js";

type FieldPlans = Pick<
  PreparedInputExistence,
  "fieldPlan" | "claimedFieldPlan"
>;

export const inputExistenceFieldPlans = ({
  fieldPlan,
  claimedFieldPlan,
}: FieldPlans): readonly FaultProofFieldOpeningPlan[] =>
  (claimedFieldPlan === null
    ? [fieldPlan]
    : [fieldPlan, claimedFieldPlan]
  ).sort((left, right) => left.fieldIndex - right.fieldIndex);

/** Resolve both fields before assembling either opening; content-based index
 * lookup still uses the complete transaction reference set in the submitter. */
export const resolveInputExistenceFieldCarriage = async ({
  prepared,
  resolveField,
}: {
  readonly prepared: FieldPlans;
  readonly resolveField: (
    planned: FaultProofFieldOpeningPlan,
  ) => Promise<
    Readonly<{ publications: readonly UTxO[]; certificate: UTxO | undefined }>
  >;
}): Promise<readonly UTxO[]> => {
  const references = new Map<string, UTxO>();
  for (const planned of inputExistenceFieldPlans(prepared)) {
    const field = await resolveField(planned);
    for (const publication of field.publications)
      references.set(outRefLabel(publication), publication);
    if (field.certificate !== undefined)
      references.set(outRefLabel(field.certificate), field.certificate);
  }
  return [...references.values()].sort(compareOutRefs);
};

/** Explicit tier-1 demotion for a caller that has published the same committed
 * field bytes. Source/coordinate/decision state is unchanged. */
export const inputExistenceWithPublishedFields = (
  prepared: PreparedInputExistence,
  owner: string,
): PreparedInputExistence => {
  const publish = (
    planned: FaultProofFieldOpeningPlan,
  ): FaultProofFieldOpeningPlan => ({
    ...planned,
    plan: planMidgardFieldCarriage({
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(planned.nativeTxId, "hex"),
      fieldIndex: planned.fieldIndex,
      preimage: planned.preimage,
      publish: true,
    }),
  });
  return {
    ...prepared,
    fieldPlan: publish(prepared.fieldPlan),
    claimedFieldPlan:
      prepared.claimedFieldPlan === null
        ? null
        : publish(prepared.claimedFieldPlan),
  };
};
