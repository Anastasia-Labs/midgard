import type { RedeemerCanonicityEvidenceV1 } from "./family-v1.js";

export type RedeemerCanonicityDurableStateV1 = Readonly<{
  stage:
    | "none"
    | "step01"
    | "step02"
    | "step03"
    | "proven"
    | "removed"
    | "cancelled";
  decodeCursor: number;
  txHash: string;
  outputReference: string | null;
}>;

export type RedeemerCanonicityJournalV1 = Readonly<{
  load: (
    identity: string,
  ) => Promise<readonly RedeemerCanonicityDurableStateV1[]>;
  append: (
    identity: string,
    expectedLength: number,
    state: RedeemerCanonicityDurableStateV1,
  ) => Promise<void>;
}>;

export type RedeemerCanonicityActuatorV1 = Readonly<{
  observe: (identity: string) => Promise<RedeemerCanonicityDurableStateV1>;
  submit: (
    input: Readonly<{
      identity: string;
      action: "init" | "bind" | "decode" | "finalize" | "remove";
      decodeCursor: number;
      evidence: RedeemerCanonicityEvidenceV1;
    }>,
  ) => Promise<RedeemerCanonicityDurableStateV1>;
}>;

export const redeemerCanonicityEvidenceIdentityV1 = (
  evidence: RedeemerCanonicityEvidenceV1,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.redeemerIndex.toString(),
    evidence.fieldCommitmentHex,
  ].join(":");

const rank = (stage: RedeemerCanonicityDurableStateV1["stage"]): number =>
  [
    "none",
    "step01",
    "step02",
    "step03",
    "proven",
    "removed",
    "cancelled",
  ].indexOf(stage);

export const reconcileRedeemerCanonicityStateV1 = ({
  journal,
  observed,
}: {
  readonly journal: readonly RedeemerCanonicityDurableStateV1[];
  readonly observed: RedeemerCanonicityDurableStateV1;
}): RedeemerCanonicityDurableStateV1 => {
  const recorded = journal.at(-1);
  if (recorded === undefined) return observed;
  if (rank(observed.stage) < rank(recorded.stage))
    throw new Error(
      "redeemerCanonicity: authenticated chain regressed behind journal",
    );
  if (
    observed.stage === "step02" &&
    observed.decodeCursor < recorded.decodeCursor
  )
    throw new Error("redeemerCanonicity: decode checkpoint regressed");
  return observed;
};

/** Restart-safe runner; authenticated chain observation is always authoritative. */
export const runRedeemerCanonicityWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
}: {
  readonly evidence: RedeemerCanonicityEvidenceV1;
  readonly journal: RedeemerCanonicityJournalV1;
  readonly actuator: RedeemerCanonicityActuatorV1;
}): Promise<"removed" | "cancelled"> => {
  const identity = redeemerCanonicityEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const state = reconcileRedeemerCanonicityStateV1({
      journal: entries,
      observed: await actuator.observe(identity),
    });
    if (state.stage === "removed" || state.stage === "cancelled")
      return state.stage;
    const action =
      state.stage === "none"
        ? "init"
        : state.stage === "step01"
          ? "bind"
          : state.stage === "step02"
            ? "decode"
            : state.stage === "step03"
              ? "finalize"
              : "remove";
    const next = await actuator.submit({
      identity,
      action,
      decodeCursor: state.decodeCursor,
      evidence,
    });
    await journal.append(identity, entries.length, next);
  }
};
