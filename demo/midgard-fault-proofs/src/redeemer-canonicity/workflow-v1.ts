import type { RedeemerCanonicityEvidence } from "./family-v1.js";

export type RedeemerCanonicityDurableState = Readonly<{
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

export type RedeemerCanonicityJournal = Readonly<{
  load: (
    identity: string,
  ) => Promise<readonly RedeemerCanonicityDurableState[]>;
  append: (
    identity: string,
    expectedLength: number,
    state: RedeemerCanonicityDurableState,
  ) => Promise<void>;
}>;

export type RedeemerCanonicityActuator = Readonly<{
  observe: (identity: string) => Promise<RedeemerCanonicityDurableState>;
  submit: (
    input: Readonly<{
      identity: string;
      action: "init" | "bind" | "decode" | "finalize" | "remove";
      decodeCursor: number;
      evidence: RedeemerCanonicityEvidence;
    }>,
  ) => Promise<RedeemerCanonicityDurableState>;
}>;

export const redeemerCanonicityEvidenceIdentity = (
  evidence: RedeemerCanonicityEvidence,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.redeemerIndex.toString(),
    evidence.fieldCommitmentHex,
  ].join(":");

const rank = (stage: RedeemerCanonicityDurableState["stage"]): number =>
  [
    "none",
    "step01",
    "step02",
    "step03",
    "proven",
    "removed",
    "cancelled",
  ].indexOf(stage);

export const reconcileRedeemerCanonicityState = ({
  journal,
  observed,
}: {
  readonly journal: readonly RedeemerCanonicityDurableState[];
  readonly observed: RedeemerCanonicityDurableState;
}): RedeemerCanonicityDurableState => {
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
export const runRedeemerCanonicityWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  readonly evidence: RedeemerCanonicityEvidence;
  readonly journal: RedeemerCanonicityJournal;
  readonly actuator: RedeemerCanonicityActuator;
}): Promise<"removed" | "cancelled"> => {
  const identity = redeemerCanonicityEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const state = reconcileRedeemerCanonicityState({
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
