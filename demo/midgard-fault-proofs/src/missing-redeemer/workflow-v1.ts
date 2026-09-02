import {
  missingRedeemerEvidenceIdentityV1,
  type MissingRedeemerEvidenceV1,
} from "./family-v1.js";

export type MissingRedeemerStageV1 =
  | "none"
  | "step01"
  | "step02"
  | "step02a"
  | "step02b"
  | "step03"
  | "step04"
  | "step05"
  | "proven"
  | "removed"
  | "cancelled";
export type MissingRedeemerDurableStateV1 = Readonly<{
  stage: MissingRedeemerStageV1;
  scanCursor: number;
  txHash: string;
  outputReference: string | null;
}>;
export type MissingRedeemerJournalV1 = Readonly<{
  load: (identity: string) => Promise<readonly MissingRedeemerDurableStateV1[]>;
  append: (
    identity: string,
    expectedLength: number,
    state: MissingRedeemerDurableStateV1,
  ) => Promise<void>;
}>;
export type MissingRedeemerActuatorV1 = Readonly<{
  observe: (identity: string) => Promise<MissingRedeemerDurableStateV1>;
  submit: (
    input: Readonly<{
      identity: string;
      action:
        | "init"
        | "bind"
        | "authenticatePurpose"
        | "authenticateTrace"
        | "authenticateSelection"
        | "openRedeemers"
        | "scan"
        | "finalize"
        | "remove";
      scanCursor: number;
      evidence: MissingRedeemerEvidenceV1;
    }>,
  ) => Promise<MissingRedeemerDurableStateV1>;
}>;

const order: readonly MissingRedeemerStageV1[] = [
  "none",
  "step01",
  "step02",
  "step02a",
  "step02b",
  "step03",
  "step04",
  "step05",
  "proven",
  "removed",
  "cancelled",
];
export const reconcileMissingRedeemerStateV1 = (
  journal: readonly MissingRedeemerDurableStateV1[],
  observed: MissingRedeemerDurableStateV1,
): MissingRedeemerDurableStateV1 => {
  const prior = journal.at(-1);
  if (prior === undefined) return observed;
  if (order.indexOf(observed.stage) < order.indexOf(prior.stage))
    throw new Error(
      "missingRedeemer: authenticated chain regressed behind journal",
    );
  if (observed.scanCursor < prior.scanCursor)
    throw new Error("missingRedeemer: scan checkpoint regressed");
  return observed;
};

export const runMissingRedeemerWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
}: {
  readonly evidence: MissingRedeemerEvidenceV1;
  readonly journal: MissingRedeemerJournalV1;
  readonly actuator: MissingRedeemerActuatorV1;
}): Promise<"removed" | "cancelled"> => {
  const identity = missingRedeemerEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const state = reconcileMissingRedeemerStateV1(
      entries,
      await actuator.observe(identity),
    );
    if (state.stage === "removed" || state.stage === "cancelled")
      return state.stage;
    const action =
      state.stage === "none"
        ? "init"
        : state.stage === "step01"
          ? "bind"
          : state.stage === "step02"
            ? "authenticatePurpose"
            : state.stage === "step02a"
              ? "authenticateTrace"
              : state.stage === "step02b"
                ? "authenticateSelection"
                : state.stage === "step03"
                  ? "openRedeemers"
                  : state.stage === "step04"
                    ? "scan"
                    : state.stage === "step05"
                      ? "finalize"
                      : "remove";
    const next = await actuator.submit({
      identity,
      action,
      scanCursor: state.scanCursor,
      evidence,
    });
    await journal.append(identity, entries.length, next);
  }
};
