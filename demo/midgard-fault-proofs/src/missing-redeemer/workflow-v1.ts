import {
  type MissingRedeemerEvidence,
  missingRedeemerEvidenceIdentity,
} from "./family-v1.js";

export type MissingRedeemerStage =
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
export type MissingRedeemerDurableState = Readonly<{
  stage: MissingRedeemerStage;
  scanCursor: number;
  txHash: string;
  outputReference: string | null;
}>;
export type MissingRedeemerJournal = Readonly<{
  load: (identity: string) => Promise<readonly MissingRedeemerDurableState[]>;
  append: (
    identity: string,
    expectedLength: number,
    state: MissingRedeemerDurableState,
  ) => Promise<void>;
}>;
export type MissingRedeemerActuator = Readonly<{
  observe: (identity: string) => Promise<MissingRedeemerDurableState>;
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
      evidence: MissingRedeemerEvidence;
    }>,
  ) => Promise<MissingRedeemerDurableState>;
}>;

const order: readonly MissingRedeemerStage[] = [
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
export const reconcileMissingRedeemerState = (
  journal: readonly MissingRedeemerDurableState[],
  observed: MissingRedeemerDurableState,
): MissingRedeemerDurableState => {
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

export const runMissingRedeemerWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  readonly evidence: MissingRedeemerEvidence;
  readonly journal: MissingRedeemerJournal;
  readonly actuator: MissingRedeemerActuator;
}): Promise<"removed" | "cancelled"> => {
  const identity = missingRedeemerEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    const state = reconcileMissingRedeemerState(
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
