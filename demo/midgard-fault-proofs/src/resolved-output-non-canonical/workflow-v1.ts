import type { ResolvedOutputNonCanonicalEvidenceV1 } from "./resolved-output-non-canonical-v1.js";
import { resolvedOutputEvidenceIdentityV1 } from "./resolved-output-non-canonical-v1.js";

export const RESOLVED_OUTPUT_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "reconstructing",
  "step05",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ResolvedOutputStageV1 = (typeof RESOLVED_OUTPUT_STAGES_V1)[number];
export type ResolvedOutputActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitReconstruction"
  | "submitStep05"
  | "removeDescendants"
  | "done";

export type ResolvedOutputJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  stage: ResolvedOutputStageV1;
  action?: Exclude<ResolvedOutputActionV1, "done">;
  phase?: "intent" | "submitted" | "confirmed";
  txHash: string;
  outputReference: string | null;
}>;
export type ResolvedOutputJournalV1 = Readonly<{
  load(identity: string): Promise<readonly ResolvedOutputJournalEntryV1[]>;
  append(entry: ResolvedOutputJournalEntryV1): Promise<void>;
}>;
export type ResolvedOutputSubmissionAdapterV1 = Readonly<{
  observe(identity: string): Promise<ResolvedOutputStageV1>;
  build(
    action: Exclude<ResolvedOutputActionV1, "done">,
    evidence: ResolvedOutputNonCanonicalEvidenceV1,
  ): Promise<Readonly<{ txHash: string; submit(): Promise<string> }>>;
  transactionConfirmed(txHash: string): Promise<boolean>;
}>;

export const nextResolvedOutputActionV1 = (
  stage: ResolvedOutputStageV1,
): ResolvedOutputActionV1 => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "reconstructing":
      return "submitReconstruction";
    case "step05":
      return "submitStep05";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};

const targetStage = (
  action: Exclude<ResolvedOutputActionV1, "done">,
  observed: ResolvedOutputStageV1,
): ResolvedOutputStageV1 => {
  switch (action) {
    case "submitInit":
      return "step01";
    case "submitStep01":
      return "step02";
    case "submitStep02":
      return "step03";
    case "submitStep03":
      return "reconstructing";
    case "submitReconstruction":
      return observed === "reconstructing" ? "reconstructing" : "step05";
    case "submitStep05":
      return "proven";
    case "removeDescendants":
      return "removed";
  }
};

const lastIntent = (
  entries: readonly ResolvedOutputJournalEntryV1[],
): ResolvedOutputJournalEntryV1 | undefined =>
  [...entries]
    .reverse()
    .find(
      (entry) =>
        entry.phase === "intent" &&
        !entries.some(
          (later) =>
            later.sequence > entry.sequence &&
            entry.action !== undefined &&
            later.action === entry.action &&
            later.txHash === entry.txHash &&
            later.phase === "confirmed",
        ),
    );

/** Crash-safe family driver: intent precedes submit and restart reconciles the exact hash. */
export const runResolvedOutputNonCanonicalWorkflowV1 = async ({
  evidence,
  journal,
  submission,
}: {
  readonly evidence: ResolvedOutputNonCanonicalEvidenceV1;
  readonly journal: ResolvedOutputJournalV1;
  readonly submission: ResolvedOutputSubmissionAdapterV1;
}): Promise<ResolvedOutputStageV1> => {
  const identity = resolvedOutputEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    if (
      entries.some(
        (entry, i) => entry.identity !== identity || entry.sequence !== i,
      )
    )
      throw new Error(
        "resolvedOutputNonCanonical: journal identity/sequence changed",
      );
    const pending = lastIntent(entries);
    if (pending !== undefined) {
      if (pending.action === undefined)
        throw new Error(
          "resolvedOutputNonCanonical: pending journal intent omitted action",
        );
      if (!(await submission.transactionConfirmed(pending.txHash)))
        throw new Error(
          "resolvedOutputNonCanonical: exact intended transaction is unresolved",
        );
      const observed = await submission.observe(identity);
      await journal.append({
        ...pending,
        sequence: entries.length,
        phase: "confirmed",
        stage: targetStage(pending.action, observed),
        outputReference: pending.outputReference,
      });
      continue;
    }
    const observed = await submission.observe(identity);
    const action = nextResolvedOutputActionV1(observed);
    if (action === "done") return observed;
    const built = await submission.build(action, evidence);
    if (!/^[0-9a-f]{64}$/u.test(built.txHash))
      throw new Error(
        "resolvedOutputNonCanonical: locally evaluated tx hash is invalid",
      );
    const intent: ResolvedOutputJournalEntryV1 = {
      sequence: entries.length,
      identity,
      stage: observed,
      action,
      phase: "intent",
      txHash: built.txHash,
      outputReference: null,
    };
    await journal.append(intent);
    const submittedHash = await built.submit();
    if (submittedHash !== built.txHash)
      throw new Error(
        "resolvedOutputNonCanonical: submitter changed exact transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: entries.length + 1,
      phase: "submitted",
    });
  }
};
