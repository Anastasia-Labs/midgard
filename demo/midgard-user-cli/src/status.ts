/**
 * `midgard-user status`: read-only. Prints the record of one submission id,
 * the exact status of each recorded transaction on the user's indexer, the
 * indexed slot and a verdict. Never builds, signs or submits.
 */
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  observeUserSubmissionAttempt,
  outputReferenceToPlutusDataCbor,
} from "@al-ft/midgard-sdk";

import { type UserCliDeps } from "./deps.js";
import { resolveL1Access } from "./l1-view.js";
import { loadUserDeployment } from "./manifest.js";
import { parseSubmissionId, refuse } from "./options.js";
import { readAttempts, readIntent } from "./record-store.js";
import {
  type CommonOptions,
  manifestPathOf,
  recordDirectoryOf,
} from "./submission.js";

export type StatusOptions = CommonOptions & {
  readonly kind: string;
};

export type StatusVerdict = "confirmed" | "in-flight" | "dead" | "no-attempts";

export type StatusReport = {
  readonly kind: "Deposit" | "Withdrawal";
  readonly submissionId: string;
  readonly recordDirectory: string;
  readonly intent: Readonly<Record<string, unknown>>;
  readonly indexedSlot: number;
  readonly attempts: readonly {
    readonly attempt: number;
    readonly phase: string;
    readonly txHash: string;
    readonly nonce: string;
    readonly eventId: string;
    readonly ttlSlot: number;
    readonly status: string;
  }[];
  readonly verdict: StatusVerdict;
};

const kindOf = (value: string) =>
  value === "deposit"
    ? ("Deposit" as const)
    : value === "withdrawal"
      ? ("Withdrawal" as const)
      : refuse("--kind must be deposit or withdrawal.");

export const runStatus = async (
  options: StatusOptions,
  deps: UserCliDeps,
): Promise<StatusReport> => {
  const submissionId = parseSubmissionId(options.submissionId);
  const kind = kindOf(options.kind);
  const deployment = await loadUserDeployment(manifestPathOf(options, deps));
  const access = resolveL1Access(options, deps.env, deployment.network);
  const directory = recordDirectoryOf(
    options,
    deps,
    deployment.manifestId,
    kind,
    submissionId,
  );
  const intent = await readIntent(directory);
  if (intent === undefined)
    return refuse(
      `No ${kind.toLowerCase()} record for --submission-id ${JSON.stringify(submissionId)} under ${directory}.`,
    );
  const recorded = await readAttempts(directory);
  const view = await deps.openL1View({
    access,
    network: deployment.network,
    slotConfig: deployment.slotConfig,
  });
  const indexedSlot = await view.indexedSlot();
  const attempts = [];
  for (const attempt of recorded)
    attempts.push({
      attempt: attempt.attempt,
      phase: attempt.phase,
      txHash: attempt.txHash,
      nonce: outRefLabel(attempt.nonce),
      eventId: outputReferenceToPlutusDataCbor(attempt.nonce),
      ttlSlot: attempt.ttlSlot,
      status: (await observeUserSubmissionAttempt(view.lucid, attempt.txHash))
        .status,
    });
  const latest = attempts.at(-1);
  const verdict: StatusVerdict = attempts.some(
    (attempt) =>
      attempt.phase === "Admission" && attempt.status === "confirmed",
  )
    ? "confirmed"
    : latest === undefined
      ? "no-attempts"
      : latest.status !== "confirmed" && indexedSlot > latest.ttlSlot
        ? "dead"
        : "in-flight";
  return {
    kind,
    submissionId,
    recordDirectory: directory,
    intent: intent.intent,
    indexedSlot,
    attempts,
    verdict,
  };
};

const VERDICT_TEXT: Record<StatusVerdict, string> = {
  confirmed: "confirmed: the admission is on L1",
  "in-flight":
    "in flight: the latest transaction may still land; rerun the command with the same --submission-id to follow it",
  dead: "dead: the latest transaction expired unseen; rerun the command with the same --submission-id to build a new one",
  "no-attempts":
    "no transaction recorded yet; rerun the command with the same --submission-id",
};

export const formatStatusReport = (report: StatusReport): string =>
  [
    `${report.kind} ${report.submissionId}`,
    `record: ${report.recordDirectory}`,
    `indexed slot: ${report.indexedSlot.toString()}`,
    ...report.attempts.map(
      (attempt) =>
        `attempt ${attempt.attempt.toString()} ${attempt.phase} ${attempt.txHash} ttl ${attempt.ttlSlot.toString()}: ${attempt.status} (event id ${attempt.eventId})`,
    ),
    VERDICT_TEXT[report.verdict],
    "",
  ].join("\n");
