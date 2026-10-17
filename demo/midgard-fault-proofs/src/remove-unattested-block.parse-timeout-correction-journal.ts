import { createHash } from "node:crypto";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { basename, dirname, join } from "node:path";

import {
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { outRefLabel } from "./runtime.js";
import { inspectSignedWorkflowTransaction } from "./workflow/signed-transaction-reconciliation.js";

export type TimeoutCorrectionTxKind = "prune-descendant" | "remove-block";

export type TimeoutCorrectionTxStatus =
  | "prepared"
  | "submitted"
  | "confirmed"
  | "superseded"
  /** Impossible at the tip but not retired: replaced at once, and adopted
   * as confirmed if a rollback lands it after all. */
  | "abandoned"
  | "retired";

export type TimeoutCorrectionJournalStep = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly removedHeaderHash: string;
  readonly inputOutRefs: readonly string[];
  readonly txHash: string;
  readonly signedCbor: string;
  readonly validFromSlot: string;
  readonly validToSlot: string;
  readonly status: TimeoutCorrectionTxStatus;
};

export type TimeoutCorrectionJournal = {
  readonly version: 1;
  readonly targetHeaderHash: string;
  readonly targetDeadlineMs: string;
  readonly steps: readonly TimeoutCorrectionJournalStep[];
  readonly completed: boolean;
};

export interface TimeoutCorrectionJournalStore {
  readonly load: () => Promise<TimeoutCorrectionJournal | undefined>;
  readonly save: (journal: TimeoutCorrectionJournal) => Promise<void>;
  readonly archive?: (journal: TimeoutCorrectionJournal) => Promise<void>;
}

const HEADER_HASH_PATTERN = /^[0-9a-f]{56}$/;

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/;

const OUT_REF_PATTERN = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/;

const DECIMAL_NATURAL_PATTERN = /^(?:0|[1-9][0-9]*)$/;

const hasExactKeys = (
  value: object,
  expectedKeys: readonly string[],
): boolean => {
  const actualKeys = Object.keys(value).sort();
  const canonicalExpectedKeys = [...expectedKeys].sort();
  return (
    actualKeys.length === canonicalExpectedKeys.length &&
    actualKeys.every((key, index) => key === canonicalExpectedKeys[index])
  );
};

export const transactionInputOutRefs = (
  inputs: CML.TransactionInputList,
): string[] =>
  Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();

export const parseTimeoutCorrectionJournal = (
  value: unknown,
): TimeoutCorrectionJournal => {
  if (
    typeof value !== "object" ||
    value === null ||
    !hasExactKeys(value, [
      "version",
      "targetHeaderHash",
      "targetDeadlineMs",
      "steps",
      "completed",
    ]) ||
    (value as { version?: unknown }).version !== 1 ||
    typeof (value as { targetHeaderHash?: unknown }).targetHeaderHash !==
      "string" ||
    typeof (value as { targetDeadlineMs?: unknown }).targetDeadlineMs !==
      "string" ||
    !Array.isArray((value as { steps?: unknown }).steps) ||
    typeof (value as { completed?: unknown }).completed !== "boolean"
  ) {
    throw new Error("Invalid attestation-timeout correction journal V1.");
  }
  const candidate = value as {
    readonly targetHeaderHash: string;
    readonly targetDeadlineMs: string;
    readonly steps: readonly unknown[];
    readonly completed: boolean;
  };
  if (
    !HEADER_HASH_PATTERN.test(candidate.targetHeaderHash) ||
    !DECIMAL_NATURAL_PATTERN.test(candidate.targetDeadlineMs)
  ) {
    throw new Error(
      "Timeout-correction journal target hash/deadline is non-canonical.",
    );
  }
  const seenTxHashes = new Set<string>();
  const steps = candidate.steps.map((rawStep, index) => {
    if (
      typeof rawStep !== "object" ||
      rawStep === null ||
      !hasExactKeys(rawStep, [
        "kind",
        "removedHeaderHash",
        "inputOutRefs",
        "txHash",
        "signedCbor",
        "validFromSlot",
        "validToSlot",
        "status",
      ])
    ) {
      throw new Error(`Timeout-correction journal step ${index} is invalid.`);
    }
    const step = rawStep as Partial<TimeoutCorrectionJournalStep>;
    if (
      (step.kind !== "prune-descendant" && step.kind !== "remove-block") ||
      !HEADER_HASH_PATTERN.test(step.removedHeaderHash ?? "") ||
      !TX_HASH_PATTERN.test(step.txHash ?? "") ||
      typeof step.signedCbor !== "string" ||
      !DECIMAL_NATURAL_PATTERN.test(step.validFromSlot ?? "") ||
      !DECIMAL_NATURAL_PATTERN.test(step.validToSlot ?? "") ||
      (step.status !== "prepared" &&
        step.status !== "submitted" &&
        step.status !== "confirmed" &&
        step.status !== "superseded" &&
        step.status !== "abandoned" &&
        step.status !== "retired") ||
      !Array.isArray(step.inputOutRefs) ||
      step.inputOutRefs.length < 3 ||
      step.inputOutRefs.some(
        (outRef) => typeof outRef !== "string" || !OUT_REF_PATTERN.test(outRef),
      ) ||
      new Set(step.inputOutRefs).size !== step.inputOutRefs.length
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} has non-canonical fields.`,
      );
    }
    const inspected = inspectSignedWorkflowTransaction({
      transactionHash: step.txHash!,
      signedTransactionCborHex: step.signedCbor!,
    });
    const actualInputs = transactionInputOutRefs(inspected.body.inputs());
    if (
      inspected.validFromSlot?.toString() !== step.validFromSlot ||
      inspected.expiresAtSlot?.toString() !== step.validToSlot ||
      BigInt(step.validFromSlot!) >= BigInt(step.validToSlot!) ||
      actualInputs.join(",") !== [...step.inputOutRefs].sort().join(",")
    )
      throw new Error(
        "Timeout-correction signed bytes disagree with journal inputs or validity.",
      );
    if (seenTxHashes.has(step.txHash!)) {
      throw new Error(
        `Timeout-correction journal repeats transaction hash ${step.txHash}.`,
      );
    }
    seenTxHashes.add(step.txHash!);
    if (
      (step.kind === "remove-block") !==
      (step.removedHeaderHash === candidate.targetHeaderHash)
    ) {
      throw new Error(
        `Timeout-correction journal step ${index} does not match the target-removal topology.`,
      );
    }
    return step as TimeoutCorrectionJournalStep;
  });
  if (
    candidate.completed &&
    steps.some(
      (step) => step.status === "prepared" || step.status === "submitted",
    )
  ) {
    throw new Error(
      "Completed timeout-correction journal has a non-terminal transaction.",
    );
  }
  return {
    version: 1,
    targetHeaderHash: candidate.targetHeaderHash,
    targetDeadlineMs: candidate.targetDeadlineMs,
    steps,
    completed: candidate.completed,
  };
};

export type TimeoutCorrectionTransactionStatus =
  | "pending"
  | "confirmed"
  | "failed"
  | "not_found"
  | "expired"
  | "invalidated"
  /** Impossible at the tip, not yet retirable. */
  | "superseded"
  | "unknown";

export type TimeoutCorrectionStepReconciliation = {
  readonly disposition: "none" | "pending" | "confirmed" | "superseded";
  readonly journal: TimeoutCorrectionJournal;
};

/** Durable single-file journal. Rename makes each state transition atomic. */
export const createFileTimeoutCorrectionJournalStore = (
  journalPath: string,
): TimeoutCorrectionJournalStore => ({
  load: async () => {
    try {
      return parseTimeoutCorrectionJournal(
        JSON.parse(await readFile(journalPath, "utf8")),
      );
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return undefined;
      }
      throw error;
    }
  },
  archive: async (journal) => {
    const bytes = `${JSON.stringify(journal, null, 2)}\n`;
    const digest = createHash("sha256").update(bytes).digest("hex");
    const archivePath = `${journalPath}.archive-${digest}.json`;
    await mkdir(dirname(journalPath), { recursive: true });
    try {
      await writeFile(archivePath, bytes, {
        encoding: "utf8",
        mode: 0o600,
        flag: "wx",
      });
    } catch (error) {
      if (
        !(
          typeof error === "object" &&
          error !== null &&
          "code" in error &&
          error.code === "EEXIST"
        )
      )
        throw error;
      if ((await readFile(archivePath, "utf8")) !== bytes)
        throw new Error(
          "Timeout correction archive content does not match its identity.",
        );
    }
  },
  save: async (journal) => {
    const directory = dirname(journalPath);
    await mkdir(directory, { recursive: true });
    const temporaryPath = join(
      directory,
      `.${basename(journalPath)}.${process.pid.toString()}.tmp`,
    );
    await writeFile(temporaryPath, `${JSON.stringify(journal, null, 2)}\n`, {
      encoding: "utf8",
      mode: 0o600,
    });
    await rename(temporaryPath, journalPath);
  },
});

export const headerHashOf = (node: StateQueueUTxO): string => {
  if (node.datum.key === "Empty") {
    throw new Error("Confirmed-state root does not have a block header hash.");
  }
  const headerHash = node.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (node.datum.key.Key.key !== headerHash) {
    throw new Error("State-queue node key does not match its NFT asset name.");
  }
  return headerHash;
};

export const outRefsOf = (
  nodes: readonly StateQueueUTxO[],
): readonly string[] => nodes.map((node) => outRefLabel(node.utxo)).sort();

export const replaceJournalStepStatus = (
  journal: TimeoutCorrectionJournal,
  stepIndex: number,
  status: "confirmed" | "superseded" | "abandoned" | "retired",
): TimeoutCorrectionJournal => {
  return {
    ...journal,
    steps: journal.steps.map(
      (step, index): TimeoutCorrectionJournalStep =>
        index === stepIndex ? { ...step, status } : step,
    ),
  };
};
