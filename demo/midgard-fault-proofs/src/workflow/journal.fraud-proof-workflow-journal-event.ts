import {
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTerminal,
  type JournalJsonObject,
} from "./journal.fraud-proof-workflow-terminal.js";

export type FraudProofWorkflowJournalEvent =
  | { readonly kind: "started" }
  | {
      readonly kind: "prepared";
      readonly artifact: JournalJsonObject;
      readonly artifactDigest: string;
    }
  | {
      readonly kind: "preflight_passed";
      readonly actionId: string;
      /** Hash of the locally evaluated transaction body. */
      readonly txHash: string;
      readonly localEvaluator: string;
      readonly referenceScripts: readonly {
        readonly role: string;
        readonly outRef: string;
        readonly scriptHash: string;
      }[];
    }
  | {
      readonly kind: "submission_intent";
      readonly actionId: string;
      readonly actionInput: JournalJsonObject;
      /** Adapter recovery state persisted before any network submission. */
      readonly durableRecovery?: JournalJsonObject;
      readonly attempt: number;
      /** The exact locally evaluated body this intent permits submitting. */
      readonly txHash: string;
    }
  | {
      readonly kind: "rebroadcast_intent";
      readonly actionId: string;
      readonly txHash: string;
      /** Initial submission counts as one; this counter survives restarts. */
      readonly attempt: number;
    }
  | {
      readonly kind: "submission_ambiguous";
      readonly actionId: string;
      readonly attempt: number;
      readonly txHash?: string;
      readonly detail: string;
    }
  | {
      readonly kind: "submitted";
      readonly actionId: string;
      readonly attempt: number;
      readonly txHash: string;
    }
  | {
      readonly kind: "reconciled";
      readonly actionId: string;
      readonly outcome: "confirmed" | "pending" | "not_found";
      readonly txHash?: string;
    }
  | {
      readonly kind: "confirmed";
      readonly actionId: string;
      readonly txHash: string;
    }
  | {
      /** Current chain state requires this previously submitted action again. */
      readonly kind: "reobserved";
      readonly actionId: string;
      readonly txHash: string;
    }
  | {
      readonly kind: "terminal_included";
      readonly terminal: FraudProofWorkflowTerminal;
      readonly terminalDigest: string;
    }
  | {
      readonly kind: "completed";
      readonly terminal: FraudProofWorkflowTerminal;
      readonly terminalDigest: string;
    }
  | {
      readonly kind: "stalled";
      readonly reason: string;
    };

export type FraudProofWorkflowJournalEntry = {
  readonly schemaVersion: typeof FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly sequence: number;
  readonly recordedAt: string;
  readonly event: FraudProofWorkflowJournalEvent;
};

export interface FraudProofWorkflowJournalStore {
  load(workflowId: string): Promise<readonly FraudProofWorkflowJournalEntry[]>;
  append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void>;
}

export class ConcurrentFraudProofWorkflowWriteError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ConcurrentFraudProofWorkflowWriteErrorV1";
  }
}

export const workflowIdPattern = /^[0-9a-f]{64}$/u;

export const validateWorkflowId = (workflowId: string): void => {
  if (!workflowIdPattern.test(workflowId)) {
    throw new Error("workflowId must be 32-byte lowercase hex");
  }
};

export const requireRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const requireExactKeys = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const record = requireRecord(value, label);
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${label} keys must be exactly [${expected.join(",")}], got [${actual.join(",")}]`,
    );
  }
  return record;
};

export const requireOptionalExactKeys = (
  value: unknown,
  required: readonly string[],
  optional: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const record = requireRecord(value, label);
  const actual = Object.keys(record);
  const allowed = new Set([...required, ...optional]);
  if (
    required.some((key) => !(key in record)) ||
    actual.some((key) => !allowed.has(key))
  ) {
    throw new Error(
      `${label} has missing or unknown keys; required=[${required.join(",")}] optional=[${optional.join(",")}] actual=[${actual.sort().join(",")}]`,
    );
  }
  return record;
};

/**
 * The journal validator as a left fold: `push` accepts the next entry or
 * throws, carrying the same protocol state (open intents, confirmed hashes,
 * attempt counters) between calls that the whole-journal validator threads
 * through its loop. Pushing entries 0..n one at a time is exactly validating
 * the n+1-entry journal; the stores lean on that to validate each append
 * against retained state instead of refolding the whole history, which is
 * what made a long-running workflow's appends quadratic.
 *
 * A fold that has thrown is poisoned: the failed entry may have mutated part
 * of the state before the check that rejected it. Discard it and refold from
 * the durable entries.
 */
export type FraudProofWorkflowJournalFold = Readonly<{
  /** Number of entries accepted so far, which is also the next sequence. */
  readonly length: number;
  push(entry: FraudProofWorkflowJournalEntry): void;
}>;
