import { createHash, randomUUID } from "node:crypto";
import { link, mkdir, open, readdir, readFile, unlink } from "node:fs/promises";
import { join } from "node:path";

import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

export const FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION =
  "midgard-fraud-proof-workflow-identity-v1" as const;
export const FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION =
  "midgard-fraud-proof-workflow-journal-entry-v1" as const;

export type FraudProofWorkflowTarget =
  | {
      readonly kind: "state_queue_header";
      /** Canonical 28-byte Midgard header hash. */
      readonly headerHash: string;
    }
  | {
      readonly kind: "settlement_claim";
      /** Canonical, externally authenticated claim identity. */
      readonly claimId: string;
    };

export type FraudProofWorkflowIdentity = {
  readonly schemaVersion: typeof FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly target: FraudProofWorkflowTarget;
  /** Present on production runs; omitted only by lower-level diagnostics. */
  readonly decisionDigest?: string;
};

export type JournalJsonPrimitive = string | number | boolean | null;
export type JournalJsonValue =
  | JournalJsonPrimitive
  | readonly JournalJsonValue[]
  | { readonly [key: string]: JournalJsonValue };
export type JournalJsonObject = {
  readonly [key: string]: JournalJsonValue;
};

export const FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION =
  "midgard-fraud-proof-workflow-terminal-v1" as const;

/**
 * The terminal state a workflow is allowed to persist.  These are chain facts,
 * not an adapter-owned success message: the orchestrator independently asks a
 * production terminal verifier to authenticate them before appending the
 * terminal journal entry.
 */
export type FraudProofWorkflowTerminal = {
  readonly schemaVersion: typeof FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly proofToken: {
    readonly unit: string;
    readonly outRef: string;
    readonly createdByTxHash: string;
    readonly retainedAtFinalState: true;
  };
  readonly correction: {
    readonly removalTxHash: string;
    readonly removedStateQueueOutRef: string;
    readonly fraudulentHeaderAbsent: true;
    /** Removal consumed the fraud witness by reference, without spending it. */
    readonly referencedProofTokenOutRef: string;
  };
  readonly economics: {
    readonly operatorCredential: string;
    readonly proverCredential: string;
    /** Exact directory-node input consumed by slashing, null if already slashed. */
    readonly operatorBondInputOutRef: string | null;
    readonly operatorBondInputLovelace: string;
    readonly slashedLovelace: string;
    /** Exact reward output, null for the current compiled zero-reward profile. */
    readonly proverRewardOutputOutRef: string | null;
    readonly proverRewardLovelace: string;
    readonly removalFeeLovelace: string;
    readonly duplicateRewardAbsent: true;
  };
  readonly observedAt: {
    readonly slot: string;
    readonly blockHash: string;
    readonly confirmationDepth: number;
  };
};

const normalizeHeaderHash = (value: string): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]{56}$/u.test(normalized)) {
    throw new Error("workflow headerHash must be 28-byte lowercase hex");
  }
  return normalized;
};

const normalizeClaimId = (value: string): string => {
  const normalized = value.trim();
  if (normalized.length === 0 || normalized !== value) {
    throw new Error(
      "workflow claimId must be a non-empty canonical string without surrounding whitespace",
    );
  }
  return normalized;
};

const normalizeDecisionDigest = (value: string): string => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error("workflow decisionDigest must be 32-byte lowercase hex");
  }
  return value;
};

export const normalizeFraudProofWorkflowIdentity = (
  identity: FraudProofWorkflowIdentity,
): FraudProofWorkflowIdentity => {
  if (identity.schemaVersion !== FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION) {
    throw new Error(
      `workflow identity schemaVersion must be ${String(FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION)}`,
    );
  }
  if (
    !(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER as readonly string[]).includes(
      identity.category,
    )
  ) {
    throw new Error(`unknown workflow category: ${String(identity.category)}`);
  }
  const target: FraudProofWorkflowTarget =
    identity.target.kind === "state_queue_header"
      ? {
          kind: "state_queue_header",
          headerHash: normalizeHeaderHash(identity.target.headerHash),
        }
      : identity.target.kind === "settlement_claim"
        ? {
            kind: "settlement_claim",
            claimId: normalizeClaimId(identity.target.claimId),
          }
        : (() => {
            throw new Error("unknown workflow target kind");
          })();
  return Object.freeze({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: normalizeDaDeploymentFingerprintHex(
      identity.deploymentFingerprint,
    ),
    category: identity.category,
    target: Object.freeze(target),
    ...(identity.decisionDigest === undefined
      ? {}
      : { decisionDigest: normalizeDecisionDigest(identity.decisionDigest) }),
  });
};

const stableJson = (value: JournalJsonValue): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, child]) => `${JSON.stringify(key)}:${stableJson(child)}`)
    .join(",")}}`;
};

export const normalizeJournalJson = (
  value: unknown,
  field = "journal value",
): JournalJsonValue => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value) || !Number.isSafeInteger(value)) {
      throw new Error(`${field} number must be a finite safe integer`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return Object.freeze(
      value.map((entry, index) =>
        normalizeJournalJson(entry, `${field}[${index.toString()}]`),
      ),
    );
  }
  if (typeof value !== "object" || value === null) {
    throw new Error(`${field} must be JSON-safe`);
  }
  const prototype = Object.getPrototypeOf(value) as unknown;
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must be a plain JSON object`);
  }
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value as Readonly<Record<string, unknown>>)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [
          key,
          normalizeJournalJson(child, `${field}.${key}`),
        ]),
    ),
  );
};

export const journalJsonDigest = (value: JournalJsonValue): string =>
  createHash("sha256").update(stableJson(value)).digest("hex");

export const computeFraudProofWorkflowId = (
  identity: FraudProofWorkflowIdentity,
): string => {
  const normalized = normalizeFraudProofWorkflowIdentity(identity);
  const target =
    normalized.target.kind === "state_queue_header"
      ? `header:${normalized.target.headerHash}`
      : `claim:${normalized.target.claimId}`;
  return createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        normalized.deploymentFingerprint,
        normalized.category,
        target,
        ...(normalized.decisionDigest === undefined
          ? []
          : [`decision:${normalized.decisionDigest}`]),
      ].join("\u0000"),
    )
    .digest("hex");
};

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

const workflowIdPattern = /^[0-9a-f]{64}$/u;

const validateWorkflowId = (workflowId: string): void => {
  if (!workflowIdPattern.test(workflowId)) {
    throw new Error("workflowId must be 32-byte lowercase hex");
  }
};

const requireRecord = (
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

const requireExactKeys = (
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

const requireOptionalExactKeys = (
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

export const createFraudProofWorkflowJournalFold = ({
  workflowId,
  expectedIdentity,
}: {
  readonly workflowId: string;
  readonly expectedIdentity?: FraudProofWorkflowIdentity;
}): FraudProofWorkflowJournalFold => {
  validateWorkflowId(workflowId);
  // Every accepted entry's identity derives `workflowId` (checked below), so
  // an entry matches the requested identity exactly when the requested
  // identity derives the same id. Hashing it once here replaces a pair of
  // identity hashes per entry.
  const expectedIdentityMatches =
    expectedIdentity === undefined ||
    computeFraudProofWorkflowId(
      normalizeFraudProofWorkflowIdentity(expectedIdentity),
    ) === workflowId;
  const latestPreflightByAction = new Map<
    string,
    Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "preflight_passed" }
    >
  >();
  const latestIntentByAction = new Map<
    string,
    Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "submission_intent" }
    >
  >();
  const unresolvedSubmissionByAction = new Map<
    string,
    "intent" | "submitted" | "ambiguous" | "pending" | "reconciled_confirmed"
  >();
  const confirmedReconciliationByAction = new Map<string, string>();
  const confirmedTransactionHashes = new Set<string>();
  const attemptsByAction = new Map<string, number>();
  const broadcastsByTransaction = new Map<string, number>();
  let completed = false;
  let previousEvent: FraudProofWorkflowJournalEvent | undefined;
  const requireTxHash = (value: string, field: string): void => {
    if (!/^[0-9a-f]{64}$/u.test(value)) {
      throw new Error(`${field} must be 32-byte lowercase hex`);
    }
  };
  const requireOutRef = (value: string, field: string): void => {
    if (!/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(value)) {
      throw new Error(`${field} must be a canonical transaction outRef`);
    }
  };
  let length = 0;
  const step = (
    entry: FraudProofWorkflowJournalEntry,
    sequence: number,
  ): void => {
    if (completed) {
      throw new Error("journal contains an event after terminal completion");
    }
    requireExactKeys(
      entry,
      [
        "schemaVersion",
        "workflowId",
        "identity",
        "sequence",
        "recordedAt",
        "event",
      ],
      `journal entry ${sequence.toString()}`,
    );
    requireOptionalExactKeys(
      entry.identity,
      ["schemaVersion", "deploymentFingerprint", "category", "target"],
      ["decisionDigest"],
      `journal entry ${sequence.toString()} identity`,
    );
    const target = requireRecord(
      entry.identity.target,
      `journal entry ${sequence.toString()} identity target`,
    );
    if (target.kind === "state_queue_header") {
      requireExactKeys(
        target,
        ["kind", "headerHash"],
        `journal entry ${sequence.toString()} state-queue target`,
      );
    } else if (target.kind === "settlement_claim") {
      requireExactKeys(
        target,
        ["kind", "claimId"],
        `journal entry ${sequence.toString()} settlement target`,
      );
    } else {
      throw new Error(
        `journal entry ${sequence.toString()} has an unknown target kind`,
      );
    }
    if (entry.schemaVersion !== FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION) {
      throw new Error(`journal entry ${sequence.toString()} has wrong schema`);
    }
    if (entry.sequence !== sequence) {
      throw new Error(
        `journal entry sequence gap: expected=${sequence.toString()} actual=${String(entry.sequence)}`,
      );
    }
    if (entry.workflowId !== workflowId) {
      throw new Error(
        `journal entry ${sequence.toString()} changed workflowId`,
      );
    }
    if (computeFraudProofWorkflowId(entry.identity) !== workflowId) {
      throw new Error(
        `journal entry ${sequence.toString()} identity does not derive workflowId`,
      );
    }
    if (!expectedIdentityMatches) {
      throw new Error(
        `journal entry ${sequence.toString()} does not match requested workflow identity`,
      );
    }
    if (
      !Number.isFinite(Date.parse(entry.recordedAt)) ||
      new Date(entry.recordedAt).toISOString() !== entry.recordedAt
    ) {
      throw new Error(
        `journal entry ${sequence.toString()} recordedAt is invalid`,
      );
    }
    const event = entry.event;
    const previous = previousEvent;
    if (event.kind !== "stalled") previousEvent = event;
    const eventRecord = requireRecord(
      event,
      `journal entry ${sequence.toString()} event`,
    );
    if (sequence === 0 && eventRecord.kind !== "started") {
      throw new Error("journal must begin with exactly one started event");
    }
    if (sequence > 0 && eventRecord.kind === "started") {
      throw new Error("journal contains a duplicate started event");
    }
    if (sequence === 1 && eventRecord.kind !== "prepared") {
      throw new Error("journal second event must be the prepared artifact");
    }
    if (sequence > 1 && eventRecord.kind === "prepared") {
      throw new Error("journal contains a duplicate prepared artifact");
    }
    if (eventRecord.kind === "started") {
      requireExactKeys(event, ["kind"], "journal started event");
      return;
    }
    if (event.kind === "prepared") {
      if (previous?.kind !== "started") {
        throw new Error(
          "journal prepared artifact must immediately follow started",
        );
      }
      requireExactKeys(
        event,
        ["kind", "artifact", "artifactDigest"],
        "journal prepared event",
      );
      requireRecord(event.artifact, "journal prepared artifact");
      normalizeJournalJson(event.artifact, "journal prepared artifact");
      if (!/^[0-9a-f]{64}$/u.test(event.artifactDigest)) {
        throw new Error("journal prepared artifactDigest must be 32-byte hex");
      }
      if (journalJsonDigest(event.artifact) !== event.artifactDigest) {
        throw new Error(
          `journal entry ${sequence.toString()} prepared artifact digest mismatch`,
        );
      }
      return;
    }
    if (event.kind === "preflight_passed") {
      if (unresolvedSubmissionByAction.size > 0) {
        throw new Error(
          "journal cannot preflight another submission before reconciling the unresolved intent",
        );
      }
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash", "localEvaluator", "referenceScripts"],
        "journal preflight event",
      );
      if (
        event.actionId.trim().length === 0 ||
        event.actionId.trim() !== event.actionId ||
        event.localEvaluator.trim().length === 0
      ) {
        throw new Error(
          "journal preflight actionId/evaluator must be canonical non-empty strings",
        );
      }
      requireTxHash(event.txHash, "journal preflight txHash");
      if (!Array.isArray(event.referenceScripts)) {
        throw new Error("journal preflight referenceScripts must be an array");
      }
      const roles = new Set<string>();
      for (const reference of event.referenceScripts) {
        requireExactKeys(
          reference,
          ["role", "outRef", "scriptHash"],
          "journal preflight reference script",
        );
        if (
          reference.role.trim().length === 0 ||
          reference.role.trim() !== reference.role ||
          roles.has(reference.role)
        ) {
          throw new Error(
            "journal preflight reference-script roles must be unique canonical strings",
          );
        }
        roles.add(reference.role);
        requireOutRef(
          reference.outRef,
          "journal preflight reference-script outRef",
        );
        if (!/^[0-9a-f]{56}$/u.test(reference.scriptHash)) {
          throw new Error(
            "journal preflight reference-script hash must be 28-byte hex",
          );
        }
      }
      latestPreflightByAction.set(event.actionId, event);
      return;
    }
    if (event.kind === "submission_intent") {
      if (
        previous?.kind !== "preflight_passed" ||
        previous.actionId !== event.actionId
      ) {
        throw new Error(
          "journal submission intent must immediately follow its preflight",
        );
      }
      requireOptionalExactKeys(
        event,
        ["kind", "actionId", "actionInput", "attempt", "txHash"],
        ["durableRecovery"],
        "journal submission intent",
      );
      requireRecord(event.actionInput, "journal action input");
      normalizeJournalJson(event.actionInput, "journal action input");
      if (event.durableRecovery !== undefined) {
        requireRecord(event.durableRecovery, "journal durable recovery");
        normalizeJournalJson(event.durableRecovery, "journal durable recovery");
      }
      if (
        event.actionId.trim().length === 0 ||
        event.actionId.trim() !== event.actionId ||
        !Number.isSafeInteger(event.attempt) ||
        event.attempt < 1
      ) {
        throw new Error("journal submission intent fields are not canonical");
      }
      requireTxHash(event.txHash, "journal intent txHash");
      const preflight = latestPreflightByAction.get(event.actionId);
      if (preflight === undefined || preflight.txHash !== event.txHash) {
        throw new Error(
          `journal intent ${event.actionId} lacks a matching exact-body preflight`,
        );
      }
      const expectedAttempt = (attemptsByAction.get(event.actionId) ?? 0) + 1;
      if (event.attempt !== expectedAttempt) {
        throw new Error(
          `journal intent ${event.actionId} attempt mismatch: expected=${expectedAttempt.toString()} actual=${event.attempt.toString()}`,
        );
      }
      attemptsByAction.set(event.actionId, event.attempt);
      latestIntentByAction.set(event.actionId, event);
      broadcastsByTransaction.set(event.txHash, 1);
      unresolvedSubmissionByAction.set(event.actionId, "intent");
      return;
    }
    if (event.kind === "rebroadcast_intent") {
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash", "attempt"],
        "journal rebroadcast intent",
      );
      const intent = latestIntentByAction.get(event.actionId);
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        !unresolvedSubmissionByAction.has(event.actionId) ||
        unresolvedSubmissionByAction.get(event.actionId) ===
          "reconciled_confirmed" ||
        intent?.txHash !== event.txHash ||
        !Number.isSafeInteger(event.attempt) ||
        event.attempt !== (broadcastsByTransaction.get(event.txHash) ?? 0) + 1
      )
        throw new Error(
          "journal rebroadcast differs from its unresolved exact transaction",
        );
      broadcastsByTransaction.set(event.txHash, event.attempt);
      return;
    }
    if (event.kind === "submitted" || event.kind === "submission_ambiguous") {
      if (
        previous?.kind !== "submission_intent" ||
        previous.actionId !== event.actionId ||
        previous.attempt !== event.attempt
      ) {
        throw new Error(
          `journal ${event.kind} must immediately follow its durable intent`,
        );
      }
      if (event.kind === "submitted") {
        requireExactKeys(
          event,
          ["kind", "actionId", "attempt", "txHash"],
          "journal submitted event",
        );
      } else {
        requireOptionalExactKeys(
          event,
          ["kind", "actionId", "attempt", "detail"],
          ["txHash"],
          "journal ambiguous-submission event",
        );
        if (event.detail.trim().length === 0) {
          throw new Error(
            "journal ambiguous-submission detail must not be empty",
          );
        }
      }
      if (!Number.isSafeInteger(event.attempt) || event.attempt < 1) {
        throw new Error(
          "journal submission attempt must be a positive integer",
        );
      }
      const intent = latestIntentByAction.get(event.actionId);
      if (intent === undefined || intent.attempt !== event.attempt) {
        throw new Error(
          `journal ${event.kind} ${event.actionId} lacks its matching durable intent`,
        );
      }
      if (event.txHash !== undefined) {
        requireTxHash(event.txHash, `journal ${event.kind} txHash`);
        if (event.txHash !== intent.txHash) {
          throw new Error(
            `journal ${event.kind} ${event.actionId} changed the intended transaction hash`,
          );
        }
      }
      unresolvedSubmissionByAction.set(
        event.actionId,
        event.kind === "submitted" ? "submitted" : "ambiguous",
      );
      return;
    }
    if (event.kind === "reobserved") {
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash"],
        "journal reobservation",
      );
      requireTxHash(event.txHash, "journal reobservation txHash");
      const intent = latestIntentByAction.get(event.actionId);
      if (intent?.txHash !== event.txHash)
        throw new Error("journal reobservation lacks its exact signed intent");
      // Submission history remains intact. Only the current reconciliation cursor
      // moves back; later attempts must be observed again before they are reused.
      unresolvedSubmissionByAction.clear();
      unresolvedSubmissionByAction.set(event.actionId, "pending");
      confirmedReconciliationByAction.delete(event.actionId);
      confirmedTransactionHashes.delete(event.txHash);
      return;
    }
    if (event.kind === "reconciled") {
      const unresolvedState = unresolvedSubmissionByAction.get(event.actionId);
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        unresolvedState === undefined ||
        unresolvedState === "reconciled_confirmed"
      ) {
        throw new Error(
          "journal reconciliation must follow an unresolved matching submission",
        );
      }
      requireOptionalExactKeys(
        event,
        ["kind", "actionId", "outcome"],
        ["txHash"],
        "journal reconciliation event",
      );
      if (
        event.outcome !== "confirmed" &&
        event.outcome !== "pending" &&
        event.outcome !== "not_found"
      ) {
        throw new Error("journal reconciliation outcome is unknown");
      }
      const intent = latestIntentByAction.get(event.actionId);
      if (intent === undefined) {
        throw new Error(
          `journal reconciliation ${event.actionId} lacks a durable intent`,
        );
      }
      if (event.txHash !== undefined) {
        requireTxHash(event.txHash, "journal reconciliation txHash");
        if (event.txHash !== intent.txHash) {
          throw new Error(
            `journal reconciliation ${event.actionId} changed the intended transaction hash`,
          );
        }
      }
      if (event.outcome === "confirmed") {
        if (event.txHash === undefined) {
          throw new Error(
            `journal confirmed reconciliation ${event.actionId} omitted txHash`,
          );
        }
        confirmedReconciliationByAction.set(event.actionId, event.txHash);
        unresolvedSubmissionByAction.set(
          event.actionId,
          "reconciled_confirmed",
        );
      } else if (event.outcome === "pending") {
        unresolvedSubmissionByAction.set(event.actionId, "pending");
      } else {
        unresolvedSubmissionByAction.delete(event.actionId);
      }
      return;
    }
    if (event.kind === "confirmed") {
      if (
        unresolvedSubmissionByAction.size !== 1 ||
        unresolvedSubmissionByAction.get(event.actionId) !==
          "reconciled_confirmed"
      ) {
        throw new Error(
          "journal confirmation must follow matching confirmed reconciliation",
        );
      }
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash"],
        "journal confirmation event",
      );
      requireTxHash(event.txHash, "journal confirmed txHash");
      if (
        confirmedReconciliationByAction.get(event.actionId) !== event.txHash
      ) {
        throw new Error(
          `journal confirmation ${event.actionId} lacks matching authenticated reconciliation`,
        );
      }
      confirmedTransactionHashes.add(event.txHash);
      unresolvedSubmissionByAction.delete(event.actionId);
      return;
    }
    if (event.kind === "completed" || event.kind === "terminal_included") {
      requireExactKeys(
        event,
        ["kind", "terminal", "terminalDigest"],
        "journal completed event",
      );
      requireExactKeys(
        event.terminal,
        [
          "schemaVersion",
          "category",
          "headerHash",
          "proofToken",
          "correction",
          "economics",
          "observedAt",
        ],
        "journal terminal",
      );
      requireExactKeys(
        event.terminal.proofToken,
        ["unit", "outRef", "createdByTxHash", "retainedAtFinalState"],
        "journal terminal proof token",
      );
      requireExactKeys(
        event.terminal.correction,
        [
          "removalTxHash",
          "removedStateQueueOutRef",
          "fraudulentHeaderAbsent",
          "referencedProofTokenOutRef",
        ],
        "journal terminal correction",
      );
      requireExactKeys(
        event.terminal.economics,
        [
          "operatorCredential",
          "proverCredential",
          "operatorBondInputOutRef",
          "operatorBondInputLovelace",
          "slashedLovelace",
          "proverRewardOutputOutRef",
          "proverRewardLovelace",
          "removalFeeLovelace",
          "duplicateRewardAbsent",
        ],
        "journal terminal economics",
      );
      requireExactKeys(
        event.terminal.observedAt,
        ["slot", "blockHash", "confirmationDepth"],
        "journal terminal observation",
      );
      if (
        event.terminal.schemaVersion !==
        FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION
      ) {
        throw new Error("journal terminal has an unsupported schema");
      }
      const terminalJson = normalizeJournalJson(
        event.terminal,
        "journal terminal",
      );
      if (!/^[0-9a-f]{64}$/u.test(event.terminalDigest)) {
        throw new Error("journal terminalDigest must be 32-byte hex");
      }
      if (journalJsonDigest(terminalJson) !== event.terminalDigest) {
        throw new Error(
          `journal entry ${sequence.toString()} terminal digest mismatch`,
        );
      }
      if (
        entry.identity.target.kind !== "state_queue_header" ||
        event.terminal.category !== entry.identity.category ||
        event.terminal.headerHash !== entry.identity.target.headerHash
      ) {
        throw new Error("journal terminal does not match workflow identity");
      }
      if (!/^(?:[0-9a-f]{2}){28,60}$/u.test(event.terminal.proofToken.unit)) {
        throw new Error(
          "journal terminal contains a malformed proof-token unit",
        );
      }
      requireOutRef(
        event.terminal.proofToken.outRef,
        "journal terminal proof-token outRef",
      );
      requireOutRef(
        event.terminal.correction.removedStateQueueOutRef,
        "journal terminal removed state-queue outRef",
      );
      requireOutRef(
        event.terminal.correction.referencedProofTokenOutRef,
        "journal terminal referenced proof-token outRef",
      );
      if (event.terminal.economics.operatorBondInputOutRef !== null) {
        requireOutRef(
          event.terminal.economics.operatorBondInputOutRef,
          "journal terminal operator-bond input outRef",
        );
      }
      if (event.terminal.economics.proverRewardOutputOutRef !== null) {
        requireOutRef(
          event.terminal.economics.proverRewardOutputOutRef,
          "journal terminal prover-reward output outRef",
        );
      }
      if (
        event.terminal.correction.fraudulentHeaderAbsent !== true ||
        event.terminal.economics.duplicateRewardAbsent !== true ||
        !/^[0-9a-f]{56}$/u.test(event.terminal.economics.operatorCredential) ||
        !/^[0-9a-f]{56}$/u.test(event.terminal.economics.proverCredential) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.operatorBondInputLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(event.terminal.economics.slashedLovelace) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.proverRewardLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(
          event.terminal.economics.removalFeeLovelace,
        ) ||
        !/^(0|[1-9][0-9]*)$/u.test(event.terminal.observedAt.slot) ||
        !/^[0-9a-f]{64}$/u.test(event.terminal.observedAt.blockHash) ||
        !Number.isSafeInteger(event.terminal.observedAt.confirmationDepth) ||
        event.terminal.observedAt.confirmationDepth < 1
      ) {
        throw new Error("journal terminal facts are not canonical");
      }
      const proofCreationTxHash = event.terminal.proofToken.createdByTxHash;
      const removalTxHash = event.terminal.correction.removalTxHash;
      requireTxHash(
        proofCreationTxHash,
        "journal terminal proof creation txHash",
      );
      requireTxHash(removalTxHash, "journal terminal removal txHash");
      if (
        proofCreationTxHash === removalTxHash ||
        !confirmedTransactionHashes.has(proofCreationTxHash) ||
        !confirmedTransactionHashes.has(removalTxHash)
      ) {
        throw new Error(
          "journal terminal requires distinct confirmed proof creation and removal transactions",
        );
      }
      if (
        event.terminal.proofToken.retainedAtFinalState !== true ||
        event.terminal.correction.referencedProofTokenOutRef !==
          event.terminal.proofToken.outRef ||
        "spentByTxHash" in event.terminal.proofToken ||
        "proofTokenSpent" in event.terminal.correction
      ) {
        throw new Error(
          "journal terminal must retain and exactly reference the permanent proof token",
        );
      }
      completed = event.kind === "completed";
      return;
    }
    if (event.kind === "stalled") {
      requireExactKeys(event, ["kind", "reason"], "journal stalled event");
      if (event.reason.trim().length === 0) {
        throw new Error("journal stalled reason must not be empty");
      }
      return;
    }
    throw new Error(
      `journal entry ${sequence.toString()} has unknown event kind: ${String(eventRecord.kind)}`,
    );
  };
  return {
    get length() {
      return length;
    },
    push(entry) {
      step(entry, length);
      length += 1;
    },
  };
};

export const validateFraudProofWorkflowJournal = ({
  workflowId,
  entries,
  expectedIdentity,
}: {
  readonly workflowId: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly expectedIdentity?: FraudProofWorkflowIdentity;
}): readonly FraudProofWorkflowJournalEntry[] => {
  const fold = createFraudProofWorkflowJournalFold({
    workflowId,
    expectedIdentity,
  });
  for (const entry of entries) fold.push(entry);
  return entries;
};

/**
 * A validated journal prefix together with the fold that accepted it, so the
 * next append validates one entry against retained state.
 */
type ValidatedJournal = {
  readonly entries: FraudProofWorkflowJournalEntry[];
  readonly fold: FraudProofWorkflowJournalFold;
};

const foldJournal = (
  workflowId: string,
  entries: readonly FraudProofWorkflowJournalEntry[],
): ValidatedJournal => {
  const fold = createFraudProofWorkflowJournalFold({ workflowId });
  for (const entry of entries) fold.push(entry);
  return { entries: [...entries], fold };
};

/**
 * Validates `entry` as the next entry of `journal`. Appending an entry whose
 * identity does not derive the workflow id fails inside the fold, which is
 * the same rejection the whole-journal validator reaches through its
 * requested-identity check. The fold is poisoned when `push` throws, so the
 * caller must drop the cached journal on failure.
 */
const pushValidated = (
  journal: ValidatedJournal,
  entry: FraudProofWorkflowJournalEntry,
): void => {
  journal.fold.push(entry);
  journal.entries.push(entry);
};

/** In-memory store with optimistic sequence checks, useful for embedded use. */
export class MemoryFraudProofWorkflowJournalStore
  implements FraudProofWorkflowJournalStore
{
  private readonly journalsByWorkflow = new Map<string, ValidatedJournal>();

  async load(
    workflowId: string,
  ): Promise<readonly FraudProofWorkflowJournalEntry[]> {
    validateWorkflowId(workflowId);
    return [...(this.journalsByWorkflow.get(workflowId)?.entries ?? [])];
  }

  async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    const current = this.journalsByWorkflow.get(entry.workflowId);
    const currentLength = current?.entries.length ?? 0;
    if (
      currentLength !== expectedSequence ||
      entry.sequence !== expectedSequence
    ) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `journal sequence changed: expected=${expectedSequence.toString()} actual=${currentLength.toString()}`,
      );
    }
    const journal = current ?? foldJournal(entry.workflowId, []);
    try {
      pushValidated(journal, entry);
    } catch (error) {
      // The fold is poisoned; rebuild from the entries that were accepted.
      if (current !== undefined) {
        this.journalsByWorkflow.set(
          entry.workflowId,
          foldJournal(entry.workflowId, current.entries),
        );
      }
      throw error;
    }
    this.journalsByWorkflow.set(entry.workflowId, journal);
  }
}

/**
 * Crash-safe filesystem journal: one immutable, fsynced file per sequence.
 * The final hard-link is an atomic compare-and-append; concurrent processes
 * racing for the same sequence cannot both win. Temporary files are ignored on
 * recovery and never treated as submitted/confirmed evidence.
 */
export class DirectoryFraudProofWorkflowJournalStore
  implements FraudProofWorkflowJournalStore
{
  constructor(private readonly rootDirectory: string) {}

  /**
   * The validated journal per workflow as this store last read it from disk,
   * with the entry file names it covers. Entry files are immutable once
   * linked into place (the append protocol below never rewrites one), so a
   * later listing that still begins with exactly these names extends the
   * cached prefix: only the newer files are read and folded. Any other
   * listing, such as a directory that was removed and rebuilt, is refolded
   * from scratch. Nothing here is trusted across the cache's own name check.
   */
  private readonly journalsByWorkflow = new Map<
    string,
    ValidatedJournal & { readonly entryNames: readonly string[] }
  >();

  private workflowDirectory(workflowId: string): string {
    validateWorkflowId(workflowId);
    return join(this.rootDirectory, workflowId);
  }

  private async loadValidated(
    workflowId: string,
  ): Promise<
    (ValidatedJournal & { readonly entryNames: readonly string[] }) | undefined
  > {
    const directory = this.workflowDirectory(workflowId);
    let names: string[];
    try {
      names = await readdir(directory);
    } catch (error) {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        this.journalsByWorkflow.delete(workflowId);
        return undefined;
      }
      throw error;
    }
    const entryNames = names
      .filter((name) => /^\d{8}\.json$/u.test(name))
      .sort();
    const cached = this.journalsByWorkflow.get(workflowId);
    const extendsCached =
      cached !== undefined &&
      cached.entryNames.length <= entryNames.length &&
      cached.entryNames.every((name, index) => name === entryNames[index]);
    const journal =
      extendsCached && cached !== undefined
        ? cached
        : { ...foldJournal(workflowId, []), entryNames: [] as string[] };
    const newNames = entryNames.slice(journal.entryNames.length);
    const newEntries = await Promise.all(
      newNames.map(
        async (name) =>
          JSON.parse(
            await readFile(join(directory, name), "utf8"),
            (_key, value) => value,
          ) as FraudProofWorkflowJournalEntry,
      ),
    );
    try {
      for (const entry of newEntries) pushValidated(journal, entry);
    } catch (error) {
      this.journalsByWorkflow.delete(workflowId);
      throw error;
    }
    const validated = { ...journal, entryNames };
    this.journalsByWorkflow.set(workflowId, validated);
    return validated;
  }

  async load(
    workflowId: string,
  ): Promise<readonly FraudProofWorkflowJournalEntry[]> {
    const journal = await this.loadValidated(workflowId);
    return journal === undefined ? [] : [...journal.entries];
  }

  async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    if (entry.sequence !== expectedSequence) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `entry sequence ${entry.sequence.toString()} does not equal expected ${expectedSequence.toString()}`,
      );
    }
    const directory = this.workflowDirectory(entry.workflowId);
    await mkdir(directory, { recursive: true });
    const current = await this.loadValidated(entry.workflowId);
    if (current === undefined) {
      throw new Error("journal directory vanished while appending");
    }
    if (current.entries.length !== expectedSequence) {
      throw new ConcurrentFraudProofWorkflowWriteError(
        `journal sequence changed: expected=${expectedSequence.toString()} actual=${current.entries.length.toString()}`,
      );
    }
    // Validate against the fold's retained state. The fold now carries the
    // entry, so until the hard-link below has made it part of the on-disk
    // history any failure must drop the cached journal rather than leave a
    // fold ahead of the directory.
    const finalName = `${expectedSequence.toString().padStart(8, "0")}.json`;
    const commitCache = (): void => {
      this.journalsByWorkflow.set(entry.workflowId, {
        entries: [...current.entries, entry],
        fold: current.fold,
        entryNames: [...current.entryNames, finalName],
      });
    };
    this.journalsByWorkflow.delete(entry.workflowId);
    current.fold.push(entry);
    const finalPath = join(directory, finalName);
    const temporaryPath = join(
      directory,
      `.${expectedSequence.toString().padStart(8, "0")}.${randomUUID()}.tmp`,
    );
    const handle = await open(temporaryPath, "wx", 0o600);
    try {
      await handle.writeFile(`${JSON.stringify(entry)}\n`, "utf8");
      await handle.sync();
    } finally {
      await handle.close();
    }
    try {
      await link(temporaryPath, finalPath);
    } catch (error) {
      await unlink(temporaryPath).catch(() => undefined);
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "EEXIST"
      ) {
        throw new ConcurrentFraudProofWorkflowWriteError(
          `journal sequence ${expectedSequence.toString()} was written concurrently`,
        );
      }
      throw error;
    }
    commitCache();
    await unlink(temporaryPath);
    const directoryHandle = await open(directory, "r");
    try {
      await directoryHandle.sync();
    } finally {
      await directoryHandle.close();
    }
  }
}
