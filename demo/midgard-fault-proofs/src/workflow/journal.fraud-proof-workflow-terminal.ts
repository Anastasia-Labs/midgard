import { createHash } from "node:crypto";

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
