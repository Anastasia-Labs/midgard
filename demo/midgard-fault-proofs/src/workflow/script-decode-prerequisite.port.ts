import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
// The leaf module, not the barrel: the barrel can still be evaluating when
// the assembly loads this class, and a class must extend a defined value.
import { WorkflowFundingReservationUnavailableError } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "./journal.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import type { SCRIPT_DECODE_PREREQUISITE } from "./script-decode-prerequisite.actions.js";
import type { SignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";

/** The reason prefix of a record the prover cannot advance on its own. */
export const SCRIPT_DECODE_RECORD_HELD = "script-decode-record-held" as const;

/** The reason prefix of a record paused until the prover wallet is funded. */
export const SCRIPT_DECODE_RECORD_UNDERFUNDED =
  "script-decode-record-underfunded" as const;

/**
 * The prover wallet cannot pay for the next record transaction. The
 * workflow pauses on it, and resumes on a later observation once funded.
 */
export class ScriptDecodeRecordUnderfundedError extends WorkflowFundingReservationUnavailableError {
  constructor(
    readonly stage: string,
    override readonly cause: unknown,
  ) {
    super();
    this.name = "ScriptDecodeRecordUnderfundedError";
    this.message = `${SCRIPT_DECODE_RECORD_UNDERFUNDED}: the prover wallet cannot fund ${stage}: ${cause instanceof Error ? cause.message : String(cause)}`;
  }
}

/**
 * The next record transaction does not build: the workflow holds, with this
 * reason, instead of submitting it or stalling.
 */
export class ScriptDecodeRecordHeldError extends WorkflowActionChangedError {
  constructor(
    readonly stage: string,
    override readonly cause: unknown,
  ) {
    super(
      `${SCRIPT_DECODE_RECORD_HELD}: ${stage} does not build: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
    this.name = "ScriptDecodeRecordHeldError";
  }
}

/**
 * The script-decoding item bytes whose Plutus script a base action of the
 * family needs recorded first, or null when it needs none.
 */
export type ScriptDecodeItemForAction = (
  input: Readonly<{
    action: FraudProofWorkflowAction;
    artifact: JournalJsonObject;
  }>,
) => Uint8Array | null | Promise<Uint8Array | null>;

export type ScriptDecodeInspection =
  | { readonly kind: "not_required" | "satisfied" }
  | { readonly kind: "pending"; readonly reason: string }
  | { readonly kind: "required"; readonly action: FraudProofWorkflowAction }
  | {
      /**
       * `action` was rejected on all its bounded attempts: the record holds
       * there, pausing until a fresh observation, and submits it no more.
       */
      readonly kind: "held";
      readonly action: FraudProofWorkflowAction;
      readonly reason: string;
    };

export interface ScriptDecodePrerequisitePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof SCRIPT_DECODE_PREREQUISITE;
  readonly category: Category;
  inspect(input: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<ScriptDecodeInspection>;
  capture(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<{
    readonly transaction: LocallyEvaluatedTransaction;
    readonly durableRecovery: JournalJsonObject;
  }>;
  reconcile(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly txHash?: string;
    readonly durableRecovery?: JournalJsonObject;
    readonly signedTransactionCborHex?: string;
    readonly authorizeResubmission?: (
      input: SignedWorkflowTransaction,
    ) => Promise<void>;
  }): Promise<FraudProofWorkflowReconcileResult>;
}
