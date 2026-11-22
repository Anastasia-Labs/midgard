import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { WorkflowActionChangedError } from "./action-changed.js";
import type { JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";
import type { SCRIPT_DECODE_PREREQUISITE } from "./script-decode-prerequisite.actions.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";
import { WorkflowFundingUnavailableError } from "./workflow-funding-shortfall.js";

/** The reason prefix of a record the prover cannot advance on its own. */
export const SCRIPT_DECODE_RECORD_HELD = "script-decode-record-held" as const;

/** The reason prefix of a record paused until the prover wallet is funded. */
export const SCRIPT_DECODE_RECORD_UNDERFUNDED =
  "script-decode-record-underfunded" as const;

/**
 * The prover wallet cannot pay for the next record transaction. The
 * workflow pauses on it, and resumes on a later observation once funded.
 * Record transactions are funded from plain-ADA coins only, and the reason
 * says so: a coin holding a token is never spent.
 */
export class ScriptDecodeRecordUnderfundedError extends WorkflowFundingUnavailableError {
  constructor(
    readonly stage: string,
    override readonly cause: unknown,
  ) {
    super();
    this.name = "ScriptDecodeRecordUnderfundedError";
    this.message = `${SCRIPT_DECODE_RECORD_UNDERFUNDED}: the prover wallet cannot fund ${stage} from its plain-ADA coins (a coin holding a token is never spent): ${cause instanceof Error ? cause.message : String(cause)}`;
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
  | {
      readonly kind: "required";
      readonly action: FraudProofWorkflowAction;
    };

export interface ScriptDecodePrerequisitePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof SCRIPT_DECODE_PREREQUISITE;
  readonly category: Category;
  inspect(input: {
    readonly headerHash: string;
    /** The workflow that finds the staged chunks, and its objective. */
    readonly holder: ProofMaterialsHolder;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<ScriptDecodeInspection>;
  capture(input: {
    readonly headerHash: string;
    readonly holder: ProofMaterialsHolder;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<{
    readonly transaction: LocallyEvaluatedTransaction;
  }>;
}
