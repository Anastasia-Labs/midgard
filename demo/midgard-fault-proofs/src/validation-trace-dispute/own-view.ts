/** What the validation-trace dispute knows of its own state on the block a
 * workflow challenges: the claim it disputes, its own thread, pending claim,
 * cases and record, the receipts of the threads it closed, its staging
 * outputs and what its resolve moves read of them, and its field
 * publications. Every read of the family's own state goes through this one
 * port, read from the ledger with our node's mempool applied
 * (own-view.chain.ts). */
import type { UTxO } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { ValidationTraceOutputClaim } from "./actions.js";
import type { ValidationTraceDisputeArtifact } from "./artifact.js";
import type {
  SignedValidationTraceStaging,
  ValidationTraceResolveReader,
  ValidationTraceStagingJournal,
} from "./one-step-staging-journal.js";

/** A receipt of a thread we closed, live as our close left it. */
export type ValidationTraceOwnReceipt = Readonly<{
  outRef: string;
  utxo: UTxO;
}>;

/** A field publication of ours at our proof-materials address. */
export type ValidationTraceOwnPublication = Readonly<{
  outRef: string;
  datumCbor: string;
}>;

/** The family's own state on one challenged block. */
export interface ValidationTraceOwnView {
  /** The claim the workflow disputes: its block, trace root, step count and
   * terminal. Throws for another family's workflow or another block. */
  readonly artifact: () => ValidationTraceDisputeArtifact;
  /** Our thread at the dispute opener, while it is live as Init left it. */
  readonly thread: () => Promise<ValidationTraceOutputClaim | undefined>;
  /** The thread out-ref our latest Init left, live or not: the one a fresh
   * Init names as the thread it replaces. */
  readonly replacedThread: () => Promise<string | undefined>;
  /** Our pending claim at the source, while it is live as Open left it. */
  readonly pendingClaim: () => Promise<ValidationTraceOutputClaim | undefined>;
  /** The opener out-ref of each case an Open of ours started on the block,
   * newest first: with the block and the event key it names the case's
   * token. None means we never opened a case. */
  readonly cases: () => Promise<readonly string[]>;
  /** The case output holding the case token `caseUnit`, while it is live. */
  readonly caseAt: (caseUnit: string) => Promise<UTxO | undefined>;
  /** The fraud-proof record our close of our case minted, while it is live
   * as that close left it. */
  readonly record: () => Promise<ValidationTraceOutputClaim | undefined>;
  /** The receipt of each thread we closed, oldest close first, that is live
   * as our close left it and that no slash of ours has spent. */
  readonly receipts: () => Promise<readonly ValidationTraceOwnReceipt[]>;
  /** The token of each thread of the cases an Open of ours started, live or
   * not. */
  readonly caseThreads: () => Promise<readonly string[]>;
  /** Our staging outputs and the resolve moves of ours that read them. */
  readonly stagings: () => Promise<ValidationTraceStagingJournal>;
  /** Every staging output a staging transaction of ours pays, landed or
   * not: the outputs our proof-materials needs pin. */
  readonly signedStagings: () => Promise<
    readonly SignedValidationTraceStaging[]
  >;
  /** Each resolve move of ours in the next view, with how deep the ledger
   * holds it, the out-refs it read and the outputs it paid, oldest first. */
  readonly resolveReads: () => Promise<readonly ValidationTraceResolveReader[]>;
  /** Our field publications either carriage prerequisite left. */
  readonly publications: () => Promise<
    readonly ValidationTraceOwnPublication[]
  >;
}

/** What the own view of a workflow is read from. */
export type ValidationTraceOwnViewContext = Pick<
  FraudProofWorkflowAdapterContext,
  "workflowId" | "identity" | "artifact"
>;

/** The own view of the workflow `context` runs. */
export type ValidationTraceOwnViewOf = (
  context: ValidationTraceOwnViewContext,
) => ValidationTraceOwnView;
