/** The actions of the validation-trace dispute: Init the thread, Open the
 * claim, VerifySource it into a case and its bond, close the convicted case
 * into its fraud-proof record, and the challenger's moves on a defended
 * case's threads: reveal a midpoint, time out a defender whose clock ran
 * out, enter a thread ready for its one step into the resolution and
 * resolve it hop by hop to the award, close a thread won either way into
 * the case, and slash the defender who lost it by its lost-defense receipt.
 * Each action is keyed by the output of ours it consumes; the outputs it
 * leaves are read back from the chain. */
import { CML } from "@lucid-evolution/lucid";

import {
  exact,
  OUT_REF,
  sha256,
} from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";

export const VALIDATION_TRACE_DISPUTE_ACTION =
  "midgard-validation-trace-dispute-action-v1" as const;

export const VALIDATION_TRACE_STAGES = [
  "init",
  "open",
  "verify_source",
  "close",
  "reveal",
  "timeout",
  "close_thread",
  "enter_resolution",
  "resolve",
  "close_awarded",
  "slash_lost_defense",
] as const;

export type ValidationTraceStage = (typeof VALIDATION_TRACE_STAGES)[number];

/** The challenger's moves on a defended case's threads, and the slash of a
 * defender who lost one, which spends that thread's receipt as a move. */
export const isValidationTraceMove = (stage: ValidationTraceStage): boolean =>
  stage !== "init" &&
  stage !== "open" &&
  stage !== "verify_source" &&
  stage !== "close";

/** The outputs each stage leaves, in the order its recovery lists them. */
export const VALIDATION_TRACE_STAGE_ROLES = {
  init: ["thread"],
  open: ["opener"],
  verify_source: ["case", "bond"],
  close: ["proof"],
  reveal: ["thread"],
  timeout: ["thread"],
  close_thread: ["case", "receipt"],
  enter_resolution: ["thread"],
  resolve: ["thread"],
  close_awarded: ["case", "receipt"],
  slash_lost_defense: [],
} as const satisfies Record<ValidationTraceStage, readonly string[]>;

export type ValidationTraceOutputRole =
  (typeof VALIDATION_TRACE_STAGE_ROLES)[ValidationTraceStage][number];

export type ParsedValidationTraceAction = Readonly<{
  stage: ValidationTraceStage;
  /**
   * Init: the thread of ours it replaces after that one vanished (null
   * first). Open: our thread it spends. VerifySource: our opener it spends.
   * Close: our case it spends, beside the case's bond. Reveal and timeout:
   * the defended thread at the game they spend. Close thread: the thread at
   * the timeout validator it closes into the case. Enter resolution: the
   * thread at the game ready for its one step. Resolve: the thread at the
   * one-step holder it moves on. Close awarded: the thread at the award it
   * closes into the case. Slash lost defense: the receipt of our won thread
   * it spends.
   */
  consumes: string | null;
}>;

export const validationTraceActionId = ({
  stage,
  consumes,
}: ParsedValidationTraceAction): string =>
  `validation-trace-${stage.replaceAll("_", "-")}:${consumes ?? "first"}`;

export const validationTraceAction = (
  parsed: ParsedValidationTraceAction,
): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: validationTraceActionId(parsed),
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_DISPUTE_ACTION,
      stage: parsed.stage,
      consumes: parsed.consumes,
    }),
  });

/** The exact fields of a validation-trace action, or throws. */
export const parseValidationTraceAction = (
  action: FraudProofWorkflowAction,
): ParsedValidationTraceAction => {
  const input = exact(
    action.input,
    ["schemaVersion", "stage", "consumes"],
    "validation-trace action",
  );
  const stage = input.stage as ValidationTraceStage;
  const consumes = input.consumes;
  if (
    input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION ||
    !VALIDATION_TRACE_STAGES.includes(stage) ||
    !(
      (consumes === null && stage === "init") ||
      (typeof consumes === "string" && OUT_REF.test(consumes))
    )
  )
    throw new Error("validation-trace action is malformed");
  const parsed = Object.freeze({ stage, consumes: consumes as string | null });
  if (action.actionId !== validationTraceActionId(parsed))
    throw new Error("validation-trace action changed identity");
  return parsed;
};

/** One output of ours a stage left, as our own view finds it. */
export type ValidationTraceOutputClaim = Readonly<{
  role: ValidationTraceOutputRole;
  outRef: string;
  address: string;
  datumCborSha256: string;
  unit: string;
}>;

/** The content digest of an inline datum, over its canonical encoding. */
export const validationTraceDatumDigest = (datumCbor: string): string =>
  sha256(CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex());
