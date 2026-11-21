/** The alarm a validation-trace action names when the reservation cannot
 * fund it, as its own build would: a lost-defense slash or a move of our
 * case names what must land and by when. */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.js";
import {
  confirmedValidationTraceStages,
  isValidationTraceMove,
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { validationTraceArtifactOf } from "./artifact.js";
import type { validationTraceCaseReads } from "./case-reads.js";
import { validationTraceLostDefenseSlashUnfunded } from "./lost-defense-slash.js";
import type { ValidationTraceLostDefenseSlashDeadlines } from "./lost-defense-slash-due.js";
import { validationTraceThreadMoveUnfunded } from "./move-dispatch.js";

export const validationTraceUnfunded =
  ({
    lucid,
    deadlines,
    ourCase,
  }: {
    readonly lucid: LucidEvolution;
    readonly deadlines: ValidationTraceLostDefenseSlashDeadlines;
    readonly ourCase: ReturnType<typeof validationTraceCaseReads>["ourCase"];
  }): NonNullable<FraudProofFamilyWorkflowAdapter["unfunded"]> =>
  async (context) => {
    const { action, shortfall } = context;
    if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
      return shortfall;
    const { stage, consumes } = parseValidationTraceAction(action);
    const artifact = validationTraceArtifactOf(context);
    if (stage === "slash_lost_defense")
      return await validationTraceLostDefenseSlashUnfunded({
        lucid,
        receiptOutRef: consumes!,
        deadlineMs: async () =>
          await deadlines.ofReceipt(artifact.headerHash, consumes!),
        cause: shortfall.cause,
      });
    if (!isValidationTraceMove(stage)) return shortfall;
    const found = await ourCase(
      artifact,
      confirmedValidationTraceStages(context.entries),
    );
    return found === undefined
      ? shortfall
      : await validationTraceThreadMoveUnfunded({
          stage,
          lucid,
          caseUnit: found.caseUnit,
          threadOutRef: consumes!,
          caseDatum: found.caseDatum,
          cause: shortfall.cause,
        });
  };
