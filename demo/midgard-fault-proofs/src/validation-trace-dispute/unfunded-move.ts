/** The alarm a validation-trace action names when our wallet cannot fund
 * its build: a lost-defense slash or a move of our case names what must
 * land and by when. */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.js";
import {
  isValidationTraceMove,
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import type { validationTraceCaseReads } from "./case-reads.js";
import { validationTraceLostDefenseSlashUnfunded } from "./lost-defense-slash.js";
import type { ValidationTraceLostDefenseSlashDeadlines } from "./lost-defense-slash-due.js";
import { validationTraceThreadMoveUnfunded } from "./move-dispatch.js";
import type { ValidationTraceOwnViewOf } from "./own-view.js";

export const validationTraceUnfunded =
  ({
    lucid,
    deadlines,
    ourCase,
    ownView,
  }: {
    readonly lucid: LucidEvolution;
    readonly deadlines: ValidationTraceLostDefenseSlashDeadlines;
    readonly ourCase: ReturnType<typeof validationTraceCaseReads>["ourCase"];
    readonly ownView: ValidationTraceOwnViewOf;
  }): NonNullable<FraudProofFamilyWorkflowAdapter["unfunded"]> =>
  async (context) => {
    const { action, shortfall } = context;
    if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
      return shortfall;
    const { stage, consumes } = parseValidationTraceAction(action);
    const view = ownView(context);
    const artifact = view.artifact();
    if (stage === "slash_lost_defense")
      return await validationTraceLostDefenseSlashUnfunded({
        lucid,
        receiptOutRef: consumes!,
        deadlineMs: async () =>
          await deadlines.ofReceipt(artifact.headerHash, consumes!),
        cause: shortfall.cause,
      });
    if (!isValidationTraceMove(stage)) return shortfall;
    const found = await ourCase(view, consumes!);
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
