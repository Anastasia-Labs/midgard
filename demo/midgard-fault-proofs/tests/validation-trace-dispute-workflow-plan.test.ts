import { describe, expect, it } from "vitest";

import type {
  ValidationTraceDisputeChainStage,
  ValidationTraceDisputeSemanticGroup,
} from "../src/validation-dispute/workflow-chain-state.js";
import {
  planValidationTraceDisputeMove,
  type ValidationTraceDisputeMove,
} from "../src/validation-dispute/workflow-engine.js";
import {
  assertValidationTraceDisputeRosterIsManifestBound,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
} from "../src/validation-dispute/workflow-family.js";

const thread = "aa".repeat(32) + "#0";

const gameStage = (
  turn: "awaiting_operator" | "awaiting_challenger" | "ready_for_one_step",
  timeoutClaimable: boolean,
): ValidationTraceDisputeChainStage =>
  ({
    kind: "game",
    threadOutRef: thread,
    dispute: {} as never,
    turn,
    round: 3,
    lowIndex: 2,
    highIndex: 5,
    responseDeadline: 1_000,
    timeoutClaimable,
  }) as const;

const actionStage = (move: ValidationTraceDisputeMove): string => {
  if (move.kind !== "act") throw new Error(`expected act, got ${move.kind}`);
  return move.action.stage;
};

describe("validationTraceDispute workflow planning (ruling R6)", () => {
  it("binds the manifest roster and category identity at startup", () => {
    expect(() =>
      assertValidationTraceDisputeRosterIsManifestBound(),
    ).not.toThrow();
    expect(VALIDATION_TRACE_DISPUTE_CATEGORY_ID).toBe("00000006");
  });

  it("always owns a move or a deadline from every cursor stage", () => {
    const stages: readonly ValidationTraceDisputeChainStage[] = [
      { kind: "not_started", stateQueueBlockOutRef: thread },
      { kind: "init", threadOutRef: thread, stateQueueBlockOutRef: thread },
      { kind: "open_pending_source", threadOutRef: thread },
      gameStage("awaiting_challenger", false),
      gameStage("ready_for_one_step", false),
      gameStage("awaiting_operator", false),
      gameStage("awaiting_operator", true),
      { kind: "timeout_pending", threadOutRef: thread },
      { kind: "resolution_boundary", threadOutRef: thread },
      {
        kind: "prepare_selected_pending",
        threadOutRef: thread,
        resolverIndex: 4,
      },
      {
        kind: "semantic_pending",
        threadOutRef: thread,
        semanticResolverGlobalIndex: 28,
      },
      {
        kind: "semantic_in_flight",
        threadOutRef: thread,
        group: "cek_core_stage",
        role: "cekCoreStages.step",
      },
      { kind: "award_pending", threadOutRef: thread },
      {
        kind: "proof_token",
        fraudProofOutRef: thread,
        nextRemovalOutRef: thread,
      },
      { kind: "removed" },
    ];
    for (const stage of stages) {
      const move = planValidationTraceDisputeMove({ stage });
      expect(["act", "await_counterparty", "completed"]).toContain(move.kind);
    }
  });

  it("routes the interactive game turns per the honest-challenger discipline", () => {
    expect(
      actionStage(
        planValidationTraceDisputeMove({
          stage: gameStage("awaiting_challenger", false),
        }),
      ),
    ).toBe("reveal");
    expect(
      actionStage(
        planValidationTraceDisputeMove({
          stage: gameStage("ready_for_one_step", false),
        }),
      ),
    ).toBe("enter_resolution");
    expect(
      actionStage(
        planValidationTraceDisputeMove({
          stage: gameStage("awaiting_operator", true),
        }),
      ),
    ).toBe("enter_timeout");
    const waiting = planValidationTraceDisputeMove({
      stage: gameStage("awaiting_operator", false),
    });
    expect(waiting).toEqual({
      kind: "await_counterparty",
      threadOutRef: thread,
      responseDeadline: 1_000,
    });
  });

  it("terminates through timeout, award, and removal stages", () => {
    expect(
      actionStage(
        planValidationTraceDisputeMove({
          stage: { kind: "timeout_pending", threadOutRef: thread },
        }),
      ),
    ).toBe("timeout");
    expect(
      actionStage(
        planValidationTraceDisputeMove({
          stage: { kind: "award_pending", threadOutRef: thread },
        }),
      ),
    ).toBe("award");
    const removal = planValidationTraceDisputeMove({
      stage: {
        kind: "proof_token",
        fraudProofOutRef: "bb".repeat(32) + "#1",
        nextRemovalOutRef: "cc".repeat(32) + "#0",
      },
    });
    expect(removal).toEqual({
      kind: "act",
      action: {
        stage: "remove",
        nextRemovalOutRef: "cc".repeat(32) + "#0",
        fraudProofOutRef: "bb".repeat(32) + "#1",
      },
    });
    expect(
      planValidationTraceDisputeMove({ stage: { kind: "removed" } }),
    ).toEqual({ kind: "completed" });
  });

  it("always owns the cancel move at every interrupted staged-route checkpoint", () => {
    const groups: readonly ValidationTraceDisputeSemanticGroup[] = [
      "cek_material_traversal",
      "cek_core_stage",
      "cek_context_stage",
      "cek_context_item_stage",
      "canonical_decode_item_stage",
      "script_sources_item_stage",
      "proof_item",
    ];
    const retained = {
      transitionCborHex: "d879",
      auxiliaryCborHex: "d87a",
    } as const;
    for (const group of groups) {
      const stage: ValidationTraceDisputeChainStage = {
        kind: "semantic_in_flight",
        threadOutRef: thread,
        group,
        role: group,
      };
      // With or without retained material the workflow owns one legal
      // transaction: cancel the stalled route and restart from init.
      expect(
        actionStage(planValidationTraceDisputeMove({ stage, retained })),
      ).toBe("cancel_semantic_route");
      expect(actionStage(planValidationTraceDisputeMove({ stage }))).toBe(
        "cancel_semantic_route",
      );
    }
  });

  it("threads retained shared-item preparation into the semantic move", () => {
    const move = planValidationTraceDisputeMove({
      stage: {
        kind: "semantic_pending",
        threadOutRef: thread,
        semanticResolverGlobalIndex: 28,
      },
      retained: { scriptSourcesItemPreparedCbor: "d905" },
    });
    expect(move).toEqual({
      kind: "act",
      action: {
        stage: "semantic_resolution",
        threadOutRef: thread,
        scriptSourcesItemPreparedCbor: "d905",
      },
    });
  });
});
