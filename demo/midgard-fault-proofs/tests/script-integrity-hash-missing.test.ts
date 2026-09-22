import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyScriptIntegrityHashMissingFinding,
  encodeScriptIntegrityHashMissingDecisionState,
  prepareScriptIntegrityHashMissingEvidence,
  runScriptIntegrityHashMissingWorkflow,
  type ScriptIntegrityHashMissingEvidence,
  scriptIntegrityHashMissingEvidenceDigest,
  scriptIntegrityHashMissingFaultHolds,
  type ScriptIntegrityHashMissingJournal,
  type ScriptIntegrityHashMissingWorkflowStage,
  selectScriptIntegrityHashMissingCarriage,
} from "../src/script-integrity-hash-missing/family.js";
import { scriptIntegrityHashMissingUsesDirectRoute } from "../src/script-integrity-hash-missing/staged-plan.js";

const TX_ID = "42".repeat(32);
const HEADER = "33".repeat(28);

const acceptedEvidence = (
  overrides: Partial<ScriptIntegrityHashMissingEvidence> = {},
): ScriptIntegrityHashMissingEvidence => ({
  finding: {
    category: "scriptIntegrityHashMissing",
    headerHash: HEADER,
    transactionId: TX_ID,
    direction: "wrongfulAcceptance",
    source: "accepted",
    rejectionReason: null,
  },
  subject: acceptedVerdictSubject(TX_ID),
  nativeTxCompactCbor: "80",
  witnessSetCompactCbor: "80",
  fieldPreimageLengthsCbor: "80",
  scriptWitnessesPreimageCbor: "80",
  redeemersPreimageCbor: "81",
  scriptIntegrityHash: EMPTY_NULL_ROOT.toString("hex"),
  scriptLanguages: [3],
  redeemerCount: 0,
  ...overrides,
});

describe("scriptIntegrityHashMissing V1", () => {
  it("matches canonical Phase-A semantics in both decisive effectful arms", () => {
    expect(scriptIntegrityHashMissingFaultHolds(acceptedEvidence())).toBe(true);
    expect(
      scriptIntegrityHashMissingFaultHolds(
        acceptedEvidence({ scriptLanguages: [0], redeemerCount: 1 }),
      ),
    ).toBe(true);
    expect(
      scriptIntegrityHashMissingFaultHolds(
        acceptedEvidence({ scriptLanguages: [0], redeemerCount: 0 }),
      ),
    ).toBe(false);
    expect(
      scriptIntegrityHashMissingFaultHolds(
        acceptedEvidence({ scriptIntegrityHash: "01".repeat(32) }),
      ),
    ).toBe(false);
  });

  it("binds the exact nullary forced reason and refuses other families", () => {
    const forced = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
      rejectionReason: "ScriptIntegrityHashMissing",
    });
    expect(forced.rejection_reason).toBe("ScriptIntegrityHashMissing");
    expect(() =>
      classifyScriptIntegrityHashMissingFinding({
        ...acceptedEvidence().finding,
        direction: "wrongfulRejection",
        source: "forced",
        rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
      }),
    ).toThrow(/not ScriptIntegrityHashMissing/);
  });

  it("has a pinned cross-language decision-state wire", () => {
    expect(
      encodeScriptIntegrityHashMissingDecisionState(
        acceptedEvidence(),
      ).toString("hex"),
    ).toBe(
      `845828860100005820${TX_ID}40805820${EMPTY_NULL_ROOT.toString("hex")}d87a80d87980`,
    );
  });

  it("refuses one-coordinate and source mutations before submission", () => {
    expect(() =>
      prepareScriptIntegrityHashMissingEvidence(
        acceptedEvidence({
          subject: acceptedVerdictSubject("99".repeat(32)),
        }),
      ),
    ).toThrow(/transaction differs/);
    expect(() =>
      prepareScriptIntegrityHashMissingEvidence(
        acceptedEvidence({
          finding: { ...acceptedEvidence().finding, source: "forced" },
        }),
      ),
    ).toThrow(/source\/direction differs/);
  });

  it("binds durable journal identity to all authenticated retained bytes", () => {
    const baseline = acceptedEvidence();
    expect(scriptIntegrityHashMissingEvidenceDigest(baseline)).not.toBe(
      scriptIntegrityHashMissingEvidenceDigest({
        ...baseline,
        scriptWitnessesPreimageCbor: "81",
      }),
    );
    expect(scriptIntegrityHashMissingEvidenceDigest(baseline)).not.toBe(
      scriptIntegrityHashMissingEvidenceDigest({
        ...baseline,
        nativeTxCompactCbor: "81",
      }),
    );
  });

  it("selects direct, published-root, raw-field, and certified carriage deterministically", () => {
    expect(
      selectScriptIntegrityHashMissingCarriage({
        membershipBytes: 100,
        fieldBytes: 100,
      }),
    ).toBe("direct");
    expect(
      selectScriptIntegrityHashMissingCarriage({
        membershipBytes: 9_000,
        fieldBytes: 10,
      }),
    ).toBe("published");
    expect(
      selectScriptIntegrityHashMissingCarriage({
        membershipBytes: 10,
        fieldBytes: 9_000,
      }),
    ).toBe("rawFields");
    expect(
      selectScriptIntegrityHashMissingCarriage({
        membershipBytes: 10,
        fieldBytes: 15_149,
      }),
    ).toBe("certifiedFields");
  });

  it("takes the direct route only when both fields fit its item cap and byte budget", () => {
    const small = {
      scriptItemCount: 1,
      redeemerItemCount: 64,
      fieldBytes: 900,
    };
    expect(scriptIntegrityHashMissingUsesDirectRoute(small)).toBe(true);
    // One redeemer past the cap has no direct route however small the bytes.
    expect(
      scriptIntegrityHashMissingUsesDirectRoute({
        ...small,
        redeemerItemCount: 65,
      }),
    ).toBe(false);
    expect(
      scriptIntegrityHashMissingUsesDirectRoute({
        ...small,
        scriptItemCount: 65,
      }),
    ).toBe(false);
    expect(
      scriptIntegrityHashMissingUsesDirectRoute({
        ...small,
        fieldBytes: 15_149,
      }),
    ).toBe(false);
    expect(() =>
      scriptIntegrityHashMissingUsesDirectRoute({
        ...small,
        redeemerItemCount: -1,
      }),
    ).toThrow(/negative/);
  });

  it("reconstructs init through permanent mint/removal from chain state after restart", async () => {
    let stage: ScriptIntegrityHashMissingWorkflowStage = "absent";
    const entries: ScriptIntegrityHashMissingJournal[] = [];
    const order: ScriptIntegrityHashMissingWorkflowStage[] = [];
    const progression: Record<string, ScriptIntegrityHashMissingWorkflowStage> =
      {
        init: "init",
        step01: "step01",
        step02: "step02",
        step03: "step03",
        scriptGrammar: "scriptGrammar",
        scriptScan: "scriptScan",
        redeemerGrammar: "redeemerGrammar",
        step04: "step04",
        remove: "complete",
      };
    const deps = {
      loadJournal: async () => entries.at(-1) ?? null,
      appendJournal: async (entry: ScriptIntegrityHashMissingJournal) => {
        entries.push(entry);
      },
      observeStage: async () => stage,
      submit: async (
        action:
          | "init"
          | "step01"
          | "step02"
          | "step03"
          | "scriptGrammar"
          | "scriptScan"
          | "redeemerGrammar"
          | "step04"
          | "remove",
      ) => {
        order.push(action);
        stage = progression[action]!;
        return {
          txHash: action.charCodeAt(0).toString(16).padStart(2, "0").repeat(32),
        };
      },
    };
    const result = await runScriptIntegrityHashMissingWorkflow({
      workflowId: "sihm-1",
      evidence: acceptedEvidence(),
      deps,
    });
    expect(order).toEqual([
      "init",
      "step01",
      "step02",
      "step03",
      "scriptGrammar",
      "scriptScan",
      "redeemerGrammar",
      "step04",
      "remove",
    ]);
    expect(result.stage).toBe("complete");
    order.length = 0;
    await runScriptIntegrityHashMissingWorkflow({
      workflowId: "sihm-1",
      evidence: acceptedEvidence(),
      deps,
    });
    expect(order).toEqual([]);
  });

  it("covers maximum evidence counts without changing the decisive rule", () => {
    const languages = Array.from({ length: 224 }, (_, index) =>
      index === 223 ? 128 : 0,
    ) as (0 | 128)[];
    expect(
      scriptIntegrityHashMissingFaultHolds(
        acceptedEvidence({ scriptLanguages: languages, redeemerCount: 224 }),
      ),
    ).toBe(true);
    expect(() =>
      scriptIntegrityHashMissingFaultHolds(
        acceptedEvidence({ redeemerCount: -1 }),
      ),
    ).toThrow(/non-negative/);
  });
});
