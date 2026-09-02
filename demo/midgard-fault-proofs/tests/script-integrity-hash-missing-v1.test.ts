import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyScriptIntegrityHashMissingFindingV1,
  encodeScriptIntegrityHashMissingDecisionStateV1,
  prepareScriptIntegrityHashMissingEvidenceV1,
  runScriptIntegrityHashMissingWorkflowV1,
  scriptIntegrityHashMissingEvidenceDigestV1,
  type ScriptIntegrityHashMissingEvidenceV1,
  scriptIntegrityHashMissingFaultHoldsV1,
  type ScriptIntegrityHashMissingJournalV1,
  type ScriptIntegrityHashMissingWorkflowStageV1,
  selectScriptIntegrityHashMissingCarriageV1,
} from "../src/script-integrity-hash-missing/family-v1.js";

const TX_ID = "42".repeat(32);
const HEADER = "33".repeat(28);

const acceptedEvidence = (
  overrides: Partial<ScriptIntegrityHashMissingEvidenceV1> = {},
): ScriptIntegrityHashMissingEvidenceV1 => ({
  finding: {
    category: "scriptIntegrityHashMissing",
    headerHash: HEADER,
    transactionId: TX_ID,
    direction: "wrongfulAcceptance",
    source: "accepted",
    rejectionReason: null,
  },
  subject: acceptedVerdictSubjectV1(TX_ID),
  nativeTxCompactCbor: "80",
  witnessSetCompactCbor: "80",
  fieldPreimageLengthsCbor: "80",
  scriptWitnessesPreimageCbor: "80",
  redeemersPreimageCbor: "81",
  scriptIntegrityHash: "00".repeat(32),
  scriptLanguages: [3],
  redeemerCount: 0,
  ...overrides,
});

describe("scriptIntegrityHashMissing V1", () => {
  it("matches canonical Phase-A semantics in both decisive effectful arms", () => {
    expect(scriptIntegrityHashMissingFaultHoldsV1(acceptedEvidence())).toBe(
      true,
    );
    expect(
      scriptIntegrityHashMissingFaultHoldsV1(
        acceptedEvidence({ scriptLanguages: [0], redeemerCount: 1 }),
      ),
    ).toBe(true);
    expect(
      scriptIntegrityHashMissingFaultHoldsV1(
        acceptedEvidence({ scriptLanguages: [0], redeemerCount: 0 }),
      ),
    ).toBe(false);
    expect(
      scriptIntegrityHashMissingFaultHoldsV1(
        acceptedEvidence({ scriptIntegrityHash: "01".repeat(32) }),
      ),
    ).toBe(false);
  });

  it("binds the exact nullary forced reason and refuses other families", () => {
    const forced = forcedVerdictSubjectV1({
      transactionId: TX_ID,
      sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
      rejectionReason: "ScriptIntegrityHashMissing",
    });
    expect(forced.rejection_reason).toBe("ScriptIntegrityHashMissing");
    expect(() =>
      classifyScriptIntegrityHashMissingFindingV1({
        ...acceptedEvidence().finding,
        direction: "wrongfulRejection",
        source: "forced",
        rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
      }),
    ).toThrow(/not ScriptIntegrityHashMissing/);
  });

  it("has a pinned cross-language decision-state wire", () => {
    expect(
      encodeScriptIntegrityHashMissingDecisionStateV1(
        acceptedEvidence(),
      ).toString("hex"),
    ).toBe(`845828860100005820${TX_ID}40805820${"00".repeat(32)}d87a80d87980`);
  });

  it("refuses one-coordinate and source mutations before submission", () => {
    expect(() =>
      prepareScriptIntegrityHashMissingEvidenceV1(
        acceptedEvidence({
          subject: acceptedVerdictSubjectV1("99".repeat(32)),
        }),
      ),
    ).toThrow(/transaction differs/);
    expect(() =>
      prepareScriptIntegrityHashMissingEvidenceV1(
        acceptedEvidence({
          finding: { ...acceptedEvidence().finding, source: "forced" },
        }),
      ),
    ).toThrow(/source\/direction differs/);
  });

  it("binds durable journal identity to all authenticated retained bytes", () => {
    const baseline = acceptedEvidence();
    expect(scriptIntegrityHashMissingEvidenceDigestV1(baseline)).not.toBe(
      scriptIntegrityHashMissingEvidenceDigestV1({
        ...baseline,
        scriptWitnessesPreimageCbor: "81",
      }),
    );
    expect(scriptIntegrityHashMissingEvidenceDigestV1(baseline)).not.toBe(
      scriptIntegrityHashMissingEvidenceDigestV1({
        ...baseline,
        nativeTxCompactCbor: "81",
      }),
    );
  });

  it("selects direct, published-root, raw-field, and certified carriage deterministically", () => {
    expect(
      selectScriptIntegrityHashMissingCarriageV1({
        membershipBytes: 100,
        fieldBytes: 100,
      }),
    ).toBe("direct");
    expect(
      selectScriptIntegrityHashMissingCarriageV1({
        membershipBytes: 9_000,
        fieldBytes: 10,
      }),
    ).toBe("published");
    expect(
      selectScriptIntegrityHashMissingCarriageV1({
        membershipBytes: 10,
        fieldBytes: 9_000,
      }),
    ).toBe("rawFields");
    expect(
      selectScriptIntegrityHashMissingCarriageV1({
        membershipBytes: 10,
        fieldBytes: 15_149,
      }),
    ).toBe("certifiedFields");
  });

  it("reconstructs init through permanent mint/removal from chain state after restart", async () => {
    let stage: ScriptIntegrityHashMissingWorkflowStageV1 = "absent";
    const entries: ScriptIntegrityHashMissingJournalV1[] = [];
    const order: ScriptIntegrityHashMissingWorkflowStageV1[] = [];
    const progression: Record<
      string,
      ScriptIntegrityHashMissingWorkflowStageV1
    > = {
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
      appendJournal: async (entry: ScriptIntegrityHashMissingJournalV1) => {
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
    const result = await runScriptIntegrityHashMissingWorkflowV1({
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
    await runScriptIntegrityHashMissingWorkflowV1({
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
      scriptIntegrityHashMissingFaultHoldsV1(
        acceptedEvidence({ scriptLanguages: languages, redeemerCount: 224 }),
      ),
    ).toBe(true);
    expect(() =>
      scriptIntegrityHashMissingFaultHoldsV1(
        acceptedEvidence({ redeemerCount: -1 }),
      ),
    ).toThrow(/non-negative/);
  });
});
