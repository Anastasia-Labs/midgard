import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import {
  encodeScriptIntegrityHashMissingDecisionState,
  prepareScriptIntegrityHashMissingEvidence,
  type ScriptIntegrityHashMissingEvidence,
  scriptIntegrityHashMissingFaultHolds,
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
