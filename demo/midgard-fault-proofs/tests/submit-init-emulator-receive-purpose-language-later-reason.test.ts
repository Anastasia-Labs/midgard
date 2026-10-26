/**
 * `receivePurposeLanguage` genuine-Init rows for a forced rejection whose
 * transaction executes a PlutusV3 receive purpose with a redeemer. The
 * forced admission screen refuses a transaction carrying redeemers, so these
 * subjects are committed directly: the rows exercise the phase-11 position
 * in the first-fault order. A reason recorded after that execution's
 * language check, or the arm recorded at a later receive purpose, convicts
 * through the first forbidden execution; a reason recorded before the check,
 * and the arm recorded at the forbidden execution itself, reach the terminal
 * and are refused.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  ReceivePurposeLanguageHonestLeaf,
  requireConvictingReceivePurposeLanguageProof,
} from "../src/receive-purpose-language/index.js";
import {
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveReceivePurposeLanguageThread,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receiveCoordinate,
  receivePurposeLanguageVerdict,
} from "./support/receive-purpose-language-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/receive-purpose-language-emulator.rows.js";

/** `count` receive purposes, each a PlutusV3 source a redeemer reaches. */
const forbidden = (count: number): MissingRedeemerSubjectShape => ({
  observers: [],
  receives: Array.from({ length: count }, (_, seed) => ({
    script: plutusScript(seed + 3),
    redeemer: true,
  })),
});

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});

const factCoordinate = (claim: SDK.DirectClaimedPredicateFact) =>
  SDK.receivePurposeLanguageFactCoordinate(claim);

describe("receive-purpose-language genuine-Init rows under a later recorded reason", () => {
  it.each([
    [
      "PlutusExecutionFailed at the same receive purpose",
      forbidden(1),
      rejected({
        PlutusExecutionFailed: { purpose_kind: 3n, purpose_index: 0n },
      }),
    ],
    [
      "the arm at a later receive purpose",
      forbidden(2),
      receivePurposeLanguageVerdict(3n, 1n),
    ],
  ] as const)(
    "reaches a fraud-proof token through the first forbidden execution under a recorded %s",
    async (label, shape, verdict) => {
      const harness = await makeReceivePurposeLanguageHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape,
        source: { kind: "forced", verdict },
      });
      const { run } = await convictAndCheck(harness, scenario, label);
      expect(run.prepared.final.claim).toHaveProperty("RedeemerPresent");
      expect(factCoordinate(run.prepared.final.claim)).toStrictEqual(
        receiveCoordinate(0n),
      );
    },
    1_200_000,
  );

  it.each([
    [
      "a reason before the language check",
      rejected("ScriptIntegrityHashMissing"),
    ],
    ["the arm naming that execution", receivePurposeLanguageVerdict(3n, 0n)],
  ] as const)(
    "refuses the forbidden execution under %s",
    async (_label, verdict) => {
      const harness = await makeReceivePurposeLanguageHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape: forbidden(1),
        source: { kind: "forced", verdict },
      });
      await expect(
        requireConvictingReceivePurposeLanguageProof({
          material: scenario.material,
          contracts: harness.receivePurposeLanguage,
          prover: harness.proverSigner.paymentKeyHash,
        }),
      ).rejects.toThrow(ReceivePurposeLanguageHonestLeaf);
      const references = await publishReceivePurposeLanguageRoles(harness);
      const run = await driveReceivePurposeLanguageThread({
        harness,
        scenario,
        references,
        observed: receiveCoordinate(0n),
      });
      expect(run.prepared.final.claim).toHaveProperty("RedeemerPresent");
      expect(factCoordinate(run.prepared.final.claim)).toStrictEqual(
        receiveCoordinate(0n),
      );
      await expectTerminalRefusal(harness, run);
    },
    1_200_000,
  );
});
