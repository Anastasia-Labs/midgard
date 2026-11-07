/**
 * `unusedRedeemer` genuine-Init rows for a forced rejection recorded under a
 * reason the unused audit precedes. The forced admission screen refuses a
 * transaction carrying redeemers, so these subjects are committed directly:
 * the rows exercise the audit's position in the first-fault order. A reason
 * recorded after the audit, or an UnusedRedeemer naming a later redeemer,
 * convicts through the first unused redeemer; a reason recorded before the
 * audit, and an UnusedRedeemer naming the first unused redeemer, reach the
 * terminal and are refused.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  requireConvictingUnusedRedeemerProof,
  UnusedRedeemerHonestLeaf,
} from "../src/unused-redeemer/index.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerSubjectShape,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveUnusedRedeemerThread,
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/unused-redeemer-emulator.rows.js";

const DATA = Buffer.from(Data.to(0n), "hex");

/** One observer at Reward 0 carrying no redeemer, then `count` Reward
 * redeemers past it, none of which a purpose consumes. */
const purposeless = (count: number): MissingRedeemerSubjectShape => ({
  ...honestForcedObserverShape(5),
  paddingRedeemers: Array.from({ length: count }, () => DATA),
});

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});

/** The first unused redeemer: Reward 1, which no observer occupies. */
const FIRST_UNUSED = {
  ImpossiblePurpose: { coordinate: { kind: 2n, index: 1n } },
};

describe("unused-redeemer genuine-Init rows under a later recorded reason", () => {
  it.each([
    [
      "PlutusExecutionFailed",
      purposeless(1),
      rejected({
        PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
      }),
    ],
    [
      "UnusedRedeemer at a later redeemer",
      purposeless(2),
      unusedRedeemerVerdict(1n),
    ],
  ] as const)(
    "reaches a fraud-proof token through the first unused redeemer under a recorded %s",
    async (label, shape, verdict) => {
      const harness = await makeUnusedRedeemerHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape,
        source: { kind: "forced", verdict },
      });
      const { run } = await convictAndCheck(harness, scenario, label);
      expect(run.prepared.redeemerIndex).toBe(0n);
      expect(run.prepared.final.claim).toStrictEqual(FIRST_UNUSED);
    },
    1_200_000,
  );

  it.each([
    ["a reason before the audit", rejected("ScriptIntegrityHashMissing")],
    ["an UnusedRedeemer naming that redeemer", unusedRedeemerVerdict(0n)],
  ] as const)(
    "refuses the first unused redeemer under %s",
    async (_label, verdict) => {
      const harness = await makeUnusedRedeemerHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape: purposeless(1),
        source: { kind: "forced", verdict },
      });
      await expect(
        requireConvictingUnusedRedeemerProof({
          material: scenario.material,
          contracts: harness.unusedRedeemer,
          prover: harness.proverSigner.paymentKeyHash,
        }),
      ).rejects.toThrow(UnusedRedeemerHonestLeaf);
      const references = await publishUnusedRedeemerRoles(harness);
      const run = await driveUnusedRedeemerThread({
        harness,
        scenario,
        references,
        redeemerIndex: 0n,
      });
      expect(run.prepared.final.claim).toStrictEqual(FIRST_UNUSED);
      await expectTerminalRefusal(harness, run);
    },
    1_200_000,
  );
});
