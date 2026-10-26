/**
 * `unusedRedeemer` emulator verticals from a genuine Init: an accepted
 * transaction carrying a redeemer with no purpose, a native purpose's
 * pointer, or a repeated pointer convicts and its block is removed; a forced
 * rejection naming an UnusedRedeemer the transaction does not carry
 * convicts, as does a Spend pointer at a key-locked input; the walk to an
 * honest transaction's consumed observer or Spend redeemer is refused;
 * and an admitted forced transaction, which carries no redeemers, gives the
 * prover nothing and the terminal refuses the walk past its empty field.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  requireConvictingUnusedRedeemerProof,
  UnusedRedeemerHonestLeaf,
} from "../src/unused-redeemer/index.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  nativeScript,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  driveUnusedRedeemerThread,
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
  removeConvictedBlock,
} from "./support/unused-redeemer-emulator.rows.js";

const DATA = Buffer.from(Data.to(0n), "hex");

/** Every redeemer is consumed: one Plutus observer with its witness. */
const CONSUMED: MissingRedeemerSubjectShape = {
  observers: [{ script: plutusScript(1), redeemer: true }],
};

/** The honest forced subject: no redeemer, and an observer with no source. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...honestForcedObserverShape(5),
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};

describe("unused-redeemer genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction carrying a redeemer with no purpose and removes its block", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: { ...CONSUMED, paddingRedeemers: [DATA] },
      source: { kind: "normal" },
    });
    const { run, outcome } = await convictAndCheck(
      harness,
      scenario,
      "A-N purposeless",
    );
    expect(run.prepared.redeemerIndex).toBe(1n);
    expect(run.prepared.final.claim).toStrictEqual({
      ImpossiblePurpose: { coordinate: { kind: 2n, index: 1n } },
    });
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.unusedRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
    expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
    for (const step of harness.unusedRedeemer.steps)
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(0);
    const removal = await removeConvictedBlock(harness, scenario);
    expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
      "remove-target",
    ]);
  }, 1_200_000);

  it("A-N: convicts an accepted transaction carrying a native purpose's pointer", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: { observers: [{ script: nativeScript(5n), redeemer: true }] },
      source: { kind: "normal" },
    });
    const { run } = await convictAndCheck(harness, scenario, "A-N native");
    expect(run.prepared.redeemerIndex).toBe(0n);
    expect(run.prepared.final.claim).toHaveProperty("NativeSource");
  }, 1_200_000);

  it("A-N: convicts an accepted transaction repeating a pointer, decided by the walk", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x41,
          [null],
          scriptAddress(plutusScript(9)),
        ),
        witnesses: [plutusScript(9)],
        spendRedeemers: [0n, 0n],
      },
      source: { kind: "normal" },
    });
    const { run } = await convictAndCheck(harness, scenario, "A-N repeated");
    expect(run.prepared.redeemerIndex).toBe(1n);
    expect(run.prepared.final.claim).toBe("RedeemerRepeated");
    // No claim custody: the walk hands its result straight to the terminal.
    expect(
      run.prepared.transitions.some((step) => step.role === "claimStart"),
    ).toBe(false);
  }, 1_200_000);

  it("R-F: convicts a forced rejection naming an UnusedRedeemer the transaction does not carry", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SOURCELESS,
      source: { kind: "forced", verdict: unusedRedeemerVerdict(0n) },
    });
    expect(scenario.material.bound.subject.source_kind).toBe(
      SDK.PROOF_THREAD_SOURCE_KIND_FORCED,
    );
    const { run } = await convictAndCheck(harness, scenario, "R-F false");
    expect(run.prepared.final.claim).toBe("RedeemerOutOfRange");
  }, 1_200_000);

  it("refuses the walk to an honest transaction's consumed redeemer", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: CONSUMED,
      source: { kind: "normal" },
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
    expect(run.prepared.final.claim).toHaveProperty("SourcePresent");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);

  it("refuses the walk to an honest transaction's consumed Spend redeemer", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x44,
          [null],
          scriptAddress(plutusScript(9)),
        ),
        witnesses: [plutusScript(9)],
        spendRedeemers: [0n],
      },
      source: { kind: "normal" },
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
    // The pointer's spend coordinate is the one the claim opens.
    expect(run.prepared.transitions.map(({ role }) => role)).toContain(
      "claimSpendScan",
    );
    expect(run.prepared.final.claim).toHaveProperty("SourcePresent");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);

  it("A-N: convicts an accepted transaction's Spend pointer at a key-locked input", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: {
        observers: [],
        spend: missingRedeemerLedgerEntries(0x45, [null]),
        spendRedeemers: [0n],
      },
      source: { kind: "normal" },
    });
    const { run } = await convictAndCheck(harness, scenario, "A-N key spend");
    expect(run.prepared.redeemerIndex).toBe(0n);
    expect(run.prepared.transitions.map(({ role }) => role)).toContain(
      "claimSpendScan",
    );
    expect(run.prepared.final.claim).toStrictEqual({
      ImpossiblePurpose: { coordinate: { kind: 0n, index: 0n } },
    });
  }, 1_200_000);

  it("A-F: an admitted forced transaction carries no redeemer; the prover finds nothing and the terminal refuses the walk", async () => {
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SOURCELESS,
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    expect(scenario.material.fields[8]).toHaveLength(0);
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
    expect(run.prepared.final.claim).toBe("RedeemerOutOfRange");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);
});
