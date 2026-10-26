/**
 * `receivePurposeLanguage` emulator verticals from a genuine Init: an
 * accepted transaction executing a PlutusV3 receive purpose with a redeemer
 * convicts and its block is removed; a forced rejection recording
 * `ReceivePurposePlutusV3Forbidden` convicts at every coordinate where that
 * execution does not exist (a non-receive coordinate, an ordinal past the
 * last receive purpose, an absent source, a non-PlutusV3 source, a PlutusV3
 * source no redeemer reaches); an accepted transaction's non-PlutusV3
 * receive is refused; and an admitted forced transaction, which carries no
 * redeemers, gives the prover nothing and the terminal refuses the observed
 * route to its PlutusV3 receive.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ReceivePurposeLanguageHonestLeaf,
  requireConvictingReceivePurposeLanguageProof,
} from "../src/receive-purpose-language/index.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerSubjectShape,
  nativeScript,
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
  removeConvictedBlock,
} from "./support/receive-purpose-language-emulator.rows.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

/** One receive output whose script is PlutusV3, carried as a witness. */
const receiving = (
  script: MissingRedeemerSubjectShape["observers"][number]["script"],
  { redeemer, witness = true }: { redeemer: boolean; witness?: boolean },
): MissingRedeemerSubjectShape => ({
  observers: [],
  receives: [{ script, redeemer, witness }],
});

const claimKind = (claim: SDK.DirectClaimedPredicateFact): string =>
  typeof claim === "object" ? Object.keys(claim)[0]! : claim;

describe("receive-purpose-language genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction executing a PlutusV3 receive purpose with a redeemer and removes its block", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: receiving(plutusScript(3), { redeemer: true }),
      source: { kind: "normal" },
    });
    const { run, outcome } = await convictAndCheck(harness, scenario, "A-N");
    expect(claimKind(run.prepared.final.claim)).toBe("RedeemerPresent");
    expect(
      SDK.receivePurposeLanguageFactCoordinate(run.prepared.final.claim),
    ).toStrictEqual(receiveCoordinate(0n));
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.receivePurposeLanguage.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
    expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
    for (const step of harness.receivePurposeLanguage.steps)
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

  it.each([
    [
      "a non-receive coordinate",
      honestForcedObserverShape(5),
      receivePurposeLanguageVerdict(2n, 0n),
      "ImpossiblePurpose",
      { kind: 2n, index: 0n },
    ],
    [
      "an ordinal past the last receive purpose",
      receiving(plutusScript(4), { redeemer: false }),
      receivePurposeLanguageVerdict(3n, 1n),
      "ImpossiblePurpose",
      receiveCoordinate(1n),
    ],
    [
      "a receive purpose with no source",
      receiving(plutusScript(4), { redeemer: false, witness: false }),
      receivePurposeLanguageVerdict(3n, 0n),
      "SourceAbsent",
      receiveCoordinate(0n),
    ],
    [
      "a receive purpose with a native source",
      receiving(nativeScript(7n), { redeemer: false }),
      receivePurposeLanguageVerdict(3n, 0n),
      "SourcePresent",
      receiveCoordinate(0n),
    ],
    [
      "a PlutusV3 receive purpose no redeemer reaches",
      receiving(plutusScript(4), { redeemer: false }),
      receivePurposeLanguageVerdict(3n, 0n),
      "RedeemerAbsent",
      receiveCoordinate(0n),
    ],
  ] as const)(
    "R-F: convicts a forced rejection recording the arm at %s",
    async (label, shape, verdict, claim, coordinate) => {
      const harness = await makeReceivePurposeLanguageHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape,
        source: { kind: "forced", verdict },
      });
      expect(scenario.material.bound.subject.source_kind).toBe(
        SDK.PROOF_THREAD_SOURCE_KIND_FORCED,
      );
      const { run } = await convictAndCheck(harness, scenario, `R-F ${label}`);
      expect(claimKind(run.prepared.final.claim)).toBe(claim);
      expect(
        SDK.receivePurposeLanguageFactCoordinate(run.prepared.final.claim),
      ).toStrictEqual(coordinate);
      // A non-receive coordinate goes from the family entry to the terminal.
      expect(
        run.prepared.transitions.some((step) => step.role === "receiveStart"),
      ).toBe(coordinate.kind === 3n);
    },
    1_200_000,
  );

  it("refuses an accepted transaction's native receive purpose with a redeemer", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: receiving(nativeScript(7n), { redeemer: true }),
      source: { kind: "normal" },
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
    expect(claimKind(run.prepared.final.claim)).toBe("SourcePresent");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);

  it("A-F: an admitted forced transaction carries no redeemer; the prover finds nothing and the terminal refuses the observed PlutusV3 receive", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: receiving(plutusScript(4), { redeemer: false }),
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    expect(scenario.material.fields[8]).toHaveLength(0);
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
    expect(claimKind(run.prepared.final.claim)).toBe("RedeemerAbsent");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);
});
