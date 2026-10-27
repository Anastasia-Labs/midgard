/**
 * `missingRedeemer` purpose-route emulator rows from a genuine Init: a
 * forced rejection whose RedeemerMissing names a purpose the transaction
 * does not lack a redeemer for is convicted through that kind's claim
 * custody (spend, mint, receive); an admitted transaction whose spend, mint
 * or receive purpose has no redeemer is convicted through that kind's
 * observed custody; an honest forced rejection at a spend cannot reach a
 * fraud-proof token.
 */
import { describe, expect, it } from "vitest";

import {
  MissingRedeemerHonestLeaf,
  requireConvictingMissingRedeemerProof,
} from "../src/missing-redeemer/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  type MissingRedeemerHarness,
  publishMissingRedeemerRoles,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.bytes);
  }
};

const convictAndCheck = async (
  harness: MissingRedeemerHarness,
  scenario: Awaited<ReturnType<typeof setupMissingRedeemerScenario>>,
  label: string,
) => {
  const references = await publishMissingRedeemerRoles(harness);
  const run = await driveMissingRedeemerThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`MR ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.missingRedeemer.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  return run;
};

const scenario = async (
  harness: MissingRedeemerHarness,
  shape: MissingRedeemerSubjectShape,
  source: MissingRedeemerSource,
) => setupMissingRedeemerScenario({ harness, shape, source });

const forced = (kind: bigint, index: bigint): MissingRedeemerSource => ({
  kind: "forced",
  verdict: redeemerMissingVerdict(kind, index),
});

/** A signed spend of a key-locked output, with no other purpose. */
const signedSpend = (): MissingRedeemerSubjectShape => ({
  ...honestForcedObserverShape(5),
  observers: [],
  reference: [],
});

/** A spend of an output locked by a Plutus script sourced from a
 * reference input. */
const scriptSpend = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  observers: [],
  spend: missingRedeemerLedgerEntries(
    0x41,
    [null],
    scriptAddress(plutusScript(9)),
  ),
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(9)]),
  ...(redeemer ? { spendRedeemers: [0n] } : {}),
});

/** A signed spend plus a Plutus mint policy sourced from a reference input. */
const referenceMint = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  ...signedSpend(),
  mints: [{ script: plutusScript(7), witness: false, redeemer }],
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(7)]),
});

/** A signed spend plus a protected output whose Plutus credential is
 * sourced from a reference input. */
const referenceReceive = (redeemer: boolean): MissingRedeemerSubjectShape => ({
  ...signedSpend(),
  receives: [{ script: plutusScript(8), witness: false, redeemer }],
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(8)]),
});

describe("missing-redeemer purpose-route emulator rows", () => {
  it("F-A spend claim: convicts a forced rejection naming the key-locked spend", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, signedSpend(), forced(0n, 0n)),
      "F-A spend claim",
    );
    expect(run.prepared.final.claim).toHaveProperty("ImpossiblePurpose");
  }, 1_200_000);

  it("F-A mint claim: convicts a forced rejection naming a mint the transaction lacks", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, signedSpend(), forced(1n, 0n)),
      "F-A mint claim",
    );
    expect(run.prepared.final.claim).toHaveProperty("ImpossiblePurpose");
  }, 1_200_000);

  it("F-A receive claim: convicts a forced rejection naming a receive purpose the transaction lacks", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, signedSpend(), forced(3n, 0n)),
      "F-A receive claim",
    );
    expect(run.prepared.final.claim).toHaveProperty("ImpossiblePurpose");
  }, 1_200_000);

  it("F-A spend observed: convicts an admitted transaction whose script spend has no redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, scriptSpend(false), { kind: "normal" }),
      "F-A spend observed",
    );
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
  }, 1_200_000);

  it("F-A mint observed: convicts an admitted transaction whose mint has no redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, referenceMint(false), { kind: "normal" }),
      "F-A mint observed",
    );
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
  }, 1_200_000);

  it("F-A receive observed: convicts an admitted transaction whose protected output has no redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const run = await convictAndCheck(
      harness,
      await scenario(harness, referenceReceive(false), { kind: "normal" }),
      "F-A receive observed",
    );
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
  }, 1_200_000);

  it("F-A spend honest: refuses a forced rejection whose claimed spend redeemer is absent", async () => {
    const harness = await makeMissingRedeemerHarness();
    const current = await scenario(harness, scriptSpend(false), forced(0n, 0n));
    await expect(
      requireConvictingMissingRedeemerProof({
        material: current.material,
        contracts: harness.missingRedeemer,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingRedeemerHonestLeaf);
    const references = await publishMissingRedeemerRoles(harness);
    const run = await driveMissingRedeemerThread({
      harness,
      scenario: current,
      references,
    });
    expect(run.prepared.convicts).toBe(false);
    expect(run.prepared.final.claim).toHaveProperty("RedeemerAbsent");
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/direct_script_derivation/claim_terminal",
      check: /^Validator returned false$/u,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.missingRedeemer.steps.at(-1)!.spendingScriptAddress,
        run.unit,
      ),
    ).resolves.toHaveLength(1);
  }, 1_200_000);
});
