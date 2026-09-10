import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario as runScenario,
} from "./support/submit-init-emulator-shared.js";

/**
 * Acceptance criteria for this family, stated once.
 *
 * The two exec-unit ceilings are the reason the phase-A item journeys are run
 * on the emulator at all: the ledger's per-transaction budget is what a
 * forged-successor proof has to fit inside, and a change that made any leg
 * cost more than this would make the family unprovable on L1. They used to sit
 * behind `MIDGARD_WRITE_FIT_LEDGER`, so no CI run ever evaluated them (§14);
 * they are now checked on every submitted transaction of every scenario.
 *
 * The former `/tmp/nip-phase-a-item-measurements.json` writer and the
 * `docs/fault-proofs/size-plans/validation-trace-phase-a-native-item-fit-ledger.json`
 * writer are gone with it. Neither product had a consumer — the ledger file
 * was never checked in and nothing read it — and a saved measurement is not an
 * acceptance criterion (§13). The measurements themselves are retained: they
 * are what the ceilings below are evaluated against.
 */
const MEMORY_UNIT_CEILING = 13_200_000n;
const CPU_UNIT_CEILING = 8_000_000_000n;

/**
 * The catalogue identity the correction must carry. Taken from the SDK
 * catalogue's registration for this family, not from the runner's own output.
 */
const VALIDATION_TRACE_DISPUTE_CATEGORY_ID = "00000006";

/**
 * Every scenario this file is required to execute, split by outcome: ten
 * forged-successor journeys are started, of which exactly six reach award and
 * removal and four are refused at semantic resolution. `afterAll` checks both
 * numbers, so a scenario that stops being discovered — renamed away, filtered
 * out by a shared helper, or lost to an `it.each` that generated nothing —
 * fails the file instead of shrinking it silently (§14), and an
 * always-refusing implementation cannot satisfy the six accepting cases (§5).
 * A deliberately filtered run (`vitest -t …`) is expected to trip this gate.
 */
const REQUIRED_SCENARIO_OUTCOMES = { started: 10, completed: 6 } as const;

let started = 0;
let completed = 0;
const runForcedValidationDisputeScenario = async (
  ...[fixture, options]: Parameters<typeof runScenario>
) => {
  const shape = expect.getState().currentTestName!;
  const attempt = started;
  started += 1;
  const result = await runScenario(fixture, {
    ...options,
    onSubmittedTransaction: (m, cbor) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      const outputs = body.outputs();
      let publication = m.executionMemory === 0n && m.executionSteps === 0n;
      for (let i = 0; i < outputs.len(); i++)
        publication ||= outputs.get(i).script_ref() !== undefined;
      const mint = body.mint();
      if (mint !== undefined) {
        const policies = mint.keys();
        const name = CML.AssetName.from_hex(
          FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
        );
        for (let i = 0; i < policies.len(); i++)
          publication ||= (mint.get(policies.get(i), name) ?? 0n) > 0n;
      }
      const label = `${shape}/attempt-${attempt.toString()}/${publication ? "publication" : "lifecycle"}`;
      expect(m.executionMemory, `${label} memory units`).toBeLessThanOrEqual(
        MEMORY_UNIT_CEILING,
      );
      expect(m.executionSteps, `${label} cpu units`).toBeLessThanOrEqual(
        CPU_UNIT_CEILING,
      );
    },
  });
  completed++;
  return result;
};

/**
 * A completed forged-successor journey: the award transaction landed, and the
 * correction removed the block the scenario actually forged.
 */
const expectAwardedAndRemoved = (
  result: Awaited<ReturnType<typeof runForcedValidationDisputeScenario>>,
): void => {
  expect(result.awardResult?.txHash).toMatch(/^[0-9a-f]{64}$/u);
  const removal = result.removal;
  if (removal === undefined) throw new Error("removal did not run");
  expect(removal.transactions.length).toBeGreaterThan(0);
  // `removedHeaderHash` is read back off the state-queue node's own asset
  // name, so this states that the correction consumed the forged block rather
  // than merely that some removal transaction succeeded.
  const headerHash = result.setup.headerHash;
  expect(removal.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
    removal.transactions.map(() => headerHash),
  );
  // The permanent proof unit the correction referenced, and the catalogue
  // identity it was minted under.
  expect(removal.fraudProofOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
  expect(removal.fraudCategory).toBe("validationTraceDispute");
  expect(removal.fraudCategoryId).toBe(VALIDATION_TRACE_DISPUTE_CATEGORY_ID);
};

afterAll(() => {
  expect({ started, completed }).toEqual(REQUIRED_SCENARIO_OUTCOMES);
});

it.each(["native", "foreign"] as const)(
  "proves the %s phase-A item successor through permanent proof and removal",
  async (kind) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "phaseANativeScripts",
          plutusSelection: kind === "foreign",
        }),
      { phaseANativeItemYieldKind: kind },
    );
    expectAwardedAndRemoved(result);
  },
  180_000,
);

it.each(["native", "foreign"] as const)(
  "refuses a forged %s successor against an honest trace",
  async (kind) => {
    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "phaseANativeScripts",
            plutusSelection: kind === "foreign",
            dishonestChallenger: true,
          }),
        { phaseANativeItemYieldKind: kind },
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);

it("resumes the exact late native continuation through permanent proof and removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "phaseANativeScripts",
        lateNativeItem: true,
      }),
    { phaseANativeItemYieldKind: "native" },
  );
  expectAwardedAndRemoved(result);
}, 180_000);

it.each(["native", "foreign"] as const)(
  "refuses the opposite yield for a %s item",
  async (kind) => {
    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "phaseANativeScripts",
            plutusSelection: kind === "foreign",
          }),
        { phaseANativeItemYieldKind: kind === "native" ? "foreign" : "native" },
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);

it("proves the maximum 32KiB certified native item through removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now, prepareFieldCarriage }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        prepareFieldCarriage,
        disputedPhase: "phaseANativeScripts",
        nativeItemWidth: 10918,
      }),
    { phaseANativeItemYieldKind: "native", phaseANativeItemMaximum: true },
  );
  expectAwardedAndRemoved(result);
}, 900_000);

it("cancels the prepared native item and completes a fresh out-ref-driven attempt", async () => {
  const build = ({
    operatorVkey,
    now,
  }: {
    operatorVkey: string;
    now: number;
  }) =>
    buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey,
      now,
      disputedPhase: "phaseANativeScripts",
    });
  const cancelled = await runForcedValidationDisputeScenario(build, {
    phaseANativeItemYieldKind: "native",
    cancelPreparedSemantic: true,
  });
  expect(cancelled.cancellation?.txHash).toMatch(/^[0-9a-f]{64}$/u);
  expect(cancelled.awardResult).toBeUndefined();
  expect(cancelled.removal).toBeUndefined();
  const resumed = await runForcedValidationDisputeScenario(build, {
    phaseANativeItemYieldKind: "native",
  });
  expectAwardedAndRemoved(resumed);
}, 180_000);
