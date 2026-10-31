import { ChainFixture } from "@al-ft/midgard-test-support/chain-fixture";
import { disposableDatabaseName } from "@al-ft/midgard-test-support/database-identity";
import { RecoveryScenario } from "@al-ft/midgard-test-support/recovery-scenarios";
import { witnessFixture } from "@al-ft/midgard-test-support/witness-fixture";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { OutputReference } from "../src/common.js";

describe("contributor fixtures", () => {
  it("keeps height, slot, inclusion count and ancestry distinct under provider divergence", () => {
    const chain = new ChainFixture();
    const root = chain.append({ slot: 100 });
    const included = chain.append({ parent: root, slot: 700 });
    const tip = chain.append({ parent: included, slot: 900 });
    const fork = chain.append({ parent: root, slot: 901 });
    chain.observe("kupo", tip);
    chain.observe("ogmios", fork);
    expect(chain.evidence(included, "kupo")).toEqual({
      confirmations: 2,
      recoveryDistance: 1,
    });
    expect(chain.evidence(included, "ogmios")).toBeUndefined();
    chain.rollback("kupo", included);
    expect(chain.evidence(included, "kupo")).toEqual({
      confirmations: 1,
      recoveryDistance: 0,
    });
    expect(() => chain.rollback("kupo", fork)).toThrow("ancestor");
    expect(included.height).toBe(1);
    expect(included.synthetic).toBe(true);
  });

  it("encodes a typed witness before assertion and labels deliberate malformed mutation", () => {
    const value = { transactionId: "11".repeat(32), outputIndex: 0n };
    const fixture = witnessFixture(value, (input) =>
      Buffer.from(Data.to(input, OutputReference), "hex"),
    );
    expect(Data.from(fixture.bytes().toString("hex"), OutputReference)).toEqual(
      value,
    );
    const malformed = fixture.mutate((bytes) => {
      bytes[0] = 0xff;
    });
    expect(malformed.malformed).toBe(true);
    expect(malformed.basis).toBe(fixture.identity);
    expect(() =>
      Data.from(malformed.bytes.toString("hex"), OutputReference),
    ).toThrow();
    expect(() => fixture.mutate(() => {})).toThrow("did not change");
    expect(Data.from(fixture.bytes().toString("hex"), OutputReference)).toEqual(
      value,
    );
  });

  it("retains funding on shallow or contradictory evidence, invalidates plans and reopens after rollback", () => {
    const scenario = new RecoveryScenario();
    const stale = scenario.plan();
    scenario.observe({ kind: "included", inclusionHash: "block-a" });
    expect(() => scenario.release(stale)).toThrow("generation");
    expect(() => scenario.release(scenario.plan())).toThrow(
      "authenticated recovery-final",
    );
    scenario.observe({
      kind: "recovery-final",
      inclusionHash: "block-a",
      authenticated: true,
    });
    scenario.release(scenario.plan());
    expect(scenario.snapshot().resourcesHeld).toBe(false);
    scenario.observe({ kind: "orphaned" });
    expect(scenario.snapshot().resourcesHeld).toBe(true);
    scenario.observe({ kind: "included", inclusionHash: "block-b" });
    scenario.observe({ kind: "confirmed", inclusionHash: "block-c" });
    expect(scenario.snapshot().observation.kind).toBe("contradictory");
    expect(() => scenario.release(scenario.plan())).toThrow(
      "authenticated recovery-final",
    );
  });

  it.each([
    "midgard",
    "production",
    'midgard_test"; DROP DATABASE midgard;--',
    "midgard_test_".repeat(10),
  ])("refuses destructive or injectable database identity %s", (prefix) => {
    expect(() => disposableDatabaseName(prefix, 1)).toThrow(
      "Disposable database prefix",
    );
  });
});
