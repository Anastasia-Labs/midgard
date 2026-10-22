/**
 * The witness field's carriage through the watcher's installed application,
 * with other parties' outputs holding the same datums at the addresses its
 * own carriage goes to.
 *
 * The field is wide enough to need publications and a field certificate.
 * The application reads each publication and its certificate by the
 * out-refs its own signed transactions gave them, never by what an output
 * at the address holds: unrelated outputs with the same datums, confirmed
 * before its own or landing beside them and sorting first, outputs with the
 * same datums that carry a token, another party's certificate of the same
 * field, and a token-bearing coin at the prover address leave its
 * transactions and their fees as they are.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import { plantTokenCoin } from "./support/own-outputs-installed.js";
import { expectBaselineCost } from "./support/script-decode-installed-run.js";
import {
  expectOwnFieldCarriage,
  placeBeside,
  runWith,
} from "./support/witness-field-carriage-installed-run.js";

describe("witness field carriage found by the out-refs it published, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("publishes and certifies the field with nothing else at its addresses: the cost every other row matches", async () => {
    const { run, watch, harness, cost } = await runWith(hooks, () => []);
    expect(run.convicted).toBe(true);
    const carriage = expectOwnFieldCarriage(watch, harness);
    expect(
      carriage.publications.flatMap(({ outputs }) => outputs).length,
    ).toBeGreaterThan(1);
    baseline = cost;
  }, 900_000);

  it.each([
    { kind: "publication", confirmed: true, when: "confirmed before them" },
    {
      kind: "publication",
      confirmed: false,
      when: "sorting first in their block",
    },
    { kind: "certificate", confirmed: true, when: "confirmed before it" },
    {
      kind: "certificate",
      confirmed: false,
      when: "sorting first in its block",
    },
  ] as const)(
    "reads its own $kind past another party's outputs for it $when",
    async ({ kind, confirmed }) => {
      const beside = placeBeside(kind, { confirmed });
      const { run, watch, harness, cost } = await runWith(hooks, () => [], {
        decide: beside.decide,
      });
      expect(beside.failures()).toEqual([]);
      expect(beside.copies()).toBeGreaterThan(0);
      if (confirmed && kind === "publication")
        expect(beside.withToken()).toBeGreaterThan(0);
      expect(run.convicted).toBe(true);
      expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
      expectOwnFieldCarriage(watch, harness);
      expectBaselineCost(cost, baseline);
    },
    900_000,
  );

  it("funds its carriage from plain coins, leaving a larger token-bearing coin at the prover address unspent", async () => {
    const { run, watch, harness, cost } = await runWith(
      hooks,
      (harness, watch) => [
        () =>
          plantTokenCoin(
            watch,
            harness.funderLucid,
            harness.proverSigner.address,
          ),
      ],
    );
    expect(run.convicted).toBe(true);
    expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
    expectOwnFieldCarriage(watch, harness);
    expectBaselineCost(cost, baseline);
  }, 900_000);
});
