/**
 * The script-decode record through the watcher's installed application:
 * the committed block's undecodable Plutus item is detected by the
 * watcher's classifier, and the admitted runner, funded by the watcher's
 * prover-funding authority, records the decode and convicts, turn by turn,
 * from its durable journal, with no proof material prepared outside it.
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

import {
  INSTALLED_OUTPUT_REFERENCE as OUTPUT_REFERENCE,
  INSTALLED_WITNESS as WITNESS,
  runInstalled,
} from "./support/script-decode-installed-run.js";
import {
  MAX_RECORD_AUTH_TIMELOCK_MS,
  maximumDelayChainReferenceScript,
  maximumDelayChainWitnessScript,
  plutusItem,
  plutusReferenceOutput,
  referenceScriptSubject,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";
import { undecodablePlutusShape } from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

describe("script-decode record through the installed watcher application", () => {
  it("convicts an accepted undecodable Plutus witness from detection, recording it in the workflow", async () => {
    // The record takes 9 polls; a run that keeps acting never stops polling.
    const run = await runInstalled(
      { ...WITNESS, nativeTx: () => undecodablePlutusShape().nativeTx },
      hooks,
      { maxPolls: 100 },
    );
    expect(run.convicted).toBe(true);
    // About 5 s: a run that spins without polling fails on this timeout.
  }, 180_000);

  it("convicts the maximum undecodable Plutus witness (kind 0), every record action funded by the watcher", async () => {
    const run = await runInstalled(
      {
        ...WITNESS,
        referenceScriptAuthTimelockMs: MAX_RECORD_AUTH_TIMELOCK_MS,
        nativeTx: () =>
          witnessSubject([plutusItem(maximumDelayChainWitnessScript())])
            .nativeTx,
      },
      hooks,
    );
    expect(run.convicted).toBe(true);
  }, 3_600_000);

  it("convicts the maximum undecodable reference script (kind 1), every record action funded by the watcher", async () => {
    const run = await runInstalled(
      {
        ...OUTPUT_REFERENCE,
        referenceScriptAuthTimelockMs: MAX_RECORD_AUTH_TIMELOCK_MS,
        nativeTx: () =>
          referenceScriptSubject(
            plutusReferenceOutput(maximumDelayChainReferenceScript()),
          ).nativeTx,
      },
      hooks,
    );
    expect(run.convicted).toBe(true);
  }, 3_600_000);
});
