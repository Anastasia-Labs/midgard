/**
 * The script-decode record through the watcher's installed application:
 * the committed block's undecodable Plutus item is detected by the
 * watcher's classifier, and the admitted runner, funded by the watcher's
 * prover-funding authority, records the decode and convicts, turn by turn,
 * from its durable journal, with no proof material prepared outside it.
 */
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION } from "../src/witness-script-decoding/workflow.js";
import { WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
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
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { installedWorkflowEmulatorClock } from "./support/installed-workflow.advance-emulator-observation.js";
import { installApplicationClock } from "./support/script-decode-installed-host.js";
import {
  commitScriptDecodeBlock,
  type InstalledFamily,
  installScriptDecodeApplication,
} from "./support/script-decode-installed-workflow.js";
import { undecodablePlutusShape } from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

const MAX_TURNS = 400;

type Scenario = Readonly<{
  definition: InstalledFamily;
  replayer: Parameters<typeof installScriptDecodeApplication>[0]["replayer"];
  contractOptions: NonNullable<
    Parameters<typeof makeFaultProofEmulatorHarness>[0]
  >["contractOptions"];
  referenceScriptAuthTimelockMs?: number;
  nativeTx: () => Parameters<typeof commitScriptDecodeBlock>[0]["nativeTx"];
  violationId: string;
}>;

const runInstalled = async (scenario: Scenario) => {
  const recorder = recordCrossBlockRawEmulator();
  let clock: ReturnType<typeof installApplicationClock> | undefined;
  let close: (() => Promise<void>) | undefined;
  try {
    hooks.authority = recorder.authority;
    const harness = await makeFaultProofEmulatorHarness({
      ...(scenario.referenceScriptAuthTimelockMs === undefined
        ? {}
        : {
            referenceScriptAuthTimelockMs:
              scenario.referenceScriptAuthTimelockMs,
          }),
      contractOptions: scenario.contractOptions,
    });
    const committed = await commitScriptDecodeBlock({
      harness,
      definition: scenario.definition,
      nativeTx: scenario.nativeTx(),
    });
    const workflowClock = installedWorkflowEmulatorClock(harness.emulator);
    workflowClock.awaitReleaseDepth();
    clock = installApplicationClock(harness.emulator);
    const application = await installScriptDecodeApplication({
      harness,
      definition: scenario.definition,
      replayer: scenario.replayer,
      committed,
      recorder,
      setBinding: (binding) => {
        hooks.binding = binding;
      },
    });
    close = application.close;
    expect(application.decision).toMatchObject({
      decision: "fault_detected",
      category: scenario.definition.category,
      violationId: scenario.violationId,
    });
    const categoryId =
      harness.catalogue.categories[scenario.definition.category]!.categoryId;
    const convictions = async () =>
      await harness.proverLucid.utxosAtWithUnit(
        harness.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          harness.contracts.fraudProof.policyId,
          categoryId + committed.headerHash,
        ),
      );
    // Conviction is the proof token. Removal after it is not driven here:
    // under the funding reservation a bond-backed slash needs the finalized
    // manifest's slash authority, which this legacy-deployment harness lacks.
    let turns = 0;
    for (;;) {
      const result = await application.turn();
      turns += 1;
      if ((await convictions()).length > 0) break;
      // Only a run that resumes by itself on a fresh observation continues.
      expect(
        result.kind === "pending" ||
          (result.kind === "stalled" && result.resumeOnObservation === true),
        JSON.stringify(result.reason ?? result.kind),
      ).toBe(true);
      expect(turns, "conviction within the turn budget").toBeLessThan(
        MAX_TURNS,
      );
      await workflowClock.advance(result);
    }
    expect(await convictions()).toHaveLength(1);
    return { turns, polls: clock.state.polls };
  } finally {
    clock?.restore();
    recorder.restore();
    await close?.();
  }
};

describe("script-decode record through the installed watcher application", () => {
  it("convicts an accepted undecodable Plutus witness from detection, recording it in the workflow", async () => {
    await runInstalled({
      definition: WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION as never,
      replayer: WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      contractOptions: {
        realWitnessScriptDecoding: true,
        alwaysFraudProofCatalogue: true,
      },
      nativeTx: () => undecodablePlutusShape().nativeTx,
      violationId: "witness-plutus-script-undecodable",
    });
  }, 1_800_000);
});
