import { describe, expect, it, vi } from "vitest";

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

import {
  kindOf,
  scriptDecodeRoles,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  INSTALLED_WITNESS,
  runInstalled,
} from "./support/script-decode-installed-run.js";
import {
  delayChainScript,
  plutusItem,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";

/** An undecodable witness whose record stages, binds, walks and finishes. */
const ITEM = plutusItem(delayChainScript(600));
const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => witnessSubject([ITEM]).nativeTx,
};
/** Turns the run may take: a recovered run convicts well inside this. */
const TURN_CAP = 12;

/**
 * Stops the watcher process once its first transaction of `kind` is on
 * chain, lets one block pass, and starts another: the new process holds
 * nothing of the stopped one, the watcher detects the fault again, and only
 * the chain keeps the objective's progress. Every later turn must make
 * progress: a turn that throws again fails the run, and no action is built
 * twice.
 */
const runWithLostDataDirectory = async (kind: "stage" | "step") => {
  let stops = 0;
  let stoppedAfter: readonly string[] = [];
  const run = await runInstalled(SCENARIO, hooks, {
    maxTurns: TURN_CAP,
    beforeTurns: ({ harness, clock }) => {
      const watch = watchLedger(harness.emulator);
      const roles = scriptDecodeRoles({
        emulator: harness.emulator,
        materialsAddress: harness.proverSigner.proofMaterialsAddress,
        item: ITEM,
      });
      clock.state.onObservation = async () => {
        if (stoppedAfter.length !== 0) return;
        const kinds = watch
          .ours()
          .map((transaction) => kindOf(transaction, roles.staging, roles.run));
        if (!kinds.includes(kind)) return;
        stoppedAfter = kinds;
        clock.state.stopAtNextObservation = true;
      };
      return () => {
        clock.state.onObservation = undefined;
        watch.restore();
      };
    },
    onStopped: async (error, { application, harness }) => {
      expect(String(error)).toContain("installed application process stopped");
      stops += 1;
      expect(stops, "only the stop the row engineered").toBe(1);
      harness.emulator.awaitBlock();
      application.restart();
    },
  });
  expect(stoppedAfter.filter((value) => value === kind)).toHaveLength(1);
  expect(stops).toBe(1);
  // The restarted process built nothing the chain already held.
  expect(new Set(run.sent.map(({ actionId }) => actionId)).size).toBe(
    run.sent.length,
  );
  return run;
};

describe("script-decode restart from the chain alone in the installed watcher application", () => {
  it("convicts after a restart once a field publication is on chain", async () => {
    const run = await runWithLostDataDirectory("stage");
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("convicts after a restart once the thread has stepped on chain, spending its own thread output", async () => {
    const run = await runWithLostDataDirectory("step");
    expect(run.convicted).toBe(true);
  }, 1_800_000);
});
