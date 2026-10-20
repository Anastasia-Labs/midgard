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
/** Confirmation polls one process may take before it counts as spinning. */
const POLL_CAP = 200;
/** Turns the run may take: a recovered run convicts well inside this. */
const TURN_CAP = 12;

/**
 * Stops the watcher process once its `count`-th transaction of `kind` is on
 * chain, lets one block pass, and starts another whose workflow journal is
 * gone (and its reservation store with it, with `reservations`). Every
 * later turn must make progress: a turn that throws again fails the run.
 */
const runWithLostJournal = async ({
  kind,
  count = 1,
  reservations = false,
}: {
  readonly kind: "stage" | "step";
  readonly count?: number;
  readonly reservations?: boolean;
}) => {
  let stops = 0;
  let stoppedAfter: readonly string[] = [];
  const run = await runInstalled(SCENARIO, hooks, {
    maxPolls: POLL_CAP,
    maxTurns: TURN_CAP,
    beforeTurns: ({ harness, clock }) => {
      const watch = watchLedger(harness.emulator);
      const roles = scriptDecodeRoles({
        emulator: harness.emulator,
        proverAddress: harness.proverSigner.address,
        item: ITEM,
      });
      clock.state.onPoll = async () => {
        if (stoppedAfter.length !== 0) return;
        const kinds = watch
          .ours()
          .map((transaction) => kindOf(transaction, roles.staging, roles.run));
        if (kinds.filter((value) => value === kind).length < count) return;
        stoppedAfter = kinds;
        clock.state.crashAfterPolls = clock.state.polls;
      };
      return () => {
        clock.state.onPoll = undefined;
        watch.restore();
      };
    },
    onStopped: async (error, { application, clock, harness }) => {
      expect(String(error)).toContain("installed application process stopped");
      stops += 1;
      expect(stops, "only the stop the row engineered").toBe(1);
      harness.emulator.awaitBlock();
      await application.loseJournal({ reservations });
      clock.state.crashAfterPolls = clock.state.polls + POLL_CAP;
    },
  });
  expect(stoppedAfter.filter((value) => value === kind)).toHaveLength(count);
  expect(stops).toBe(1);
  return run;
};

describe("script-decode workflow journal loss in the installed watcher application", () => {
  it("convicts after the workflow journal is lost once a field publication is on chain", async () => {
    const run = await runWithLostJournal({ kind: "stage" });
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("convicts after the workflow journal is lost once the thread has stepped on chain", async () => {
    const run = await runWithLostJournal({ kind: "step" });
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("convicts after the workflow journal and reservation store are both lost once a field publication is on chain", async () => {
    const run = await runWithLostJournal({
      kind: "stage",
      reservations: true,
    });
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("convicts after the workflow journal and reservation store are both lost once the thread has stepped on chain", async () => {
    const run = await runWithLostJournal({
      kind: "step",
      reservations: true,
    });
    expect(run.convicted).toBe(true);
  }, 1_800_000);
});
