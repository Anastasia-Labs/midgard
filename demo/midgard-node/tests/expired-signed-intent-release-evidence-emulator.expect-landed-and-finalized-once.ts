import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { runLocalFinalizationRecoveryWorker } from "./deposit-flow-emulator-shared.js";
import {
  type openCorrectionRewindScenario,
  readJournal,
} from "./helpers/correction-rewind-scenario.js";
import {
  type Handle,
  nativeRoot,
  readPlans,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

/** The evidence checks the signed-intent release suites share. */

export const C = Pending.Columns;

export type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;

/** The served queue reduced to its root, whose confirmed state is `header`
 * (the merged block) over `previous` (by default the confirmed header before
 * it, as one merge leaves it): every node output is dropped. */
export const mergedIntoRootView =
  (h: Pick<Handle, "fixture">, header: string, previous?: string) =>
  (outputs: readonly LedgerSnapshotOutput[]) => {
    const { policyId } = h.fixture.contracts.stateQueue;
    const rootUnit = policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
    const roots = outputs.filter((output) => output.assets[rootUnit] === 1n);
    expect(roots).toHaveLength(1);
    const root = roots[0]!;
    const datum = Data.from(root.datum!, SDK.LinkedListDatum);
    if (!("Root" in datum.data)) throw new Error("The root has a node datum");
    const confirmed = Data.castFrom(datum.data.Root.data, SDK.ConfirmedState);
    const merged: LedgerSnapshotOutput = {
      ...root,
      datum: Data.to(
        {
          data: {
            Root: {
              data: SDK.castConfirmedStateToData({
                ...confirmed,
                prevHeaderHash: previous ?? confirmed.headerHash,
                headerHash: header,
              }) as never,
            },
          },
          link: null,
        },
        SDK.LinkedListDatum,
      ),
    };
    return [
      ...outputs.filter(
        (output) =>
          !Object.keys(output.assets).some((unit) => unit.startsWith(policyId)),
      ),
      merged,
    ];
  };

/** Locally finalize the block the history owner made available. No
 * confirmation pass runs first: the emulator's own ledger is not the chain
 * the served view shows (only the source view carries the merge), and local
 * finalization reads only the authenticated node the owner recorded. */
export const finalizeRecordedBlock = async (h: Handle, headerHash: string) => {
  const { fixture, lucidService, globals, production } = h;
  const finalized = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixture.runtimeOverrides!.deploymentIdentity,
    production.nodeConfig,
    { ...production, globals },
  );
  expect(finalized.type, inspect(finalized, { depth: 20 })).toBe(
    "SuccessfulLocalFinalizationRecoveryOutput",
  );
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The block must be locally finalized");
  expect(finalized.finalizedHeaderHash).toBe(headerHash);
  await synchronizeWithin(h);
  return finalized;
};

/** The block local finalization replays next, named by its node's asset. */
export const availableBlockAssetName = (h: Handle) => {
  const available = Effect.runSync(
    Ref.get(h.globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
  );
  return available === "" ? "" : available.assetName;
};

export const expectLandedAndFinalizedOnce = async (
  h: Handle,
  journal: Pending.Record,
  finalize: (
    h: Handle,
    headerHash: string,
  ) => Promise<unknown> = finalizeRecordedBlock,
) => {
  const header = journal[C.HEADER_HASH].toString("hex");
  const observed = await readJournal(header);
  expect(observed[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
  expect(observed[C.CORRECTION_TRANSITION_DIGEST]).toBeNull();
  expect(await readPlans()).toEqual([]);
  expect(Effect.runSync(Ref.get(h.globals.LOCAL_FINALIZATION_PENDING))).toBe(
    true,
  );
  await finalize(h, header);
  expect((await readJournal(header))[C.STATUS]).not.toBe(
    Pending.Status.Abandoned,
  );
  expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
  expect(await readPlans()).toEqual([]);
};
