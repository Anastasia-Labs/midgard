/**
 * The process's proof-materials roster. The progress authority tells it each
 * objective an admission returns to drive and each objective that is no
 * longer driven; the first pass completes at the first admission once a
 * pass at tip lifted the proof roster's prune floor in this process; and an
 * objective whose header leaves the queue releases its needs whether or not
 * its workflow ended, so its materials become sweepable.
 * Nothing is read from a stored record: every view starts empty.
 */
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { Provider, UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { createWatcherProofMaterialsViews } from "../../src/fault-proofs/proof-materials-views.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

afterEach(cleanupClassifiedFaultFixtures);

const ADDRESS = "addr_test1_proof_materials";
const DATUM = "d87980";

const material = (byte: string, datum = DATUM): UTxO => ({
  txHash: byte.repeat(32),
  outputIndex: 0,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n },
  datum,
});

const label = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** The process's views over a chain that holds `chain` at the address. */
const viewsOver = (chain: readonly UTxO[]) =>
  createWatcherProofMaterialsViews({
    getUtxos: async (address: string) =>
      chain.filter((utxo) => utxo.address === address),
  } as unknown as Provider);

describe("proof-materials first pass and roster", () => {
  it("waits for the first admission once a pass at tip lifted the roster floor, then for each objective admitted by then to observe", async () => {
    const views = viewsOver([material("a1")]);
    const view = views.viewFor(ADDRESS);
    let lifted = false;
    views.passAtTip(() => lifted);
    const a = { category: "doubleSpend", headerHash: "aa".repeat(28) };
    const b = { category: "doubleSpend", headerHash: "bb".repeat(28) };
    const pending =
      "the first admission pass at the follower tip has not completed in this process";
    expect(view.readiness()).toEqual({ open: false, reason: pending });
    // Catching up: an admission before any pass at tip lifted the floor.
    views.admitted([a]);
    expect(view.readiness()).toEqual({ open: false, reason: pending });
    expect(views.sweepClosed()).toEqual([`${ADDRESS}: ${pending}`]);
    // Once a pass at tip lifted it, the next admission completes the pass;
    // A was admitted by then.
    lifted = true;
    views.admitted([a]);
    expect(view.readiness()).toEqual({
      open: false,
      reason: `doubleSpend/${a.headerHash} has not observed since this process started`,
    });
    expect(await view.sweepable(100)).toEqual([]);
    // B, admitted after the pass, is not waited on.
    views.admitted([b]);
    view.observed(a);
    expect(view.readiness()).toEqual({ open: true });
    expect(views.sweepClosed()).toEqual([]);
    expect((await view.sweepable(100)).map(label)).toEqual([
      label(material("a1")),
    ]);
    // Once complete it never closes again in the process.
    lifted = false;
    expect(view.readiness()).toEqual({ open: true });
  });

  it("never completes the first pass without a tip signal", () => {
    const views = viewsOver([]);
    const view = views.viewFor(ADDRESS);
    views.admitted([]);
    expect(view.readiness().open).toBe(false);
  });

  it("drops an objective that leaves the roster, so the sweep no longer waits on it", () => {
    const views = viewsOver([]);
    const view = views.viewFor(ADDRESS);
    views.passAtTip(() => true);
    const a = { category: "doubleSpend", headerHash: "aa".repeat(28) };
    views.admitted([a]);
    expect(view.readiness().open).toBe(false);
    views.released(a);
    expect(view.readiness()).toEqual({ open: true });
  });
});

describe("proof-materials needs of an objective whose header leaves the queue", () => {
  it("releases the needs of an objective whose workflow has not ended, and its materials become sweepable", async () => {
    const fixture = await setupClassifiedFaultFixture();
    const hubOraclePolicyId = deploymentIdentity.hubOraclePolicyId;
    const chain = [material("c1"), material("c2", "d87a80")];
    const views = viewsOver(chain);
    const view = views.viewFor(ADDRESS);
    views.passAtTip(() => true);
    const authority = createWatcherFaultProofProgressAuthority({
      journalRoot: fixture.journalRoot,
      hubOraclePolicyId,
      categories: installedLaunchScope(fixture.old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
      retention: storelessProofRetention,
      proofMaterials: views,
    });
    const objective = {
      category: "doubleSpend",
      headerHash: fixture.old.headerHash,
    } as const;
    /** An observation at `revision`; with the header queued it carries the
     * live fault. */
    const admit = (revision: number, headerQueued: boolean) => {
      const observation = progressObservation({
        hubOraclePolicyId,
        revision,
        ...(headerQueued ? { header: fixture.fixture } : {}),
      });
      return authority.admit({
        observation,
        rollbackGeneration: "2",
        ...(headerQueued
          ? {
              fault: {
                decision: fixture.old,
                deadline: watcherFaultProofDeadline(
                  observation.finalizedHeaders[0]!,
                ),
                actuationPermit: createWorkflowActuationPermitController({
                  decision: fixture.old,
                  rollbackGeneration: "2",
                }).permit,
              },
            }
          : {}),
      });
    };
    // The objective's workflow found one copy through the view.
    const [found] = await view.find([DATUM], {
      workflowId: "workflow-of-the-objective",
      objective,
    });
    expect(found).toEqual(chain[0]);
    await admit(1, true);
    view.observed(objective);
    expect((await view.sweepable(100)).map(label)).toEqual([label(chain[1]!)]);
    // Its header leaves the queue; the workflow has not ended.
    await admit(2, false);
    expect(view.readiness()).toEqual({ open: true });
    expect((await view.sweepable(100)).map(label)).toEqual(chain.map(label));
  });
});
