/**
 * The watcher process's views of its proof-materials addresses: one per
 * address, shared by every workflow the process runs, so the needs one
 * workflow registers keep its materials out of another's sweep. Each view
 * reads the follower's bounded plain-output reads when the provider is the
 * follower, and the provider's address read otherwise.
 *
 * Nothing here is persisted. The first pass completes at the first
 * admission that reaches its final return once a pass at tip has lifted the
 * proof roster's prune floor in this process (`passAtTip` answers that,
 * from `WatcherProofRosterRetention.lifted`); once complete it stays so.
 * The roster is the objectives admitted up to that pass, less those since
 * settled, removed or forgotten. A view sweeps
 * nothing until the first pass completed and every objective on the roster
 * has observed once in this process.
 */
import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
  type ProofMaterialsObjective,
  type ProofMaterialsView,
} from "@al-ft/midgard-fault-proofs/workflow/proof-materials-view";
import {
  followerProofMaterialsReader,
  isProofMaterialsFollower,
} from "@al-ft/midgard-fault-proofs/workflow/proof-materials-view.follower-reader";
import type { Provider } from "@lucid-evolution/lucid";

export type WatcherProofMaterialsViews = Readonly<{
  /** The process's one view of `address`. */
  viewFor(address: string): ProofMaterialsView;
  /** An admission reached its final return, returning `objectives` to
   * drive. */
  admitted(objectives: readonly ProofMaterialsObjective[]): void;
  /** The objective is no longer driven: its needs go, and it leaves the
   * roster. */
  released(objective: ProofMaterialsObjective): void;
  /** Whether a pass at tip has lifted the proof roster's prune floor in
   * this process. Until it is given, the first pass never completes and
   * nothing is swept. */
  passAtTip(signal: () => boolean): void;
  /** Why each view sweeps nothing now, one line per address; empty while
   * every view is open. */
  sweepClosed(): readonly string[];
  /** Whether an open view holds materials a sweep may take now; `unknown`
   * while no view is open. */
  sweepable(): Promise<"some" | "none" | "unknown">;
}>;

const objectiveKey = ({ category, headerHash }: ProofMaterialsObjective) =>
  `${category}/${headerHash.toLowerCase()}`;

export const createWatcherProofMaterialsViews = (
  provider: Provider,
): WatcherProofMaterialsViews => {
  const views = new Map<string, ProofMaterialsView>();
  const roster = new Map<string, ProofMaterialsObjective>();
  let passAtTip: (() => boolean) | null = null;
  let completed = false;
  return Object.freeze({
    viewFor: (address) => {
      let view = views.get(address);
      if (view === undefined) {
        view = createProofMaterialsView({
          address,
          reader: isProofMaterialsFollower(provider)
            ? followerProofMaterialsReader(provider, address)
            : lucidProofMaterialsReader(
                { utxosAt: async (at) => await provider.getUtxos(at) },
                address,
              ),
          liveObjectives: () => (completed ? [...roster.values()] : undefined),
        });
        views.set(address, view);
      }
      return view;
    },
    admitted: (objectives) => {
      // Objectives admitted after the first pass are not waited on.
      if (completed) return;
      for (const objective of objectives)
        roster.set(objectiveKey(objective), objective);
      completed = passAtTip?.() === true;
    },
    released: (objective) => {
      roster.delete(objectiveKey(objective));
      for (const view of views.values()) view.releaseObjective(objective);
    },
    passAtTip: (signal) => {
      passAtTip = signal;
    },
    sweepClosed: () =>
      [...views.values()].flatMap((view) => {
        const readiness = view.readiness();
        return readiness.open ? [] : [`${view.address}: ${readiness.reason}`];
      }),
    sweepable: async () => {
      let open = false;
      for (const view of views.values()) {
        if (!view.readiness().open) continue;
        open = true;
        if ((await view.sweepable(1)).length > 0) return "some";
      }
      return open ? "none" : "unknown";
    },
  });
};
