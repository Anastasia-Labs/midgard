// Old-code comparison tooling (plan §14). This file reads the current
// committee scanner and is deleted with it at the C1 cutover.
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import type { DepthParameters, FactStore } from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import {
  reading,
  type ShadowComparator,
  type ShadowContext,
  type ShadowReading,
  unavailable,
} from "@al-ft/midgard-l1-follower/shadow";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ChainPoint, ObservedStateQueueSnapshot } from "../../domain.js";
import type { SlotTime } from "../follower/obligations.js";
import {
  type CommitteeView,
  readCommitteeView,
} from "../follower/projection.js";
import type { CommitteeQueueParameters } from "../follower/queue-derivation.js";
import { stateQueueUtxosToObservedSnapshot } from "../provider.parse-fixture-chain-sync-events.js";
import {
  scanStateQueue,
  type StateQueueScanConfig,
} from "../state-queue-scanner.js";

/**
 * The committee's queue as both sides are compared on it. Depth is plan §9
 * depth (the tip block has depth 1); the current code counts descendants
 * (`confirmations - 1`), so its depth is converted by adding one. Whether a
 * header may be signed is not compared: the current code signs at
 * `descendants >= cd`, plan §9 at `depth >= cd`, one block earlier, by
 * design (reported as an escalation, not a projection bug).
 */
export type CommitteeComparedView =
  | Readonly<{ healthy: false }>
  | Readonly<{
      healthy: true;
      confirmed: Readonly<{ headerHash: string; outRef: string }>;
      /** In list order, from the root. */
      nodes: readonly Readonly<{
        outRef: string;
        headerHash: string;
        status: string;
        validationErrors: readonly string[];
        datum: string;
      }>[];
      awaiting: readonly Readonly<{
        headerHash: string;
        outRef: string;
        depth: number;
      }>[];
    }>;

/** The new projections' view, in the compared shape. */
export const projectedComparedView = (
  view: CommitteeView,
): CommitteeComparedView => {
  const { queue } = view;
  if (!queue.healthy || queue.root === null) return { healthy: false };
  return {
    healthy: true,
    confirmed: {
      headerHash: queue.root.headerHash,
      outRef: queue.root.outRef,
    },
    nodes: queue.nodes.map((node) => ({
      outRef: node.outRef,
      headerHash: node.headerHash,
      status: node.status,
      validationErrors: [...node.problems],
      datum: node.datumHex,
    })),
    awaiting: view.awaiting.map((header) => ({
      headerHash: header.headerHash,
      outRef: header.outRef,
      depth: header.depth,
    })),
  };
};

/** The scan settings the current code needs, as far as the comparison reads. */
export type CurrentScanIdentity = Pick<
  StateQueueScanConfig,
  | "deploymentFingerprint"
  | "deploymentIdentityDigest"
  | "stateQueuePolicyId"
  | "daAttestationPolicyId"
  | "finalityDepth"
  | "automaticRecoveryMaxDepth"
>;

/**
 * The current code's view of one snapshot: `scanStateQueue` over a provider
 * that serves only that snapshot, with no stored records and no replay
 * anchor (the scan's view of the queue as it stands, not its memory).
 */
export const currentComparedView = async (
  snapshot: ObservedStateQueueSnapshot,
  identity: CurrentScanIdentity,
): Promise<CommitteeComparedView> => {
  let records: Awaited<ReturnType<typeof scanStateQueue>>;
  try {
    records = await scanStateQueue(
      {
        fetchStateQueueNodes: () => Promise.resolve(snapshot.nodes),
        fetchStateQueueSnapshot: () => Promise.resolve(snapshot),
      },
      { ...identity, consensusProfile: MIDGARD_CONSENSUS_PROFILE },
    );
  } catch {
    // The scan refusing the snapshot is the current code refusing the
    // queue, as a tick that cannot observe it would.
    return { healthy: false };
  }
  const byOutRef = new Map(
    records.map((record) => [record.stateQueueOutRef, record]),
  );
  const nodes = snapshot.nodes.map((node) => {
    const record = byOutRef.get(node.outRef);
    if (record === undefined)
      throw new Error(`the current scan returned no record for ${node.outRef}`);
    return { node, record };
  });
  return {
    healthy: true,
    confirmed: {
      headerHash: snapshot.confirmedHeaderHash.toLowerCase(),
      outRef: snapshot.confirmedStateOutRef,
    },
    nodes: nodes.map(({ node, record }) => ({
      outRef: node.outRef,
      headerHash: record.headerHash,
      status: record.status,
      validationErrors: [...record.validationErrors],
      datum: (node.rawDatumCbor ?? "").toLowerCase(),
    })),
    awaiting: nodes
      .filter(({ record }) => record.status === "unattested")
      .map(({ node, record }) => ({
        headerHash: record.headerHash,
        outRef: node.outRef,
        depth: (record.observedChainPoint.depth ?? -1) + 1,
      })),
  };
};

/**
 * The current code's pipeline over the follower's own live outputs at the
 * queue address: `utxosToStateQueueUTxOs` (which drops what it cannot read),
 * `sortStateQueueUTxOs` (which walks from the root and drops what the walk
 * misses), then the snapshot builder, with each output's depth counted the
 * current way (descendants of its block, at the store's cursor). A failure
 * anywhere is the current code refusing the queue: unhealthy.
 *
 * Fed with the follower's facts, this compares the projections' logic with
 * the current code's on the same outputs; the follower's facts against the
 * node's own ledger are the ledger comparator's business.
 */
export const factFedSnapshot = async (
  store: FactStore,
  context: ShadowContext,
  queue: CommitteeQueueParameters,
): Promise<ObservedStateQueueSnapshot | "unhealthy" | ShadowReading> => {
  const read = await store.liveUtxos(
    { by: "address", address: queue.stateQueueAddress },
    context.at.point,
  );
  if (read.kind !== "ok") return unavailable(`live outputs: ${read.kind}`);
  const heights = new Map<number, number>();
  const pointOf = new Map<string, ChainPoint>();
  const utxos: UTxO[] = [];
  for (const stored of read.utxos) {
    if (stored.created === null)
      return unavailable("a queue output came from the seed, not a block");
    let height = heights.get(stored.created.slot);
    if (height === undefined) {
      const block = await store.blockAtOrBeforeSlot(stored.created.slot);
      if (block === null || block.slot !== stored.created.slot)
        return unavailable(
          `no stored block at slot ${stored.created.slot.toString()}`,
        );
      height = block.height;
      heights.set(stored.created.slot, height);
    }
    const utxo = toLucidUtxo(stored.outRef, stored.output);
    utxos.push(utxo);
    pointOf.set(`${utxo.txHash}#${utxo.outputIndex.toString()}`, {
      slot: stored.created.slot,
      blockHeight: height,
      // The current code's depth: descendants of the output's block.
      depth: context.at.height - height,
    });
  }
  const resolve = (utxo: UTxO): Promise<ChainPoint> => {
    const point = pointOf.get(`${utxo.txHash}#${utxo.outputIndex.toString()}`);
    return point === undefined
      ? Promise.reject(new Error("unknown queue output"))
      : Promise.resolve(point);
  };
  try {
    const sorted = await Effect.runPromise(
      SDK.utxosToStateQueueUTxOs(utxos, queue.stateQueuePolicyId).pipe(
        Effect.flatMap(SDK.sortStateQueueUTxOs),
      ),
    );
    return await stateQueueUtxosToObservedSnapshot(
      sorted,
      "l1-follower-facts",
      resolve,
    );
  } catch {
    return "unhealthy";
  }
};

export type CommitteeComparatorInputs = Readonly<{
  name: string;
  parameters: DepthParameters;
  slotTime: SlotTime;
  identity: CurrentScanIdentity;
  /** The current code's snapshot at `context.at`, or why there is none. */
  snapshot: (
    context: ShadowContext,
  ) => Promise<ObservedStateQueueSnapshot | "unhealthy" | ShadowReading>;
}>;

const isReading = (value: unknown): value is ShadowReading =>
  typeof value === "object" &&
  value !== null &&
  "kind" in value &&
  (value.kind === "value" || value.kind === "unavailable");

/**
 * The per-block comparator of the committee projections (landed queue and
 * headers awaiting attestation) against the current scanner's view of the
 * same block. Obligations are not compared: the current code releases at a
 * cd-deep, not a k-deep, point by design (D-C1), so there is nothing equal
 * to compare them with.
 */
export const committeeComparator = (
  inputs: CommitteeComparatorInputs,
): ShadowComparator => ({
  role: "committee",
  name: inputs.name,
  projected: async ({ store, at }): Promise<ShadowReading> => {
    const view = await readCommitteeView(store, {
      parameters: inputs.parameters,
      slotTime: inputs.slotTime,
    });
    if (view === null) return unavailable("the store has no cursor");
    if (view.at.height !== at.height || view.at.generation !== at.generation)
      return unavailable("the store moved past the compared block");
    return reading(projectedComparedView(view));
  },
  current: async (context): Promise<ShadowReading> => {
    const snapshot = await inputs.snapshot(context);
    if (snapshot === "unhealthy") return reading({ healthy: false });
    if (isReading(snapshot)) return snapshot;
    return reading(await currentComparedView(snapshot, inputs.identity));
  },
});
