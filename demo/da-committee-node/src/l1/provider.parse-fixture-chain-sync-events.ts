import { readFile } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  ChainPoint,
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../domain.js";
import {
  type ChainPointResolver,
  resolveChainPoints,
} from "./provider.chain-point-batch.js";
import { createOgmiosChainSyncRequest } from "./provider.create-ogmios-chain-sync-request.js";
import { type OgmiosChainSyncRequest } from "./provider.local-node-chain-authority.js";
import {
  type CanonicalChainPoint,
  type ChainSyncCursor,
  type ChainSyncEvent,
  type ChainSyncEventBatch,
  type ChainSyncEventSource,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

export class OgmiosChainSyncEventSource implements ChainSyncEventSource {
  private readonly request: OgmiosChainSyncRequest;

  constructor(
    private readonly ogmiosUrl: string,
    private readonly network: string,
    private readonly authorityNodeId: string,
    request?: OgmiosChainSyncRequest,
    /** Proves the live chain's identity on a network with no built-in magic. */
    private readonly networkMagic?: number,
  ) {
    this.request = request ?? createOgmiosChainSyncRequest();
  }

  async next(
    cursor: ChainSyncCursor | undefined,
    intersectionCandidates?: readonly CanonicalChainPoint[],
  ): Promise<ChainSyncEventBatch> {
    const response = await this.request(
      this.ogmiosUrl,
      cursor?.point,
      intersectionCandidates,
      this.network,
      this.authorityNodeId,
      this.networkMagic,
    );
    return response;
  }
}

export class FixtureChainSyncEventSource implements ChainSyncEventSource {
  constructor(
    private readonly path: string,
    private readonly network: string,
    private readonly authorityNodeId: string,
  ) {}

  async next(
    cursor: ChainSyncCursor | undefined,
  ): Promise<ChainSyncEventBatch> {
    const parsed = JSON.parse(await readFile(this.path, "utf8")) as unknown;
    const events = parseFixtureChainSyncEvents(
      parsed,
      this.network,
      this.authorityNodeId,
    );
    const event = events[(cursor?.sequence ?? -1) + 1];
    if (event === undefined) {
      if (cursor === undefined) {
        throw new Error("chain-sync fixture contains no events");
      }
      return { tip: cursor.point };
    }
    return { event, tip: events.at(-1)!.point };
  }
}

export class FixtureStateQueueProvider implements StateQueueProvider {
  private readonly path: string;
  private readonly network: string;

  constructor(path: string, network: string) {
    this.path = path;
    this.network = network;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const raw = await readFile(this.path, "utf8");
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) {
      throw new Error("fixture provider file must contain an array");
    }
    return parsed as readonly ObservedStateQueueNode[];
  }

  async currentChainPoint(): Promise<CanonicalChainPoint> {
    const nodes = await this.fetchStateQueueNodes();
    const point = nodes[0]?.chainPoint;
    if (
      point?.slot === undefined ||
      point.blockHash === undefined ||
      point.providerSource === undefined
    ) {
      throw new Error(
        "fixture query provider requires node-derived slot, blockHash, and providerSource provenance",
      );
    }
    return {
      ...point,
      network: this.network,
      slot: point.slot,
      blockHash: point.blockHash,
      providerSource: point.providerSource,
      observedAt: point.observedAt ?? new Date().toISOString(),
    };
  }
}

const observedNodes = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPoints: readonly ChainPoint[] | undefined,
): Promise<readonly ObservedStateQueueNode[]> => {
  const observed: ObservedStateQueueNode[] = [];
  for (const [index, stateQueueUtxo] of stateQueueUtxos.entries()) {
    // Callers pass the non-root UTxOs; the guard narrows the datum key.
    if (stateQueueUtxo.datum.key === "Empty") continue;
    const stateQueueNode = await Effect.runPromise(
      SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
    );
    const resolved = chainPoints?.[index];
    const chainPoint = {
      providerSource,
      observedAt: new Date().toISOString(),
      ...(resolved === undefined ? {} : declaredChainPoint(resolved)),
    } satisfies ChainPoint;
    observed.push({
      outRef: outRefLabel(stateQueueUtxo.utxo),
      assetName: stateQueueUtxo.assetName,
      linkedListKey: stateQueueUtxo.datum.key.Key.key,
      rawDatumCbor: SDK.encodeLinkedListNodeView(stateQueueUtxo.datum),
      header: stateQueueNode.header,
      daAttestation: stateQueueNode.da_attestation,
      chainPoint,
    });
  }
  return observed;
};

const nonRootUtxos = (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
): readonly SDK.StateQueueUTxO[] =>
  stateQueueUtxos.filter(
    (stateQueueUtxo) => stateQueueUtxo.datum.key !== "Empty",
  );

export const stateQueueUtxosToObservedNodes = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPointResolver?: ChainPointResolver,
): Promise<readonly ObservedStateQueueNode[]> => {
  const nodes = nonRootUtxos(stateQueueUtxos);
  return observedNodes(
    nodes,
    providerSource,
    chainPointResolver === undefined
      ? undefined
      : await resolveChainPoints(
          chainPointResolver,
          nodes.map(({ utxo }) => utxo),
        ),
  );
};

/**
 * One snapshot's nodes and confirmed root. Their chain points are resolved
 * together, through the resolver's `resolveAll` when it has one, so the
 * whole snapshot is judged against one pinned chain tip.
 */
export const stateQueueUtxosToObservedSnapshot = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPointResolver?: ChainPointResolver,
): Promise<ObservedStateQueueSnapshot> => {
  const confirmed = stateQueueUtxos[0];
  if (confirmed === undefined || confirmed.datum.key !== "Empty") {
    throw new Error("state queue snapshot has no confirmed root node");
  }
  const nodeUtxos = nonRootUtxos(stateQueueUtxos);
  const [{ data }, chainPoints] = await Promise.all([
    Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(confirmed.datum),
    ),
    chainPointResolver === undefined
      ? undefined
      : resolveChainPoints(chainPointResolver, [
          confirmed.utxo,
          ...nodeUtxos.map(({ utxo }) => utxo),
        ]),
  ]);
  const [confirmedPoint, ...nodePoints] = chainPoints ?? [];
  const nodes = await observedNodes(
    nodeUtxos,
    providerSource,
    chainPoints === undefined ? undefined : nodePoints,
  );
  const observedChainPoint = {
    providerSource,
    observedAt: new Date().toISOString(),
    ...(confirmedPoint === undefined ? {} : declaredChainPoint(confirmedPoint)),
  } satisfies ChainPoint;
  return {
    nodes,
    confirmedHeaderHash: data.headerHash,
    confirmedStateOutRef: outRefLabel(confirmed.utxo),
    observedChainPoint,
  };
};

/**
 * Copies exactly the fields `ChainPoint` declares, leaving out undefined ones.
 * A point typed as a wider type, such as a `CanonicalChainPoint`, still
 * satisfies `ChainPoint`, so spreading it would carry fields the stored
 * records' exact-keys parser rejects.
 */
export const declaredChainPoint = (point: ChainPoint): ChainPoint => {
  const declared: ChainPoint = {
    slot: point.slot,
    blockHash: point.blockHash,
    blockHeight: point.blockHeight,
    observedAt: point.observedAt,
    depth: point.depth,
    finalized: point.finalized,
    providerSource: point.providerSource,
  };
  return Object.fromEntries(
    Object.entries(declared).filter(([, value]) => value !== undefined),
  );
};

const parseFixtureChainSyncEvents = (
  value: unknown,
  network: string,
  authorityNodeId: string,
): readonly ChainSyncEvent[] => {
  if (!Array.isArray(value)) {
    throw new Error("chain-sync fixture must contain an event array");
  }
  return value.map((entry, index) => {
    const event = getRecord(
      entry,
      `chain-sync fixture event ${index.toString()}`,
    );
    if (
      event.direction !== "roll_forward" &&
      event.direction !== "roll_backward"
    ) {
      throw new Error(
        `chain-sync fixture event ${index.toString()} has an invalid direction`,
      );
    }
    return {
      direction: event.direction,
      point: {
        network,
        slot: safeSlot(
          event.slot,
          `chain-sync fixture event ${index.toString()} slot`,
        ),
        blockHash: safeBlockHash(
          event.blockHash,
          `chain-sync fixture event ${index.toString()} block hash`,
        ),
        providerSource: `chain-sync:${authorityNodeId}`,
        observedAt:
          typeof event.observedAt === "string"
            ? event.observedAt
            : new Date().toISOString(),
      },
    };
  });
};

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
