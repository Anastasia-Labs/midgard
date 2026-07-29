import { readFile } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Blockfrost,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { WatcherConfig } from "../config.js";
import type { ChainPoint, ObservedStateQueueNode } from "../domain.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

type CardanoNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";
export type OgmiosPoint = {
  readonly slot: number;
  readonly id: string;
  readonly height: number;
};
export type StateQueueObservation = {
  readonly nodes: readonly ObservedStateQueueNode[];
  readonly tip: OgmiosPoint;
};
export type TipAwareStateQueueProvider = StateQueueProvider & {
  fetchStateQueueObservation(): Promise<StateQueueObservation>;
};
export type LocalChainSyncSnapshot = {
  readonly tip: OgmiosPoint;
  readonly rollbackSequence: number;
};
export interface LocalChainSyncAuthority {
  synchronize(): Promise<LocalChainSyncSnapshot>;
}

export class FixtureStateQueueProvider implements StateQueueProvider {
  private readonly path: string;

  constructor(path: string) {
    this.path = path;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const raw = await readFile(this.path, "utf8");
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) {
      throw new Error("fixture provider file must contain an array");
    }
    return parsed as readonly ObservedStateQueueNode[];
  }
}

export class LucidStateQueueProvider implements StateQueueProvider {
  private readonly lucid: LucidEvolution;
  private readonly stateQueueAddress: string;
  private readonly stateQueuePolicyId: string;
  private readonly providerSource: string;
  private readonly chainPointResolver?: (
    utxo: UTxO,
    tip?: OgmiosPoint,
  ) => Promise<ChainPoint>;
  private readonly tipResolver?: () => Promise<OgmiosPoint>;

  constructor({
    lucid,
    stateQueueAddress,
    stateQueuePolicyId,
    providerSource,
    chainPointResolver,
    tipResolver,
  }: {
    readonly lucid: LucidEvolution;
    readonly stateQueueAddress: string;
    readonly stateQueuePolicyId: string;
    readonly providerSource: string;
    readonly chainPointResolver?: (
      utxo: UTxO,
      tip?: OgmiosPoint,
    ) => Promise<ChainPoint>;
    readonly tipResolver?: () => Promise<OgmiosPoint>;
  }) {
    this.lucid = lucid;
    this.stateQueueAddress = stateQueueAddress;
    this.stateQueuePolicyId = stateQueuePolicyId;
    this.providerSource = providerSource;
    this.chainPointResolver = chainPointResolver;
    this.tipResolver = tipResolver;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    if (this.tipResolver !== undefined) {
      return (await this.fetchStateQueueObservation()).nodes;
    }
    const stateQueueUtxos = await SDK.fetchSortedStateQueueUTxOs(this.lucid, {
      stateQueueAddress: this.stateQueueAddress,
      stateQueuePolicyId: this.stateQueuePolicyId,
    });
    return stateQueueUtxosToObservedNodes(
      stateQueueUtxos,
      this.providerSource,
      this.chainPointResolver,
    );
  }

  async fetchStateQueueObservation(): Promise<StateQueueObservation> {
    if (this.tipResolver === undefined) {
      throw new Error("state-queue provider does not expose an indexed tip");
    }
    const tip = await this.tipResolver();
    const stateQueueUtxos = await SDK.fetchSortedStateQueueUTxOs(this.lucid, {
      stateQueueAddress: this.stateQueueAddress,
      stateQueuePolicyId: this.stateQueuePolicyId,
    });
    return {
      nodes: await stateQueueUtxosToObservedNodes(
        stateQueueUtxos,
        this.providerSource,
        this.chainPointResolver === undefined
          ? undefined
          : (utxo) => this.chainPointResolver!(utxo, tip),
      ),
      tip,
    };
  }
}

export class MultiStateQueueProvider implements StateQueueProvider {
  private readonly providers: readonly StateQueueProvider[];
  private readonly identities: readonly string[];
  private readonly mergedIdentities?: readonly string[];
  private readonly sourceMode: "local_node" | "external_providers";

  constructor(
    providers: readonly StateQueueProvider[],
    options: {
      readonly sourceMode?: "local_node" | "external_providers";
      readonly identities?: readonly string[];
    } = {},
  ) {
    if (providers.length === 0) {
      throw new Error("at least one state-queue provider is required");
    }
    const sourceMode = options.sourceMode ?? "external_providers";
    if (sourceMode === "external_providers" && providers.length < 2) {
      throw new Error(
        "external_providers mode requires at least two state-queue providers",
      );
    }
    const identities =
      options.identities ??
      providers.map((_, index) => `provider-${index.toString()}`);
    if (
      identities.length !== providers.length ||
      new Set(identities).size !== identities.length
    ) {
      throw new Error(
        "state-queue provider identities must be complete and distinct",
      );
    }
    this.providers = providers;
    this.identities = identities;
    this.mergedIdentities = options.identities;
    this.sourceMode = sourceMode;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const results = await Promise.all(
      this.providers.map((provider) => provider.fetchStateQueueNodes()),
    );
    const sortedResults = results.map(sortObservedNodes);
    const baseline = canonicalObservedNodes(sortedResults[0]!);
    for (const [index, nodes] of sortedResults.entries()) {
      const candidate = canonicalObservedNodes(nodes);
      if (!canonicalArraysEqual(candidate, baseline)) {
        throw new Error(
          `state queue provider disagreement in ${this.sourceMode} mode between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    return mergeAgreedObservedNodes(sortedResults, this.mergedIdentities);
  }
}

export class LocalNodeStateQueueProvider implements StateQueueProvider {
  private readonly authority: LocalChainSyncAuthority;
  private readonly authorityIdentity: string;
  private readonly queryProviders: readonly TipAwareStateQueueProvider[];
  private readonly identities: readonly string[];

  constructor({
    authority,
    authorityIdentity,
    queryProviders,
    identities,
  }: {
    readonly authority: LocalChainSyncAuthority;
    readonly authorityIdentity: string;
    readonly queryProviders: readonly TipAwareStateQueueProvider[];
    readonly identities: readonly string[];
  }) {
    if (queryProviders.length === 0) {
      throw new Error("local_node mode requires a query surface");
    }
    if (
      identities.length !== queryProviders.length ||
      new Set(identities).size !== identities.length
    ) {
      throw new Error(
        "local_node query identities must be complete and distinct",
      );
    }
    this.authority = authority;
    this.authorityIdentity = authorityIdentity;
    this.queryProviders = queryProviders;
    this.identities = identities;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
    const before = await this.authority.synchronize();
    const observations = await Promise.all(
      this.queryProviders.map((provider) =>
        provider.fetchStateQueueObservation(),
      ),
    );
    const after = await this.authority.synchronize();
    if (after.rollbackSequence !== before.rollbackSequence) {
      throw new Error(
        "local_node chain-sync rollback occurred while query snapshots were being read",
      );
    }
    for (const [index, observation] of observations.entries()) {
      if (
        !sameOgmiosPoint(observation.tip, before.tip) &&
        !sameOgmiosPoint(observation.tip, after.tip)
      ) {
        throw new Error(
          `local_node query surface ${this.identities[index]!} is stale or not aligned with the chain-sync authority`,
        );
      }
    }
    const sortedResults = observations.map(({ nodes }) =>
      sortObservedNodes(nodes),
    );
    const baseline = canonicalObservedNodes(sortedResults[0]!);
    for (const [index, nodes] of sortedResults.entries()) {
      if (!canonicalArraysEqual(canonicalObservedNodes(nodes), baseline)) {
        throw new Error(
          `state queue provider disagreement in local_node mode between ${this.identities[0]!} and ${this.identities[index]!}`,
        );
      }
    }
    return mergeAgreedObservedNodes(sortedResults, this.identities).map(
      (node) => ({
        ...node,
        chainPoint: {
          ...node.chainPoint,
          providerSource: [
            `chain-sync:${this.authorityIdentity}`,
            node.chainPoint.providerSource,
          ]
            .filter((source): source is string => source !== undefined)
            .join(","),
        },
      }),
    );
  }
}

export class OgmiosChainSyncAuthority implements LocalChainSyncAuthority {
  private readonly url: string;
  private history: OgmiosPoint[] = [];
  private rollbackSequence = 0;

  constructor(url: string) {
    this.url = url;
  }

  async synchronize(): Promise<LocalChainSyncSnapshot> {
    return withOgmiosRpc(this.url, async (request) => {
      const candidateHistory = this.history.slice(-256);
      const candidates =
        candidateHistory.length === 0
          ? ["origin"]
          : [
              ...candidateHistory
                .reverse()
                .map(({ id, slot }) => ({ id, slot })),
              "origin",
            ];
      const intersectionResponse = await request("findIntersection", {
        points: candidates,
      });
      const intersectionResult = asRecord(intersectionResponse.result);
      const targetTip = parseOgmiosPoint(
        intersectionResult.tip,
        "chain-sync tip",
      );
      if (targetTip === "origin") {
        throw new Error("local_node chain-sync authority is still at origin");
      }
      const intersection = parseOgmiosPoint(
        intersectionResult.intersection,
        "chain-sync intersection",
      );
      const submittedIntersection =
        intersection === "origin"
          ? true
          : candidateHistory.some((point) =>
              sameOgmiosPoint(point, intersection),
            );
      if (!submittedIntersection) {
        throw new Error(
          "local_node chain-sync returned an intersection that was not one of the submitted bounded-history candidates",
        );
      }
      if (this.history.length === 0) {
        this.history = [targetTip];
        return this.snapshot(targetTip);
      }
      const previous = this.history.at(-1)!;
      if (
        intersection === "origin" ||
        !sameOgmiosPoint(intersection, previous)
      ) {
        this.rollbackSequence += 1;
        this.history =
          intersection === "origin"
            ? []
            : this.history.slice(
                0,
                this.history.findIndex((point) =>
                  sameOgmiosPoint(point, intersection),
                ) + 1,
              );
      }
      let cursor = intersection;
      let currentTarget = targetTip;
      let eventCount = 0;
      let awaitingIntersectionRollback = true;
      while (cursor === "origin" || !sameOgmiosPoint(cursor, currentTarget)) {
        if (eventCount >= 4_096) {
          throw new Error(
            "local_node chain-sync catch-up exceeded the bounded event window",
          );
        }
        eventCount += 1;
        const response = await request("nextBlock");
        const result = asRecord(response.result);
        const direction = result.direction;
        if (direction === "backward") {
          const point = parseOgmiosPoint(result.point, "rollback point");
          const isIntersectionPositioning =
            awaitingIntersectionRollback &&
            (point === "origin"
              ? intersection === "origin"
              : intersection !== "origin" &&
                sameOgmiosPoint(point, intersection));
          if (!isIntersectionPositioning) {
            this.rollbackSequence += 1;
            if (point === "origin") {
              this.history = [];
            } else {
              const rollbackIndex = this.history.findIndex((known) =>
                sameOgmiosPoint(known, point),
              );
              if (rollbackIndex < 0) {
                throw new Error(
                  "local_node chain-sync rolled back beyond the bounded canonical history",
                );
              }
              this.history = this.history.slice(0, rollbackIndex + 1);
            }
          }
          awaitingIntersectionRollback = false;
          cursor = point;
        } else if (direction === "forward") {
          awaitingIntersectionRollback = false;
          const point = parseOgmiosPoint(result.block, "roll-forward block");
          if (point === "origin") {
            throw new Error("roll-forward block cannot be origin");
          }
          this.history.push(point);
          this.history = this.history.slice(-512);
          cursor = point;
        } else {
          throw new Error(
            "local_node chain-sync returned an unknown direction",
          );
        }
        const nextTarget = parseOgmiosPoint(result.tip, "next-block tip");
        if (nextTarget === "origin") {
          throw new Error("local_node chain-sync tip regressed to origin");
        }
        currentTarget = nextTarget;
      }
      return this.snapshot(cursor);
    });
  }

  private snapshot(tip: OgmiosPoint): LocalChainSyncSnapshot {
    return {
      tip,
      rollbackSequence: this.rollbackSequence,
    };
  }
}

export const stateQueueUtxosToObservedNodes = async (
  stateQueueUtxos: readonly SDK.StateQueueUTxO[],
  providerSource: string,
  chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>,
): Promise<readonly ObservedStateQueueNode[]> => {
  const observed: ObservedStateQueueNode[] = [];
  for (const stateQueueUtxo of stateQueueUtxos) {
    if (stateQueueUtxo.datum.key === "Empty") {
      continue;
    }
    const stateQueueNode = await Effect.runPromise(
      SDK.getStateQueueNodeV1FromStateQueueDatum(stateQueueUtxo.datum),
    );
    const chainPoint = {
      providerSource,
      observedAt: new Date().toISOString(),
      ...(chainPointResolver === undefined
        ? {}
        : await chainPointResolver(stateQueueUtxo.utxo)),
    };
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

export const providerFromConfig = async (
  config: WatcherConfig,
): Promise<StateQueueProvider> => {
  if (config.l1Source.sourceMode === "local_node") {
    const localSource = config.l1Source;
    if (localSource.chainSyncOgmiosUrl.startsWith("fixture-chain-sync:")) {
      const providers = await Promise.all(
        localSource.queryProviderUrls.map((url) =>
          providerFromUrl(url, config),
        ),
      );
      return new MultiStateQueueProvider(providers, {
        sourceMode: "local_node",
        identities: localSource.queryProviderUrls.map(
          (_, index) =>
            `query:${localSource.authorityNodeId}:${index.toString()}`,
        ),
      });
    }
    const providers = await Promise.all(
      localSource.queryProviderUrls.map((url) => providerFromUrl(url, config)),
    );
    if (!providers.every(isTipAwareStateQueueProvider)) {
      throw new Error(
        "local_node query surfaces must expose an Ogmios-backed canonical tip",
      );
    }
    return new LocalNodeStateQueueProvider({
      authority: new OgmiosChainSyncAuthority(
        localSource.chainSyncOgmiosUrl.slice("ogmios-chain-sync:".length),
      ),
      authorityIdentity: localSource.authorityNodeId,
      queryProviders: providers,
      identities: localSource.queryProviderUrls.map(
        (_, index) =>
          `query:${localSource.authorityNodeId}:${index.toString()}`,
      ),
    });
  }
  const providers = await Promise.all(
    config.l1Source.providers.map(({ url }) => providerFromUrl(url, config)),
  );
  return new MultiStateQueueProvider(providers, {
    sourceMode: "external_providers",
    identities: config.l1Source.providers.map(({ identity }) => identity),
  });
};

export const providerFromUrl = async (
  url: string,
  config: Pick<
    WatcherConfig,
    "network" | "stateQueueAddress" | "stateQueuePolicyId"
  >,
): Promise<StateQueueProvider> => {
  if (url.startsWith("fixture:")) {
    return new FixtureStateQueueProvider(url.slice("fixture:".length));
  }
  if (url.startsWith("file:")) {
    return new FixtureStateQueueProvider(new URL(url).pathname);
  }
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    const lucid = await Lucid(
      new Blockfrost(apiUrl, projectId),
      normalizeNetwork(config.network),
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `blockfrost:${apiUrl}`,
      chainPointResolver: blockfrostChainPointResolver(
        lucid,
        apiUrl,
        projectId,
      ),
    });
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl, headers),
      normalizeNetwork(config.network),
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
      chainPointResolver: kupmiosChainPointResolver(lucid),
      tipResolver: () => fetchKupmiosIndexedTip(kupoUrl, ogmiosUrl),
    });
  }
  throw new Error(
    `unsupported CARDANO_PROVIDER_URLS entry ${url}; supported forms are fixture:<path>, file:<path>, blockfrost:<api-url>#<project-id>, and kupmios:<kupo-url>|<ogmios-url>`,
  );
};

const parseBlockfrostUrl = (
  value: string,
): { readonly apiUrl: string; readonly projectId: string } => {
  const raw = value.slice("blockfrost:".length);
  const hashIndex = raw.lastIndexOf("#");
  if (hashIndex <= 0 || hashIndex === raw.length - 1) {
    throw new Error(
      "blockfrost provider URL must be blockfrost:<api-url>#<project-id>",
    );
  }
  return {
    apiUrl: raw.slice(0, hashIndex),
    projectId: raw.slice(hashIndex + 1),
  };
};

const parseKupmiosUrl = (
  value: string,
): {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly headers?: Record<string, string>;
} => {
  const raw = value.slice("kupmios:".length);
  const [kupoUrl, ogmiosUrl] = raw.split("|");
  if (kupoUrl === undefined || ogmiosUrl === undefined) {
    throw new Error(
      "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
    );
  }
  return { kupoUrl, ogmiosUrl };
};

export const lucidChainPointResolver = (
  lucid: LucidEvolution,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  return async (utxo) => {
    const status = await lucid.transactionStatus(utxo.txHash);
    if (status.status !== "confirmed") {
      throw new Error(
        `state-queue transaction ${utxo.txHash} is not confirmed: ${status.status}`,
      );
    }
    const { slot, blockHash, blockHeight, confirmations } = status.confirmation;
    // Lucid counts the inclusion block; Midgard depth counts descendants.
    return {
      ...(slot === undefined ? {} : { slot }),
      ...(blockHash === undefined ? {} : { blockHash }),
      ...(blockHeight === undefined ? {} : { blockHeight }),
      ...(confirmations === undefined
        ? {}
        : { depth: Math.max(0, confirmations - 1) }),
    };
  };
};

/**
 * Kupmios transaction status may omit confirmation count. In that case depth
 * is derived only from inclusion/tip block heights. Slot distance is not a
 * confirmation metric because empty Cardano slots do not create descendants.
 */
export const kupmiosChainPointResolver = (
  lucid: LucidEvolution,
): ((utxo: UTxO, tip?: OgmiosPoint) => Promise<ChainPoint>) => {
  const resolveInclusion = lucidChainPointResolver(lucid);
  return async (utxo, tip) => {
    const inclusion = await resolveInclusion(utxo);
    if (inclusion.depth !== undefined) {
      return inclusion;
    }
    return {
      ...inclusion,
      ...(tip === undefined || inclusion.blockHeight === undefined
        ? {}
        : { depth: Math.max(0, tip.height - inclusion.blockHeight) }),
    };
  };
};

const blockfrostChainPointResolver =
  (lucid: LucidEvolution, apiUrl: string, projectId: string) =>
  async (utxo: UTxO): Promise<ChainPoint> => {
    const [inclusion, latest] = await Promise.all([
      lucidChainPointResolver(lucid)(utxo),
      blockfrostJson<BlockfrostLatestBlock>(
        apiUrl,
        projectId,
        "/blocks/latest",
      ),
    ]);
    const blockHeight = inclusion.blockHeight;
    const latestHeight = latest.height;
    return {
      ...inclusion,
      depth:
        typeof blockHeight === "number" && typeof latestHeight === "number"
          ? Math.max(0, latestHeight - blockHeight)
          : undefined,
      finalized: undefined,
    };
  };

const blockfrostJson = async <T>(
  apiUrl: string,
  projectId: string,
  path: string,
): Promise<T> => {
  const response = await fetch(`${apiUrl.replace(/\/$/, "")}${path}`, {
    headers: { project_id: projectId },
  });
  if (!response.ok) {
    throw new Error(
      `Blockfrost ${path} returned ${response.status.toString()} ${response.statusText}`,
    );
  }
  return (await response.json()) as T;
};

type BlockfrostLatestBlock = {
  readonly height?: number;
};

const isTipAwareStateQueueProvider = (
  provider: StateQueueProvider,
): provider is TipAwareStateQueueProvider =>
  "fetchStateQueueObservation" in provider &&
  typeof provider.fetchStateQueueObservation === "function";

const sameOgmiosPoint = (left: OgmiosPoint, right: OgmiosPoint): boolean =>
  left.slot === right.slot &&
  left.id === right.id &&
  left.height === right.height;

type OgmiosRpcResponse = {
  readonly id?: { readonly requestId?: string };
  readonly result?: unknown;
  readonly error?: unknown;
};
type OgmiosRequest = (
  method: "findIntersection" | "nextBlock",
  params?: Record<string, unknown>,
) => Promise<OgmiosRpcResponse>;
type OgmiosMessageEvent = { readonly data: unknown };
type OgmiosSocket = {
  send(data: string): void;
  close(): void;
  addEventListener(
    type: "open" | "message" | "error" | "close",
    listener: (event: OgmiosMessageEvent) => void,
  ): void;
  removeEventListener(
    type: "message",
    listener: (event: OgmiosMessageEvent) => void,
  ): void;
};
type OgmiosSocketConstructor = new (url: string) => OgmiosSocket;

const withOgmiosRpc = async <T>(
  url: string,
  operation: (request: OgmiosRequest) => Promise<T>,
): Promise<T> => {
  const Socket = (
    globalThis as unknown as { WebSocket?: OgmiosSocketConstructor }
  ).WebSocket;
  if (Socket === undefined) {
    throw new Error(
      "Node.js WebSocket support is required for Ogmios chain-sync",
    );
  }
  const socket = new Socket(url);
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(
      () => reject(new Error("Ogmios chain-sync connection timed out")),
      10_000,
    );
    const settle = (callback: () => void): void => {
      clearTimeout(timeout);
      callback();
    };
    socket.addEventListener("open", () => settle(resolve));
    socket.addEventListener("error", () =>
      settle(() => reject(new Error("Ogmios chain-sync connection failed"))),
    );
    socket.addEventListener("close", () =>
      settle(() =>
        reject(new Error("Ogmios chain-sync connection closed before open")),
      ),
    );
  });
  let requestSequence = 0;
  const request: OgmiosRequest = async (method, params) => {
    requestSequence += 1;
    const requestId = `midgard-da-${requestSequence.toString()}`;
    return new Promise<OgmiosRpcResponse>((resolve, reject) => {
      const onMessage = (event: OgmiosMessageEvent): void => {
        let response: OgmiosRpcResponse;
        try {
          response = JSON.parse(String(event.data)) as OgmiosRpcResponse;
        } catch {
          return;
        }
        if (response.id?.requestId !== requestId) {
          return;
        }
        clearTimeout(timeout);
        socket.removeEventListener("message", onMessage);
        if (response.error !== undefined || response.result === undefined) {
          reject(new Error(`Ogmios ${method} request failed`));
          return;
        }
        resolve(response);
      };
      const timeout = setTimeout(() => {
        socket.removeEventListener("message", onMessage);
        reject(new Error(`Ogmios ${method} request timed out`));
      }, 10_000);
      socket.addEventListener("message", onMessage);
      socket.send(
        JSON.stringify({
          jsonrpc: "2.0",
          method,
          ...(params === undefined ? {} : { params }),
          id: { requestId },
        }),
      );
    });
  };
  try {
    return await operation(request);
  } finally {
    socket.close();
  }
};

const fetchOgmiosTipPoint = (url: string): Promise<OgmiosPoint> =>
  withOgmiosRpc(ogmiosWebSocketEndpoint(url), async (request) => {
    const response = await request("findIntersection", { points: ["origin"] });
    const point = parseOgmiosPoint(
      asRecord(response.result).tip,
      "Ogmios query tip",
    );
    if (point === "origin") {
      throw new Error("Ogmios query authority is still at origin");
    }
    return point;
  });

export const ogmiosWebSocketEndpoint = (value: string): string => {
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error("Ogmios endpoint must be an absolute URL");
  }
  if (endpoint.protocol === "http:") {
    endpoint.protocol = "ws:";
  } else if (endpoint.protocol === "https:") {
    endpoint.protocol = "wss:";
  } else if (endpoint.protocol !== "ws:" && endpoint.protocol !== "wss:") {
    throw new Error("Ogmios endpoint must use http(s) or ws(s)");
  }
  return endpoint.toString();
};

const fetchKupmiosIndexedTip = async (
  kupoUrl: string,
  ogmiosUrl: string,
): Promise<OgmiosPoint> => {
  const [kupoSlot, ogmiosTip] = await Promise.all([
    fetchKupoTipSlot(kupoUrl),
    fetchOgmiosTipPoint(ogmiosUrl),
  ]);
  if (kupoSlot !== ogmiosTip.slot) {
    throw new Error(
      `local query index is not aligned with its Ogmios authority: kupo_slot=${kupoSlot.toString()} ogmios_slot=${ogmiosTip.slot.toString()}`,
    );
  }
  return ogmiosTip;
};

const fetchKupoTipSlot = async (kupoUrl: string): Promise<number> => {
  const response = await fetch(`${kupoUrl.replace(/\/+$/, "")}/health`, {
    headers: { accept: "application/json" },
  });
  if (!response.ok) {
    throw new Error(
      `Kupo health lookup failed: ${response.status.toString()} ${await response.text()}`,
    );
  }
  return kupoIndexedSlotFromHealth(await response.json());
};

export const kupoIndexedSlotFromHealth = (value: unknown): number => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("Kupo health response is malformed");
  }
  const health = value as Record<string, unknown>;
  if (health.connection_status !== "connected") {
    throw new Error("Kupo health response is not connected to its node");
  }
  const checkpoint = health.most_recent_checkpoint;
  if (!Number.isSafeInteger(checkpoint) || Number(checkpoint) < 0) {
    throw new Error(
      "Kupo health response does not expose a valid indexed checkpoint",
    );
  }
  return Number(checkpoint);
};

const parseOgmiosPoint = (
  value: unknown,
  label: string,
): OgmiosPoint | "origin" => {
  if (value === "origin") {
    return value;
  }
  const point = asRecord(value);
  const height = point.height ?? point.blockNo;
  if (
    typeof point.id !== "string" ||
    !/^[0-9a-f]{64}$/u.test(point.id) ||
    !Number.isSafeInteger(point.slot) ||
    Number(point.slot) < 0 ||
    !Number.isSafeInteger(height) ||
    Number(height) < 0 ||
    (point.height !== undefined &&
      point.blockNo !== undefined &&
      point.height !== point.blockNo)
  ) {
    throw new Error(`${label} is malformed`);
  }
  return {
    id: point.id,
    slot: Number(point.slot),
    height: Number(height),
  };
};

const asRecord = (value: unknown): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("Ogmios chain-sync response is malformed");
  }
  return value as Record<string, unknown>;
};

const sortObservedNodes = (
  nodes: readonly ObservedStateQueueNode[],
): readonly ObservedStateQueueNode[] =>
  [...nodes].sort((left, right) =>
    canonicalObservedNode(left).localeCompare(canonicalObservedNode(right)),
  );

const canonicalObservedNodes = (
  nodes: readonly ObservedStateQueueNode[],
): readonly string[] => nodes.map(canonicalObservedNode);

const canonicalObservedNode = (node: ObservedStateQueueNode): string =>
  canonicalJson({
    outRef: node.outRef,
    assetName: node.assetName,
    linkedListKey: node.linkedListKey,
    rawDatumCbor: node.rawDatumCbor ?? null,
    header: node.header,
    daAttestation: node.daAttestation,
    chainPoint: {
      slot: node.chainPoint.slot ?? null,
      blockHash: node.chainPoint.blockHash ?? null,
      blockHeight: node.chainPoint.blockHeight ?? null,
    },
  });

const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const mergeAgreedObservedNodes = (
  sortedResults: readonly (readonly ObservedStateQueueNode[])[],
  identities?: readonly string[],
): readonly ObservedStateQueueNode[] =>
  sortedResults[0]!.map((node, index) => ({
    ...node,
    chainPoint: mergeChainPoints(
      sortedResults.map((nodes, providerIndex) => ({
        ...nodes[index]!.chainPoint,
        providerSource:
          identities?.[providerIndex] ??
          nodes[index]!.chainPoint.providerSource,
      })),
    ),
  }));

const mergeChainPoints = (points: readonly ChainPoint[]): ChainPoint => {
  const primary = points[0] ?? {};
  const sources = points
    .map((point) => point.providerSource)
    .filter((source): source is string => source !== undefined);
  const depths = points
    .map((point) => point.depth)
    .filter((depth): depth is number => depth !== undefined);
  const allDepthsKnown = depths.length === points.length;
  const finalized =
    points.length > 0 && points.every((point) => point.finalized === true)
      ? true
      : points.some((point) => point.finalized === false)
        ? false
        : undefined;
  return {
    ...primary,
    providerSource:
      sources.length === 0 ? primary.providerSource : sources.join(","),
    observedAt: new Date().toISOString(),
    depth: allDepthsKnown ? Math.min(...depths) : undefined,
    finalized,
  };
};

const canonicalJson = (value: unknown): string =>
  JSON.stringify(canonicalValue(value));

const canonicalValue = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString();
  }
  if (Array.isArray(value)) {
    return value.map(canonicalValue);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([key, entry]) => [key, canonicalValue(entry)]),
    );
  }
  return value;
};

const normalizeNetwork = (network: string): CardanoNetwork => {
  if (
    network === "Mainnet" ||
    network === "Preprod" ||
    network === "Preview" ||
    network === "Custom"
  ) {
    return network;
  }
  throw new Error(
    `unsupported Lucid network ${network}; expected Mainnet, Preprod, Preview, or Custom`,
  );
};

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
