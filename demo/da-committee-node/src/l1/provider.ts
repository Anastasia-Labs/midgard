import { readFile } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { Lucid, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import * as LucidRuntime from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { WatcherConfig } from "../config.js";
import type { ChainPoint, ObservedStateQueueNode } from "../domain.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

type LucidProviderRuntime = {
  readonly Blockfrost: new (url: string, projectId?: string) => unknown;
  readonly Kupmios: new (
    kupoUrl: string,
    ogmiosUrl: string,
    headers?: Record<string, string>,
  ) => unknown;
};

type CardanoNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

type KupoCreatedAt = {
  readonly slot_no?: number;
  readonly header_hash?: string;
};

type KupoMatch = {
  readonly transaction_id?: string;
  readonly output_index?: number;
  readonly created_at?: KupoCreatedAt | null;
};

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
  private readonly chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>;

  constructor({
    lucid,
    stateQueueAddress,
    stateQueuePolicyId,
    providerSource,
    chainPointResolver,
  }: {
    readonly lucid: LucidEvolution;
    readonly stateQueueAddress: string;
    readonly stateQueuePolicyId: string;
    readonly providerSource: string;
    readonly chainPointResolver?: (utxo: UTxO) => Promise<ChainPoint>;
  }) {
    this.lucid = lucid;
    this.stateQueueAddress = stateQueueAddress;
    this.stateQueuePolicyId = stateQueuePolicyId;
    this.providerSource = providerSource;
    this.chainPointResolver = chainPointResolver;
  }

  async fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]> {
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
}

export class MultiStateQueueProvider implements StateQueueProvider {
  private readonly providers: readonly StateQueueProvider[];

  constructor(providers: readonly StateQueueProvider[]) {
    if (providers.length === 0) {
      throw new Error("at least one state-queue provider is required");
    }
    this.providers = providers;
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
          `state queue provider disagreement between provider 0 and provider ${index.toString()}`,
        );
      }
    }
    return mergeAgreedObservedNodes(sortedResults);
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
      SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
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
): Promise<StateQueueProvider> =>
  providerFromUrls(config.cardanoProviderUrls, config);

export const providerFromUrls = async (
  urls: readonly string[],
  config: Pick<
    WatcherConfig,
    "network" | "stateQueueAddress" | "stateQueuePolicyId"
  >,
): Promise<StateQueueProvider> => {
  if (urls.length === 0) {
    throw new Error("at least one CARDANO_PROVIDER_URLS entry is required");
  }
  const providers = await Promise.all(
    urls.map((url) => providerFromUrl(url, config)),
  );
  return providers.length === 1
    ? providers[0]!
    : new MultiStateQueueProvider(providers);
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
    const runtime = LucidRuntime as unknown as LucidProviderRuntime;
    const lucid = await Lucid(
      new runtime.Blockfrost(apiUrl, projectId) as never,
      normalizeNetwork(config.network) as never,
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `blockfrost:${apiUrl}`,
      chainPointResolver: blockfrostChainPointResolver(apiUrl, projectId),
    });
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    const runtime = LucidRuntime as unknown as LucidProviderRuntime;
    const lucid = await Lucid(
      new runtime.Kupmios(kupoUrl, ogmiosUrl, headers) as never,
      normalizeNetwork(config.network) as never,
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
      chainPointResolver: kupoChainPointResolver(
        kupoUrl,
        config.stateQueueAddress,
      ),
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

export const kupoChainPointResolver = (
  kupoUrl: string,
  address: string,
  fetchFn: typeof fetch = fetch,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  return async (utxo) => {
    const [entries, tip] = await Promise.all([
      fetchKupoMatches(kupoUrl, address, fetchFn),
      fetchKupoTipSlot(kupoUrl, fetchFn),
    ]);
    const entry = entries.find(
      (candidate) =>
        candidate.transaction_id?.toLowerCase() === utxo.txHash.toLowerCase() &&
        candidate.output_index === utxo.outputIndex,
    );
    const slot = numberOrUndefined(entry?.created_at?.slot_no);
    const blockHash = entry?.created_at?.header_hash;
    const depth =
      slot === undefined || tip === undefined
        ? undefined
        : Math.max(0, tip - slot);
    return {
      ...(slot === undefined ? {} : { slot }),
      ...(typeof blockHash === "string" && blockHash.length > 0
        ? { blockHash }
        : {}),
      ...(depth === undefined ? {} : { depth }),
    };
  };
};

const fetchKupoMatches = async (
  kupoUrl: string,
  address: string,
  fetchFn: typeof fetch,
): Promise<readonly KupoMatch[]> => {
  const response = await fetchFn(
    `${kupoUrl.replace(/\/+$/, "")}/matches/${encodeURIComponent(address)}?unspent`,
  );
  if (!response.ok) {
    throw new Error(
      `Kupo state-queue match lookup failed: ${response.status.toString()} ${await response.text()}`,
    );
  }
  const parsed = (await response.json()) as unknown;
  if (!Array.isArray(parsed)) {
    throw new Error("Kupo state-queue match response must be an array");
  }
  return parsed as readonly KupoMatch[];
};

const fetchKupoTipSlot = async (
  kupoUrl: string,
  fetchFn: typeof fetch,
): Promise<number | undefined> => {
  const response = await fetchFn(`${kupoUrl.replace(/\/+$/, "")}/health`);
  if (!response.ok) {
    throw new Error(
      `Kupo health lookup failed: ${response.status.toString()} ${await response.text()}`,
    );
  }
  const body = await response.text();
  const match =
    body.match(/^kupo_most_recent_node_tip\s+([0-9]+(?:\.[0-9]+)?)/m) ??
    body.match(/^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/m);
  return match === null ? undefined : numberOrUndefined(Number(match[1]));
};

const numberOrUndefined = (value: unknown): number | undefined =>
  typeof value === "number" && Number.isFinite(value) ? value : undefined;

const blockfrostChainPointResolver =
  (apiUrl: string, projectId: string) =>
  async (utxo: UTxO): Promise<ChainPoint> => {
    const [tx, latest] = await Promise.all([
      blockfrostJson<BlockfrostTx>(apiUrl, projectId, `/txs/${utxo.txHash}`),
      blockfrostJson<BlockfrostLatestBlock>(
        apiUrl,
        projectId,
        "/blocks/latest",
      ),
    ]);
    const blockHeight = tx.block_height;
    const latestHeight = latest.height;
    return {
      slot: tx.slot,
      blockHash: tx.block,
      blockHeight,
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

type BlockfrostTx = {
  readonly block?: string;
  readonly block_height?: number;
  readonly slot?: number;
};

type BlockfrostLatestBlock = {
  readonly height?: number;
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
  });

const canonicalArraysEqual = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const mergeAgreedObservedNodes = (
  sortedResults: readonly (readonly ObservedStateQueueNode[])[],
): readonly ObservedStateQueueNode[] =>
  sortedResults[0]!.map((node, index) => ({
    ...node,
    chainPoint: mergeChainPoints(
      sortedResults.map((nodes) => nodes[index]!.chainPoint),
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
