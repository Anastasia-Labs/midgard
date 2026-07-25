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
      chainPointResolver: kupmiosChainPointResolver(lucid, kupoUrl),
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
 * Kupmios transaction status currently exposes inclusion slot/hash but not a
 * confirmation count. Preserve the existing slot-depth finality semantics
 * with the narrowest possible Kupo fallback until the provider supplies it.
 */
export const kupmiosChainPointResolver = (
  lucid: LucidEvolution,
  kupoUrl: string,
  fetchFn: typeof fetch = fetch,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  const resolveInclusion = lucidChainPointResolver(lucid);
  return async (utxo) => {
    const inclusion = await resolveInclusion(utxo);
    if (inclusion.depth !== undefined || inclusion.slot === undefined) {
      return inclusion;
    }
    const tipSlot = await fetchKupoTipSlot(kupoUrl, fetchFn);
    return {
      ...inclusion,
      ...(tipSlot === undefined
        ? {}
        : { depth: Math.max(0, tipSlot - inclusion.slot) }),
    };
  };
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
  if (match === null) {
    return undefined;
  }
  const slot = Number(match[1]);
  return Number.isFinite(slot) ? slot : undefined;
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
