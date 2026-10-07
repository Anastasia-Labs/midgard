import { createHash } from "node:crypto";
import { resolve } from "node:path";

import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { LoadedCommitteeConfig } from "../config.js";
import type { ChainPoint } from "../domain.js";
import { canonicalJson } from "./canonical-json.js";
import { FileChainSyncCursorStore } from "./provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "./provider.local-node-chain-authority.js";
import { localNodeChainCursorPath } from "./provider.local-node-state-queue-provider.js";
import {
  KUPO_HEALTH_TIMEOUT_MS,
  localAuthorityRegistry,
} from "./provider.ogmios-rpc-session.js";
import {
  FixtureChainSyncEventSource,
  OgmiosChainSyncEventSource,
} from "./provider.parse-fixture-chain-sync-events.js";
import {
  type ChainSyncEventSource,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";

export const localNodeChainAuthorityFromConfig = (
  config: LoadedCommitteeConfig,
): LocalNodeChainAuthority => {
  if (config.l1Source.sourceMode !== "local_node") {
    throw new Error(
      "local chain authority is only available in local_node mode",
    );
  }
  const source = config.l1Source;
  const cursorPath = localNodeChainCursorPath(source);
  const registryKey = [
    config.network,
    source.authorityNodeId,
    localAuthorityFingerprint(
      config.network,
      source.authorityNodeId,
      source.chainSyncProviderUrl,
    ),
    cursorPath,
  ].join("\u0000");
  const existing = localAuthorityRegistry.get(registryKey);
  if (existing !== undefined) {
    return existing;
  }
  const chainSyncUrl = source.chainSyncProviderUrl.slice("chain-sync:".length);
  let eventSource: ChainSyncEventSource;
  if (chainSyncUrl.startsWith("ogmios:")) {
    eventSource = new OgmiosChainSyncEventSource(
      chainSyncUrl.slice("ogmios:".length),
      config.network,
      source.authorityNodeId,
      undefined,
      config.cardanoL1Source.networkMagic,
    );
  } else if (chainSyncUrl.startsWith("kupmios:")) {
    const { ogmiosUrl } = parseKupmiosUrl(chainSyncUrl);
    eventSource = new OgmiosChainSyncEventSource(
      ogmiosUrl,
      config.network,
      source.authorityNodeId,
      undefined,
      config.cardanoL1Source.networkMagic,
    );
  } else if (chainSyncUrl.startsWith("fixture:")) {
    eventSource = new FixtureChainSyncEventSource(
      chainSyncUrl.slice("fixture:".length),
      config.network,
      source.authorityNodeId,
    );
  } else if (chainSyncUrl.startsWith("file:")) {
    eventSource = new FixtureChainSyncEventSource(
      new URL(chainSyncUrl).pathname,
      config.network,
      source.authorityNodeId,
    );
  } else {
    throw new Error(`unsupported local-node chain-sync source ${chainSyncUrl}`);
  }
  const authority = new LocalNodeChainAuthority(
    source.authorityNodeId,
    config.network,
    eventSource,
    new FileChainSyncCursorStore(
      cursorPath,
      localAuthorityFingerprint(
        config.network,
        source.authorityNodeId,
        source.chainSyncProviderUrl,
      ),
    ),
    // A catch-up after a long outage runs chunk after chunk: one line per
    // chunk shows it moving instead of looking hung.
    ({ caughtUp, events, cursorSlot, tipSlot }) =>
      process.stdout.write(
        `${JSON.stringify({
          event: caughtUp
            ? "l1_chain_sync_caught_up"
            : "l1_chain_sync_catching_up",
          events,
          cursorSlot,
          tipSlot,
        })}\n`,
      ),
  );
  localAuthorityRegistry.set(registryKey, authority);
  return authority;
};

export const localAuthorityFingerprint = (
  network: string,
  authorityNodeId: string,
  chainSyncProviderUrl: string,
): string => {
  const source = chainSyncProviderUrl.slice("chain-sync:".length);
  let canonicalSource: string;
  if (source.startsWith("ogmios:")) {
    canonicalSource = `ogmios:${normalizeAuthorityEndpoint(source.slice("ogmios:".length))}`;
  } else if (source.startsWith("kupmios:")) {
    canonicalSource = `ogmios:${normalizeAuthorityEndpoint(parseKupmiosUrl(source).ogmiosUrl)}`;
  } else if (source.startsWith("fixture:")) {
    canonicalSource = `fixture:${resolve(source.slice("fixture:".length))}`;
  } else if (source.startsWith("file:")) {
    canonicalSource = `fixture:${resolve(new URL(source).pathname)}`;
  } else {
    throw new Error("unsupported local chain authority source");
  }
  return createHash("sha256")
    .update(
      canonicalJson({
        network,
        authorityNodeId,
        canonicalSource,
      }),
    )
    .digest("hex");
};

export const l1AuthorityProviderSource = (
  config: Pick<LoadedCommitteeConfig, "cardanoL1Source">,
  providerIndex: number,
  surfaceIdentity: string,
): string => {
  const authority = config.cardanoL1Source;
  const surfaceDigest = createHash("sha256")
    .update(surfaceIdentity)
    .digest("hex");
  if (authority.sourceMode === "local_node") {
    return [
      "local_node",
      authority.authorityNodeId,
      authority.authorityDigest,
      surfaceDigest,
    ].join(":");
  }
  const providerAuthorityId = authority.providerAuthorityIds[providerIndex];
  if (providerAuthorityId === undefined) {
    throw new Error(
      "external provider is missing its configured authority identity",
    );
  }
  return [
    "external_providers",
    providerAuthorityId,
    authority.authorityDigest,
    surfaceDigest,
  ].join(":");
};

export const parseKupmiosUrl = (
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

const normalizeAuthorityEndpoint = (value: string): string => {
  const endpoint = new URL(value);
  if (endpoint.username !== "" || endpoint.password !== "") {
    throw new Error("local authority endpoint must not embed credentials");
  }
  const protocol =
    endpoint.protocol === "wss:"
      ? "https:"
      : endpoint.protocol === "ws:"
        ? "http:"
        : endpoint.protocol;
  if (protocol !== "http:" && protocol !== "https:") {
    throw new Error("local authority endpoint must use HTTP(S) or WS(S)");
  }
  const port =
    (protocol === "http:" && endpoint.port === "80") ||
    (protocol === "https:" && endpoint.port === "443")
      ? ""
      : endpoint.port;
  const path = endpoint.pathname.replace(/\/+$/u, "");
  const hostname = endpoint.hostname.toLowerCase().replace(/\.$/u, "");
  return `${protocol}//${hostname}${port === "" ? "" : `:${port}`}${path}`;
};

export const lucidChainPointResolver = (
  lucid: LucidEvolution,
): ((utxo: UTxO) => Promise<ChainPoint>) => {
  return async (utxo) => {
    const status = getRecord(
      (await lucid.transactionStatus(utxo.txHash)) as unknown,
      "Cardano transaction status",
    );
    if (status.status !== "confirmed") {
      throw new Error(
        `state-queue transaction ${utxo.txHash} is not confirmed: ${String(status.status)}`,
      );
    }
    const confirmation = getRecord(
      status.confirmation,
      "confirmed Cardano transaction provenance",
    );
    const slot =
      confirmation.slot === undefined
        ? undefined
        : safeSlot(confirmation.slot, "transaction inclusion slot");
    const blockHash =
      confirmation.blockHash === undefined
        ? undefined
        : safeBlockHash(
            confirmation.blockHash,
            "transaction inclusion block hash",
          );
    const blockHeight =
      confirmation.blockHeight === undefined
        ? undefined
        : safeSlot(
            confirmation.blockHeight,
            "transaction inclusion block height",
          );
    const confirmations =
      confirmation.confirmations === undefined
        ? undefined
        : safeSlot(
            confirmation.confirmations,
            "transaction confirmation count",
          );
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

type KupoCheckpoint = {
  readonly slot: number;
  readonly blockHash: string;
};

export const fetchKupoCheckpoint = async (
  kupoUrl: string,
  fetchFn: typeof fetch,
  timeoutMs = KUPO_HEALTH_TIMEOUT_MS,
): Promise<KupoCheckpoint> => {
  const response = await fetchFn(`${kupoUrl.replace(/\/+$/, "")}/health`, {
    headers: { accept: "text/plain" },
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!response.ok) {
    throw new Error(
      `Kupo health lookup failed: ${response.status.toString()} ${await response.text()}`,
    );
  }
  const body = await response.text();
  const match =
    body.match(/^kupo_most_recent_checkpoint\s+([0-9]+(?:\.[0-9]+)?)/mu) ??
    body.match(/^kupo_most_recent_node_tip\s+([0-9]+(?:\.[0-9]+)?)/mu);
  if (match === null) {
    throw new Error("Kupo health omitted its current checkpoint slot");
  }
  const slot = Number(match[1]);
  if (!Number.isSafeInteger(slot) || slot < 0) {
    throw new Error("Kupo health returned an invalid checkpoint slot");
  }
  const rawEtag = response.headers.get("etag");
  const blockHash = rawEtag
    ?.replace(/^W\//u, "")
    .replace(/^"|"$/gu, "")
    .toLowerCase();
  return {
    slot,
    blockHash: safeBlockHash(blockHash, "Kupo checkpoint ETag"),
  };
};
