import {
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
} from "@al-ft/midgard-core/native-reward-account";
import { Blockfrost, Lucid, type LucidEvolution } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import type { NativeLedgerConfig } from "../config.js";
import {
  type CommitteeLucidNetwork,
  committeeLucidSlotOptions,
} from "./lucid-network.js";

type CardanoNetwork = CommitteeLucidNetwork;

const lucidOptions = {
  evaluator: createScalusEvaluator(),
};

export const NATIVE_LEDGER_QUERY_TIMEOUT_MS = 30_000;

/**
 * Kupmios reward-account state comes from the local node ledger, because
 * Ogmios omits registered accounts without a stake-pool delegation. Without
 * `nativeLedger` the Kupmios provider refuses reward-account reads.
 */
export const lucidFromProviderUrl = async (
  url: string,
  network: string,
  nativeLedger: NativeLedgerConfig | undefined,
  networkMagic: number,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly providerSource: string;
}> => {
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    const cardanoNetwork = normalizeNetwork(network);
    const slotOptions = await committeeLucidSlotOptions({
      network: cardanoNetwork,
      route: { provider: "blockfrost", apiUrl },
      networkMagic,
    });
    return {
      lucid: await Lucid(new Blockfrost(apiUrl, projectId), cardanoNetwork, {
        ...lucidOptions,
        ...slotOptions,
      }),
      providerSource: `blockfrost:${apiUrl}`,
    };
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    const cardanoNetwork = normalizeNetwork(network);
    const slotOptions = await committeeLucidSlotOptions({
      network: cardanoNetwork,
      route: { provider: "kupmios", ogmiosUrl },
      networkMagic,
    });
    return {
      lucid: await Lucid(
        new NativeLedgerKupmios(
          kupoUrl,
          ogmiosUrl,
          nativeLedgerAuthoritySource(
            nativeLedger === undefined
              ? undefined
              : {
                  ...nativeLedger,
                  network: cardanoNetwork,
                  timeoutMs: NATIVE_LEDGER_QUERY_TIMEOUT_MS,
                },
          ),
          headers,
        ),
        cardanoNetwork,
        { ...lucidOptions, ...slotOptions },
      ),
      providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
    };
  }
  throw new Error(`unsupported Cardano provider for Lucid: ${url}`);
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

const normalizeNetwork = (value: string): CardanoNetwork => {
  const normalized = value.trim().toLowerCase();
  switch (normalized) {
    case "mainnet":
      return "Mainnet";
    case "preprod":
    case "pre-production":
    case "preproduction":
      return "Preprod";
    case "preview":
      return "Preview";
    case "custom":
      return "Custom";
    default:
      throw new Error(
        `unsupported Cardano network ${value}; expected Mainnet, Preprod, Preview, or Custom`,
      );
  }
};
