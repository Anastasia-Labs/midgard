import { Lucid, type LucidEvolution } from "@lucid-evolution/lucid";
import * as LucidRuntime from "@lucid-evolution/lucid";

type LucidProviderRuntime = {
  readonly Blockfrost: new (url: string, projectId?: string) => unknown;
  readonly Kupmios: new (
    kupoUrl: string,
    ogmiosUrl: string,
    headers?: Record<string, string>,
  ) => unknown;
};

type CardanoNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const lucidFromProviderUrl = async (
  url: string,
  network: string,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly providerSource: string;
}> => {
  const runtime = LucidRuntime as unknown as LucidProviderRuntime;
  if (url.startsWith("blockfrost:")) {
    const { apiUrl, projectId } = parseBlockfrostUrl(url);
    return {
      lucid: await Lucid(
        new runtime.Blockfrost(apiUrl, projectId) as never,
        normalizeNetwork(network) as never,
      ),
      providerSource: `blockfrost:${apiUrl}`,
    };
  }
  if (url.startsWith("kupmios:")) {
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    return {
      lucid: await Lucid(
        new runtime.Kupmios(kupoUrl, ogmiosUrl, headers) as never,
        normalizeNetwork(network) as never,
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
