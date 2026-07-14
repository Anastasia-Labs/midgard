import type { LucidEvolution } from "@lucid-evolution/lucid";

/**
 * Safely extracts the normalized Kupo base URL from a Lucid Kupmios provider.
 * Other providers and incomplete test doubles deliberately return undefined.
 */
export const kupmiosKupoUrlFromLucid = (
  lucid: Pick<LucidEvolution, "config">,
): string | undefined => {
  const config = (lucid as { readonly config?: unknown }).config;
  if (typeof config !== "function") return undefined;
  const provider = (config as () => { readonly provider?: unknown })()
    .provider as { readonly kupoUrl?: unknown } | undefined;
  if (
    provider === undefined ||
    typeof provider.kupoUrl !== "string" ||
    provider.kupoUrl.trim().length === 0
  ) {
    return undefined;
  }
  return provider.kupoUrl.trim().replace(/\/+$/, "");
};
