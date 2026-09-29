/**
 * What a committee Lucid client needs, beside its provider, to run on the
 * configured network. Mainnet, Preprod and Preview carry Lucid's built-in slot
 * mapping. `Custom` has none, so the mapping is read from the local Ogmios the
 * client reads and submits through: its network magic must be the configured
 * one, then its Shelley genesis epoch and a live submit-slot snapshot must
 * agree. Settle, Close, response and pool transactions carry validity
 * intervals, and the pool's `unlock_at` and withdraw deadlines are
 * time-based, so a wrong `zeroTime` would move every one of them.
 */
import {
  customSlotConfigFromShelleyGenesis,
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import type { SlotConfig } from "@lucid-evolution/lucid";

export type CommitteeLucidNetwork =
  | "Mainnet"
  | "Preprod"
  | "Preview"
  | "Custom";

/** The L1 route a committee Lucid client is built on. */
export type CommitteeLucidRoute =
  | { readonly provider: "kupmios"; readonly ogmiosUrl: string }
  | { readonly provider: "blockfrost"; readonly apiUrl: string };

/**
 * A `Custom` Lucid client refused before it was built: no slot mapping could
 * be derived from the chain it would read.
 */
export class DaCommitteeCustomSlotMappingError extends Error {
  override readonly name = "DaCommitteeCustomSlotMappingError";
}

/**
 * The Lucid options for `network` on `route`: none on a named network (no
 * Ogmios query is made), and on `Custom` the slot mapping derived from the
 * route's Ogmios. There is no configured or default mapping to fall back to:
 * any failure refuses the client, naming the Ogmios it queried.
 */
export const committeeLucidSlotOptions = async (input: {
  readonly network: CommitteeLucidNetwork;
  readonly route: CommitteeLucidRoute;
  readonly networkMagic: number;
}): Promise<{ readonly slotConfig?: SlotConfig }> => {
  if (input.network !== "Custom") {
    return {};
  }
  const { route } = input;
  if (route.provider === "blockfrost") {
    throw new DaCommitteeCustomSlotMappingError(
      `Refusing the Custom Lucid client on Blockfrost at ${route.apiUrl}: Blockfrost serves no Custom network, so no slot mapping can be read from it; use kupmios:<kupo-url>|<ogmios-url>`,
    );
  }
  const { ogmiosUrl } = route;
  const stage = async <A>(label: string, run: () => Promise<A>) => {
    try {
      return await run();
    } catch (cause) {
      throw new DaCommitteeCustomSlotMappingError(
        `Refusing the Custom Lucid client: ${label} from the Ogmios at ${ogmiosUrl} failed: ${cause instanceof Error ? cause.message : String(cause)}`,
        { cause },
      );
    }
  };
  await stage("the network-magic check", () =>
    assertOgmiosNetworkMagic(ogmiosUrl, input.networkMagic),
  );
  const snapshot = await stage("the submit-slot snapshot", () =>
    queryLocalOgmiosSubmitSlotSnapshot({ ogmiosUrl }),
  );
  const genesis = await stage("the Shelley genesis query", () =>
    queryLocalOgmiosShelleyGenesisSlotConfig({ ogmiosUrl }),
  );
  const slotConfig = await stage("the slot mapping check", async () =>
    customSlotConfigFromShelleyGenesis(genesis, snapshot),
  );
  return { slotConfig };
};

export const assertOgmiosNetworkMagic = async (
  ogmiosUrl: string,
  expectedNetworkMagic: number,
  fetchFn: typeof fetch = fetch,
): Promise<void> => {
  const endpoint = ogmiosHttpEndpoint(ogmiosUrl);
  let response: Response;
  try {
    response = await fetchFn(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryNetwork/genesisConfiguration",
        params: { era: "shelley" },
        id: "midgard-network-magic-preflight",
      }),
      signal: AbortSignal.timeout(10_000),
    });
  } catch {
    throw new Error("Ogmios network-magic preflight failed");
  }
  if (!response.ok) {
    throw new Error("Ogmios network-magic preflight failed");
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    throw new Error("Ogmios network-magic preflight returned invalid JSON");
  }
  const actualNetworkMagic = ogmiosNetworkMagic(body);
  if (actualNetworkMagic !== expectedNetworkMagic) {
    throw new Error(
      "Ogmios network magic does not match configured Cardano network authority",
    );
  }
};

const ogmiosHttpEndpoint = (ogmiosUrl: string): string => {
  let parsed: URL;
  try {
    parsed = new URL(ogmiosUrl);
  } catch {
    throw new Error("Kupmios Ogmios URL is invalid");
  }
  if (parsed.protocol === "ws:") {
    parsed.protocol = "http:";
  } else if (parsed.protocol === "wss:") {
    parsed.protocol = "https:";
  } else if (parsed.protocol !== "http:" && parsed.protocol !== "https:") {
    throw new Error("Kupmios Ogmios URL must use http, https, ws, or wss");
  }
  return parsed.toString();
};

const ogmiosNetworkMagic = (body: unknown): number => {
  if (typeof body !== "object" || body === null || Array.isArray(body)) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  const result = (body as Record<string, unknown>).result;
  if (typeof result !== "object" || result === null || Array.isArray(result)) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  const networkMagic = (result as Record<string, unknown>).networkMagic;
  if (
    !Number.isSafeInteger(networkMagic) ||
    (networkMagic as number) < 0 ||
    (networkMagic as number) > 4_294_967_295
  ) {
    throw new Error(
      "Ogmios genesis configuration is missing an unsigned network magic",
    );
  }
  return networkMagic as number;
};
