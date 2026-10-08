/**
 * What a committee Lucid client needs, beside its provider, to run on the
 * configured network. Mainnet, Preprod and Preview carry Lucid's built-in slot
 * mapping. `Custom` has none, so the mapping is read from the local Ogmios the
 * client reads and submits through: its network magic must be the configured
 * one, then its Shelley genesis epoch and a live submit-slot snapshot must
 * agree. Settle, Close, response and pool transactions carry validity
 * intervals, and the pool's `unlock_at` and withdraw deadlines are
 * time-based, so a wrong `zeroTime` would move every one of them.
 *
 * The mapping is a pure function of the genesis, so it is derived from the
 * genesis alone and kept for the process once one healthy, clock-agreeing
 * snapshot has confirmed it. Every later construction still checks the
 * network magic and re-reads the genesis, neither of which depends on tip
 * freshness: an Ogmios swapped behind the same URL is refused, and a chain
 * reset under it is mapped afresh. While the Ogmios is unreachable, resyncing or
 * between blocks past the tip-age bound, construction waits and re-reads,
 * logging the unready reason; a wrong network, a malformed answer or a slot
 * length other than the profile's still refuses at once.
 */
import {
  assertNoOgmiosJsonRpcError,
  customSlotConfigFromShelleyGenesis,
  customSlotConfigFromShelleyGenesisAtWallClock,
  normalizeOgmiosHttpUrl,
  ogmiosSlotEvidenceUnavailableCause,
  OgmiosSlotEvidenceUnavailableError,
  ogmiosTipMaxAgeMsFromShelleyGenesis,
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
export type CommitteeLucidRoute = {
  readonly provider: "kupmios";
  readonly ogmiosUrl: string;
};

/**
 * A `Custom` Lucid client refused before it was built: no slot mapping could
 * be derived from the chain it would read.
 */
export class DaCommitteeCustomSlotMappingError extends Error {
  override readonly name = "DaCommitteeCustomSlotMappingError";
}

/** How a Custom Lucid construction waits out transient Ogmios evidence. */
export type CommitteeSlotEvidenceWait = {
  readonly log?: (message: string) => void;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly baseDelayMs?: number;
  readonly maxDelayMs?: number;
};

const confirmedMappings = new Map<string, Promise<SlotConfig>>();

/**
 * The Lucid options for `network` on `route`: none on a named network (no
 * Ogmios query is made), and on `Custom` the slot mapping derived from the
 * route's Ogmios. There is no configured or default mapping to fall back to:
 * any non-transient failure refuses the client, naming the Ogmios it queried.
 */
export const committeeLucidSlotOptions = async (
  input: {
    readonly network: CommitteeLucidNetwork;
    readonly route: CommitteeLucidRoute;
    readonly networkMagic: number;
  },
  wait: CommitteeSlotEvidenceWait = {},
): Promise<{ readonly slotConfig?: SlotConfig }> => {
  if (input.network !== "Custom") {
    return {};
  }
  const { route } = input;
  const key = `${normalizeOgmiosHttpUrl(route.ogmiosUrl)}#${input.networkMagic.toString()}`;
  const cached = confirmedMappings.get(key);
  if (cached !== undefined) {
    const confirmed = await cached;
    const { slotConfig } = await genesisSlotConfig(
      evidenceStage(route.ogmiosUrl, wait),
      route.ogmiosUrl,
      input.networkMagic,
    );
    if (sameSlotConfig(slotConfig, confirmed)) {
      return { slotConfig: confirmed };
    }
    if (confirmedMappings.get(key) === cached) {
      confirmedMappings.delete(key);
    }
    (wait.log ?? console.warn)(
      `Custom Lucid slot mapping changed: the Shelley genesis of the Ogmios at ${route.ogmiosUrl} no longer gives the kept mapping; deriving it afresh.`,
    );
  }
  let mapping = confirmedMappings.get(key);
  if (mapping === undefined) {
    mapping = deriveCustomSlotConfig(route.ogmiosUrl, input.networkMagic, wait);
    confirmedMappings.set(key, mapping);
    // Only a confirmed mapping is kept: a refusal is re-derived next time.
    mapping.catch(() => confirmedMappings.delete(key));
  }
  return { slotConfig: await mapping };
};

const sameSlotConfig = (left: SlotConfig, right: SlotConfig): boolean =>
  left.zeroTime === right.zeroTime &&
  left.zeroSlot === right.zeroSlot &&
  left.slotLength === right.slotLength;

type EvidenceStage = <A>(label: string, run: () => Promise<A>) => Promise<A>;

const evidenceStage =
  (
    ogmiosUrl: string,
    {
      log = (message) => console.warn(message),
      sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
      baseDelayMs = 1_000,
      maxDelayMs = 15_000,
    }: CommitteeSlotEvidenceWait,
  ): EvidenceStage =>
  async <A>(label: string, run: () => Promise<A>) => {
    for (let attempt = 1; ; attempt += 1) {
      try {
        return await run();
      } catch (cause) {
        const unavailable = ogmiosSlotEvidenceUnavailableCause(cause);
        if (unavailable === undefined) {
          throw new DaCommitteeCustomSlotMappingError(
            `Refusing the Custom Lucid client: ${label} from the Ogmios at ${ogmiosUrl} failed: ${cause instanceof Error ? cause.message : String(cause)}`,
            { cause },
          );
        }
        const delayMs = Math.min(maxDelayMs, baseDelayMs * 2 ** (attempt - 1));
        log(
          `Custom Lucid slot mapping unready: reason=${unavailable.reason}; ${label} from the Ogmios at ${ogmiosUrl} waits ${delayMs.toString()}ms and re-reads. cause=${unavailable.message}`,
        );
        await sleep(delayMs);
      }
    }
  };

/** The network-magic check, then the mapping the Shelley genesis gives. */
const genesisSlotConfig = async (
  stage: EvidenceStage,
  ogmiosUrl: string,
  networkMagic: number,
) => {
  await stage("the network-magic check", () =>
    assertOgmiosNetworkMagic(ogmiosUrl, networkMagic),
  );
  const genesis = await stage("the Shelley genesis query", () =>
    queryLocalOgmiosShelleyGenesisSlotConfig({ ogmiosUrl }),
  );
  const slotConfig = await stage("the slot mapping check", async () =>
    customSlotConfigFromShelleyGenesisAtWallClock(genesis, {
      nowMs: Date.now(),
    }),
  );
  return { genesis, slotConfig };
};

const deriveCustomSlotConfig = async (
  ogmiosUrl: string,
  networkMagic: number,
  wait: CommitteeSlotEvidenceWait,
): Promise<SlotConfig> => {
  const stage = evidenceStage(ogmiosUrl, wait);
  const { genesis, slotConfig } = await genesisSlotConfig(
    stage,
    ogmiosUrl,
    networkMagic,
  );
  const maxHealthAgeMs = await stage("the tip-age bound", async () =>
    ogmiosTipMaxAgeMsFromShelleyGenesis(genesis),
  );
  await stage("the submit-slot clock check", async () =>
    customSlotConfigFromShelleyGenesis(
      genesis,
      await queryLocalOgmiosSubmitSlotSnapshot({ ogmiosUrl, maxHealthAgeMs }),
    ),
  );
  return slotConfig;
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
  } catch (cause) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_unreachable",
      "Ogmios network-magic preflight failed",
      { cause },
    );
  }
  if (!response.ok) {
    throw TRANSIENT_HTTP_STATUSES.has(response.status)
      ? new OgmiosSlotEvidenceUnavailableError(
          "ogmios_unreachable",
          "Ogmios network-magic preflight failed",
        )
      : new Error("Ogmios network-magic preflight failed");
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    throw new Error("Ogmios network-magic preflight returned invalid JSON");
  }
  // An Ogmios that answers a query error has not yet acquired its ledger.
  assertNoOgmiosJsonRpcError(body, "Ogmios network-magic preflight");
  const actualNetworkMagic = ogmiosNetworkMagic(body);
  if (actualNetworkMagic !== expectedNetworkMagic) {
    throw new Error(
      "Ogmios network magic does not match configured Cardano network authority",
    );
  }
};

const TRANSIENT_HTTP_STATUSES: ReadonlySet<number> = new Set([
  408, 425, 429, 500, 502, 503, 504,
]);

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
