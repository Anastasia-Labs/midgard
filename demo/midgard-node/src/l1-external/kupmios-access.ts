/**
 * The Kupmios adapter of the L1-access port (`../l1-access.ts`; option E):
 * tools only (the prover CLI's history reads, remote developers, the journey
 * harness). Role processes never load this module: it is reached only by a
 * dynamic import from the tool adapter layer, and the role boundary test
 * pins that.
 *
 * - Reads, submit and confirmation: provider-native (Kupo and Ogmios).
 * - Tip (the clock): the Ogmios `queryNetwork/tip` slot.
 * - View point: the Ogmios tip; the synchronized view point is that tip once
 *   Kupo's most recent checkpoint has reached it, so reads after it see the
 *   chain at least that far.
 * - Submit slot: the local Ogmios submit-slot snapshot.
 */
import {
  kupoCheckpointSlotOf,
  L1ExternalBehindError,
  ogmiosTipPointOf,
} from "@al-ft/midgard-core/l1-provider-tip";
import { normalizeOgmiosHttpUrl } from "@al-ft/midgard-core/ogmios-slot";
import {
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot-query";
import * as LE from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1AccessAdapter,
  type L1ViewPoint,
  openL1Access,
} from "../l1-access.js";

export type KupmiosAccessInput = Readonly<{
  network: LE.Network;
  kupoUrl: string;
  ogmiosUrl: string;
  /** A Kupmios the caller built (with a ledger reward authority, say). */
  provider?: LE.Provider;
  /** The slot mapping, when the caller already knows it. */
  slotConfig?: LE.SlotConfig;
  /** How long a synchronized view point waits for Kupo (default 60 s). */
  syncTimeoutMs?: number;
  /** Bound on one HTTP request (default 10 s). */
  requestTimeoutMs?: number;
  fetchImpl?: typeof fetch;
}>;

export type KupmiosAccessAdapter = L1AccessAdapter &
  Readonly<{
    kind: "kupmios";
    kupoUrl: string;
    ogmiosUrl: string;
    /** The Ogmios tip now. */
    ledgerTip: () => Promise<L1ViewPoint>;
  }>;

export type KupmiosAccess = L1Access<KupmiosAccessAdapter>;

const DEFAULT_SYNC_TIMEOUT_MS = 60_000;
const DEFAULT_REQUEST_TIMEOUT_MS = 10_000;
const SYNC_POLL_MS = 500;

/** Opens the Kupmios adapter; reads nothing until a read asks. */
export const openKupmiosAccess = (input: KupmiosAccessInput): KupmiosAccess => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const timeoutMs = input.requestTimeoutMs ?? DEFAULT_REQUEST_TIMEOUT_MS;
  const ogmiosUrl = normalizeOgmiosHttpUrl(input.ogmiosUrl);
  const kupoUrl = input.kupoUrl.trim().replace(/\/+$/u, "");
  const provider =
    input.provider ?? new LE.Kupmios(kupoUrl, input.ogmiosUrl.trim());
  const json = async (url: string, init: RequestInit): Promise<unknown> => {
    const response = await fetchImpl(url, {
      ...init,
      signal: AbortSignal.timeout(timeoutMs),
    });
    if (!response.ok)
      throw new Error(`${url} answered HTTP ${response.status.toString()}`);
    return await response.json();
  };
  const ledgerTip = async (): Promise<L1ViewPoint> =>
    ogmiosTipPointOf(
      await json(ogmiosUrl, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method: "queryNetwork/tip",
          id: "midgard-l1-access",
        }),
      }),
    );
  const kupoCheckpointSlot = async (): Promise<number | null> =>
    kupoCheckpointSlotOf(
      await json(`${kupoUrl}/health`, {
        headers: { accept: "application/json" },
      }),
    );
  let slotConfig: Promise<LE.SlotConfig> | undefined;
  const readSlotConfig = (): Promise<LE.SlotConfig> => {
    slotConfig ??= (async () => {
      if (input.slotConfig !== undefined) return input.slotConfig;
      if (input.network !== "Custom")
        return { ...LE.SLOT_CONFIG_NETWORK[input.network] };
      const genesis = await queryLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl,
        fetchImpl,
        timeoutMs,
      });
      return {
        zeroTime: genesis.startTimeMs,
        zeroSlot: 0,
        slotLength: genesis.slotLengthMs,
      };
    })().catch((error: unknown) => {
      slotConfig = undefined;
      throw error;
    });
    return slotConfig;
  };
  return openL1Access({
    kind: "kupmios",
    provider,
    kupoUrl,
    ogmiosUrl,
    ledgerTip,
    endpoint: `kupo=${kupoUrl},ogmios=${ogmiosUrl}`,
    slotConfig: readSlotConfig,
    tipSlot: async () => (await ledgerTip()).slot,
    viewPoint: ledgerTip,
    synchronizedViewPoint: async () => {
      const deadline =
        Date.now() + (input.syncTimeoutMs ?? DEFAULT_SYNC_TIMEOUT_MS);
      const tip = await ledgerTip();
      for (;;) {
        const checkpoint = await kupoCheckpointSlot();
        if (checkpoint !== null && checkpoint >= tip.slot) return tip;
        if (Date.now() >= deadline)
          throw new L1ExternalBehindError(
            `Kupo's most recent checkpoint ${String(checkpoint)} has not reached the Ogmios tip ${tip.slot.toString()}`,
          );
        await new Promise((resolve) => setTimeout(resolve, SYNC_POLL_MS));
      }
    },
    submitSlotSnapshot: () =>
      queryLocalOgmiosSubmitSlotSnapshot({ ogmiosUrl, fetchImpl, timeoutMs }),
    close: async () => undefined,
  });
};
