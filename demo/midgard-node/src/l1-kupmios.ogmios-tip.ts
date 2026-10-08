import { normalizeOgmiosHttpUrl } from "@al-ft/midgard-core/ogmios-slot";

import {
  DEFAULT_L1_READ_TIMEOUT_MS,
  type FetchLike,
  HEX_32,
} from "./l1-kupmios.l1-chain-point.js";

/** A local Ogmios tip: its point and the block height bound to it. */
export type OgmiosTip = Readonly<{
  blockHash: string;
  slot: number;
  blockNo: number;
}>;

const TIP_READ_ATTEMPTS = 5;

const queryOgmios = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
  method: string,
): Promise<unknown> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method,
      params: {},
      id: "midgard-local-ogmios-tip-v1",
    }),
  });
  const body = (await response.json()) as { result?: unknown };
  if (!response.ok) throw new Error(`Ogmios ${method} query failed`);
  return body.result;
};

const fetchTipPoint = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
): Promise<Omit<OgmiosTip, "blockNo">> => {
  const point = (await queryOgmios(
    ogmiosUrl,
    fetchImpl,
    "queryNetwork/tip",
  )) as { id?: unknown; slot?: unknown } | undefined;
  if (
    typeof point?.id !== "string" ||
    !HEX_32.test(point.id) ||
    typeof point.slot !== "number" ||
    !Number.isSafeInteger(point.slot) ||
    point.slot < 0
  ) {
    throw new Error("Ogmios tip query returned no canonical point");
  }
  return { blockHash: point.id, slot: point.slot };
};

/** Ogmios v6 answers queryNetwork/tip with a point only; the height comes from
 * queryNetwork/blockHeight and is bound to the tip only when two tip reads
 * bracketing it agree. A chain that keeps moving fails after a few tries. */
const fetchTip = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
): Promise<OgmiosTip> => {
  for (let attempt = 0; attempt < TIP_READ_ATTEMPTS; attempt += 1) {
    const before = await fetchTipPoint(ogmiosUrl, fetchImpl);
    const height = await queryOgmios(
      ogmiosUrl,
      fetchImpl,
      "queryNetwork/blockHeight",
    );
    if (
      typeof height !== "number" ||
      !Number.isSafeInteger(height) ||
      height < 0
    ) {
      throw new Error("Ogmios block height query returned no block height");
    }
    const after = await fetchTipPoint(ogmiosUrl, fetchImpl);
    if (before.blockHash === after.blockHash && before.slot === after.slot) {
      return { ...before, blockNo: height };
    }
  }
  throw new Error(
    `Ogmios tip moved during each of ${TIP_READ_ATTEMPTS.toString()} block height reads`,
  );
};

/** A hung Ogmios fails the read after `timeoutMs` instead of wedging the
 * caller that awaits it; a caller's own signal still applies. */
const withRequestTimeout =
  (fetchImpl: FetchLike, timeoutMs: number): FetchLike =>
  (url, init) => {
    const timeout = AbortSignal.timeout(timeoutMs);
    return fetchImpl(url, {
      ...init,
      signal:
        init?.signal === undefined || init.signal === null
          ? timeout
          : AbortSignal.any([init.signal, timeout]),
    });
  };

/** The local Ogmios tip with its height, each read bounded like the other
 * point reads here (`DEFAULT_L1_READ_TIMEOUT_MS` unless `timeoutMs`). */
export const readLocalOgmiosTip = (
  ogmiosUrl: string,
  options: Readonly<{ fetchImpl?: FetchLike; timeoutMs?: number }> = {},
): Promise<OgmiosTip> =>
  fetchTip(
    ogmiosUrl,
    withRequestTimeout(
      options.fetchImpl ?? fetch,
      options.timeoutMs ?? DEFAULT_L1_READ_TIMEOUT_MS,
    ),
  );
