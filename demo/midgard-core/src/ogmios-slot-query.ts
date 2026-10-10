/**
 * The local Ogmios slot evidence (tools only): a submit-slot snapshot from
 * `/health` and `queryNetwork/tip`, and the Shelley genesis epoch from
 * `queryNetwork/genesisConfiguration`. Role processes read L1 through the
 * follower and never import this module (the role boundary test pins it).
 */
import {
  DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS,
  SUBMIT_SLOT_LENGTH_MS,
} from "./ogmios-slot.js";
import {
  assertNoOgmiosJsonRpcError,
  type FetchLike,
  fetchTextWithTimeout,
  joinUrl,
  normalizeOgmiosHttpUrl,
  type OgmiosHealthEvidence,
  ogmiosHealthTipFieldsNotYetKnown,
  OgmiosSlotEvidenceUnavailableError,
  parseJson,
  parseOgmiosHealthEvidence,
  parseOgmiosShelleyGenesisSlotConfig,
  parseOgmiosTipSlot,
  type ShelleyGenesisSlotEvidence,
  type SubmitSlotSnapshot,
} from "./ogmios-slot.parse-ogmios-evidence.js";

export type LocalOgmiosSubmitSlotOptions = {
  readonly ogmiosUrl: string;
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
  readonly nowMs?: number;
  readonly maxHealthAgeMs?: number;
  readonly signal?: AbortSignal;
};

export type LocalOgmiosShelleyGenesisSlotOptions = Pick<
  LocalOgmiosSubmitSlotOptions,
  "ogmiosUrl" | "fetchImpl" | "timeoutMs" | "signal"
>;

const assertHealthyOgmios = (
  health: OgmiosHealthEvidence,
  notYetKnown: ReturnType<typeof ogmiosHealthTipFieldsNotYetKnown>,
  nowMs: number,
  maxHealthAgeMs: number,
): void => {
  // A field Ogmios reports as null before its first block is absent evidence
  // to wait for; a present field that does not parse is a malformed answer.
  const missing = (
    field: "networkSynchronization" | "lastKnownTip" | "lastTipUpdate",
    message: string,
  ): Error =>
    notYetKnown.has(field)
      ? new OgmiosSlotEvidenceUnavailableError("ogmios_no_tip", message)
      : new Error(message);
  if (health.connectionStatus === undefined) {
    throw new Error("Ogmios health response is missing connectionStatus");
  }
  if (health.connectionStatus.toLowerCase() !== "connected") {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_not_connected",
      `Ogmios is not connected: ${health.connectionStatus}`,
    );
  }
  if (health.networkSynchronization === undefined) {
    throw missing(
      "networkSynchronization",
      "Ogmios health response is missing networkSynchronization",
    );
  }
  if (health.networkSynchronization < 0.99) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_not_synchronized",
      `Ogmios is not sufficiently synchronized: ${health.networkSynchronization.toString()}`,
    );
  }
  if (health.lastKnownTipSlot === undefined) {
    throw missing(
      "lastKnownTip",
      "Ogmios health response is missing lastKnownTip.slot",
    );
  }
  if (health.lastTipUpdate === undefined) {
    throw missing(
      "lastTipUpdate",
      "Ogmios health response is missing lastTipUpdate",
    );
  }
  const lastTipUpdateMs = Date.parse(health.lastTipUpdate);
  if (Number.isNaN(lastTipUpdateMs)) {
    throw new Error(
      `Ogmios lastTipUpdate is not parseable: ${health.lastTipUpdate}`,
    );
  }
  if (nowMs - lastTipUpdateMs > maxHealthAgeMs) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_tip_stale",
      `Ogmios lastTipUpdate is stale: ageMs=${(nowMs - lastTipUpdateMs).toString()},maxAgeMs=${maxHealthAgeMs.toString()}`,
    );
  }
};

const deriveLiveSlotFromOgmiosHealth = (
  health: OgmiosHealthEvidence,
  nowMs: number,
): number => {
  if (health.lastKnownTipSlot === undefined) {
    throw new Error("Ogmios health response is missing lastKnownTip.slot");
  }
  if (health.lastTipUpdate === undefined) {
    throw new Error("Ogmios health response is missing lastTipUpdate");
  }
  const lastTipUpdateMs = Date.parse(health.lastTipUpdate);
  if (Number.isNaN(lastTipUpdateMs)) {
    throw new Error(
      `Ogmios lastTipUpdate is not parseable: ${health.lastTipUpdate}`,
    );
  }
  const elapsedSlots = Math.max(
    0,
    Math.floor((nowMs - lastTipUpdateMs) / SUBMIT_SLOT_LENGTH_MS),
  );
  return health.lastKnownTipSlot + elapsedSlots;
};

export const queryLocalOgmiosSubmitSlotSnapshot = async ({
  ogmiosUrl,
  fetchImpl = fetch,
  timeoutMs = 5_000,
  nowMs = Date.now(),
  maxHealthAgeMs = DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS,
  signal,
}: LocalOgmiosSubmitSlotOptions): Promise<SubmitSlotSnapshot> => {
  const baseUrl = normalizeOgmiosHttpUrl(ogmiosUrl);
  const healthBody = await fetchTextWithTimeout(
    fetchImpl,
    joinUrl(baseUrl, "/health"),
    { signal },
    timeoutMs,
  );
  const healthPayload = parseJson(healthBody, "Ogmios health");
  const health = parseOgmiosHealthEvidence(healthPayload);
  assertHealthyOgmios(
    health,
    ogmiosHealthTipFieldsNotYetKnown(healthPayload),
    nowMs,
    maxHealthAgeMs,
  );

  const tipBody = await fetchTextWithTimeout(
    fetchImpl,
    baseUrl,
    {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryNetwork/tip",
        id: "midgard-submit-slot",
      }),
      signal,
    },
    timeoutMs,
  );
  const tipPayload = parseJson(tipBody, "Ogmios tip");
  assertNoOgmiosJsonRpcError(tipPayload, "Ogmios queryNetwork/tip");
  const queriedTipSlot = parseOgmiosTipSlot(tipPayload);
  const derivedLiveSlot = deriveLiveSlotFromOgmiosHealth(health, nowMs);
  return {
    source: "local_ogmios_tip",
    currentSlot: Math.max(queriedTipSlot, derivedLiveSlot),
    ledgerTipSlot: queriedTipSlot,
    observedAtMs: nowMs,
    slotLengthMs: SUBMIT_SLOT_LENGTH_MS,
    health,
  };
};

export const queryLocalOgmiosShelleyGenesisSlotConfig = async ({
  ogmiosUrl,
  fetchImpl = fetch,
  timeoutMs = 5_000,
  signal,
}: LocalOgmiosShelleyGenesisSlotOptions): Promise<ShelleyGenesisSlotEvidence> => {
  const baseUrl = normalizeOgmiosHttpUrl(ogmiosUrl);
  const body = await fetchTextWithTimeout(
    fetchImpl,
    baseUrl,
    {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryNetwork/genesisConfiguration",
        params: { era: "shelley" },
        id: "midgard-custom-slot-config",
      }),
      signal,
    },
    timeoutMs,
  );
  const payload = parseJson(body, "Ogmios Shelley genesis");
  assertNoOgmiosJsonRpcError(payload, "Ogmios Shelley genesis");
  return parseOgmiosShelleyGenesisSlotConfig(payload);
};
