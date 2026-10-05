/**
 * The local Ogmios slot evidence and the Custom-network Lucid slot mapping
 * derived from it: a submit-slot snapshot from `/health` and
 * `queryNetwork/tip`, the Shelley genesis epoch from
 * `queryNetwork/genesisConfiguration`, and the check that ties them together.
 * midgard-node and da-committee-node both build their Custom Lucid clients
 * from this one implementation.
 */
import type { SlotConfig } from "@lucid-evolution/lucid";

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
  type ShelleyGenesisSlotConfig,
  type ShelleyGenesisSlotEvidence,
  type SubmitSlotSnapshot,
} from "./ogmios-slot.parse-ogmios-evidence.js";

export {
  assertNoOgmiosJsonRpcError,
  type FetchLike,
  normalizeOgmiosHttpUrl,
  ogmiosSlotEvidenceUnavailableCause,
  OgmiosSlotEvidenceUnavailableError,
  type OgmiosSlotEvidenceUnavailableReason,
  parseOgmiosHealthEvidence,
  parseOgmiosShelleyGenesisSlotConfig,
  parseOgmiosTipSlot,
  type ShelleyGenesisSlotConfig,
  type ShelleyGenesisSlotEvidence,
  type SubmitSlotSnapshot,
} from "./ogmios-slot.parse-ogmios-evidence.js";

export const SUBMIT_SLOT_LENGTH_MS = 1_000;
export const SUBMIT_SLOT_VALIDITY_BUFFER = 2;

/**
 * The tip-age bound for callers that do not derive one from genesis. The
 * node and the committee derive theirs with
 * {@link ogmiosTipMaxAgeMsFromShelleyGenesis}.
 */
export const DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS = 120_000;

/** Expected block intervals a healthy tip may go without an update. */
export const DEFAULT_OGMIOS_TIP_MAX_AGE_BLOCK_INTERVALS = 10;

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

export type CustomSlotConfig = SlotConfig;

const assertValidSubmitSlotSnapshot = ({
  currentSlot,
  observedAtMs,
  slotLengthMs,
}: Pick<
  SubmitSlotSnapshot,
  "currentSlot" | "observedAtMs" | "slotLengthMs"
>) => {
  if (!Number.isSafeInteger(currentSlot) || currentSlot < 0) {
    throw new Error(
      `Invalid Custom slot snapshot currentSlot=${String(currentSlot)}`,
    );
  }
  if (!Number.isSafeInteger(observedAtMs) || observedAtMs < 0) {
    throw new Error(
      `Invalid Custom slot snapshot observedAtMs=${String(observedAtMs)}`,
    );
  }
  if (!Number.isSafeInteger(slotLengthMs) || slotLengthMs <= 0) {
    throw new Error(
      `Invalid Custom slot snapshot slotLengthMs=${String(slotLengthMs)}`,
    );
  }
};

const assertValidShelleyGenesis = (genesis: ShelleyGenesisSlotConfig) => {
  if (!Number.isSafeInteger(genesis.startTimeMs) || genesis.startTimeMs < 0) {
    throw new Error(
      `Invalid Shelley genesis startTimeMs=${String(genesis.startTimeMs)}`,
    );
  }
  if (
    !Number.isSafeInteger(genesis.slotLengthMs) ||
    genesis.slotLengthMs <= 0
  ) {
    throw new Error(
      `Invalid Shelley genesis slotLengthMs=${String(genesis.slotLengthMs)}`,
    );
  }
};

/**
 * The longest a healthy local Ogmios tip may go without an update: a number
 * of expected block intervals, each `slotLength / f` for the genesis
 * active-slot coefficient `f` (10 intervals of 20 s on a devnet with
 * `f = 1/20` and one-second slots is 200 s). A genesis without `f` keeps
 * {@link DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS}.
 */
export const ogmiosTipMaxAgeMsFromShelleyGenesis = (
  genesis: ShelleyGenesisSlotConfig,
  blockIntervals: number = DEFAULT_OGMIOS_TIP_MAX_AGE_BLOCK_INTERVALS,
): number => {
  assertValidShelleyGenesis(genesis);
  if (!Number.isFinite(blockIntervals) || blockIntervals <= 0) {
    throw new Error(
      `Invalid Ogmios tip-age block interval count=${String(blockIntervals)}`,
    );
  }
  const coefficient = genesis.activeSlotsCoefficient;
  if (coefficient === undefined) {
    return DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS;
  }
  return Math.ceil((blockIntervals * genesis.slotLengthMs) / coefficient);
};

/**
 * Derives Lucid's Custom slot mapping from the Shelley genesis alone. The
 * mapping is a pure function of the genesis epoch, so no tip evidence enters
 * it; the wall clock only confirms the epoch has begun (a transient
 * `genesis_not_started` until it has) and the slot length is the one the
 * submit-slot arithmetic assumes (terminal otherwise).
 */
export const customSlotConfigFromShelleyGenesisAtWallClock = (
  genesis: ShelleyGenesisSlotConfig,
  {
    nowMs,
    expectedSlotLengthMs = SUBMIT_SLOT_LENGTH_MS,
  }: { readonly nowMs: number; readonly expectedSlotLengthMs?: number },
): CustomSlotConfig => {
  assertValidShelleyGenesis(genesis);
  if (genesis.slotLengthMs !== expectedSlotLengthMs) {
    throw new Error(
      `Custom slot length disagreement: expected=${expectedSlotLengthMs.toString()},genesis=${genesis.slotLengthMs.toString()}`,
    );
  }
  if (nowMs < genesis.startTimeMs) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "genesis_not_started",
      `The Shelley genesis start time is in the future: startTimeMs=${genesis.startTimeMs.toString()},nowMs=${nowMs.toString()}`,
    );
  }
  return {
    zeroTime: genesis.startTimeMs,
    zeroSlot: 0,
    slotLength: genesis.slotLengthMs,
  };
};

/**
 * Derives Lucid's Custom slot mapping from the authoritative Shelley genesis
 * epoch. The submit-slot snapshot remains a required health and clock-domain
 * check, but its wall-clock observation never defines a slot boundary.
 */
export const customSlotConfigFromShelleyGenesis = (
  genesis: ShelleyGenesisSlotConfig,
  snapshot: Pick<
    SubmitSlotSnapshot,
    "currentSlot" | "observedAtMs" | "slotLengthMs"
  >,
): CustomSlotConfig => {
  assertValidSubmitSlotSnapshot(snapshot);
  assertValidShelleyGenesis(genesis);
  if (snapshot.slotLengthMs !== genesis.slotLengthMs) {
    throw new Error(
      `Custom slot length disagreement: snapshot=${snapshot.slotLengthMs.toString()},genesis=${genesis.slotLengthMs.toString()}`,
    );
  }
  if (snapshot.observedAtMs < genesis.startTimeMs) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "genesis_not_started",
      "Custom submit-slot observation precedes the Shelley genesis start time",
    );
  }
  const genesisSlotAtObservation = Math.floor(
    (snapshot.observedAtMs - genesis.startTimeMs) / genesis.slotLengthMs,
  );
  if (
    !Number.isSafeInteger(genesisSlotAtObservation) ||
    Math.abs(snapshot.currentSlot - genesisSlotAtObservation) >
      SUBMIT_SLOT_VALIDITY_BUFFER
  ) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_clock_disagreement",
      `Custom slot clock disagreement: snapshot=${snapshot.currentSlot.toString()},genesisAtObservation=${genesisSlotAtObservation.toString()}`,
    );
  }
  return {
    zeroTime: genesis.startTimeMs,
    zeroSlot: 0,
    slotLength: genesis.slotLengthMs,
  };
};
