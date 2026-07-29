import { createHash } from "node:crypto";

import { Effect } from "effect";

export const SUBMIT_SLOT_LENGTH_MS = 1_000;
export const SUBMIT_SLOT_VALIDITY_BUFFER = 2;

const DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS = 120_000;

export type SubmitSlotSnapshot = {
  readonly source: "local_ogmios_tip" | "emulator" | "test";
  readonly currentSlot: number;
  readonly observedAtMs: number;
  readonly slotLengthMs: number;
  readonly health?: {
    readonly connectionStatus?: string;
    readonly networkSynchronization?: number;
    readonly lastKnownTipSlot?: number;
    readonly lastTipUpdate?: string;
  };
};

export type LocalOgmiosSubmitSlotOptions = {
  readonly ogmiosUrl: string;
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
  readonly nowMs?: number;
  readonly maxHealthAgeMs?: number;
  readonly signal?: AbortSignal;
};

export type ShelleyGenesisSlotConfig = {
  readonly startTimeMs: number;
  readonly slotLengthMs: number;
};

export type ShelleyGenesisSlotEvidence = ShelleyGenesisSlotConfig & {
  readonly configurationSha256: string;
};

export type LocalOgmiosShelleyGenesisSlotOptions = Pick<
  LocalOgmiosSubmitSlotOptions,
  "ogmiosUrl" | "fetchImpl" | "timeoutMs" | "signal"
>;

type FetchLike = (input: string, init?: RequestInit) => Promise<Response>;

type OgmiosHealthEvidence = NonNullable<SubmitSlotSnapshot["health"]>;

const numberFromUnknown = (value: unknown): number | null => {
  if (typeof value === "number") {
    return Number.isSafeInteger(value) && value >= 0 ? value : null;
  }
  if (typeof value === "bigint") {
    return value >= 0n && value <= BigInt(Number.MAX_SAFE_INTEGER)
      ? Number(value)
      : null;
  }
  if (typeof value === "string" && /^\d+$/.test(value)) {
    const parsed = Number(value);
    return Number.isSafeInteger(parsed) ? parsed : null;
  }
  return null;
};

const synchronizationFromUnknown = (value: unknown): number | undefined => {
  if (typeof value === "number" && Number.isFinite(value)) {
    return value > 1 ? value / 100 : value;
  }
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  if (trimmed.endsWith("%")) {
    const parsed = Number(trimmed.slice(0, -1));
    return Number.isFinite(parsed) ? parsed / 100 : undefined;
  }
  const parsed = Number(trimmed);
  return Number.isFinite(parsed)
    ? parsed > 1
      ? parsed / 100
      : parsed
    : undefined;
};

const record = (value: unknown): Record<string, unknown> | null =>
  typeof value === "object" && value !== null
    ? (value as Record<string, unknown>)
    : null;

const canonicalJsonValue = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map(canonicalJsonValue);
  }
  const object = record(value);
  if (object !== null) {
    return Object.fromEntries(
      Object.keys(object)
        .sort()
        .map((key) => [key, canonicalJsonValue(object[key])]),
    );
  }
  return value;
};

export const normalizeOgmiosHttpUrl = (url: string): string => {
  const parsed = new URL(url.trim());
  if (parsed.protocol === "ws:") {
    parsed.protocol = "http:";
  } else if (parsed.protocol === "wss:") {
    parsed.protocol = "https:";
  }
  parsed.hash = "";
  return parsed.toString().replace(/\/$/, "");
};

export const ogmiosEndpointIdentitySha256 = (url: string): string =>
  createHash("sha256").update(normalizeOgmiosHttpUrl(url)).digest("hex");

const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/, "")}/${path.replace(/^\/+/, "")}`;

const fetchTextWithTimeout = async (
  fetchImpl: FetchLike,
  url: string,
  init: RequestInit,
  timeoutMs: number,
): Promise<string> => {
  const controller = new AbortController();
  const upstreamSignal = init.signal;
  const abortFromUpstream = () => controller.abort(upstreamSignal?.reason);
  if (upstreamSignal?.aborted === true) {
    abortFromUpstream();
  } else {
    upstreamSignal?.addEventListener("abort", abortFromUpstream, {
      once: true,
    });
  }
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetchImpl(url, {
      ...init,
      signal: controller.signal,
    });
    const body = await response.text();
    if (!response.ok) {
      throw new Error(
        `HTTP ${response.status.toString()} from ${url}: ${body}`,
      );
    }
    return body;
  } finally {
    clearTimeout(timeout);
    upstreamSignal?.removeEventListener("abort", abortFromUpstream);
  }
};

const parseJson = (body: string, label: string): unknown => {
  try {
    return JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error(`Failed to parse ${label} JSON`, { cause });
  }
};

const firstSlot = (...values: readonly unknown[]): number | null => {
  for (const value of values) {
    const slot = numberFromUnknown(value);
    if (slot !== null) {
      return slot;
    }
  }
  return null;
};

export const parseOgmiosTipSlot = (payload: unknown): number => {
  const root = record(payload);
  const result = record(root?.result);
  const point = record(result?.point);
  const tip = record(result?.tip);
  const slot = firstSlot(result?.slot, point?.slot, tip?.slot);
  if (slot === null) {
    throw new Error("Ogmios queryNetwork/tip response did not include a slot");
  }
  return slot;
};

export const parseOgmiosShelleyGenesisSlotConfig = (
  payload: unknown,
): ShelleyGenesisSlotEvidence => {
  const root = record(payload);
  const result = record(root?.result);
  const startTime = result?.startTime;
  if (typeof startTime !== "string" || startTime.trim().length === 0) {
    throw new Error(
      "Ogmios Shelley genesis response did not include a startTime",
    );
  }
  const startTimeMs = Date.parse(startTime);
  if (
    !Number.isSafeInteger(startTimeMs) ||
    startTimeMs < 0 ||
    !/(?:Z|[+-]\d{2}:\d{2})$/i.test(startTime)
  ) {
    throw new Error(
      `Ogmios Shelley genesis startTime is invalid: ${startTime}`,
    );
  }

  const slotLength = record(result?.slotLength);
  const slotLengthMs = numberFromUnknown(slotLength?.milliseconds);
  if (slotLengthMs === null || slotLengthMs <= 0) {
    throw new Error(
      "Ogmios Shelley genesis response did not include a positive integer slotLength.milliseconds",
    );
  }
  return {
    startTimeMs,
    slotLengthMs,
    configurationSha256: createHash("sha256")
      .update(JSON.stringify(canonicalJsonValue(result)))
      .digest("hex"),
  };
};

export const parseOgmiosHealthEvidence = (
  payload: unknown,
): OgmiosHealthEvidence => {
  const root = record(payload);
  const lastKnownTip = record(root?.lastKnownTip);
  return {
    ...(typeof root?.connectionStatus === "string"
      ? { connectionStatus: root.connectionStatus }
      : {}),
    ...(synchronizationFromUnknown(root?.networkSynchronization) === undefined
      ? {}
      : {
          networkSynchronization: synchronizationFromUnknown(
            root?.networkSynchronization,
          )!,
        }),
    ...(numberFromUnknown(lastKnownTip?.slot) === null
      ? {}
      : { lastKnownTipSlot: numberFromUnknown(lastKnownTip?.slot)! }),
    ...(typeof root?.lastTipUpdate === "string"
      ? { lastTipUpdate: root.lastTipUpdate }
      : {}),
  };
};

const assertHealthyOgmios = (
  health: OgmiosHealthEvidence,
  nowMs: number,
  maxHealthAgeMs: number,
): void => {
  if (health.connectionStatus === undefined) {
    throw new Error("Ogmios health response is missing connectionStatus");
  }
  if (health.connectionStatus.toLowerCase() !== "connected") {
    throw new Error(`Ogmios is not connected: ${health.connectionStatus}`);
  }
  if (health.networkSynchronization === undefined) {
    throw new Error("Ogmios health response is missing networkSynchronization");
  }
  if (health.networkSynchronization < 0.99) {
    throw new Error(
      `Ogmios is not sufficiently synchronized: ${health.networkSynchronization.toString()}`,
    );
  }
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
  if (nowMs - lastTipUpdateMs > maxHealthAgeMs) {
    throw new Error(
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
  const health = parseOgmiosHealthEvidence(
    parseJson(healthBody, "Ogmios health"),
  );
  assertHealthyOgmios(health, nowMs, maxHealthAgeMs);

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
  const queriedTipSlot = parseOgmiosTipSlot(parseJson(tipBody, "Ogmios tip"));
  const derivedLiveSlot = deriveLiveSlotFromOgmiosHealth(health, nowMs);
  return {
    source: "local_ogmios_tip",
    currentSlot: Math.max(queriedTipSlot, derivedLiveSlot),
    observedAtMs: nowMs,
    slotLengthMs: SUBMIT_SLOT_LENGTH_MS,
    health,
  };
};

export const fetchLocalOgmiosSubmitSlotSnapshot = (
  options: LocalOgmiosSubmitSlotOptions,
): Effect.Effect<SubmitSlotSnapshot, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosSubmitSlotSnapshot({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios submit slot", { cause }),
  });

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
  return parseOgmiosShelleyGenesisSlotConfig(
    parseJson(body, "Ogmios Shelley genesis"),
  );
};

export const fetchLocalOgmiosShelleyGenesisSlotConfig = (
  options: LocalOgmiosShelleyGenesisSlotOptions,
): Effect.Effect<ShelleyGenesisSlotEvidence, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios Shelley genesis", {
            cause,
          }),
  });

export const makeLocalOgmiosSubmitSlotSnapshotProvider = (
  options: Omit<LocalOgmiosSubmitSlotOptions, "nowMs">,
): (() => Effect.Effect<SubmitSlotSnapshot, unknown>) => {
  return () =>
    fetchLocalOgmiosSubmitSlotSnapshot({ ...options, nowMs: Date.now() });
};

export const localOgmiosSubmitSlotEvidence = (
  snapshot: SubmitSlotSnapshot,
): string => {
  const health = snapshot.health;
  return [
    `submitSlot=${snapshot.currentSlot.toString()}`,
    `slotSource=${snapshot.source}`,
    `observedAtMs=${snapshot.observedAtMs.toString()}`,
    ...(health?.connectionStatus === undefined
      ? []
      : [`connectionStatus=${health.connectionStatus}`]),
    ...(health?.networkSynchronization === undefined
      ? []
      : [`networkSynchronization=${health.networkSynchronization.toString()}`]),
    ...(health?.lastKnownTipSlot === undefined
      ? []
      : [`lastKnownTipSlot=${health.lastKnownTipSlot.toString()}`]),
    ...(health?.lastTipUpdate === undefined
      ? []
      : [`lastTipUpdate=${health.lastTipUpdate}`]),
  ].join(",");
};
