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
};

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

const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/, "")}/${path.replace(/^\/+/, "")}`;

export const ogmiosHttpUrl = (url: string, path = ""): string =>
  path === ""
    ? normalizeOgmiosHttpUrl(url)
    : joinUrl(normalizeOgmiosHttpUrl(url), path);

const fetchTextWithTimeout = async (
  fetchImpl: FetchLike,
  url: string,
  init: RequestInit,
  timeoutMs: number,
): Promise<string> => {
  const controller = new AbortController();
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
}: LocalOgmiosSubmitSlotOptions): Promise<SubmitSlotSnapshot> => {
  const baseUrl = normalizeOgmiosHttpUrl(ogmiosUrl);
  const healthBody = await fetchTextWithTimeout(
    fetchImpl,
    joinUrl(baseUrl, "/health"),
    {},
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
    },
    timeoutMs,
  );
  const queriedTipSlot = parseOgmiosTipSlot(
    parseJson(tipBody, "Ogmios tip"),
  );
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
    try: () => queryLocalOgmiosSubmitSlotSnapshot(options),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios submit slot", { cause }),
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
