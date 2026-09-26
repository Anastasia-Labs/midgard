import type { Assets } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";

import {
  normalizeOgmiosWebSocketUrl,
  openOgmiosSession,
  type WebSocketFactory,
  type WebSocketLike,
} from "./l1-tx-order-carriage.js";

export type LedgerSnapshotPoint = Readonly<{ slot: number; id: string }>;

/** History readers need the presence of a reference script, never its bytes.
 * Keeping this distinct from Lucid's UTxO avoids inventing a script witness. */
export type LedgerSnapshotOutput = Readonly<{
  txHash: string;
  outputIndex: number;
  address: string;
  assets: Readonly<Assets>;
  datum?: string;
  datumHash?: string;
  hasReferenceScript: boolean;
}>;

/** Coherent ledger capture, NOT current canonical eligibility. An acquired
 * state may survive a later rollback. The node's generation owner must fence
 * its consumers and revalidate the capture before publishing readiness. */
export type AcquiredLedgerSnapshot = Readonly<{
  point: LedgerSnapshotPoint;
  addresses: readonly string[];
  outputs: readonly LedgerSnapshotOutput[];
}>;

const losslessJson = JSONBig({ useNativeBigInt: true, strict: true });
const hex = /^(?:[0-9a-f]{2})*$/u;
const record = (value: unknown, label: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error(`${label} must be an object`);
  return value as Record<string, unknown>;
};
const bytes = (value: unknown, label: string, size?: number): string => {
  if (
    typeof value !== "string" ||
    !hex.test(value) ||
    (size !== undefined && value.length !== size * 2)
  )
    throw new Error(
      `${label} must be lowercase base16${size === undefined ? "" : ` (${size} bytes)`}`,
    );
  return value;
};
const natural = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error(`${label} must be a safe natural number`);
  return value;
};
const quantity = (value: unknown): bigint => {
  const parsed =
    typeof value === "bigint"
      ? value
      : BigInt(natural(value, "asset quantity"));
  if (parsed < 0n) throw new Error("UTxO asset quantity must be nonnegative");
  return parsed;
};
const point = (value: unknown): LedgerSnapshotPoint => {
  const parsed = record(value, "Ogmios ledger point");
  return Object.freeze({
    slot: natural(parsed.slot, "Ogmios ledger slot"),
    id: bytes(parsed.id, "Ogmios ledger hash", 32),
  });
};
const samePoint = (left: LedgerSnapshotPoint, right: LedgerSnapshotPoint) =>
  left.slot === right.slot && left.id === right.id;

export const decodeLedgerSnapshotOutput = (
  value: unknown,
  addresses: ReadonlySet<string>,
): LedgerSnapshotOutput => {
  const parsed = record(value, "Ogmios UTxO");
  if (typeof parsed.address !== "string" || !addresses.has(parsed.address))
    throw new Error("Ogmios UTxO lies outside the requested addresses");
  const valueRecord = record(parsed.value, "Ogmios UTxO Value");
  const ada = record(valueRecord.ada, "Ogmios UTxO ADA");
  const assets: Assets = { lovelace: quantity(ada.lovelace) };
  for (const [policy, rawTokens] of Object.entries(valueRecord)) {
    if (policy === "ada") continue;
    bytes(policy, "Ogmios asset policy", 28);
    for (const [name, amount] of Object.entries(
      record(rawTokens, "Ogmios asset map"),
    )) {
      bytes(name, "Ogmios asset name");
      if (name.length > 64)
        throw new Error("Ogmios asset name exceeds 32 bytes");
      assets[policy + name] = quantity(amount);
    }
  }
  if (parsed.datum !== undefined && parsed.datumHash !== undefined)
    throw new Error("Ogmios UTxO has both inline datum and datum hash");
  return Object.freeze({
    txHash: bytes(
      record(parsed.transaction, "Ogmios transaction").id,
      "Ogmios transaction id",
      32,
    ),
    outputIndex: natural(parsed.index, "Ogmios output index"),
    address: parsed.address,
    assets: Object.freeze(assets),
    ...(parsed.datum === undefined
      ? {}
      : { datum: bytes(parsed.datum, "Ogmios inline datum") }),
    ...(parsed.datumHash === undefined
      ? {}
      : { datumHash: bytes(parsed.datumHash, "Ogmios datum hash", 32) }),
    hasReferenceScript: parsed.script !== undefined,
  });
};

/** Ogmios has no address index, so every address-scope ledger query walks the
 * whole UTxO set, and concurrent scans by other clients of the same node
 * slow each other down. The first-start image and the exact-point recovery
 * captures get this scan deadline instead of the per-request ChainSync
 * timeout. The lease keeps renewing meanwhile: the startup health loop on
 * first start, the follower's heartbeats during recovery. */
export const LEDGER_SCAN_TIMEOUT_MS = 15 * 60_000;

/** One acquired Ogmios ledger state covers all requested list, retention and
 * deployment addresses. No Kupo lookup or unpinned datum fetch is mixed in.
 * See https://ogmios.dev/mini-protocols/local-state-query/ . */
export const readAcquiredLedgerSnapshot = async ({
  ogmiosUrl,
  addresses,
  at,
  timeoutMs = 20_000,
  signal,
  verifySession,
  webSocketFactory = (url) => new WebSocket(url) as unknown as WebSocketLike,
}: {
  readonly ogmiosUrl: string;
  readonly addresses: readonly string[];
  /** Acquire this exact retained point or fail; never fall back to the tip.
   * Successful acquisition alone does not establish canonical ancestry. */
  readonly at?: LedgerSnapshotPoint;
  readonly timeoutMs?: number;
  readonly signal?: AbortSignal;
  /** Source-bound callers verify this exact socket before any ledger query. */
  readonly verifySession?: (
    session: Pick<Awaited<ReturnType<typeof openOgmiosSession>>, "request">,
  ) => Promise<void>;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<AcquiredLedgerSnapshot> => {
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0)
    throw new Error("Ledger snapshot timeout must be a positive safe integer");
  if (
    addresses.length === 0 ||
    addresses.some((address) => address.length === 0)
  )
    throw new Error("Ledger snapshot requires nonempty addresses");
  const selectedPoint = at === undefined ? undefined : point(at);
  signal?.throwIfAborted();
  const deadline = AbortSignal.timeout(timeoutMs);
  const captureSignal =
    signal === undefined ? deadline : AbortSignal.any([signal, deadline]);
  const requested = Object.freeze([...new Set(addresses)].sort());
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(ogmiosUrl),
    timeoutMs,
    webSocketFactory,
    signal: captureSignal,
    parseMessage: (text) => losslessJson.parse(text) as unknown,
  });
  try {
    await verifySession?.(session);
    captureSignal.throwIfAborted();
    const requestedPoint =
      selectedPoint ?? point(await session.request("queryLedgerState/tip", {}));
    const acquired = record(
      await session.request("acquireLedgerState", { point: requestedPoint }),
      "Ogmios acquisition",
    );
    if (
      acquired.acquired !== "ledgerState" ||
      !samePoint(point(acquired.point), requestedPoint)
    )
      throw new Error("Ogmios acquired a different ledger point");
    const rawOutputs = await session.request("queryLedgerState/utxo", {
      addresses: requested,
    });
    if (!Array.isArray(rawOutputs))
      throw new Error("Ogmios UTxO response must be a complete array");
    const addressSet = new Set(requested);
    const outputs = rawOutputs.map((entry) =>
      decodeLedgerSnapshotOutput(entry, addressSet),
    );
    if (
      new Set(outputs.map((entry) => `${entry.txHash}#${entry.outputIndex}`))
        .size !== outputs.length
    )
      throw new Error("Ogmios ledger snapshot repeats an output reference");
    // This query is deliberately still acquired. It checks the state queried,
    // not whether this point remains on the current selected branch.
    if (
      !samePoint(
        point(await session.request("queryLedgerState/tip", {})),
        requestedPoint,
      )
    )
      throw new Error("Ogmios ledger point changed during acquired capture");
    const released = record(
      await session.request("releaseLedgerState", {}),
      "Ogmios release",
    );
    if (released.released !== "ledgerState")
      throw new Error("Ogmios did not release ledger state");
    captureSignal.throwIfAborted();
    return Object.freeze({
      point: requestedPoint,
      addresses: requested,
      outputs: Object.freeze(outputs),
    });
  } finally {
    // Closing also releases any held state on malformed/error/expired queries.
    session.close();
  }
};
