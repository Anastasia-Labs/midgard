import {
  abortSignalAborted,
  digest,
  exactKeys,
  naturalNumber,
  throwIfSourceAborted,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  type FraudProofRawL1WebSocketFactory,
  type FraudProofRawL1WebSocketLike,
  LocalKupmiosTransportUnavailableError,
  type OgmiosTip,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";

export const parseOgmiosTip = (value: unknown, label: string): OgmiosTip => {
  const result = exactKeys(value, ["slot", "id", "height"], [], label);
  return {
    slot: naturalNumber(result.slot, `${label}.result.slot`),
    blockHash: digest(result.id, `${label}.result.id`),
    blockNo: naturalNumber(result.height, `${label}.result.height`),
  };
};

export type OgmiosSession = {
  request(
    method: string,
    params: Readonly<Record<string, unknown>>,
  ): Promise<unknown>;
  close(): Promise<void>;
};

export const defaultWebSocketFactory: FraudProofRawL1WebSocketFactory = (url) =>
  new WebSocket(url) as unknown as FraudProofRawL1WebSocketLike;

// Each Ogmios WebSocket opens node-to-client connections. Bound physical
// sessions across captures/sources; closing sockets still consume their permit.
const MAXIMUM_OGMIOS_SESSIONS = 4;

export const ogmiosSessionBudgets = new Map<
  string,
  { active: number; waiting: Set<() => void> }
>();

export const acquireOgmiosSession = async (
  url: string,
  timeoutMs: number,
  signal: AbortSignal | undefined,
): Promise<() => void> => {
  throwIfSourceAborted(signal);
  const endpoint = new URL(url).origin;
  let budget = ogmiosSessionBudgets.get(endpoint);
  if (budget === undefined) {
    budget = { active: 0, waiting: new Set() };
    ogmiosSessionBudgets.set(endpoint, budget);
  }
  const selected = budget;
  await new Promise<void>((resolve, reject) => {
    const cleanup = (): void => {
      clearTimeout(timer);
      signal?.removeEventListener("abort", onAbort);
      selected.waiting.delete(admit);
    };
    const fail = (error: Error): void => {
      cleanup();
      reject(error);
    };
    const onAbort = (): void =>
      fail(new DOMException("local Kupmios raw source aborted", "AbortError"));
    const admit = (): void => {
      cleanup();
      selected.active += 1;
      resolve();
    };
    const timer = setTimeout(
      () =>
        fail(
          new LocalKupmiosTransportUnavailableError(
            "Ogmios session capacity wait timed out",
          ),
        ),
      timeoutMs,
    );
    signal?.addEventListener("abort", onAbort, { once: true });
    if (signal !== undefined && abortSignalAborted.call(signal)) onAbort();
    else if (selected.active < MAXIMUM_OGMIOS_SESSIONS) admit();
    else selected.waiting.add(admit);
  });
  let released = false;
  return () => {
    if (released) return;
    released = true;
    selected.active -= 1;
    selected.waiting.values().next().value?.();
    if (selected.active === 0 && selected.waiting.size === 0)
      ogmiosSessionBudgets.delete(endpoint);
  };
};
