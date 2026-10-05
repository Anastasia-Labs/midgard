import {
  formatOgmiosJsonRpcError,
  OgmiosJsonRpcError,
} from "@al-ft/midgard-core/ogmios-json-rpc-error";

import { LocalNodeChainAuthority } from "./provider.local-node-chain-authority.js";
import {
  type CanonicalChainPoint,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import { L1SourceIntegrityError } from "./source-integrity.js";

export const localAuthorityRegistry = new Map<
  string,
  LocalNodeChainAuthority
>();

/** Bound on one Kupo /health read, the same as the Ogmios network-magic
 * preflight's; a hung Kupo fails the read instead of wedging the caller. */
export const KUPO_HEALTH_TIMEOUT_MS = 10_000;

/** Bound on one Blockfrost read (the latest block or the genesis). */
export const BLOCKFROST_REQUEST_TIMEOUT_MS = 20_000;

/**
 * Kupo indexes each block shortly after the node adopts it, so one read of
 * both tips can straddle a block arrival. Re-read briefly until they agree;
 * surfaces that still disagree after the window are not following one chain.
 */
export const KUPMIOS_TIP_ALIGNMENT_ATTEMPTS = 8;

export const KUPMIOS_TIP_ALIGNMENT_RETRY_MS = 250;

type RuntimeWebSocket = {
  onopen: ((event: unknown) => void) | null;
  onmessage: ((event: { readonly data: unknown }) => void) | null;
  onerror: ((event: unknown) => void) | null;
  onclose: ((event: unknown) => void) | null;
  send(data: string): void;
  close(): void;
};

export type RuntimeWebSocketConstructor = new (url: string) => RuntimeWebSocket;

export class OgmiosRpcSession {
  private requestId = 0;
  private pending:
    | {
        readonly id: string;
        readonly resolve: (value: unknown) => void;
        readonly reject: (error: Error) => void;
        readonly timeout: ReturnType<typeof setTimeout>;
      }
    | undefined;
  private closed = false;

  private constructor(private readonly socket: RuntimeWebSocket) {
    socket.onmessage = ({ data }) => {
      const pending = this.pending;
      if (pending === undefined) {
        this.fail(new Error("Ogmios sent an unsolicited JSON-RPC response"));
        return;
      }
      try {
        if (typeof data !== "string") {
          throw new Error("Ogmios returned a non-text WebSocket message");
        }
        const envelope = getRecord(
          JSON.parse(data) as unknown,
          "Ogmios JSON-RPC response",
        );
        if (envelope.id !== pending.id) {
          throw new Error(
            `Ogmios JSON-RPC response id ${String(envelope.id)} does not match ${pending.id}`,
          );
        }
        if (envelope.error !== undefined) {
          // Typed for its code and transience; every failure here, a refused
          // request included, fails the tick for the next one to retry.
          throw new OgmiosJsonRpcError(
            `Ogmios JSON-RPC error: ${formatOgmiosJsonRpcError(envelope.error)}`,
            envelope.error,
          );
        }
        clearTimeout(pending.timeout);
        this.pending = undefined;
        pending.resolve(envelope.result);
      } catch (error) {
        this.fail(error instanceof Error ? error : new Error(String(error)));
      }
    };
    socket.onerror = () => {
      this.fail(new Error("Ogmios WebSocket failed"));
    };
    socket.onclose = () => {
      if (!this.closed) {
        this.fail(
          new Error("Ogmios WebSocket closed while chain sync was active"),
        );
      }
    };
  }

  static async open(ogmiosUrl: string): Promise<OgmiosRpcSession> {
    const constructor = (
      globalThis as unknown as {
        readonly WebSocket?: RuntimeWebSocketConstructor;
      }
    ).WebSocket;
    if (constructor === undefined) {
      throw new Error("Node.js WebSocket support is required for Ogmios");
    }
    const socketUrl = ogmiosWebSocketUrl(ogmiosUrl);
    const socket = new constructor(socketUrl.toString());
    await new Promise<void>((resolveOpen, rejectOpen) => {
      const timeout = setTimeout(() => {
        socket.close();
        rejectOpen(new Error("Ogmios WebSocket connection timed out"));
      }, 15_000);
      socket.onopen = () => {
        clearTimeout(timeout);
        resolveOpen();
      };
      socket.onerror = () => {
        clearTimeout(timeout);
        rejectOpen(
          new Error(`Ogmios WebSocket failed for ${socketUrl.origin}`),
        );
      };
      socket.onclose = () => {
        clearTimeout(timeout);
        rejectOpen(new Error("Ogmios WebSocket closed before opening"));
      };
    });
    return new OgmiosRpcSession(socket);
  }

  async request(
    method: string,
    params: Record<string, unknown>,
  ): Promise<unknown> {
    if (this.closed) {
      throw new Error("Ogmios JSON-RPC session is closed");
    }
    if (this.pending !== undefined) {
      throw new Error("Ogmios JSON-RPC session already has an active request");
    }
    const id = `midgard-${this.requestId.toString()}`;
    this.requestId += 1;
    return new Promise((resolveRequest, rejectRequest) => {
      const timeout = setTimeout(() => {
        this.fail(new Error(`Ogmios ${method} request timed out`));
      }, 15_000);
      this.pending = {
        id,
        resolve: resolveRequest,
        reject: rejectRequest,
        timeout,
      };
      this.socket.send(JSON.stringify({ jsonrpc: "2.0", id, method, params }));
    });
  }

  close(): void {
    if (!this.closed) {
      this.closed = true;
      const pending = this.pending;
      this.pending = undefined;
      if (pending !== undefined) {
        clearTimeout(pending.timeout);
        pending.reject(new Error("Ogmios JSON-RPC session closed"));
      }
      this.socket.close();
    }
  }

  private fail(error: Error): void {
    const pending = this.pending;
    this.pending = undefined;
    if (pending !== undefined) {
      clearTimeout(pending.timeout);
      pending.reject(error);
    }
    if (!this.closed) {
      this.closed = true;
      this.socket.close();
    }
  }
}

const ogmiosWebSocketUrl = (ogmiosUrl: string): URL => {
  const socketUrl = new URL(ogmiosUrl);
  if (socketUrl.protocol === "http:") {
    socketUrl.protocol = "ws:";
  } else if (socketUrl.protocol === "https:") {
    socketUrl.protocol = "wss:";
  } else if (socketUrl.protocol !== "ws:" && socketUrl.protocol !== "wss:") {
    throw new Error("Ogmios chain-sync endpoint must use HTTP(S) or WS(S)");
  }
  return socketUrl;
};

export const parseOgmiosPoint = (
  value: unknown,
  network: string,
  providerSource: string,
  label: string,
): CanonicalChainPoint => {
  const point = getRecord(value, label);
  return {
    network,
    slot: safeSlot(point.slot, `${label} slot`),
    blockHash: safeBlockHash(point.id, `${label} block hash`),
    providerSource,
    observedAt: new Date().toISOString(),
  };
};

export const parseOgmiosPointOrOrigin = (
  value: unknown,
  network: string,
  providerSource: string,
  label: string,
): CanonicalChainPoint | undefined =>
  value === "origin"
    ? undefined
    : parseOgmiosPoint(value, network, providerSource, label);

/**
 * A live L1 read refused before it compared anything: the configured network
 * is not a named one, so only a configured network magic can prove the live
 * chain's identity, and none was given.
 */
export class L1NetworkMagicUnconfiguredError extends Error {
  override readonly name = "L1NetworkMagicUnconfiguredError";
}

/**
 * The live chain must carry the configured network's magic: the built-in one
 * for Mainnet, Preprod and Preview, and on any other network (Custom) the
 * configured `cardanoL1Source.networkMagic`.
 */
export const assertNetworkMagic = (
  configuredNetwork: string,
  liveNetworkMagic: number,
  provider: string,
  configuredNetworkMagic: number | undefined,
): void => {
  const expected =
    configuredNetwork === "Mainnet"
      ? 764_824_073
      : configuredNetwork === "Preprod"
        ? 1
        : configuredNetwork === "Preview"
          ? 2
          : configuredNetworkMagic;
  if (expected === undefined) {
    throw new L1NetworkMagicUnconfiguredError(
      `${provider} cannot prove ${configuredNetwork} network identity without configured network magic`,
    );
  }
  if (liveNetworkMagic !== expected) {
    throw new L1SourceIntegrityError(
      `${provider} network magic ${liveNetworkMagic.toString()} does not match configured ${configuredNetwork} magic ${expected.toString()}`,
    );
  }
};
