import { fetchKupoCheckpoint } from "./provider.local-node-chain-authority-from-config.js";
import {
  assertNetworkMagic,
  KUPMIOS_TIP_ALIGNMENT_ATTEMPTS,
  KUPMIOS_TIP_ALIGNMENT_RETRY_MS,
  type RuntimeWebSocketConstructor,
} from "./provider.ogmios-rpc-session.js";
import {
  type CanonicalChainPoint,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";

export const alignedKupmiosTip = async (
  network: string,
  kupoUrl: string,
  ogmiosUrl: string,
  fetchFn: typeof fetch,
  networkMagic: number | undefined,
): Promise<CanonicalChainPoint> => {
  for (let attempt = 1; ; attempt += 1) {
    const [kupoPoint, ogmiosTip] = await Promise.all([
      fetchKupoCheckpoint(kupoUrl, fetchFn),
      requestOgmiosTip(ogmiosUrl),
    ]);
    assertNetworkMagic(network, ogmiosTip.networkMagic, "Ogmios", networkMagic);
    if (
      kupoPoint.slot === ogmiosTip.slot &&
      kupoPoint.blockHash === ogmiosTip.blockHash
    ) {
      return alignedTipPoint(network, kupoUrl, ogmiosUrl, ogmiosTip);
    }
    if (attempt >= KUPMIOS_TIP_ALIGNMENT_ATTEMPTS) {
      throw new Error(
        `Kupmios query surfaces are not aligned after ${attempt.toString()} reads: Kupo=${kupoPoint.slot.toString()}:${kupoPoint.blockHash}, Ogmios=${ogmiosTip.slot.toString()}:${ogmiosTip.blockHash}`,
      );
    }
    await new Promise((resolve) =>
      setTimeout(resolve, KUPMIOS_TIP_ALIGNMENT_RETRY_MS),
    );
  }
};

const alignedTipPoint = (
  network: string,
  kupoUrl: string,
  ogmiosUrl: string,
  ogmiosTip: Awaited<ReturnType<typeof requestOgmiosTip>>,
): CanonicalChainPoint => {
  return {
    network,
    slot: ogmiosTip.slot,
    blockHash: ogmiosTip.blockHash,
    ...(ogmiosTip.blockHeight === undefined
      ? {}
      : { blockHeight: ogmiosTip.blockHeight }),
    providerSource: `kupmios:${kupoUrl}|${ogmiosUrl}`,
    observedAt: new Date().toISOString(),
  };
};

const requestOgmiosTip = async (
  ogmiosUrl: string,
): Promise<{
  readonly slot: number;
  readonly blockHash: string;
  readonly blockHeight?: number;
  readonly networkMagic: number;
}> => {
  const response = await runOgmiosSession(ogmiosUrl, [
    { id: "query-tip", method: "queryNetwork/tip", params: {} },
    {
      id: "query-genesis",
      method: "queryNetwork/genesisConfiguration",
      params: { era: "shelley" },
    },
  ]);
  const point = getRecord(response.get("query-tip"), "Ogmios network tip");
  const genesis = getRecord(
    response.get("query-genesis"),
    "Ogmios genesis configuration",
  );
  const height =
    point.height === undefined
      ? undefined
      : safeSlot(point.height, "Ogmios network tip height");
  return {
    slot: safeSlot(point.slot, "Ogmios network tip slot"),
    blockHash: safeBlockHash(point.id, "Ogmios network tip block hash"),
    ...(height === undefined ? {} : { blockHeight: height }),
    networkMagic: safeSlot(
      genesis.networkMagic ?? genesis.network_magic,
      "Ogmios network magic",
    ),
  };
};

const runOgmiosSession = async (
  ogmiosUrl: string,
  requests: readonly {
    readonly id: string;
    readonly method: string;
    readonly params: Record<string, unknown>;
  }[],
): Promise<ReadonlyMap<string, unknown>> => {
  const constructor = (
    globalThis as unknown as {
      readonly WebSocket?: RuntimeWebSocketConstructor;
    }
  ).WebSocket;
  if (constructor === undefined) {
    throw new Error("Node.js WebSocket support is required for Ogmios");
  }
  const socketUrl = new URL(ogmiosUrl);
  if (socketUrl.protocol === "http:") {
    socketUrl.protocol = "ws:";
  } else if (socketUrl.protocol === "https:") {
    socketUrl.protocol = "wss:";
  } else if (socketUrl.protocol !== "ws:" && socketUrl.protocol !== "wss:") {
    throw new Error("Ogmios chain-sync endpoint must use HTTP(S) or WS(S)");
  }
  return new Promise((resolve, reject) => {
    const socket = new constructor(socketUrl.toString());
    const results = new Map<string, unknown>();
    let requestIndex = 0;
    let settled = false;
    const timeout = setTimeout(() => {
      fail(new Error("Ogmios chain-sync request timed out"));
    }, 15_000);
    const finish = (): void => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      socket.close();
      resolve(results);
    };
    const fail = (error: Error): void => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      socket.close();
      reject(error);
    };
    const sendNext = (): void => {
      const request = requests[requestIndex];
      if (request === undefined) {
        finish();
        return;
      }
      socket.send(
        JSON.stringify({
          jsonrpc: "2.0",
          id: request.id,
          method: request.method,
          params: request.params,
        }),
      );
    };
    socket.onopen = sendNext;
    socket.onmessage = ({ data }) => {
      try {
        if (typeof data !== "string") {
          throw new Error("Ogmios returned a non-text WebSocket message");
        }
        const envelope = getRecord(
          JSON.parse(data) as unknown,
          "Ogmios JSON-RPC response",
        );
        if (envelope.error !== undefined) {
          throw new Error(
            `Ogmios JSON-RPC error: ${JSON.stringify(envelope.error)}`,
          );
        }
        const id = envelope.id;
        if (typeof id !== "string") {
          throw new Error("Ogmios JSON-RPC response omitted request id");
        }
        const expected = requests[requestIndex];
        if (expected === undefined || id !== expected.id) {
          throw new Error(
            `Ogmios JSON-RPC response id ${id} does not match the active request`,
          );
        }
        results.set(id, envelope.result);
        requestIndex += 1;
        sendNext();
      } catch (error) {
        fail(error instanceof Error ? error : new Error(String(error)));
      }
    };
    socket.onerror = () => {
      fail(new Error(`Ogmios WebSocket failed for ${socketUrl.origin}`));
    };
    socket.onclose = () => {
      if (!settled) {
        fail(
          new Error("Ogmios WebSocket closed before the response completed"),
        );
      }
    };
  });
};
