import { Socket } from "node:net";

import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { Agent, WebSocket } from "undici";

/** Real TCP ownership before connect, including an unresponsive upgrade/close. */
export const createAcceptanceNativeTransport = (input: {
  endpoint: string;
  scope: DaAvailabilityReadScope;
  validateMessage?: (text: string) => void;
}) => {
  input.scope.assertCurrent();
  const endpoint = new URL(input.endpoint);
  if (endpoint.protocol === "http:") endpoint.protocol = "ws:";
  if (
    endpoint.protocol !== "ws:" ||
    !["127.0.0.1", "[::1]"].includes(endpoint.hostname) ||
    endpoint.username !== "" ||
    endpoint.password !== "" ||
    endpoint.hash !== ""
  )
    throw new Error(
      "acceptance native query requires the recorded loopback Ogmios endpoint",
    );
  const sockets = new Set<Socket>();
  const drains: Promise<void>[] = [];
  let closed = false;
  let fault: Error | undefined;
  const destroy = (): void => {
    closed = true;
    for (const socket of sockets) socket.destroy();
  };
  input.scope.signal.addEventListener("abort", destroy, { once: true });
  // The connector owns the TCP socket before connect/upgrade can wait. Destroy
  // is physical cancellation even if the peer never answers a close handshake.
  const agent = new Agent({
    connect: (_options, callback) => {
      const socket = new Socket();
      sockets.add(socket);
      drains.push(
        new Promise<void>((resolve) =>
          socket.once("close", () => {
            sockets.delete(socket);
            resolve();
          }),
        ),
      );
      let answered = false;
      const answer = (error?: Error): void => {
        if (answered) return;
        answered = true;
        if (error !== undefined) callback(error, null);
        else callback(null, socket);
      };
      socket.once("error", answer);
      socket.once("connect", () => answer());
      socket.once("close", () =>
        answer(new Error("acceptance native socket closed before connect")),
      );
      if (input.scope.signal.aborted || closed) {
        socket.destroy();
        return;
      }
      socket.connect(
        Number(endpoint.port || 80),
        endpoint.hostname === "[::1]" ? "::1" : endpoint.hostname,
      );
    },
  });
  return {
    webSocketFactory: (url: string): WebSocket => {
      input.scope.assertCurrent();
      if (closed || new URL(url).href !== endpoint.href)
        throw new Error("acceptance native transport endpoint changed");
      const websocket = new WebSocket(endpoint, { dispatcher: agent });
      websocket.addEventListener("message", (event) => {
        try {
          if (
            typeof event.data !== "string" ||
            Buffer.byteLength(event.data) > 8 * 1024 * 1024
          )
            throw new Error("acceptance lineage frame is invalid or oversized");
          input.validateMessage?.(event.data);
        } catch (error) {
          fault =
            error instanceof Error
              ? error
              : new Error("acceptance lineage transport validation failed", {
                  cause: error,
                });
          destroy();
        }
      });
      return websocket;
    },
    destroy,
    assertCurrent: (): void => {
      input.scope.assertCurrent();
      if (fault !== undefined) throw fault;
    },
    close: async (): Promise<void> => {
      destroy();
      input.scope.signal.removeEventListener("abort", destroy);
      await agent.destroy();
      await Promise.all(drains);
    },
  };
};
