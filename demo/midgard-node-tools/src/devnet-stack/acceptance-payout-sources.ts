import { closeSync, openSync, readSync } from "node:fs";
import { Socket } from "node:net";

import { Agent, fetch as undiciFetch } from "undici";

import type { AcceptanceNativePayoutScope } from "./acceptance-native-boundary.js";
import { requireAcceptance } from "./acceptance-payout-types.js";

export const readAcceptanceBoundedFile = (
  path: string,
  maxBytes: number,
): Buffer => {
  requireAcceptance(
    Number.isSafeInteger(maxBytes) && maxBytes > 0,
    "invalid recorded evidence byte bound",
  );
  const fd = openSync(path, "r");
  const chunks: Buffer[] = [];
  let bytes = 0;
  try {
    for (;;) {
      const chunk = Buffer.alloc(Math.min(65536, maxBytes + 1 - bytes));
      const length = readSync(fd, chunk);
      if (length === 0) return Buffer.concat(chunks, bytes);
      bytes += length;
      requireAcceptance(
        bytes <= maxBytes,
        "recorded evidence exceeds explicit byte bound",
      );
      chunks.push(chunk.subarray(0, length));
    }
  } finally {
    closeSync(fd);
  }
};

/** Own sockets before connect, so revocation also drains an unfinished connect. */
export class AcceptanceReadSockets {
  private readonly sockets = new Set<Socket>();
  private readonly closes = new Set<Promise<void>>();
  private readonly revoke = () => {
    for (const socket of this.sockets) socket.destroy();
  };

  constructor(readonly scope: AcceptanceNativePayoutScope) {
    scope.signal.addEventListener("abort", this.revoke, { once: true });
  }

  open(): Socket {
    this.scope.assertCurrent();
    const socket = new Socket();
    this.sockets.add(socket);
    const closed = new Promise<void>((resolve) =>
      socket.once("close", () => {
        this.sockets.delete(socket);
        resolve();
      }),
    );
    this.closes.add(closed);
    void closed.then(() => this.closes.delete(closed));
    if (this.scope.signal.aborted) socket.destroy();
    return socket;
  }

  async close(): Promise<void> {
    this.revoke();
    await Promise.all(this.closes);
    this.scope.signal.removeEventListener("abort", this.revoke);
  }
}

export const acceptanceRemainingMs = (scope: AcceptanceNativePayoutScope) => {
  scope.assertCurrent();
  const remaining = Math.floor(scope.deadlineEpochMs - Date.now());
  requireAcceptance(remaining > 0, "read deadline expired");
  return remaining;
};

/** Kupo bytes only locate canonical transactions; no quantity is authoritative. */
export const openAcceptanceKupoReads = (
  scope: AcceptanceNativePayoutScope,
  kupoUrl: string,
  maxResponseBytes: number,
) => {
  requireAcceptance(
    Number.isSafeInteger(maxResponseBytes) && maxResponseBytes > 0,
    "invalid Kupo response bound",
  );
  const origin = new URL(kupoUrl);
  requireAcceptance(
    origin.protocol === "http:" && origin.hostname === "127.0.0.1",
    "Kupo must be the recorded local run endpoint",
  );
  const sockets = new AcceptanceReadSockets(scope);
  const agent = new Agent({
    connect(options, callback) {
      let socket: Socket;
      try {
        scope.assertCurrent();
        requireAcceptance(
          options.hostname === origin.hostname &&
            String(options.port) === origin.port,
          "Kupo connector endpoint changed",
        );
        socket = sockets.open();
      } catch (error) {
        callback(error as Error, null);
        return;
      }
      const failed = (error: Error) => callback(error, null);
      socket.once("error", failed);
      socket.connect(Number(origin.port), origin.hostname, () => {
        socket.off("error", failed);
        callback(null, socket);
      });
    },
  });
  const pending = new Set<Promise<Response>>();
  const fetchImpl = (input: string, init?: RequestInit): Promise<Response> => {
    const read = (async () => {
      acceptanceRemainingMs(scope);
      const url = new URL(input);
      requireAcceptance(
        url.origin === origin.origin,
        "Kupo URL escaped run endpoint",
      );
      const response = await undiciFetch(input, {
        method: "GET",
        dispatcher: agent,
        redirect: "error",
        signal:
          init?.signal == null
            ? scope.signal
            : AbortSignal.any([scope.signal, init.signal]),
      });
      scope.assertCurrent();
      requireAcceptance(
        response.ok && response.body !== null,
        "Kupo HTTP read failed",
      );
      const reader = response.body.getReader();
      const chunks: Uint8Array[] = [];
      let bytes = 0;
      try {
        for (;;) {
          const part = await reader.read();
          scope.assertCurrent();
          if (part.done) break;
          bytes += part.value.byteLength;
          requireAcceptance(
            bytes <= maxResponseBytes,
            "Kupo response exceeds explicit byte bound",
          );
          chunks.push(part.value);
        }
      } finally {
        await reader.cancel();
        reader.releaseLock();
      }
      return new Response(Buffer.concat(chunks), {
        status: response.status,
        headers: { "content-type": "application/json" },
      });
    })();
    pending.add(read);
    void read.then(
      () => pending.delete(read),
      () => pending.delete(read),
    );
    return read;
  };
  return {
    fetchImpl,
    async close() {
      await Promise.all([
        agent.destroy(),
        sockets.close(),
        Promise.allSettled(pending),
      ]);
    },
  };
};
