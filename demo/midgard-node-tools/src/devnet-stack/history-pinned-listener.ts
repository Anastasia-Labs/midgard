import { createHash, randomUUID, X509Certificate } from "node:crypto";
import { request as httpRequest } from "node:http";
import { connect as tlsConnect, type TLSSocket } from "node:tls";

import { CHILD_STATUS_MAX_FRAME_BYTES } from "./child-status-channel.js";
import type { HistoryWindowOffer } from "./history-child-evidence.js";
import {
  HISTORY_LISTENER_PATH,
  HISTORY_LISTENER_SCHEMA,
  type HistoryListenerBinding,
  type HistoryListenerChallenge,
  historyListenerReplyMatches,
} from "./history-listener-evidence.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";

export type HistoryPinnedListener = Readonly<{
  hostname: string;
  port: number;
  ca: string;
  binding: HistoryListenerBinding;
}>;
/** One current TLS connection to an exact recorded provider, optionally via CONNECT. */
export const provePinnedHistoryListener = async (input: {
  readonly listener: HistoryPinnedListener;
  readonly offer: HistoryWindowOffer;
  readonly timeoutMs: number;
  readonly tunnelPort?: number;
  readonly signal?: AbortSignal;
}): Promise<boolean> => {
  const deadline = historyProofDeadline(input.timeoutMs);
  if (deadline === null) return false;
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), input.timeoutMs);
  const cancel = () => controller.abort();
  input.signal?.addEventListener("abort", cancel, { once: true });
  if (input.signal?.aborted) controller.abort();
  let socket: TLSSocket | undefined;
  let raw: import("node:stream").Duplex | undefined;
  try {
    if (controller.signal.aborted) return false;
    if (input.tunnelPort !== undefined) {
      raw = await new Promise<import("node:stream").Duplex>(
        (resolve, reject) => {
          const connect = httpRequest({
            host: "127.0.0.1",
            port: input.tunnelPort,
            method: "CONNECT",
            path: `${input.listener.hostname}:443`,
            maxHeaderSize: 4096,
            signal: controller.signal,
            agent: false,
          });
          connect.once("error", reject);
          connect.once("response", (response) => {
            response.destroy();
            reject(new Error("history CONNECT refused"));
          });
          connect.once("connect", (response, stream, head) => {
            if (response.statusCode !== 200 || head.length !== 0) {
              stream.destroy();
              reject(
                new Error("history CONNECT did not establish exact TLS route"),
              );
            } else resolve(stream);
          });
          connect.end();
        },
      );
    }
    if (controller.signal.aborted || historyProofRemaining(deadline) === 0)
      return false;
    socket = tlsConnect({
      ...(raw === undefined
        ? { host: "127.0.0.1", port: input.listener.port }
        : { socket: raw }),
      servername: input.listener.hostname,
      ca: input.listener.ca,
      rejectUnauthorized: true,
    });
    const connection = socket;
    const close = () =>
      connection.destroy(new Error("history proof cancelled"));
    controller.signal.addEventListener("abort", close, { once: true });
    try {
      await new Promise<void>((resolve, reject) => {
        connection.once("error", reject);
        connection.once("close", () =>
          reject(new Error("history TLS ended before proof")),
        );
        if (controller.signal.aborted) close();
        connection.once("secureConnect", () => {
          try {
            const certificate = new X509Certificate(
              connection.getPeerCertificate().raw,
            );
            const pin = createHash("sha256")
              .update(
                certificate.publicKey.export({ type: "spki", format: "der" }),
              )
              .digest("hex");
            if (
              !connection.authorized ||
              pin !== input.listener.binding.operatorIdentitySha256
            )
              reject(new Error("history provider identity mismatch"));
            else resolve();
          } catch (error) {
            reject(
              error instanceof Error
                ? error
                : new Error("invalid history listener evidence"),
            );
          }
        });
      });
      const request: HistoryListenerChallenge = {
        schema: HISTORY_LISTENER_SCHEMA,
        challengeId: randomUUID(),
        budgetMs: historyProofRemaining(deadline),
        offer: input.offer,
      };
      const body = Buffer.from(JSON.stringify(request));
      if (
        body.length > CHILD_STATUS_MAX_FRAME_BYTES ||
        controller.signal.aborted ||
        historyProofRemaining(deadline) === 0
      )
        return false;
      const reply = await new Promise<unknown>((resolve, reject) => {
        const call = httpRequest(
          {
            host: input.listener.hostname,
            method: "POST",
            path: HISTORY_LISTENER_PATH,
            createConnection: () => connection,
            signal: controller.signal,
            maxHeaderSize: 4096,
            headers: {
              "content-type": "application/json",
              "content-length": body.length,
              connection: "close",
            },
          },
          (response) => {
            const chunks: Buffer[] = [];
            let size = 0;
            response.on("data", (chunk: Buffer) => {
              size += chunk.length;
              if (size > CHILD_STATUS_MAX_FRAME_BYTES) {
                response.destroy(new Error("oversized history proof"));
                return;
              }
              chunks.push(chunk);
            });
            response.once("error", reject);
            response.once("end", () => {
              try {
                if (response.statusCode !== 200)
                  throw new Error("history proof unavailable");
                resolve(
                  JSON.parse(
                    new TextDecoder("utf8", { fatal: true }).decode(
                      Buffer.concat(chunks),
                    ),
                  ),
                );
              } catch (error) {
                reject(
                  error instanceof Error
                    ? error
                    : new Error("invalid history listener evidence"),
                );
              }
            });
          },
        );
        call.once("error", reject);
        call.end(body);
      });
      return (
        !controller.signal.aborted &&
        historyProofRemaining(deadline) > 0 &&
        historyListenerReplyMatches(reply, request, input.listener.binding)
      );
    } finally {
      controller.signal.removeEventListener("abort", close);
    }
  } catch {
    return false;
  } finally {
    clearTimeout(timeout);
    input.signal?.removeEventListener("abort", cancel);
    controller.abort();
    socket?.destroy();
    raw?.destroy();
  }
};
