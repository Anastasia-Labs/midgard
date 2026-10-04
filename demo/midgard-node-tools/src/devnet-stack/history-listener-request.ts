import type { IncomingMessage, ServerResponse } from "node:http";

import { CHILD_STATUS_MAX_FRAME_BYTES } from "./child-status-channel.js";
import {
  HISTORY_LISTENER_PATH,
  historyListenerAnswer,
  type HistoryListenerBinding,
  parseHistoryListenerChallenge,
} from "./history-listener-evidence.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";

/** Only this bounded read-only endpoint; other provider APIs retain their dispatch. */
const handleHistoryListenerRequest = async (input: {
  readonly request: IncomingMessage;
  readonly response: ServerResponse;
  readonly directory: string;
  readonly binding: HistoryListenerBinding;
  readonly maximumBudgetMs: number;
}): Promise<boolean> => {
  const { request, response } = input;
  if (request.url !== HISTORY_LISTENER_PATH) return false;
  const deadline = historyProofDeadline(input.maximumBudgetMs);
  if (deadline === null || request.method !== "POST") {
    response.writeHead(405).end();
    return true;
  }
  const timer = setTimeout(
    () => request.destroy(),
    historyProofRemaining(deadline),
  );
  try {
    const chunks: Buffer[] = [];
    let size = 0;
    for await (const bytes of request) {
      if (!Buffer.isBuffer(bytes))
        throw new Error("history listener body is not bytes");
      size += bytes.length;
      if (
        size > CHILD_STATUS_MAX_FRAME_BYTES ||
        historyProofRemaining(deadline) === 0
      ) {
        response.writeHead(413).end();
        request.destroy();
        return true;
      }
      chunks.push(bytes);
    }
    const parsed = parseHistoryListenerChallenge(
      JSON.parse(
        new TextDecoder("utf8", { fatal: true }).decode(Buffer.concat(chunks)),
      ),
    );
    const answer =
      parsed === null || historyProofRemaining(deadline) === 0
        ? null
        : historyListenerAnswer({
            directory: input.directory,
            binding: input.binding,
            request: parsed,
            maximumBudgetMs: Math.min(
              historyProofRemaining(deadline),
              parsed.budgetMs,
            ),
          });
    const bytes = Buffer.from(JSON.stringify(answer));
    if (
      bytes.length > CHILD_STATUS_MAX_FRAME_BYTES ||
      historyProofRemaining(deadline) === 0
    ) {
      response.writeHead(503).end();
      return true;
    }
    response
      .writeHead(answer === null ? 503 : 200, {
        "content-type": "application/json",
        "content-length": bytes.length,
      })
      .end(bytes);
  } catch {
    if (!response.destroyed) response.writeHead(400).end();
  } finally {
    clearTimeout(timer);
  }
  return true;
};

/** One current proof handler; overlapping requests are refused rather than queued. */
export const createHistoryListenerRequestHandler = (binding: {
  readonly directory: string;
  readonly binding: HistoryListenerBinding;
  readonly maximumBudgetMs: number;
}) => {
  let active = false;
  return async (
    request: IncomingMessage,
    response: ServerResponse,
  ): Promise<boolean> => {
    if (request.url !== HISTORY_LISTENER_PATH) return false;
    if (active) {
      response.writeHead(503).end();
      request.resume();
      return true;
    }
    active = true;
    try {
      return await handleHistoryListenerRequest({
        ...binding,
        request,
        response,
      });
    } finally {
      active = false;
    }
  };
};
