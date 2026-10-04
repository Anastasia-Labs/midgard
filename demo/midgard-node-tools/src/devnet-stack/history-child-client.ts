import { randomUUID } from "node:crypto";
import type { Duplex } from "node:stream";

import { childStatusClient } from "./child-status-channel.js";
import {
  HISTORY_CHILD_SCHEMA,
  type HistoryChildActor,
  type HistoryChildChallenge,
  type HistoryChildReply,
  type HistoryWindowOffer,
  parseHistoryChildActor,
  parseHistoryChildChallenge,
  parseHistoryChildReply,
} from "./history-child-evidence.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";

/** Ephemeral exact-child channel; callers still check current scope/PID after awaits. */
export const historyChildClient = (input: {
  readonly actor: HistoryChildActor;
  readonly pipe: Duplex;
}) => {
  const actor = parseHistoryChildActor(input.actor);
  if (actor === null) throw new Error("invalid history child actor");
  const client = childStatusClient<HistoryChildReply>({
    pipe: input.pipe,
    request: (challengeId, expected) => {
      if (
        expected === null ||
        typeof expected !== "object" ||
        Array.isArray(expected)
      )
        throw new Error("invalid history child request");
      const request = parseHistoryChildChallenge({
        ...expected,
        schema: HISTORY_CHILD_SCHEMA,
        challengeId,
        actor,
      });
      if (request === null) throw new Error("invalid history child request");
      return request;
    },
    response: (value, challengeId) =>
      parseHistoryChildReply(value, challengeId, actor),
  });
  return {
    close: client.close,
    request: async (
      operation: HistoryChildChallenge["operation"],
      offer: HistoryWindowOffer | null,
      budgetMs: number,
    ) => {
      const deadline = historyProofDeadline(budgetMs);
      if (deadline === null || historyProofRemaining(deadline) === 0)
        return null;
      // Validate before occupying the one pending slot; the wire nonce is
      // independently replaced by childStatusClient for this actual attempt.
      const expected = parseHistoryChildChallenge({
        schema: HISTORY_CHILD_SCHEMA,
        challengeId: randomUUID(),
        actor,
        operation,
        offer,
        budgetMs: historyProofRemaining(deadline),
      });
      if (expected === null || historyProofRemaining(deadline) === 0)
        return null;
      const response = await client.request(
        expected,
        historyProofRemaining(deadline),
      );
      return historyProofRemaining(deadline) > 0 ? response : null;
    },
  };
};
