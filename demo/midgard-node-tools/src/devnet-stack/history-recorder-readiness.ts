import { answerChildStatus } from "./child-status-channel.js";
import {
  HISTORY_CHILD_SCHEMA,
  historyActorsMatch,
  type HistoryChildActor,
  parseHistoryChildChallenge,
} from "./history-child-evidence.js";
import type { HistoryReadinessDispatch } from "./history-child-startup.js";
import type { createHistoryWindowSealer } from "./history-native-window-proof.js";
import {
  historyOfferAvailable,
  offerForHistorySeal,
} from "./history-offer-availability.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";

/** The actual recorder's inherited channel challenges its live native sealer. */
export const answerHistoryRecorderReadiness = (
  input: {
    readonly actor: HistoryChildActor;
    readonly directories: readonly string[];
    readonly sealer: ReturnType<typeof createHistoryWindowSealer>;
    readonly timeoutMs: number;
    readonly current?: (deadline: number) => Promise<boolean>;
  },
  dispatch?: HistoryReadinessDispatch,
) =>
  (dispatch?.install ?? answerChildStatus)({
    parse: (value) => {
      const request = parseHistoryChildChallenge(value);
      return request !== null &&
        historyActorsMatch(request.actor, input.actor) &&
        request.operation !== "prove"
        ? request
        : null;
    },
    answer: async (request) => {
      const deadline = historyProofDeadline(
        Math.min(request.budgetMs, input.timeoutMs),
      );
      const admitted =
        deadline !== null &&
        historyProofRemaining(deadline) > 0 &&
        (input.current === undefined || (await input.current(deadline)));
      const window =
        !admitted || deadline === null || historyProofRemaining(deadline) === 0
          ? null
          : request.operation === "seal"
            ? input.sealer.seal(historyProofRemaining(deadline))
            : request.offer === null
              ? null
              : input.sealer.revalidate(
                  request.offer.window.sealId,
                  request.offer.window.generation,
                );
      const offered =
        window === null ? null : offerForHistorySeal(input.directories, window);
      const ready =
        deadline !== null &&
        historyProofRemaining(deadline) > 0 &&
        offered !== null &&
        (request.offer === null ||
          (JSON.stringify(request.offer) === JSON.stringify(offered) &&
            historyOfferAvailable(input.directories, request.offer))) &&
        input.sealer.revalidate(
          offered.window.sealId,
          offered.window.generation,
        ) !== null &&
        historyProofRemaining(deadline) > 0;
      const admittedAgain =
        ready &&
        deadline !== null &&
        (input.current === undefined || (await input.current(deadline)));
      const current =
        admittedAgain &&
        deadline !== null &&
        historyProofRemaining(deadline) > 0 &&
        offered !== null &&
        input.sealer.revalidate(
          offered.window.sealId,
          offered.window.generation,
        ) !== null &&
        historyProofRemaining(deadline) > 0;
      return {
        schema: HISTORY_CHILD_SCHEMA,
        challengeId: request.challengeId,
        actor: input.actor,
        offer: current ? offered : null,
      };
    },
  });
