import { answerChildStatus } from "./child-status-channel.js";
import type { AdmittedHistoryChild } from "./history-child-admission.js";
import {
  HISTORY_CHILD_SCHEMA,
  historyActorsMatch,
  parseHistoryChildChallenge,
} from "./history-child-evidence.js";
import type { HistoryReadinessDispatch } from "./history-child-startup.js";
import { provePinnedHistoryListener } from "./history-pinned-listener.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";
import type { RunEnv } from "./layout.js";
import { servicePorts } from "./layout.js";
import { DEFAULT_POLICY } from "./supervisor.js";

/** The actual archive/tunnel child proves its own pinned listener routes anew. */
export const answerHistoryProviderReadiness = (
  child: AdmittedHistoryChild,
  run: RunEnv,
  dispatch?: HistoryReadinessDispatch,
) =>
  (dispatch?.install ?? answerChildStatus)({
    parse: (value) => {
      const request = parseHistoryChildChallenge(value);
      return request !== null &&
        request.operation === "prove" &&
        historyActorsMatch(request.actor, child.actor)
        ? request
        : null;
    },
    answer: async (request) => {
      const deadline = historyProofDeadline(
        Math.min(request.budgetMs, DEFAULT_POLICY.probeTimeoutMs),
      );
      let ready = false;
      if (deadline !== null && request.offer !== null) {
        try {
          const admission = child.current(deadline);
          const indices =
            child.actor.role === "history-tunnel"
              ? [0, 1]
              : child.actor.role === "history-archive-a"
                ? [0]
                : child.actor.role === "history-archive-b"
                  ? [1]
                  : [];
          ready = indices.length > 0;
          for (const index of indices) {
            child.current(deadline);
            const listener = admission.providers[index];
            if (
              listener === undefined ||
              historyProofRemaining(deadline) === 0
            ) {
              ready = false;
              break;
            }
            const proven = await provePinnedHistoryListener({
              listener,
              offer: request.offer,
              timeoutMs: historyProofRemaining(deadline),
              ...(child.actor.role === "history-tunnel"
                ? { tunnelPort: servicePorts(run).historyTunnel }
                : {}),
            });
            child.current(deadline);
            if (!proven) {
              ready = false;
              break;
            }
            if (historyProofRemaining(deadline) === 0) {
              ready = false;
              break;
            }
          }
          if (ready) child.current(deadline);
          ready = ready && historyProofRemaining(deadline) > 0;
        } catch {
          ready = false;
        }
      }
      return {
        schema: HISTORY_CHILD_SCHEMA,
        challengeId: request.challengeId,
        actor: child.actor,
        offer: ready ? request.offer : null,
      };
    },
  });
