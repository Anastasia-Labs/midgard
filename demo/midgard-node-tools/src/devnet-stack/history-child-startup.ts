import { Socket } from "node:net";
import type { Duplex } from "node:stream";

import {
  answerChildStatus,
  CHILD_STATUS_ATTEMPT_ENV,
} from "./child-status-channel.js";
import {
  HISTORY_CHILD_SCHEMA,
  historyActorsMatch,
  type HistoryChildActor,
  type HistoryChildChallenge,
  parseHistoryChildChallenge,
} from "./history-child-evidence.js";

export type HistoryReadinessDispatch = Readonly<{
  signal: AbortSignal;
  install: (handler: {
    parse: (value: unknown) => HistoryChildChallenge | null;
    answer: (request: HistoryChildChallenge) => Promise<unknown>;
  }) => () => void;
}>;

/** One termination owner and fd3 reader from unadmitted startup through shutdown. */
export const startHistoryChildLifecycle = (
  suppliedActor: HistoryChildActor,
  providedPipe?: Duplex,
) => {
  const actor = Object.freeze({ ...suppliedActor });
  const stopped = new AbortController();
  let handler: Parameters<HistoryReadinessDispatch["install"]>[0] | undefined;
  let pending: Promise<unknown> | undefined;
  const stop = () => {
    handler = undefined;
    stopped.abort();
  };
  const pipe =
    providedPipe ??
    (process.env[CHILD_STATUS_ATTEMPT_ENV] === undefined
      ? undefined
      : new Socket({ fd: 3, readable: true, writable: true }));
  for (const signal of ["SIGTERM", "SIGINT"] as const)
    process.once(signal, stop);
  // Observe physical close before asking the framing owner to destroy its pipe.
  const pipeClosed =
    pipe === undefined || pipe.closed
      ? Promise.resolve()
      : new Promise<void>((resolve) => pipe.once("close", () => resolve()));
  const closeChannel =
    pipe === undefined
      ? () => undefined
      : answerChildStatus({
          pipe,
          parse: (value) => {
            const request = parseHistoryChildChallenge(value);
            return request !== null &&
              historyActorsMatch(request.actor, actor) &&
              (actor.role === "history-recorder"
                ? request.operation !== "prove"
                : request.operation === "prove")
              ? request
              : null;
          },
          answer: (request) => {
            const active = stopped.signal.aborted ? undefined : handler;
            const negative = {
              schema: HISTORY_CHILD_SCHEMA,
              challengeId: request.challengeId,
              actor,
              offer: null,
            };
            const answering =
              active === undefined || active.parse(request) === null
                ? Promise.resolve(negative)
                : active.answer(request);
            pending = answering;
            return answering
              .then((response) =>
                stopped.signal.aborted || handler !== active
                  ? negative
                  : response,
              )
              .finally(() => {
                if (pending === answering) pending = undefined;
              });
          },
        });
  const dispatch: HistoryReadinessDispatch = {
    signal: stopped.signal,
    install: (next) => {
      if (!stopped.signal.aborted) handler = next;
      return () => {
        if (handler === next) handler = undefined;
      };
    },
  };
  return {
    ...dispatch,
    close: async () => {
      stop();
      closeChannel();
      try {
        await pending;
      } finally {
        await pipeClosed;
        for (const signal of ["SIGTERM", "SIGINT"] as const)
          process.removeListener(signal, stop);
      }
    },
  };
};
