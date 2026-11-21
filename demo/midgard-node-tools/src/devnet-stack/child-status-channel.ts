import { randomUUID } from "node:crypto";
import { Socket } from "node:net";
import { Duplex } from "node:stream";

/** An inherited pipe, never a durable or externally published readiness token. */
export const CHILD_STATUS_ATTEMPT_ENV = "MIDGARD_DEVNET_READINESS_ATTEMPT";
export const CHILD_STATUS_CODE_ENV = "MIDGARD_DEVNET_READINESS_CODE";
export const CHILD_STATUS_SPECS_ENV = "MIDGARD_DEVNET_READINESS_SPECS";
export const CHILD_STATUS_MAX_FRAME_BYTES = 16_384;

/** Bounded exact JSON framing; a malformed peer permanently closes this attempt. */
const frames = (
  pipe: Duplex,
  receive: (value: unknown) => void,
  closed: () => void,
) => {
  let pending = Buffer.alloc(0);
  let ended = false;
  const end = () => {
    if (ended) return;
    ended = true;
    pending = Buffer.alloc(0);
    closed();
    pipe.destroy();
  };
  pipe.on("data", (chunk: unknown) => {
    if (ended || !Buffer.isBuffer(chunk)) return end();
    pending = Buffer.concat([pending, chunk]);
    while (!ended) {
      const newline = pending.indexOf(10);
      if (newline < 0) {
        if (pending.length > CHILD_STATUS_MAX_FRAME_BYTES) end();
        return;
      }
      if (newline === 0 || newline > CHILD_STATUS_MAX_FRAME_BYTES) return end();
      const bytes = pending.subarray(0, newline);
      pending = pending.subarray(newline + 1);
      try {
        receive(
          JSON.parse(new TextDecoder("utf8", { fatal: true }).decode(bytes)),
        );
      } catch {
        end();
      }
    }
  });
  pipe.once("error", end);
  pipe.once("close", end);
  pipe.once("end", end);
  return end;
};
const writeFrame = (pipe: Duplex, value: unknown, close: () => void) => {
  const encoded = Buffer.from(JSON.stringify(value));
  if (
    encoded.length > CHILD_STATUS_MAX_FRAME_BYTES ||
    pipe.destroyed ||
    !pipe.writable ||
    pipe.writableNeedDrain
  )
    return false;
  if (!pipe.write(Buffer.concat([encoded, Buffer.from("\n")]))) {
    // write(false) accepted this exact frame, but forbids another until drain.
    // A peer that never resumes cannot retain this attempt indefinitely.
    const timer = setTimeout(close, 1000);
    const drained = () => {
      clearTimeout(timer);
      pipe.off("drain", drained);
      pipe.off("close", drained);
    };
    pipe.once("drain", drained);
    pipe.once("close", drained);
  }
  return true;
};

/** One actual-child request at a time; overlap/timeout/exit is unknown. */
export const childStatusClient = <Response>(input: {
  readonly pipe: Duplex;
  readonly request: (challengeId: string, expected: unknown) => unknown;
  readonly response: (value: unknown, challengeId: string) => Response | null;
}) => {
  let waiting:
    | {
        readonly challengeId: string;
        readonly finish: (value: Response | null) => void;
      }
    | undefined;
  let closed = false;
  const expired: string[] = [];
  const close = frames(
    input.pipe,
    (value) => {
      const current = waiting;
      // A bounded probe may finish just after its caller's deadline. Ignore only
      // a fully validated reply to a known expired challenge, never reuse it.
      if (expired.some((id) => input.response(value, id) !== null)) return;
      if (current === undefined) throw new Error("unsolicited child status");
      const parsed = input.response(value, current.challengeId);
      if (parsed === null) throw new Error("invalid child status");
      current.finish(parsed);
    },
    () => {
      closed = true;
      waiting?.finish(null);
    },
  );
  return {
    close,
    request: (
      expected: unknown,
      timeoutMs: number,
    ): Promise<Response | null> => {
      if (
        closed ||
        waiting !== undefined ||
        input.pipe.writableNeedDrain ||
        !Number.isFinite(timeoutMs) ||
        timeoutMs <= 0
      )
        return Promise.resolve(null);
      return new Promise((resolve) => {
        const challengeId = randomUUID();
        const timer = setTimeout(() => finish(null), timeoutMs);
        const finish = (value: Response | null) => {
          if (waiting?.challengeId !== challengeId) return;
          waiting = undefined;
          if (value === null) {
            expired.push(challengeId);
            if (expired.length > 8) expired.shift();
          }
          clearTimeout(timer);
          resolve(value);
        };
        waiting = { challengeId, finish };
        if (
          !writeFrame(input.pipe, input.request(challengeId, expected), close)
        )
          finish(null);
      });
    },
  };
};

/** The actual child answers its own inherited fd3; no endpoint adoption. */
export const answerChildStatus = <Request>(input: {
  readonly parse: (value: unknown) => Request | null;
  readonly answer: (request: Request) => Promise<unknown>;
  readonly pipe?: Duplex;
}) => {
  const attempt = process.env[CHILD_STATUS_ATTEMPT_ENV];
  if (input.pipe === undefined && attempt === undefined) return () => undefined;
  const pipe =
    input.pipe ?? new Socket({ fd: 3, readable: true, writable: true });
  let answering = false;
  const close = frames(
    pipe,
    (value) => {
      const request = input.parse(value);
      if (request === null) throw new Error("invalid child challenge");
      // A timed-out caller can retry while the bounded old probe unwinds. No
      // additional work is started or queued; its next fresh challenge retries.
      if (answering || pipe.writableNeedDrain) return;
      answering = true;
      void input.answer(request).then(
        (response) => {
          answering = false;
          if (!writeFrame(pipe, response, close)) close();
        },
        () => close(),
      );
    },
    () => undefined,
  );
  return close;
};
