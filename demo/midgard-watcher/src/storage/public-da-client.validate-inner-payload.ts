import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";

import {
  PeerFailure,
  type PermitWaiter,
  WatcherPublicDaClientError,
  type WatcherPublicDaClock,
} from "./public-da-client.strict-inner-payload.js";

export const validateWithinDeadline = async (
  validate: (bytes: Buffer, signal: AbortSignal) => Promise<void>,
  bytes: Buffer,
  deadlineAt: number,
  lifecycle: {
    readonly clock: WatcherPublicDaClock;
    readonly controllers: Set<AbortController>;
    readonly isClosed: () => boolean;
  },
): Promise<void> => {
  if (lifecycle.isClosed()) throw new WatcherPublicDaClientError("closed");
  const remainingMs = deadlineAt - lifecycle.clock.now();
  if (remainingMs < 1)
    throw new PeerFailure(
      "deadline_exceeded",
      DaRequestResponseProtocol.payloadByHeader,
    );
  const controller = new AbortController();
  lifecycle.controllers.add(controller);
  let timer: unknown;
  let onAbort: (() => void) | undefined;
  try {
    await Promise.race([
      validate(bytes, controller.signal),
      new Promise<never>((_, reject) => {
        onAbort = () => reject(new WatcherPublicDaClientError("closed"));
        controller.signal.addEventListener("abort", onAbort, { once: true });
        timer = lifecycle.clock.setTimeout(() => {
          reject(
            new PeerFailure(
              "deadline_exceeded",
              DaRequestResponseProtocol.payloadByHeader,
            ),
          );
          controller.abort();
        }, remainingMs);
      }),
    ]);
  } finally {
    lifecycle.controllers.delete(controller);
    if (onAbort !== undefined)
      controller.signal.removeEventListener("abort", onAbort);
    if (timer !== undefined) lifecycle.clock.clearTimeout(timer);
  }
};

export const closePublicDaRequests = (
  controllers: Set<AbortController>,
  waiters: PermitWaiter[],
  clock: WatcherPublicDaClock,
): void => {
  for (const controller of controllers) controller.abort();
  for (const waiter of waiters.splice(0)) {
    clock.clearTimeout(waiter.timer);
    waiter.reject(new WatcherPublicDaClientError("closed"));
  }
};
