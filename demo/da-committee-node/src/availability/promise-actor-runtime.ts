import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

/** Owns unsigned callbacks beyond a refusal race. Successful resource duration
 * remains conditional; expiry disables the adopted capability immediately. */
export const committeePromiseActorRuntime = (
  breach: (reason: string) => void,
) => {
  let unsignedCallbacks = 0;
  const drained = new Set<() => void>();
  const assertIdle = (): void => {
    if (unsignedCallbacks !== 0)
      throw new Error("Actor has an unsigned callback that has not drained");
  };
  const trackUnsigned = async <T>(
    scope: DaAvailabilityReadScope,
    build: () => Promise<T>,
  ): Promise<T> => {
    scope.assertCurrent();
    assertIdle();
    unsignedCallbacks += 1;
    const expired = () => breach("unsigned_actor_attempt_expired");
    scope.signal.addEventListener("abort", expired, { once: true });
    try {
      const result = await build();
      scope.assertCurrent();
      return result;
    } finally {
      scope.signal.removeEventListener("abort", expired);
      unsignedCallbacks -= 1;
      for (const resolve of drained) resolve();
      drained.clear();
    }
  };
  return {
    assertIdle,
    trackUnsigned,
    join: async () => {
      while (unsignedCallbacks !== 0)
        await new Promise<void>((resolve) => drained.add(resolve));
    },
  };
};
