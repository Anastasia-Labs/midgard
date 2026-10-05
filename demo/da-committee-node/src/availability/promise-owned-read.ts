import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

/** Store reads retain callback ownership until completion. Expiry fences the
 * result; it does not cancel filesystem work or a shared database connection. */
export const committeePromiseOwnedRead =
  (scope?: DaAvailabilityReadScope) =>
  async <T>(callback: () => Promise<T>): Promise<T> => {
    scope?.assertCurrent();
    const result = await callback();
    scope?.assertCurrent();
    return result;
  };

/** An early sibling failure cannot abandon another owned read. */
export const committeePromiseJoinedReads = async <
  T extends readonly unknown[],
>(reads: { [K in keyof T]: Promise<T[K]> }): Promise<T> => {
  const results = await Promise.allSettled(reads);
  const failure = results.find((result) => result.status === "rejected");
  if (failure?.status === "rejected") throw failure.reason;
  return results.map((result) => {
    if (result.status === "rejected") throw result.reason;
    return result.value;
  }) as unknown as T;
};

/** SDK observation can race a store-health callback. Its configured owner joins
 * it before closing the enclosing attempt or releasing its actor. */
export const committeePromiseStoreReadOwner = () => {
  const pending = new Set<Promise<unknown>>();
  return {
    read: async <T>(callback: () => Promise<T>): Promise<T> => {
      const read = Promise.resolve().then(callback);
      pending.add(read);
      try {
        return await read;
      } finally {
        pending.delete(read);
      }
    },
    join: async () => {
      while (pending.size) await Promise.allSettled([...pending]);
    },
  };
};
