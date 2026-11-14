/** Serial replay operations own source captures/checkpoint writes until their store closes. */
export const createJourneyReplayLifetime = () => {
  let closed = false;
  let tail: Promise<void> | undefined;
  let closePromise: Promise<void> | undefined;
  return {
    isClosed: () => closed,
    run: <T>(read: () => Promise<T>): Promise<T> => {
      if (closed)
        return Promise.reject(new Error("Journey replay host is closed"));
      const invoke = async () => {
        if (closed) throw new Error("Journey replay host is closed");
        return read();
      };
      const result = tail === undefined ? invoke() : tail.then(invoke);
      const joined = result.then(
        () => {},
        () => {},
      );
      tail = joined;
      void joined.then(() => {
        if (tail === joined) tail = undefined;
      });
      return result;
    },
    close: (stop: () => Promise<void>) =>
      (closePromise ??= (async () => {
        closed = true;
        await tail;
        await stop();
      })()),
  };
};
