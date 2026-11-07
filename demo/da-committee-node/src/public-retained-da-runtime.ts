export type PublicRetainedDaRuntimeResource = {
  close?(): Promise<void> | void;
  stop?(): Promise<void> | void;
};

/** Always attempts both shutdown sides and preserves every reported failure. */
export const stopPublicRetainedDaRuntime = async ({
  listener,
  store,
}: {
  readonly listener: Required<Pick<PublicRetainedDaRuntimeResource, "stop">>;
  readonly store: Required<Pick<PublicRetainedDaRuntimeResource, "close">>;
}): Promise<void> => {
  const results = await Promise.allSettled([listener.stop(), store.close()]);
  const failures = results
    .filter(
      (result): result is PromiseRejectedResult => result.status === "rejected",
    )
    .map((result) => result.reason);
  if (failures.length > 0) {
    throw new AggregateError(
      failures,
      "public retained DA runtime shutdown failed",
    );
  }
};
