import type { AdmittedHistoryChild } from "./history-child-admission.js";

/** A typed intrinsic refusal ends its owning role; transient proof failure does not. */
export const waitHistoryChildLifetime = async (
  child: AdmittedHistoryChild,
  signal?: AbortSignal,
) => {
  let stop: () => void = () => undefined;
  const signalled = new Promise<void>((resolve) => {
    stop = () => resolve();
    if (signal?.aborted) resolve();
    else signal?.addEventListener("abort", stop, { once: true });
  });
  process.once("SIGTERM", stop);
  process.once("SIGINT", stop);
  try {
    const refusal = await Promise.race([signalled, child.refusal]);
    if (refusal !== undefined) throw refusal;
  } finally {
    signal?.removeEventListener("abort", stop);
    process.removeListener("SIGTERM", stop);
    process.removeListener("SIGINT", stop);
  }
};
