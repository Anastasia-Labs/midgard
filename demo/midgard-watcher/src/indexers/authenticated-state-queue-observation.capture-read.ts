import {
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  withLocalKupmiosReadOperation,
  withLocalKupmiosSourceCapture,
} from "@al-ft/midgard-fault-proofs";

import {
  WatcherStateQueueReadRetired,
  type WatcherStateQueueReadScopes,
} from "./authenticated-state-queue-observation.read-scopes.js";

type ReadAttempt = Readonly<{
  rawSource: LocalKupmiosFraudProofRawSource;
  assertCurrent(): void;
}>;

export const captureWatcherStateQueueRead = async <T>(input: {
  queueSource: LocalKupmiosFraudProofRawSource;
  observationDepth: "inclusion" | "release_finality";
  readScopes?: WatcherStateQueueReadScopes;
  read(attempt: ReadAttempt): Promise<T>;
}): Promise<T> => {
  const attempt = input.readScopes?.begin(input.observationDepth);
  const rawSource = attempt?.rawSource ?? input.queueSource;
  const assertCurrent = attempt?.assertCurrent ?? (() => {});
  const expected = localKupmiosHttpOgmiosRawSourceDetails(input.queueSource);
  const actual = localKupmiosHttpOgmiosRawSourceDetails(rawSource);
  try {
    if (
      expected === null ||
      actual === null ||
      expected.sourceId !== actual.sourceId ||
      expected.deploymentIdentityDigest !== actual.deploymentIdentityDigest ||
      expected.blueprintHash !== actual.blueprintHash ||
      expected.kupoHttpUrl !== actual.kupoHttpUrl ||
      expected.ogmiosUrl !== actual.ogmiosUrl ||
      expected.finalityPolicyDigest !== actual.finalityPolicyDigest ||
      expected.confirmationDepth !== actual.confirmationDepth ||
      expected.observationDepth !== actual.observationDepth
    )
      throw new Error("state-queue read attempt changed source authority");
    return await withLocalKupmiosSourceCapture(
      input.queueSource,
      async () => {
        let reads = 0;
        let lastError: unknown;
        const pending = new Set<Promise<T>>();
        const read = async (): Promise<T> => {
          for (;;) {
            assertCurrent();
            // This counts actual reads, including checkpoint and transport
            // retries together. Neither loop may renew a three-attempt budget.
            if (reads >= 3) throw lastError;
            reads += 1;
            try {
              const result = await input.read({ rawSource, assertCurrent });
              assertCurrent();
              return result;
            } catch (error) {
              lastError = error;
              if (
                !(error instanceof LocalKupmiosCheckpointChangedError) ||
                reads >= 3
              )
                throw error;
            }
          }
        };
        try {
          const result =
            attempt === undefined
              ? await read()
              : await withLocalKupmiosReadOperation(
                  rawSource,
                  ({ assertCurrent: assertTransportCurrent }) => {
                    assertTransportCurrent();
                    const operation = read();
                    pending.add(operation);
                    void operation.then(
                      () => pending.delete(operation),
                      () => pending.delete(operation),
                    );
                    return operation;
                  },
                  { scope: attempt.scope },
                );
          assertCurrent();
          return result;
        } finally {
          // The inner scope can reject before its callback settles. Keep the
          // stable ownership queue until all real callbacks/siblings drain.
          await Promise.allSettled([...pending]);
        }
      },
      attempt?.scope,
    );
  } catch (error) {
    if (
      attempt?.scope.signal.aborted === true &&
      error === attempt.scope.signal.reason
    )
      throw new WatcherStateQueueReadRetired(error);
    throw error;
  } finally {
    attempt?.close();
  }
};
