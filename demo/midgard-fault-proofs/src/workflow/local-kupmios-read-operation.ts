import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import { LocalKupmiosTransportUnavailableError } from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  type LocalKupmiosFraudProofRawSource,
  withLocalKupmiosSourceCapture,
} from "./local-kupmios-raw-l1-authority.scan-all-address-utxos.js";

export class LocalKupmiosReadGenerationExpiredError extends Error {
  constructor() {
    super("Local Kupmios read generation is no longer current");
    this.name = "LocalKupmiosReadGenerationExpiredError";
  }
}

export type LocalKupmiosReadAttempt = Readonly<{
  generation: number;
  assertCurrent: () => void;
}>;

/** Retry an entire read-only capture. Every invocation must start readBoundary
 * again, discard all unpublished evidence, and settle sibling reads on failure.
 * Never wrap signing, persistence, rebroadcast, or another mutation. The concrete
 * source must receive scope.signal so expiry cancels its actual transports. */
export const withLocalKupmiosReadOperation = <T>(
  source: LocalKupmiosFraudProofRawSource,
  read: (attempt: LocalKupmiosReadAttempt) => Promise<T>,
  options: Readonly<{
    scope: DaAvailabilityReadScope;
    maximumAttempts?: number;
  }>,
): Promise<T> => {
  const maximumAttempts = options.maximumAttempts ?? 3;
  if (
    !Number.isSafeInteger(maximumAttempts) ||
    maximumAttempts <= 0 ||
    maximumAttempts > 3
  )
    throw new Error(
      "Local Kupmios read attempts must be between one and three",
    );
  const { scope } = options;
  return withLocalKupmiosSourceCapture(
    source,
    async () => {
      let activeGeneration = -1;
      try {
        for (let generation = 0; generation < maximumAttempts; generation++) {
          activeGeneration = generation;
          const assertCurrent = () => {
            scope.assertCurrent();
            if (activeGeneration !== generation)
              throw new LocalKupmiosReadGenerationExpiredError();
          };
          assertCurrent();
          try {
            const result = await read(
              Object.freeze({ generation, assertCurrent }),
            );
            assertCurrent();
            return result;
          } catch (error) {
            assertCurrent();
            if (
              !(error instanceof LocalKupmiosTransportUnavailableError) ||
              generation + 1 === maximumAttempts
            )
              throw error;
          }
        }
        throw new Error("Local Kupmios read attempt limit exhausted");
      } finally {
        activeGeneration = -1;
      }
    },
    scope,
  );
};
