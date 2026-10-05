import {
  type LocalKupmiosFraudProofRawSource,
  withLocalKupmiosReadOperation,
  withLocalKupmiosSourceCapture,
} from "@al-ft/midgard-fault-proofs";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

/** Only the outer owning read retries. Callers reset readBoundary before
 * exact-point repinning and settle every sibling before rejecting a capture. */
export const withWatcherAvailabilityReadOperation = <T>(
  source: LocalKupmiosFraudProofRawSource,
  scope: DaAvailabilityReadScope | undefined,
  read: (assertCurrent: () => void) => Promise<T>,
): Promise<T> =>
  scope === undefined
    ? withLocalKupmiosSourceCapture(source, () => read(() => {}))
    : withLocalKupmiosReadOperation(
        source,
        ({ assertCurrent }) => read(assertCurrent),
        { scope },
      );
