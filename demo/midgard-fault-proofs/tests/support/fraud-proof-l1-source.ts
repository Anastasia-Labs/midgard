import {
  FRAUD_PROOF_L1_SOURCE,
  type FraudProofL1Source,
} from "../../src/workflow/l1-source.js";
import type { FraudProofRawL1SnapshotAuthority } from "../../src/workflow/raw-l1-snapshot.js";

/**
 * An L1 source over a test's raw snapshot authority. `authority` is read on
 * every capture, so a test may swap the transport after construction.
 */
export const fraudProofL1SourceForTest = ({
  authority,
}: Readonly<{
  authority: () => unknown;
}>): FraudProofL1Source => ({
  sourceVersion: FRAUD_PROOF_L1_SOURCE,
  snapshotAuthority: () => {
    const current = (): FraudProofRawL1SnapshotAuthority =>
      authority() as FraudProofRawL1SnapshotAuthority;
    return {
      get authorityVersion() {
        return current().authorityVersion;
      },
      capture: (request, held) => current().capture(request, held),
    };
  },
});
