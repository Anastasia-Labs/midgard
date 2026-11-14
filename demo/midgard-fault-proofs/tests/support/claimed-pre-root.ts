/** A raw producer's claimed event pre-root: it replaces the pre-root in the
 * step-03 ledger state and the step-04 exclusion claim, while the step-03
 * transition membership stays the event's own. */
import type { PreparedInputExistence } from "../../src/input-existence/prepare.js";

export const withClaimedPreUtxosRoot = (
  prepared: PreparedInputExistence,
  preUtxosRoot: string,
): PreparedInputExistence => ({
  ...prepared,
  transitionMembership: {
    ...prepared.transitionMembership,
    value: {
      ...prepared.transitionMembership.value,
      pre_utxos_root: preUtxosRoot,
    },
  },
});
