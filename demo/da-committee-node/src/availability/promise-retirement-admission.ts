import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type { StateQueueHeaderRecord } from "../domain.js";
import type {
  CommitteeRetirementGuard,
  CommitteeRetirementPort,
} from "../store/retirement-model.js";

export type CommitteePromiseSigningBoundaryIdentity = Readonly<{
  actorStateDigest?: string;
  schedulingEvidenceDigest?: string;
  retirementGuard?: CommitteeRetirementGuard;
}>;

/** The store owns the opaque guard. No admission caller can synthesize floor
 * authority or cross its generation while an asynchronous proof is running. */
export const committeePromiseRetirementAdmission = (
  port?: CommitteeRetirementPort,
) => ({
  capture: async (
    scope?: DaAvailabilityReadScope,
  ): Promise<CommitteeRetirementGuard | undefined> => {
    if (!port) return undefined;
    if (!scope)
      throw new Error("Retirement proof requires the admission scope");
    return port.capture(scope);
  },
  prove: async (
    token?: CommitteeRetirementGuard,
    scope?: DaAvailabilityReadScope,
    record?: StateQueueHeaderRecord,
  ): Promise<() => void> => {
    if (!port) return () => undefined;
    if (!scope || !token)
      throw new Error("Captured retirement authority is unavailable");
    await port.prove(token, scope);
    return () => {
      if (!record)
        throw new Error(
          "Authenticated retirement signing candidate is unavailable",
        );
      port.assert(token, record);
    };
  },
});
