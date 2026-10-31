import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type {
  CommitteePromiseAdmissionResult,
  CommitteePromiseAdmissionSource,
} from "./promise-admission.js";

type DecisionIdentity = Pick<
  CommitteePromiseAdmissionResult["decision"],
  "deploymentId" | "candidateHeaderHash" | "candidateCommitmentDigest"
>;

/** Refusals abort first, then join the source's store and physical owners. */
export const committeeAdmissionReads = (
  source: CommitteePromiseAdmissionSource,
) => {
  const refuse = async (scope?: DaAvailabilityReadScope): Promise<void> => {
    scope?.close();
    if (scope) await source.drainReadResources?.(scope);
  };
  return {
    refuse,
    incomplete:
      (
        identity: DecisionIdentity,
        scope: () => DaAvailabilityReadScope | undefined,
      ) =>
      async (reason: string): Promise<CommitteePromiseAdmissionResult> => {
        await refuse(scope());
        return {
          decision: { ...identity, status: "incomplete_evidence", reason },
        };
      },
    final: async (
      scope: DaAvailabilityReadScope | undefined,
      read: () => Promise<void>,
    ): Promise<void> => {
      try {
        await read();
      } catch (error) {
        await refuse(scope);
        throw error;
      } finally {
        scope?.close();
      }
    },
  };
};
