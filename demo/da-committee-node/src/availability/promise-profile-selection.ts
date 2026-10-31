import type { CommitteeConfig } from "../config.js";
import type { CommitteeStore } from "../store.js";
import type { CommitteePromiseAdmission } from "./promise-admission.js";

/** The adopted-only singleton survives missing policy configuration on restart. */
export const committeePromiseProfileRequired = async (
  config: Pick<CommitteeConfig, "availabilityPromiseAdoption">,
  store: Pick<CommitteeStore, "getRetirementFloor">,
): Promise<boolean> =>
  config.availabilityPromiseAdoption !== undefined ||
  (await store.getRetirementFloor()) !== undefined;

export const assertCommitteePromiseEnrollment = async (
  config: Pick<CommitteeConfig, "availabilityPromiseAdoption">,
  store: Pick<CommitteeStore, "getRetirementFloor">,
): Promise<void> => {
  if (
    config.availabilityPromiseAdoption === undefined &&
    (await store.getRetirementFloor()) !== undefined
  )
    throw new Error("Adopted committee promise profile is missing on restart");
};

export const committeePromisePolicyStatus = async (deps: {
  config: Pick<CommitteeConfig, "availabilityPromiseAdoption">;
  store: Pick<CommitteeStore, "getRetirementFloor">;
  promiseAdmission?: CommitteePromiseAdmission;
}) =>
  deps.promiseAdmission?.policyStatus() ??
  ((await committeePromiseProfileRequired(deps.config, deps.store))
    ? {
        status: "unavailable" as const,
        reason: "finite_runtime_policy_unavailable",
      }
    : undefined);
