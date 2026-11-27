import type { CommitteeRetirementSnapshot } from "./retirement-model.js";
import type { CommitteeRetirementPlan } from "./retirement-transition.js";

export type CommitteeRetirementCertificate = Readonly<{
  readonly retirementCertificate: unique symbol;
}>;
type Verified = Readonly<{
  snapshot: CommitteeRetirementSnapshot;
  plan: CommitteeRetirementPlan;
  assertCurrent: () => Promise<void>;
  assertScopeCurrent: () => void;
}>;
const certificates = new WeakMap<object, Verified>();
/** Internal source minting seam; no wire value or persisted boolean is a certificate. */
export const mintRetirementCertificate = (
  verified: Verified,
): CommitteeRetirementCertificate => {
  const token = Object.freeze({}) as CommitteeRetirementCertificate;
  certificates.set(token, verified);
  return token;
};
export const readRetirementCertificate = (
  token: CommitteeRetirementCertificate,
): Verified => {
  const verified = certificates.get(token);
  if (!verified)
    throw new Error(
      "Retirement requires a current locally verified source certificate",
    );
  return verified;
};
export const consumeRetirementCertificate = (
  token: CommitteeRetirementCertificate,
): void => {
  certificates.delete(token);
};
