import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";

import { isPublicDeploymentProfile } from "./listen-public-http.js";
import type { ResolvedTxStatus } from "./tx-status.js";

export type PublicSubmissionDiagnosticCode =
  | "E_INVALID_CANONICAL_TX"
  | "E_INVALID_NATIVE_TX"
  | "E_TX_ID_BYTES_CONFLICT"
  | "E_ADMISSION_WRITER_UNAVAILABLE";

export const publicTxStatusPresentation = (
  status: ResolvedTxStatus,
  profileName: string = SELECTED_DEPLOYMENT_PROFILE.name,
): ResolvedTxStatus => {
  if (!isPublicDeploymentProfile(profileName) || status.status !== "rejected")
    return status;
  const { reasonDetail: _diagnostic, ...facts } = status;
  return facts;
};

export const publicSubmissionDiagnostic = (
  detail: string,
  code: PublicSubmissionDiagnosticCode,
  profileName: string = SELECTED_DEPLOYMENT_PROFILE.name,
): string => (isPublicDeploymentProfile(profileName) ? code : detail);

export const includePrivateHttpDiagnosticDetails = (): boolean =>
  !isPublicDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name);
