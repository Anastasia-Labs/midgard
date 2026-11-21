import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAvailabilityChallengeAuthority,
} from "../runtime/deployment-identity.js";

/**
 * What a validation-trace dispute's VerifySource posts from the prover's
 * wallet, from the signed deployment: the manifest's dispute bond (the case
 * validator's applied bond). The case floor is the SDK's constant
 * `VALIDATION_CASE_FLOOR_LOVELACE`, and a move our wallet pays is priced by
 * `validationDisputeWalletMoveReserve`; a funding-adequacy check adds the
 * three. Any other category posts no bond and carries none.
 */
export const watcherValidationDisputeFundingTerms = (
  identity: VerifiedWatcherDeploymentIdentity,
  category: string,
): Readonly<{ validationDispute?: Readonly<{ bondLovelace: bigint }> }> =>
  category === "validationTraceDispute"
    ? {
        validationDispute: {
          bondLovelace: BigInt(
            watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
              .challengerBondLovelace,
          ),
        },
      }
    : {};
