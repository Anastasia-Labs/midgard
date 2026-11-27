import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

/** Synthetic parameters for raw snapshot unit fixtures, not applied contracts. */
export const TRANSITION_HISTORY_FIXTURE_PARAMETERS = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
  retentionAddresses: {
    deposit: credentialToAddress(
      "Preprod",
      scriptHashToCredential("70".repeat(28)),
    ),
    withdrawal: credentialToAddress(
      "Preprod",
      scriptHashToCredential("71".repeat(28)),
    ),
  },
};
