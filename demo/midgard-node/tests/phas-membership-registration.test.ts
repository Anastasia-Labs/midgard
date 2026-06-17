import { describe, expect, it } from "vitest";

import { isPhasMembershipAlreadyRegisteredError } from "@/transactions/phas-membership-registration.js";
import { TxSubmitError } from "@/transactions/utils.js";

describe("PHAS membership reward registration", () => {
  it("treats Ogmios knownCredential stake registration failures as idempotent", () => {
    const scriptHash = "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d";
    const providerError = {
      jsonrpc: "2.0",
      method: "submitTransaction",
      error: {
        code: 3145,
        message:
          "Trying to re-register some already known credentials. Stake credentials can only be registered once. This is true for both keys and scripts. The field 'data.knownCredential' points to an already known credential that's being re-registered by this transaction.",
        data: {
          from: "script",
          knownCredential: scriptHash,
        },
      },
    };
    const error = new TxSubmitError({
      message: `Failed to submit transaction: ${JSON.stringify(providerError)}`,
      cause: "knownCredential",
      txHash: "00".repeat(32),
    });

    expect(isPhasMembershipAlreadyRegisteredError(error, scriptHash)).toBe(true);
  });
});
