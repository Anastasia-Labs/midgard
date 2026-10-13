import * as SDK from "@al-ft/midgard-sdk";
import { placeholderValidationTraceDescriptorV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

/**
 * A placeholder validation descriptor for payload structure tests. The
 * transaction id stands in for the transaction commitment: committee coverage
 * checks the descriptor's shape and verdict, not its state hashes.
 */
export const fixtureValidationTrace = (
  eventKey: SDK.EventKey,
  transactionId: string,
  verdict: "accepted" | "rejected" = "accepted",
  rejectionCode: string | null = null,
): { readonly entry: SDK.DaPayloadEntry } => {
  const key = Data.to(eventKey, SDK.EventKey);
  const descriptor = SDK.validationTraceDescriptorDataFromCore(
    placeholderValidationTraceDescriptorV1(
      {
        eventKeyCbor: Buffer.from(key, "hex"),
        transactionCommitment: Buffer.from(transactionId, "hex"),
      },
      verdict,
      rejectionCode,
    ),
  );
  return { entry: [key, Data.to(descriptor, SDK.ValidationTraceDescriptor)] };
};
