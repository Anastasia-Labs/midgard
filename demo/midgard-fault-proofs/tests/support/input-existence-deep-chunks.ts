/**
 * Published proof chunks for the deep (64-level) event and transition
 * memberships of a forced input-existence lifecycle.
 *
 * A redeemer-carried counted membership travels in the step redeemer and in
 * its direct verifier's redeemer, so a maximum-depth proof carried that way
 * does not fit one transaction. The maximum shape publishes it as chunks and
 * the step reads them as reference inputs, as the registered max-carriage
 * lifecycle does. Indexed by step: step 1 opens the event, step 2 the
 * transition; the source and ledger steps carry their own proofs.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { PublishedProofChunk } from "../../src/proof-chunk-carriage.js";
import { publishProofChunks } from "../../src/publish-proof-chunks.js";
import { registerChunkedVerifyRewardAccount } from "./emulator/emulator-context.js";
import type { FaultProofEmulatorHarness } from "./emulator/harness.js";

export const deepProofChunks = async (
  harness: FaultProofEmulatorHarness,
  prepared: {
    readonly eventMembership: { readonly proof: SDK.Proof };
    readonly transitionMembership: { readonly proof: SDK.Proof };
  },
): Promise<readonly (readonly PublishedProofChunk[])[]> => {
  await registerChunkedVerifyRewardAccount(
    harness.funderLucid,
    harness.realBlueprint,
  );
  const publish = async (proof: SDK.Proof) =>
    (
      await publishProofChunks({
        lucid: harness.proverLucid,
        network: "Custom",
        signer: harness.proverSigner,
        proofCbor: Data.to(proof, SDK.Proof),
      })
    ).chunks;
  return [
    [],
    await publish(prepared.eventMembership.proof),
    await publish(prepared.transitionMembership.proof),
    [],
  ];
};
