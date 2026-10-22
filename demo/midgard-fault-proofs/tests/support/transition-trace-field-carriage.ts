import type { MintingValidator } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../../src/runtime.js";
import {
  transitionL2Replay,
  type TransitionProofInput,
} from "../../src/transition-trace/proof-material.js";
import { prepareTransitionFieldCarriage } from "../../src/transition-trace/submit.transition-field-step.js";
import type { OwnOutputJournal } from "../../src/workflow/own-outputs.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";

/** Publish the field-preimage certificate minting script as a reference. */
export const publishTransitionFieldCertificateReference = async (
  lucid: LucidEvolution,
  certificate: MintingValidator,
): Promise<UTxO> =>
  (
    await publishPlainReferenceScriptUtxo({
      lucid,
      script: certificate.mintingScript,
      label: "transition-trace field certificate",
    })
  ).utxo;

/**
 * Publish (and, at tier 3, certify) both field carriages of an accepted
 * transaction ahead of steps that are captured without submitting, which
 * only resolve existing publications. Deposits carry no field.
 */
export const publishTransitionFieldCarriages = async ({
  lucid,
  network,
  signer,
  proof,
  certificate,
  certificateReference,
  ownOutputs,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly proof: TransitionProofInput;
  readonly certificate: MintingValidator;
  readonly certificateReference: UTxO;
  /** The claims the steps that follow read the carriage back by. */
  readonly ownOutputs: OwnOutputJournal;
}): Promise<void> => {
  if (transitionL2Replay(proof) === undefined) return;
  for (const field of [0, 2] as const)
    await prepareTransitionFieldCarriage({
      lucid,
      network,
      signer,
      proof,
      field,
      certificatePolicyId: certificate.policyId,
      certificateMintingScript: certificate.mintingScript,
      certificateReferenceScript: () => Promise.resolve(certificateReference),
      publish: true,
      ownOutputs,
    });
};
