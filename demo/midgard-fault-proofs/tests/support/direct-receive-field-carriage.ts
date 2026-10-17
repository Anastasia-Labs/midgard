import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type Emulator,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  captureEmulatorSubmission,
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

/** Execute the existing strict publication/certificate producers before the
 * receive worker opens a real maximum-sized field. No fabricated certificate. */
export const publishDirectReceiveFieldCarriage = async ({
  lucid,
  emulator,
  account,
  prover,
  plan,
  certificate,
  compactCbor,
  witnessSetCompactCbor,
  walletAddressType,
}: {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly account: { readonly address: string; readonly seedPhrase: string };
  readonly prover: string;
  readonly plan: FaultProofFieldOpeningPlan;
  readonly certificate: SDK.MintingValidator;
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly walletAddressType: "Base" | "Enterprise";
}): Promise<UTxO[]> => {
  if (plan.plan.tier === "Inline") return [];
  const signer = {
    source: "N6-emulator-account",
    address: account.address,
    paymentKeyHash: prover,
    selectWallet: (selected: LucidEvolution) =>
      selected.selectWallet.fromSeed(account.seedPhrase, {
        addressType: walletAddressType,
      }),
  };
  const publication = await captureEmulatorSubmission(emulator, () =>
    publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned: plan,
      publisherAddress: account.address,
      label: "N6 actual receive field",
    }),
  );
  console.log(
    "N6 actual chunk publication components",
    JSON.stringify(
      publication.measurements.map((m) => ({
        ...m,
        executionMemory: m.executionMemory.toString(),
        executionSteps: m.executionSteps.toString(),
      })),
    ),
  );
  console.log(
    "N6 actual chunk publication encoded body",
    JSON.stringify(
      publication.transactionCbors.map((cbor) => {
        const tx = CML.Transaction.from_cbor_hex(cbor);
        const body = tx.body();
        return {
          walletAddressType,
          bodyBytes: body.to_cbor_bytes().length,
          witnessSetBytes: tx.witness_set().to_cbor_bytes().length,
          outputs: Array.from({ length: body.outputs().len() }, (_, index) => {
            const output = body.outputs().get(index);
            const decoded = coreToTxOutput(output);
            return {
              encodedBytes: output.to_cbor_bytes().length,
              addressBytes: CML.Address.from_bech32(
                decoded.address,
              ).to_raw_bytes().length,
            };
          }),
        };
      }),
    ),
  );
  const references = [...publication.result];
  for (const measurement of publication.measurements)
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
  if (plan.plan.tier !== "Certified") return references;
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid,
      script: certificate.mintingScript,
      label: "N6 actual receive certificate",
    })
  ).utxo;
  const certified = await captureEmulatorSubmission(emulator, () =>
    certifyFaultProofFieldCarriage({
      lucid,
      network,
      signer,
      planned: plan,
      certificatePolicyId: certificate.policyId,
      certificateMintingScript: certificate.mintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos: references,
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  expect(certified.measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
  expect(certified.measurement.executionMemory).toBeLessThanOrEqual(
    13_200_000n,
  );
  expect(certified.measurement.executionSteps).toBeLessThanOrEqual(
    8_000_000_000n,
  );
  references.push(certified.result.certificateUtxo);
  console.log(
    "N6 actual receive field carriage",
    JSON.stringify({
      tier: plan.plan.tier,
      fieldBytes: plan.preimage.length,
      chunks: publication.measurements.map((m) => m.completeSignedBytes),
      certificate: {
        bytes: certified.measurement.completeSignedBytes,
        memory: certified.measurement.executionMemory.toString(),
        cpu: certified.measurement.executionSteps.toString(),
      },
    }),
  );
  return references;
};
