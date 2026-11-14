import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type Emulator,
  type EmulatorAccount,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { resolveProverSigner } from "../../src/runtime.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import {
  captureEmulatorSubmission,
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

/** The address production publishes carriage from: the prover signer's. */
const carriagePublisher = (seedPhrase: string) =>
  resolveProverSigner({ network, walletSeedPhrase: seedPhrase });

/** Ledger funding for the carriage publisher. A Base-wallet row pays its steps
 * from the Base address, but carriage is published from the Enterprise address
 * production resolves, so that address is funded too. */
export const directCarriagePublisherFunding = (
  account: EmulatorAccount,
): readonly EmulatorAccount[] => {
  const address = carriagePublisher(account.seedPhrase).address;
  return address === account.address ? [] : [{ ...account, address }];
};

/** Execute the existing strict publication/certificate producers before the
 * receive worker opens a real maximum-sized field. No fabricated certificate.
 * Publication and certification run as the production prover signer
 * (`resolveProverSigner`, an Enterprise address); the caller's wallet, of
 * either address type, is selected again afterwards for the step transactions. */
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
  const signer = carriagePublisher(account.seedPhrase);
  expect(signer.paymentKeyHash).toBe(prover);
  try {
    return await publishAsProverSigner({
      lucid,
      emulator,
      signer,
      plan,
      certificate,
      compactCbor,
      witnessSetCompactCbor,
      walletAddressType,
    });
  } finally {
    lucid.selectWallet.fromSeed(account.seedPhrase, {
      addressType: walletAddressType,
    });
  }
};

const publishAsProverSigner = async ({
  lucid,
  emulator,
  signer,
  plan,
  certificate,
  compactCbor,
  witnessSetCompactCbor,
  walletAddressType,
}: {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly signer: ReturnType<typeof carriagePublisher>;
  readonly plan: FaultProofFieldOpeningPlan;
  readonly certificate: SDK.MintingValidator;
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly walletAddressType: "Base" | "Enterprise";
}): Promise<UTxO[]> => {
  const ownOutputs = createOwnOutputJournal();
  const publication = await captureEmulatorSubmission(emulator, () =>
    publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned: plan,
      publisherAddress: signer.address,
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
          publisherAddressBytes: CML.Address.from_bech32(
            signer.address,
          ).to_raw_bytes().length,
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
      ownOutputs,
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
      walletAddressType,
      fieldBytes: plan.preimage.length,
      chunkBytes: plan.plan.publications.map((p) => p.bytes.length),
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
