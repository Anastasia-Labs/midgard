import { planMidgardFieldCarriage } from "@al-ft/midgard-core";
import {
  buildUnsignedFieldPreimageCertificationProgram,
  buildUnsignedFieldPreimagePublicationProgram,
  createReferenceScriptAuthPolicy,
  fieldPreimagePublicationDatumCbor,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ResolvedProverSigner } from "../../../src/runtime.js";
import { SCRIPT_SOURCES_OBSERVER_YIELD_ROLES } from "../../../src/validation-dispute/script-sources-yields.js";
import {
  publishAuthenticatedValidationDisputeControl,
  publishPlainReferenceScriptUtxo,
} from "./reference-scripts.js";

export const PHASE_A_ITEM_YIELD_SPECS = [
  {
    contract: "phaseANativeItemNative",
    deployment: "validationTraceDisputePhaseANativeItemNativeWithdraw",
    role: "V1 validation-trace phase-A native item native yield",
  },
  {
    contract: "phaseANativeItemForeign",
    deployment: "validationTraceDisputePhaseANativeItemForeignWithdraw",
    role: "V1 validation-trace phase-A native item foreign yield",
  },
] as const;

/** Publish all evidence before freezing positional carriage in prepare-selected. */
export const preparePhaseAItemCarriage = async ({
  lucid,
  signer,
  chain,
  certificate,
  authPolicy,
  trace,
  stateIndex,
  source,
  kind,
}: {
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  chain: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
  certificate: {
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  };
  authPolicy: ReturnType<typeof createReferenceScriptAuthPolicy>;
  trace: DeterministicValidationMachineTrace;
  stateIndex: number;
  source: { compact_cbor: string; witness_set_compact_cbor: string };
  kind: "native" | "foreign" | "observer" | "redeemer";
}) => {
  const scriptSourcesObserver =
    kind === "observer" && trace.states[stateIndex]!.phase === "scriptSources";
  const fieldIndex = kind === "redeemer" ? 8 : kind === "observer" ? 3 : 6;
  const auxiliary = trace.witnesses[stateIndex]!.auxiliary;
  if (
    (auxiliary?.kind !== "transactionFieldChunk" &&
      auxiliary?.kind !== "transactionRedeemerItemBegin") ||
    auxiliary.fieldIndex !== fieldIndex
  )
    throw new Error(
      "phase-A item fixture requires a matching field item witness",
    );
  if (auxiliary.fieldPreimage.length !== (kind === "observer" ? 32763 : 32768))
    throw new Error(
      "maximum fixture must reach the exact field-specific byte bound",
    );
  const plan = planMidgardFieldCarriage({
    owner: Buffer.from(signer.paymentKeyHash, "hex"),
    txId: trace.states[stateIndex]!.transactionId,
    fieldIndex,
    preimage: auxiliary.fieldPreimage,
  });
  if (plan.tier !== "Certified")
    throw new Error("maximum phase-A fixture must use Certified carriage");
  signer.selectWallet(lucid);
  const semanticPublication = await publishPlainReferenceScriptUtxo({
    lucid,
    script:
      chain.semanticResolvers[
        scriptSourcesObserver
          ? 57
          : kind === "redeemer"
            ? 47
            : kind === "observer"
              ? 25
              : 11
      ].spendingScript,
    label: "phase-A item semantic",
  });
  const yields = [];
  for (const spec of scriptSourcesObserver
    ? SCRIPT_SOURCES_OBSERVER_YIELD_ROLES
    : kind === "observer" || kind === "redeemer"
      ? []
      : PHASE_A_ITEM_YIELD_SPECS) {
    const contract = chain.yields[spec.contract];
    const publication = await publishAuthenticatedValidationDisputeControl({
      lucid,
      authPolicy,
      target: {
        control: spec.contract,
        name: spec.role,
        script: contract.withdrawalScript,
      },
    });
    yields.push({ spec, contract, publication });
  }
  const chunks: UTxO[] = [];
  for (const chunk of plan.publications) {
    const datumCbor = fieldPreimagePublicationDatumCbor(chunk.bytes);
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationProgram(lucid, {
        publisherAddress: signer.address,
        publication: {
          chunkIndex: chunk.chunkIndex,
          datumCbor,
          byteLength: chunk.bytes.length,
          digestHex: chunk.digest.toString("hex"),
        },
      }),
    );
    const signed = await unsigned.sign.withWallet().complete();
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    const output = (await lucid.utxosAt(signer.address)).find(
      (u) => u.txHash === hash && u.datum === datumCbor,
    );
    if (output === undefined)
      throw new Error("missing exact field publication");
    chunks.push(output);
  }
  const certificateReference = await publishPlainReferenceScriptUtxo({
    lucid,
    script: certificate.mintingScript,
    label: "phase-A field certificate",
  });
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimageCertificationProgram(lucid, {
      sourceKind: trace.states[stateIndex]!.sourceKind === "forced" ? 1n : 0n,
      plan,
      certificatePolicyId: certificate.policyId,
      certificateAddress: certificate.spendingScriptAddress,
      certificateWitness: {
        kind: "reference_script",
        referenceUtxo: certificateReference.utxo,
      },
      chunkUtxos: chunks,
      compactCbor: source.compact_cbor,
      witnessSetCompactCbor: source.witness_set_compact_cbor,
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  const certificateUtxo = (
    await lucid.utxosAt(certificate.spendingScriptAddress)
  ).find((u) => u.txHash === hash);
  if (certificateUtxo === undefined)
    throw new Error("missing certified field output");
  const material = {
    plan,
    referenceUtxos: [...chunks, certificateUtxo],
    certificatePolicyId: certificate.policyId,
  };
  const referenceInputs = [
    ...material.referenceUtxos,
    semanticPublication.utxo,
    ...(scriptSourcesObserver
      ? yields.map((y) => y.publication.utxo)
      : kind === "observer" || kind === "redeemer"
        ? []
        : [yields[kind === "native" ? 0 : 1]!.publication.utxo]),
  ];
  const carriage = resolveMidgardFieldCarriageAgainstReferenceInputs({
    plan,
    referenceInputs,
    certificatePolicyId: certificate.policyId,
  });
  return {
    semanticPublication,
    yields,
    material,
    resolveFieldCarriage: (input: {
      fieldIndex: number;
      fieldPreimage: Buffer;
    }) => {
      if (
        input.fieldIndex !== fieldIndex ||
        !input.fieldPreimage.equals(auxiliary.fieldPreimage)
      )
        throw new Error("phase-A field source changed");
      return carriage;
    },
  };
};
