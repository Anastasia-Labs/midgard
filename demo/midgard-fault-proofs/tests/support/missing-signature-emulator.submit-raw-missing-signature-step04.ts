import { encodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  planMissingSignatureAddressWitnessesOpening,
  requireMissingSignatureStepState,
  requireMissingSignatureThreadUtxo,
} from "../../src/missing-signature/index.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import {
  makeMissingSignatureEmulatorHarness,
  type MissingSignatureScenario,
} from "./missing-signature-emulator.build-missing-signature-subject.js";

/** Publish and mint the §8.6 material for a genuinely tier-3 field-7 proof. */
export const publishMissingSignatureField07Certificate = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly scenario: MissingSignatureScenario;
}): Promise<UTxO> => {
  const planned = planMissingSignatureAddressWitnessesOpening({
    anchorSourceKind: 0n,
    anchorTxId: scenario.block.nativeTxId,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    addrTxWits: scenario.subject.addrTxWits,
    witnessSet: scenario.subject.witnessSetCompact,
    anchorWitnessSetHash:
      scenario.subject.nativeTx.compact.transactionWitnessSetHash.toString(
        "hex",
      ),
    owner: harness.proverSigner.paymentKeyHash,
  });
  if (planned.plan.tier !== "Certified") {
    throw new Error(
      `fat missing-signature fixture selected ${planned.plan.tier}, not Certified`,
    );
  }
  harness.proverSigner.selectWallet(harness.proverLucid);
  const chunkUtxos = await publishFaultProofFieldCarriage({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "missing-signature tier-3 field-7",
  });
  const certificate = harness.contracts.fieldPreimageCertificate;
  const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompact({
    addrTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.addr_tx_wits_hash,
      "hex",
    ),
    scriptTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.script_tx_wits_hash,
      "hex",
    ),
    redeemerTxWitsHash: Buffer.from(
      scenario.subject.witnessSetCompact.redeemer_tx_wits_hash,
      "hex",
    ),
  }).toString("hex");
  const unsigned = await Effect.runPromise(
    SDK.buildUnsignedFieldPreimageCertificationProgram(harness.proverLucid, {
      sourceKind: 0n,
      plan: planned.plan,
      certificatePolicyId: certificate.policyId,
      certificateAddress: certificate.spendingScriptAddress,
      certificateWitness: {
        kind: "inline_emulator_only",
        certificateScript: certificate.mintingScript,
      },
      chunkUtxos,
      compactCbor: scenario.block.nativeTxCompactCbor,
      witnessSetCompactCbor,
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  const certificateUtxo = (
    await harness.proverLucid.utxosAt(certificate.spendingScriptAddress)
  ).find((utxo) => utxo.txHash === txHash);
  if (certificateUtxo === undefined) {
    throw new Error("missing-signature §8.6 certificate output was not found");
  }
  return certificateUtxo;
};

/**
 * Raw guard-bypassing finalizer for the adversarial polarity. It duplicates
 * the production transaction shape but deliberately omits the local absence
 * check, so an honest witness reaches step-04's on-chain fold.
 */
export const submitRawMissingSignatureStep04 = async ({
  harness,
  threadOutRef,
  scenario,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingSignatureEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly scenario: MissingSignatureScenario;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.missingSignature,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
  });
  const state = requireMissingSignatureStepState({
    threadUtxo,
    signer: harness.proverSigner,
    schema: SDK.MissingSignatureStep04Datum,
    stepIndex: 3,
  });
  const planned = planMissingSignatureAddressWitnessesOpening({
    anchorSourceKind: 0n,
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    addrTxWits: scenario.subject.addrTxWits,
    witnessSet: scenario.subject.witnessSetCompact,
    anchorWitnessSetHash: state.verified_witness_set_hash,
    owner: harness.proverSigner.paymentKeyHash,
  });
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    planned,
    publisherAddress: harness.proverSigner.address,
    label: "raw missing-signature step-04",
  });
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.missingSignature.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-signature step-04 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: harness.missingSignature.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw missing-signature step-04 fraud-proof mint",
  });
  const referenceInputs = [
    referenceScriptUtxo,
    ...computationThreadCarriage.referenceInputs,
    ...fraudProofCarriage.referenceInputs,
    ...carriageUtxos,
  ];
  const opening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId:
      harness.missingSignature.fieldPreimageCertificatePolicyId,
    label: "raw missing-signature step-04",
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const walletUtxos = await harness.proverLucid.wallet().getUtxos();
  const candidates = [...carriageUtxos].reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const feeInput = selectFeeInput(candidates);
  const proofUnit = toUnit(
    harness.missingSignature.fraudProof.policyId,
    threadToken.assetName,
  );
  const proofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.missingSignature.fraudProof.spendingScriptAddress,
    datum: proofDatum,
    unit: proofUnit,
  });
  const spend = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            Finalize: {
              input_index: SDK.requireInputIndex(
                ctx,
                threadUtxo,
                "raw missing-signature step-04",
              ),
              output_index: SDK.requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw missing-signature fraud-proof output",
              ),
              fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
                ctx,
                harness.missingSignature.fraudProof.policyId,
                "raw missing-signature fraud-proof mint",
              ),
              addr_tx_wits_opening: opening,
              checkpoint_cbor: null,
            },
          },
        ],
      },
      SDK.MissingSignatureStep04SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      harness.missingSignature.computationThread.policyId,
      "raw missing-signature thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          harness.missingSignature.computationThread.policyId,
          "raw missing-signature thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const base = harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .readFrom(referenceInputs)
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [proofUnit]: 1n }, mint)
    .pay.ToContract(
      harness.missingSignature.fraudProof.spendingScriptAddress,
      { kind: "inline", value: proofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [proofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};
