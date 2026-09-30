import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  requireFaultProofStepReferenceScript,
  resolveInvalidSignatureDeploymentContracts,
} from "../../src/runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import {
  type InvalidSignatureEmulatorHarness,
  type InvalidSignatureSubject,
} from "./invalid-signature-emulator.build-invalid-signature-subject.js";
import { network } from "./submit-init-emulator-shared.js";

/**
 * Raw guard-bypassing finalizer for the adversarial polarity.
 *
 * It reproduces `submitInvalidSignatureStep02`'s transaction shape exactly —
 * the same §8.8 opening, the same complete reference-input set, the same burn /
 * mint / output layout — and omits one thing: the local
 * `verifyAddressWitness(...)` refusal. An honest witness therefore reaches
 * step-02's on-chain `verify_ed25519_signature(...) == False`, which is the
 * check that must refuse the attack.
 */
export const submitRawInvalidSignatureStep02 = async ({
  harness,
  deploymentInfo,
  threadOutRef,
  subject,
  referenceScriptUtxo,
  badAddrTxWitIndex,
}: {
  readonly harness: Pick<
    InvalidSignatureEmulatorHarness,
    "proverLucid" | "proverSigner" | "realBlueprint" | "witnessReferenceScripts"
  >;
  readonly deploymentInfo: unknown;
  readonly threadOutRef: string;
  readonly subject: InvalidSignatureSubject;
  readonly referenceScriptUtxo: UTxO;
  readonly badAddrTxWitIndex?: bigint;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const { invalidSignatureCategory, contracts } =
    await resolveInvalidSignatureDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const [txHashPart, outputIndexPart] = threadOutRef.split("#");
  if (txHashPart === undefined || outputIndexPart === undefined) {
    throw new Error(`Malformed thread out-ref ${threadOutRef}`);
  }
  const [threadUtxo] = await lucid.utxosByOutRef([
    { txHash: txHashPart, outputIndex: Number(outputIndexPart) },
  ]);
  if (threadUtxo === undefined) {
    throw new Error(`Raw step-02 thread UTxO ${threadOutRef} is not live`);
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: invalidSignatureCategory.categoryId,
    categoryLabel: "raw invalid-signature",
  });
  if (threadUtxo.datum == null) {
    throw new Error("Raw step-02 thread UTxO carries no datum");
  }
  const datum = Data.from(threadUtxo.datum, SDK.InvalidSignatureStep02Datum);
  if (datum.data === null) {
    throw new Error("Raw step-02 thread UTxO carries no step state");
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: datum.data.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: SDK.MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: datum.data.subject.transaction_id,
    nativeTxCompactCbor: subject.nativeTxCompactCbor,
    itemCbors: subject.addrTxWits.map(SDK.encodeMidgardAddressWitnessCanonical),
    owner: signer.paymentKeyHash,
    witnessSet: subject.witnessSetCompact,
    anchorWitnessSetHash: datum.data.bad_tx_witness_set_hash,
    label: "Raw invalid-signature step 02",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw invalid-signature step 02 field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.invalidSignature.steps[1].spendingScriptHash,
    label: "raw invalid-signature step 02",
  });
  const referenceInputs = [...published, stepReference];
  const addrTxWitsOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw invalid-signature step 02",
  });
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spend = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw invalid-signature step 02",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw invalid-signature step 02 fraud-proof",
            ),
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw invalid-signature step 02 fraud-proof",
            ),
            addr_tx_wits_opening: addrTxWitsOpening,
            bad_addr_tx_wit_index:
              badAddrTxWitIndex ?? subject.badAddrTxWitIndex,
          },
        ],
      },
      SDK.InvalidSignatureStep02SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw invalid-signature step 02 thread burn",
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
          contracts.computationThread.policyId,
          "raw invalid-signature step 02 thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;

  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw invalid-signature step 02 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw invalid-signature step 02 fraud-proof mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .readFrom([
      ...referenceInputs,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
