import { CML, type UTxO } from "@lucid-evolution/lucid";

import { makeCardanoBoundaryNativeScript } from "./ordered-collection-boundary.build-signed-cardano-reference-inputs-candidate.js";
import {
  CARDANO_BOUNDARY_OBSERVER_TTL,
  type SignedCardanoCollectionCandidate,
} from "./ordered-collection-boundary.find-signed-cardano-collection-boundary.js";

export const buildSignedCardanoObserverNativeScriptsCandidate = async ({
  privateKeyBech32,
  fundingInput,
  recipientAddress,
  requestedObserverCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly fundingInput: UTxO;
  readonly recipientAddress: string;
  readonly requestedObserverCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidate> => {
  if (
    !Number.isSafeInteger(requestedObserverCount) ||
    requestedObserverCount <= 0
  ) {
    throw new Error(
      "Requested Cardano observer/native-script count must be positive",
    );
  }
  const fundingLovelace = fundingInput.assets.lovelace ?? 0n;
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const signerHash = privateKey.to_public().hash();
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeObserverScript = (observerIndex: number): CML.NativeScript =>
    makeCardanoBoundaryNativeScript({
      signerHash,
      scriptIndex: observerIndex,
    });
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = fundingLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano observer/native-script candidate ${requestedObserverCount.toString()} exhausts its funding input`,
      );
    }
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(fundingInput.txHash),
        BigInt(fundingInput.outputIndex),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(CML.Value.from_coin(outputLovelace))
        .build()
        .output(),
    );
    const withdrawals = CML.MapRewardAccountToCoin.new();
    const nativeScripts = CML.NativeScriptList.new();
    const observerScripts = Array.from(
      { length: requestedObserverCount },
      (_, observerIndex) => makeObserverScript(observerIndex),
    ).sort((left, right) =>
      Buffer.compare(
        Buffer.from(left.hash().to_raw_bytes()),
        Buffer.from(right.hash().to_raw_bytes()),
      ),
    );
    for (const script of observerScripts) {
      withdrawals.insert(
        CML.RewardAddress.new(0, CML.Credential.new_script(script.hash())),
        0n,
      );
      nativeScripts.add(script);
    }
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(CARDANO_BOUNDARY_OBSERVER_TTL);
    body.set_withdrawals(withdrawals);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    witnessSet.set_native_scripts(nativeScripts);
    const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(signed.transaction, linearFee);
    if (nextFee === fee) {
      return {
        requestedItemCount: requestedObserverCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano observer/native-script candidate ${requestedObserverCount.toString()} fee did not converge`,
  );
};
