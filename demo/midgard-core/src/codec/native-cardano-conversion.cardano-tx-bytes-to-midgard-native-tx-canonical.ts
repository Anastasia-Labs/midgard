import { ensureHash32 } from "./hash.js";
import { type MidgardNativeTxCanonical } from "./native.js";
import {
  assertCardanoTxConvertibleToNative,
  cmlAnyToPreimageCbor,
  scriptWitnessesToPreimageCbor,
  withdrawalsToRequiredObserversPreimageCbor,
} from "./native-cardano-conversion.cml-mint-to-preimage-cbor.js";
import {
  asCollectionLike,
  type CardanoToMidgardNativeConstants,
  cmlCollectionToPreimageCbor,
  cmlInputsToSpendInputPreimageCbor,
  cmlOutputsToNativePreimageCbor,
  parseCardanoTx,
} from "./native-cardano-conversion.cml-script-to-midgard-versioned-script.js";
import { EMPTY_NULL_ROOT } from "./native-constants.js";
import { cardanoRedeemersToMidgardPreimageCbor } from "./native-redeemer.js";

export const cardanoTxBytesToMidgardNativeTxCanonical = (
  cardanoTxBytes: Uint8Array,
  constants: CardanoToMidgardNativeConstants,
): MidgardNativeTxCanonical => {
  const tx = parseCardanoTx(cardanoTxBytes);
  assertCardanoTxConvertibleToNative(tx);
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();
  const txOutputs = txBody.outputs();

  const spendInputsPreimageCbor = cmlInputsToSpendInputPreimageCbor(
    asCollectionLike(txBody.inputs()),
    "transaction_body.inputs",
  );
  const referenceInputsPreimageCbor = cmlInputsToSpendInputPreimageCbor(
    asCollectionLike(txBody.reference_inputs()),
    "transaction_body.reference_inputs",
  );
  const outputsPreimageCbor = cmlOutputsToNativePreimageCbor(
    asCollectionLike(txOutputs),
  );
  const requiredObserversPreimageCbor =
    withdrawalsToRequiredObserversPreimageCbor(txBody.withdrawals());
  const requiredSignersPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txBody.required_signers()),
    "transaction_body.required_signers",
  );
  const mintPreimageCbor = cmlAnyToPreimageCbor(
    txBody.mint(),
    "transaction_body.mint",
  );

  const addrTxWitsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txWitnessSet.vkeywitnesses()),
    "transaction_witness_set.vkeywitnesses",
  );
  const scriptTxWitsPreimageCbor = scriptWitnessesToPreimageCbor(txWitnessSet);
  const redeemerTxWitsPreimageCbor = cardanoRedeemersToMidgardPreimageCbor(
    txWitnessSet.redeemers(),
    "transaction_witness_set.redeemers",
  );

  const scriptDataHash = txBody.script_data_hash();
  const auxDataHash = txBody.auxiliary_data_hash();
  const network = txBody.network_id();
  const encodedNetworkId =
    network === undefined ? constants.networkIdNone : BigInt(network.network());

  return {
    version: constants.nativeTxVersion,
    validity: tx.is_valid() ? "TxIsValid" : "TxIsInvalid",
    body: {
      spendInputsPreimageCbor,
      referenceInputsPreimageCbor,
      outputsPreimageCbor,
      fee: txBody.fee(),
      validityIntervalStart:
        txBody.validity_interval_start() ?? constants.posixTimeNone,
      validityIntervalEnd: txBody.ttl() ?? constants.posixTimeNone,
      requiredObserversPreimageCbor,
      requiredSignersPreimageCbor,
      mintPreimageCbor,
      scriptIntegrityHash:
        scriptDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(
              scriptDataHash.to_raw_bytes(),
              "script_integrity_hash",
            ),
      auxiliaryDataHash:
        auxDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(auxDataHash.to_raw_bytes(), "auxiliary_data_hash"),
      networkId: encodedNetworkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor,
      scriptTxWitsPreimageCbor,
      redeemerTxWitsPreimageCbor,
    },
  };
};
