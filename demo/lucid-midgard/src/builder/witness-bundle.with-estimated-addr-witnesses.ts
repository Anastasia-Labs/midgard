import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";

import { makeVKeyWitness } from "../wallet.js";
import {
  decodeAddrWitnesses,
  encodeAddrWitnesses,
} from "./witness-bundle.decode-import-addr-witnesses.js";
import { dummyWitnessPrivateKey } from "./witness-bundle.normalize-partial-witness-bundle.js";

export const withEstimatedAddrWitnesses = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): MidgardNativeTxFull => {
  if (expectedWitnessCount === 0) {
    return tx;
  }
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  if (witnesses.length >= expectedWitnessCount) {
    return tx;
  }
  const estimatedWitnesses = [...witnesses];
  for (let index = witnesses.length; index < expectedWitnessCount; index += 1) {
    estimatedWitnesses.push(
      makeVKeyWitness(
        computeMidgardNativeTxId(tx),
        dummyWitnessPrivateKey(index),
      ),
    );
  }
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimageCbor: encodeAddrWitnesses(estimatedWitnesses),
  };
  const estimatedTx: MidgardNativeTxFull = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(
      tx.body,
      witnessSet,
      tx.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistency(estimatedTx);
  return estimatedTx;
};

export const estimatedSignedTxByteLength = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): number =>
  encodeMidgardNativeTxCanonical(
    withEstimatedAddrWitnesses(tx, expectedWitnessCount),
  ).length;
