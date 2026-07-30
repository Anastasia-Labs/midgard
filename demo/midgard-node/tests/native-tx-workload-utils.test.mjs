import assert from "node:assert/strict";
import test from "node:test";

import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompactV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
  deriveMidgardV1TxFieldPreimages,
  encodeMidgardNativeTxCanonicalV1,
  verifyMidgardNativeTxProofSourceV1,
  verifyMidgardV1TxFieldPreimage,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import { buildNativeSignedOneToOneWithFee } from "../scripts/native-tx-workload-utils.mjs";

test("workload producer emits strict canonical V1 fields with scripts at 6 and vkeys at 7", () => {
  const signer = CML.PrivateKey.generate_ed25519();
  const produced = buildNativeSignedOneToOneWithFee({
    spendOutRefCbor: Buffer.from("82005820", "hex"),
    outputCbor: Buffer.from("80", "hex"),
    signer,
    fee: 23n,
  });
  const canonicalCbor = Buffer.from(produced.txHex, "hex");
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const fields = deriveMidgardV1TxFieldPreimages(canonicalCbor);
  const transactionCommitment = computeMidgardNativeTxProofCommitmentV1(source);

  assert.deepEqual(
    encodeMidgardNativeTxCanonicalV1(transaction),
    canonicalCbor,
  );
  assert.deepEqual(computeMidgardNativeTxIdV1(transaction), produced.txId);
  assert.deepEqual(
    verifyMidgardNativeTxProofSourceV1({
      transactionId: produced.txId,
      source,
    }),
    transaction.compact,
  );
  assert.equal(fields[6]?.fieldName, "script_witnesses");
  assert.equal(fields[7]?.fieldName, "address_witnesses");
  assert.deepEqual(
    fields[6]?.preimageCbor,
    transaction.witnessSet.scriptTxWitsPreimageCbor,
  );
  assert.deepEqual(
    fields[7]?.preimageCbor,
    transaction.witnessSet.addrTxWitsPreimageCbor,
  );
  assert.deepEqual(
    decodeMidgardNativeTxWitnessSetCompactV1(source.witnessSetCompactCbor),
    deriveMidgardNativeTxWitnessSetCompactV1(transaction.witnessSet),
  );

  for (const [fieldIndex, substitutedPreimage] of [
    [6, fields[7].preimageCbor],
    [7, fields[6].preimageCbor],
  ]) {
    assert.throws(() =>
      verifyMidgardV1TxFieldPreimage({
        transactionId: produced.txId,
        transactionCommitment,
        source,
        fieldIndex,
        preimageCbor: substitutedPreimage,
      }),
    );
  }
});
