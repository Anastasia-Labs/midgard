import assert from "node:assert/strict";

import { test } from "vitest";

import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
  deriveMidgardTxFieldPreimages,
  encodeMidgardNativeTxCanonical,
  verifyMidgardNativeTxProofSource,
  verifyMidgardTxFieldPreimage,
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
    decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const fields = deriveMidgardTxFieldPreimages(canonicalCbor);
  const transactionCommitment = computeMidgardNativeTxProofCommitment(source);

  assert.deepEqual(
    encodeMidgardNativeTxCanonical(transaction),
    canonicalCbor,
  );
  assert.deepEqual(computeMidgardNativeTxId(transaction), produced.txId);
  assert.deepEqual(
    verifyMidgardNativeTxProofSource({
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
    decodeMidgardNativeTxWitnessSetCompact(source.witnessSetCompactCbor),
    deriveMidgardNativeTxWitnessSetCompact(transaction.witnessSet),
  );

  for (const [fieldIndex, substitutedPreimage] of [
    [6, fields[7].preimageCbor],
    [7, fields[6].preimageCbor],
  ]) {
    assert.throws(() =>
      verifyMidgardTxFieldPreimage({
        transactionId: produced.txId,
        transactionCommitment,
        source,
        fieldIndex,
        preimageCbor: substitutedPreimage,
      }),
    );
  }
});
