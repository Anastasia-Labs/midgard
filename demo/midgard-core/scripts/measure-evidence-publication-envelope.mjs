import { CML } from "@lucid-evolution/lucid";

const MAX_TX_BYTES = 16_384;
const RELIABILITY_RESERVE_BYTES = 512;

const hash = (fill) => Buffer.alloc(32, fill);
const input = (fill) =>
  CML.TransactionInput.new(CML.TransactionHash.from_raw_bytes(hash(fill)), 0n);

const signingKey = CML.PrivateKey.from_normal_bytes(hash(4));
const paymentKeyHash = signingKey.to_public().hash();
const changeAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), paymentKeyHash.to_raw_bytes()]),
);
const evidenceAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x22)]),
);
const policyId = CML.ScriptHash.from_raw_bytes(Buffer.alloc(28, 0x33));
const assetName = CML.AssetName.from_raw_bytes(Buffer.from("evidence"));

const evidenceDatum = (evidenceBytes) => {
  const fields = CML.PlutusDataList.new();
  fields.add(CML.PlutusData.new_bytes(Buffer.alloc(evidenceBytes, 0x55)));
  fields.add(CML.PlutusData.new_bytes(hash(0x66)));
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(0n, fields),
  );
};

const evidenceValue = (withStateToken) => {
  if (!withStateToken) {
    return CML.Value.from_coin(100_000_000n);
  }
  const assets = CML.MultiAsset.new();
  assets.set(policyId, assetName, 1n);
  return CML.Value.new(100_000_000n, assets);
};

const transactionFor = ({ evidenceBytes, withStateToken, includeWitness }) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(0));

  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      evidenceAddress,
      evidenceValue(withStateToken),
      CML.DatumOption.new_datum(evidenceDatum(evidenceBytes)),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      changeAddress,
      CML.Value.from_coin(100_000_000n),
      undefined,
      undefined,
    ),
  );

  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  body.set_ttl(0x7fff_ffff_ffff_ffffn);

  const witnessSet = CML.TransactionWitnessSet.new();
  if (includeWitness) {
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(
      CML.Vkeywitness.new(signingKey.to_public(), signingKey.sign(hash(5))),
    );
    witnessSet.set_vkeywitnesses(vkeys);
  }

  if (withStateToken) {
    const mint = CML.Mint.new();
    mint.set(policyId, assetName, 1n);
    body.set_mint(mint);

    const collateral = CML.TransactionInputList.new();
    collateral.add(input(1));
    body.set_collateral_inputs(collateral);
    body.set_total_collateral(2_000_000n);

    const references = CML.TransactionInputList.new();
    references.add(input(2));
    body.set_reference_inputs(references);

    const requiredSigners = CML.Ed25519KeyHashList.new();
    requiredSigners.add(paymentKeyHash);
    body.set_required_signers(requiredSigners);
    body.set_script_data_hash(CML.ScriptDataHash.from_raw_bytes(hash(3)));

    const redeemers = CML.MapRedeemerKeyToRedeemerVal.new();
    redeemers.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Mint, 0n),
      CML.RedeemerVal.new(
        CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
        CML.ExUnits.new(16_500_000n, 10_000_000_000n),
      ),
    );
    witnessSet.set_redeemers(
      CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemers),
    );
  }

  const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
  return {
    datumBytes: evidenceDatum(evidenceBytes).to_cbor_bytes().length,
    transactionBytes: transaction.to_cbor_bytes().length,
  };
};

const maximumEvidenceBytes = (shape, maximumTransactionBytes) => {
  let low = 0;
  let high = maximumTransactionBytes;
  while (low < high) {
    const midpoint = Math.ceil((low + high) / 2);
    const measured = transactionFor({
      ...shape,
      evidenceBytes: midpoint,
    }).transactionBytes;
    if (measured <= maximumTransactionBytes) {
      low = midpoint;
    } else {
      high = midpoint - 1;
    }
  }
  return low;
};

const shapes = [
  {
    name: "simple-signed",
    withStateToken: false,
    includeWitness: true,
  },
  {
    name: "state-token-reference-script-signed",
    withStateToken: true,
    includeWitness: true,
  },
];

const results = shapes.map((shape) => {
  const exactMaximumEvidenceBytes = maximumEvidenceBytes(shape, MAX_TX_BYTES);
  const reliableEvidenceBytes = maximumEvidenceBytes(
    shape,
    MAX_TX_BYTES - RELIABILITY_RESERVE_BYTES,
  );
  return {
    shape: shape.name,
    maxTransactionBytes: MAX_TX_BYTES,
    reliabilityReserveBytes: RELIABILITY_RESERVE_BYTES,
    exactMaximumEvidenceBytes,
    exactMaximum: transactionFor({
      ...shape,
      evidenceBytes: exactMaximumEvidenceBytes,
    }),
    firstOversized: transactionFor({
      ...shape,
      evidenceBytes: exactMaximumEvidenceBytes + 1,
    }),
    reliableEvidenceBytes,
    reliable: transactionFor({
      ...shape,
      evidenceBytes: reliableEvidenceBytes,
    }),
  };
});

console.log(JSON.stringify(results, null, 2));
