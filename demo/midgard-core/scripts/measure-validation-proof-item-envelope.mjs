import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { resolve } from "node:path";

const sdkRequire = createRequire(
  resolve(process.cwd(), "../midgard-sdk/package.json"),
);
const {
  applyParamsToScript,
  CML,
  validatorToScriptHash,
} = sdkRequire("@lucid-evolution/lucid");

const MAX_TX_BYTES = 16_384;
const RELIABILITY_RESERVE_BYTES = 512;
const COINS_PER_UTXO_BYTE = 4_310n;
const LINEAR_FEE = CML.LinearFee.new(44n, 155_381n, 15n);
const blueprint = JSON.parse(
  readFileSync(resolve(process.cwd(), "../../onchain/aiken/plutus.json"), "utf8"),
);

const compiled = (title) => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) throw new Error(`missing validator ${title}`);
  return validator.compiledCode;
};
const proofItemScript = {
  type: "PlutusV3",
  script: compiled(
    "fraud_proofs/validation_trace/proof_item_v1.main.else",
  ),
};
const proofItemScriptHash = validatorToScriptHash(proofItemScript);
const semanticScript = {
  type: "PlutusV3",
  script: applyParamsToScript(
    compiled(
      "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend",
    ),
    ["11".repeat(28), "22".repeat(28), proofItemScriptHash],
  ),
};
const semanticScriptHash = validatorToScriptHash(semanticScript);

const hash = (fill, bytes = 32) => Buffer.alloc(bytes, fill);
const input = (fill, index = 0n) =>
  CML.TransactionInput.new(
    CML.TransactionHash.from_raw_bytes(hash(fill)),
    index,
  );
const integer = (value) =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString()));
const bytes = (value) => CML.PlutusData.new_bytes(value);
const list = (values) => {
  const items = CML.PlutusDataList.new();
  values.forEach((value) => items.add(value));
  return CML.PlutusData.new_list(items);
};
const constr = (alternative, fields) => {
  const items = CML.PlutusDataList.new();
  fields.forEach((field) => items.add(field));
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(BigInt(alternative), items),
  );
};
const signingKey = CML.PrivateKey.from_normal_bytes(hash(4));
const paymentKeyHash = signingKey.to_public().hash();
const changeAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), paymentKeyHash.to_raw_bytes()]),
);
const scriptAddress = (scriptHashHex) =>
  CML.Address.from_raw_bytes(
    Buffer.concat([
      Buffer.from([0x70]),
      Buffer.from(scriptHashHex, "hex"),
    ]),
  );
const proofItemAddress = scriptAddress(proofItemScriptHash);
const awardAddress = scriptAddress("11".repeat(28));

const frontier = Array.from({ length: 9 }, (_, height) =>
  constr(0, [integer(height), bytes(hash(0x40 + height))]),
);
const siblings = Array.from({ length: 9 }, (_, index) =>
  bytes(hash(0x60 + index)),
);
const collectionProof = (itemBytes) =>
  constr(0, [
    integer(1),
    integer(2),
    integer(434),
    integer(433),
    integer(itemBytes),
    bytes(hash(0x31)),
    list(frontier),
    list(siblings),
  ]);
const proofItemDatum = (itemBytes) =>
  constr(0, [
    integer(1),
    bytes(hash(0x32)),
    bytes(hash(0x33)),
    collectionProof(itemBytes),
    bytes(Buffer.alloc(itemBytes, 0x55)),
  ]);

const outputAtMinimumAda = (address, datum) => {
  let lovelace = 0n;
  for (;;) {
    const output = CML.TransactionOutput.new(
      address,
      CML.Value.from_coin(lovelace),
      CML.DatumOption.new_datum(datum),
      undefined,
    );
    const required = CML.min_ada_required(output, COINS_PER_UTXO_BYTE);
    if (required === lovelace) return { output, lovelace };
    lovelace = required;
  }
};
const witnessSetWithRedeemer = (tag, redeemer) => {
  const redeemers = CML.MapRedeemerKeyToRedeemerVal.new();
  redeemers.insert(
    CML.RedeemerKey.new(tag, 0n),
    CML.RedeemerVal.new(
      redeemer,
      CML.ExUnits.new(7_500_000n, 3_500_000_000n),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_redeemers(
    CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemers),
  );
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(signingKey.to_public(), signingKey.sign(hash(5))),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return witnesses;
};
const finalizeFee = (build) => {
  let fee = 0n;
  for (;;) {
    const transaction = build(fee);
    const required = CML.min_no_script_fee(transaction, LINEAR_FEE);
    if (required === fee) return { transaction, fee };
    fee = required;
  }
};

const publicationTransaction = (itemBytes) => {
  const datum = proofItemDatum(itemBytes);
  const { output, lovelace } = outputAtMinimumAda(proofItemAddress, datum);
  const build = (fee) => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(input(0));
    const outputs = CML.TransactionOutputList.new();
    outputs.add(output);
    outputs.add(
      CML.TransactionOutput.new(
        changeAddress,
        CML.Value.from_coin(100_000_000n),
        undefined,
        undefined,
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(0x7fff_ffff_ffff_ffffn);
    const witnesses = CML.TransactionWitnessSet.new();
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(
      CML.Vkeywitness.new(signingKey.to_public(), signingKey.sign(hash(5))),
    );
    witnesses.set_vkeywitnesses(vkeys);
    return CML.Transaction.new(body, witnesses, true, undefined);
  };
  const { transaction, fee } = finalizeFee(build);
  return {
    datumBytes: datum.to_cbor_bytes().length,
    minAda: lovelace.toString(),
    fee: fee.toString(),
    transactionBytes: transaction.to_cbor_bytes().length,
  };
};

const machineState = constr(0, [
  integer(1),
  bytes(hash(0x01)),
  bytes(hash(0x32)),
  bytes(hash(0x33)),
  bytes(hash(0x34)),
  constr(1, []),
  bytes(hash(0x35)),
  constr(0, []),
  integer(0),
  bytes(hash(0x36)),
  integer(0),
  integer(0),
  constr(0, []),
  bytes(hash(0)),
  bytes(hash(0x37)),
]);
const transition = constr(0, [bytes(Buffer.from([0x81, 0x00])), machineState]);
const proofTransaction = (itemBytes, route) => {
  const auxiliary = constr(30, [
    collectionProof(itemBytes),
    bytes(Buffer.alloc(itemBytes, 0x55)),
  ]);
  const action =
    route === "direct"
      ? constr(0, [integer(0), integer(0), transition, auxiliary])
      : constr(1, [integer(0), integer(0), transition, integer(1)]);
  const redeemer = constr(1, [action]);
  const build = (fee) => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(input(10));
    inputs.add(input(11));
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        awardAddress,
        CML.Value.from_coin(2_000_000n),
        CML.DatumOption.new_datum(constr(0, [integer(1)])),
        undefined,
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    const references = CML.TransactionInputList.new();
    references.add(input(12));
    if (route === "reference") references.add(input(13));
    body.set_reference_inputs(references);
    const collateral = CML.TransactionInputList.new();
    collateral.add(input(14));
    body.set_collateral_inputs(collateral);
    body.set_total_collateral(2_000_000n);
    body.set_script_data_hash(CML.ScriptDataHash.from_raw_bytes(hash(0x38)));
    body.set_ttl(0x7fff_ffff_ffff_ffffn);
    const requiredSigners = CML.Ed25519KeyHashList.new();
    requiredSigners.add(paymentKeyHash);
    body.set_required_signers(requiredSigners);
    return CML.Transaction.new(
      body,
      witnessSetWithRedeemer(CML.RedeemerTag.Spend, redeemer),
      true,
      undefined,
    );
  };
  const { transaction, fee } = finalizeFee(build);
  return {
    route,
    redeemerBytes: redeemer.to_cbor_bytes().length,
    referenceInputCount: route === "reference" ? 2 : 1,
    fee: fee.toString(),
    transactionBytes: transaction.to_cbor_bytes().length,
  };
};

const maximumItemBytes = (measure, ceiling) => {
  let low = 0;
  let high = ceiling;
  while (low < high) {
    const midpoint = Math.ceil((low + high) / 2);
    if (measure(midpoint).transactionBytes <= ceiling) low = midpoint;
    else high = midpoint - 1;
  }
  return low;
};
const reportBoundary = (name, measure) => {
  const exactMaximumItemBytes = maximumItemBytes(measure, MAX_TX_BYTES);
  const reliableItemBytes = maximumItemBytes(
    measure,
    MAX_TX_BYTES - RELIABILITY_RESERVE_BYTES,
  );
  return {
    name,
    exactMaximumItemBytes,
    exactMaximum: measure(exactMaximumItemBytes),
    firstOversized: measure(exactMaximumItemBytes + 1),
    reliableItemBytes,
    reliable: measure(reliableItemBytes),
  };
};

console.log(
  JSON.stringify(
    {
      parameterSnapshot: {
        maxTxBytes: MAX_TX_BYTES,
        reliabilityReserveBytes: RELIABILITY_RESERVE_BYTES,
        coinsPerUtxoByte: COINS_PER_UTXO_BYTE.toString(),
        minFeeCoefficient: "44",
        minFeeConstant: "155381",
      },
      appliedValidators: {
        proofItemScriptHash,
        semanticScriptHash,
      },
      collectionProofShape: {
        itemCount: 434,
        itemIndex: 433,
        frontierPeaks: frontier.length,
        siblings: siblings.length,
      },
      boundaries: [
        reportBoundary("complete-item-publication", publicationTransaction),
        reportBoundary("direct-proof", (itemBytes) =>
          proofTransaction(itemBytes, "direct"),
        ),
        {
          name: "reference-proof",
          itemBytesResolvedFromUtxo: 14_000,
          ...proofTransaction(14_000, "reference"),
        },
      ],
    },
    null,
    2,
  ),
);
