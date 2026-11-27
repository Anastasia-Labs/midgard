import { CML } from "@lucid-evolution/lucid";

import { makeCardanoTxOutput } from "../midgard-output-helpers.js";

export const TEST_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

/**
 * A signed tx spending `11…11#spentInputIndex`. Txs built with the same index
 * spend the same input, so a node admits only the first of them (first-seen
 * input claims); give each tx its own index to admit several.
 */
export const makeCardanoSignedMapOutputTxBytes = (
  validity?: Readonly<{ start: bigint; ttl: bigint }>,
  spentInputIndex = 0n,
): Buffer => {
  const signerKey = CML.PrivateKey.generate_ed25519();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex("11".repeat(32)),
      spentInputIndex,
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    makeCardanoTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(signerKey.to_public().hash()),
      ).to_address(),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  if (validity !== undefined) {
    body.set_validity_interval_start(validity.start);
    body.set_ttl(validity.ttl);
  }
  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.make_vkey_witness(CML.hash_transaction(body), signerKey),
  );
  witnessSet.set_vkeywitnesses(vkeyWitnesses);
  return Buffer.from(
    CML.Transaction.new(body, witnessSet, true, undefined).to_cbor_bytes(),
  );
};

export const makeConvertibleCardanoTxBytes = (): Buffer => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    makeCardanoTxOutput(
      CML.Address.from_bech32(TEST_ADDRESS),
      CML.Value.from_coin(2_000_000n),
    ),
  );
  return Buffer.from(
    CML.Transaction.new(
      CML.TransactionBody.new(inputs, outputs, 0n),
      CML.TransactionWitnessSet.new(),
      true,
      undefined,
    ).to_cbor_bytes(),
  );
};
