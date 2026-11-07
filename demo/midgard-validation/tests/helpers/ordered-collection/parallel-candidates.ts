import {
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
  splitMidgardFieldPreimageIntoChunks,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { CML } from "@lucid-evolution/lucid";

export const buildCollateralFreeMidgardSchemaParallelCandidate = ({
  collateralizedCardanoCborHex,
  privateKeyBech32,
}: {
  readonly collateralizedCardanoCborHex: string;
  readonly privateKeyBech32: string;
}): {
  readonly cborHex: string;
  readonly collateralizedRedeemersCborHex: string;
  readonly parallelRedeemersCborHex: string;
} => {
  const collateralized = CML.Transaction.from_cbor_hex(
    collateralizedCardanoCborHex,
  );
  const sourceBody = collateralized.body();
  const sourceWitnessSet = collateralized.witness_set();
  const sourceRedeemers = sourceWitnessSet.redeemers();
  if (sourceRedeemers === undefined) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no redeemers",
    );
  }
  if ((sourceBody.collateral_inputs()?.len() ?? 0) === 0) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no collateral input",
    );
  }
  if ((sourceWitnessSet.plutus_datums()?.len() ?? 0) > 0) {
    throw new Error(
      "Collateralized Cardano feasibility candidate unexpectedly uses datum witnesses",
    );
  }

  const parallelBody = CML.TransactionBody.new(
    sourceBody.inputs(),
    sourceBody.outputs(),
    sourceBody.fee(),
  );
  const referenceInputs = sourceBody.reference_inputs();
  if (referenceInputs !== undefined) {
    parallelBody.set_reference_inputs(referenceInputs);
  }
  const validityStart = sourceBody.validity_interval_start();
  if (validityStart !== undefined) {
    parallelBody.set_validity_interval_start(validityStart);
  }
  const ttl = sourceBody.ttl();
  if (ttl !== undefined) {
    parallelBody.set_ttl(ttl);
  }
  const withdrawals = sourceBody.withdrawals();
  if (withdrawals !== undefined) {
    parallelBody.set_withdrawals(withdrawals);
  }
  const requiredSigners = sourceBody.required_signers();
  if (requiredSigners !== undefined) {
    parallelBody.set_required_signers(requiredSigners);
  }
  const mint = sourceBody.mint();
  if (mint !== undefined) {
    parallelBody.set_mint(mint);
  }
  const scriptDataHash = sourceBody.script_data_hash();
  if (scriptDataHash !== undefined) {
    parallelBody.set_script_data_hash(scriptDataHash);
  }
  const auxiliaryDataHash = sourceBody.auxiliary_data_hash();
  if (auxiliaryDataHash !== undefined) {
    parallelBody.set_auxiliary_data_hash(auxiliaryDataHash);
  }
  const networkId = sourceBody.network_id();
  if (networkId !== undefined) {
    parallelBody.set_network_id(networkId);
  }

  const parallelWitnessSet = CML.TransactionWitnessSet.new();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.make_vkey_witness(
      CML.hash_transaction(parallelBody),
      CML.PrivateKey.from_bech32(privateKeyBech32),
    ),
  );
  parallelWitnessSet.set_vkeywitnesses(vkeyWitnesses);
  const nativeScripts = sourceWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    parallelWitnessSet.set_native_scripts(nativeScripts);
  }
  const plutusV3Scripts = sourceWitnessSet.plutus_v3_scripts();
  if (plutusV3Scripts !== undefined) {
    parallelWitnessSet.set_plutus_v3_scripts(plutusV3Scripts);
  }
  parallelWitnessSet.set_redeemers(sourceRedeemers);
  const parallel = CML.Transaction.new(
    parallelBody,
    parallelWitnessSet,
    collateralized.is_valid(),
    collateralized.auxiliary_data(),
  );
  const parallelRedeemers = parallel.witness_set().redeemers();
  if (parallelRedeemers === undefined) {
    throw new Error(
      "Collateral-free Midgard-schema feasibility candidate lost its redeemers",
    );
  }
  return {
    cborHex: parallel.to_cbor_hex(),
    collateralizedRedeemersCborHex: Buffer.from(
      sourceRedeemers.to_cbor_bytes(),
    ).toString("hex"),
    parallelRedeemersCborHex: Buffer.from(
      parallelRedeemers.to_cbor_bytes(),
    ).toString("hex"),
  };
};

/**
 * The §8 carriage §8.4's partition admits for a preimage of this length, shaped
 * for measurement.
 *
 * Tier 1 is exact — the redeemer really does carry these bytes. Tiers 2–3 are
 * shaped with representative positional indices: their wire size depends on the
 * chunk *count*, which is fixed by the preimage length, and not on which UTxOs
 * the indices name.
 */
export const admissibleFieldCarriage = (
  preimageCbor: Buffer,
): MidgardFieldCarriage => {
  const tier = selectMidgardFieldCarriageTier(preimageCbor.length);
  if (tier === "Inline") {
    return { carriage: "Inline", preimage: Buffer.from(preimageCbor) };
  }
  if (tier === "RawUtxo") {
    return { carriage: "RawUtxo", refInputIndex: 0 };
  }
  return {
    carriage: "Certified",
    certRefInputIndex: 0,
    chunkRefInputIndices: splitMidgardFieldPreimageIntoChunks(preimageCbor).map(
      (_chunk, index) => index + 1,
    ),
  };
};
