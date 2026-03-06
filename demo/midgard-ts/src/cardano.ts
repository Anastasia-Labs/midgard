/**
 * Conversion functions between Cardano CML transaction types and Midgard
 * transaction types.
 *
 * Rules enforced during CML → Midgard conversion:
 *   - Governance actions (proposal_procedures) are forbidden.
 *   - Non-zero ADA withdrawals are forbidden.
 *   - Zero-ADA withdrawals (the "withdraw zero trick") with a script credential
 *     are translated into required_observers (CIP-112 observer scripts).
 *   - Zero-ADA withdrawals with a key credential are rejected.
 *   - Only Conway-format transaction outputs are accepted.
 *   - Only inline datums are accepted (datum hashes are rejected).
 *
 */

import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";

import {
  Transaction,
  TransactionBody,
  TransactionWitnessSet,
} from "./types/transaction";

import {
  Mint,
  TransactionOutput,
  Value,
  Multiasset,
  VKeyWitness,
} from "./types/output";

import { OutputReference } from "./types/primitives";

// ===========================================================================
// Errors
// ===========================================================================

export class ConversionError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ConversionError";
  }
}

// ===========================================================================
// CML → Midgard
// ===========================================================================

/**
 * Convert a Cardano CML Transaction into a Midgard Transaction.
 *
 * Throws ConversionError for any unsupported Cardano features.
 */
export function cmlToMidgard(cmlTx: CML.Transaction): Transaction {
  const body = cmlTx.body();

  // --- Governance actions are not supported in Midgard ---
  const proposals = body.proposal_procedures();
  if (proposals !== undefined && proposals.len() > 0) {
    throw new ConversionError(
      "Governance actions (proposal_procedures) are not supported in Midgard",
    );
  }

  // --- Withdrawals ---
  // Non-zero ADA withdrawals are forbidden.
  // Zero-ADA withdrawals (withdraw-zero trick) with script credentials
  // are translated into required_observers.
  let required_observers: Uint8Array[] | undefined;
  const cmlWithdrawals = body.withdrawals();
  if (cmlWithdrawals !== undefined) {
    const keys = cmlWithdrawals.keys();
    for (let i = 0; i < keys.len(); i++) {
      const rewardAddr = keys.get(i);
      const amount = cmlWithdrawals.get(rewardAddr)!;

      if (amount > 0n) {
        throw new ConversionError(
          `Non-zero ADA withdrawal of ${amount} lovelace found; only zero-ADA withdrawals are supported in Midgard`,
        );
      }

      // Zero withdrawal — must be a script credential (CIP-112 observer script).
      const cred = rewardAddr.payment();
      const scriptHash = cred.as_script();
      if (scriptHash === undefined) {
        throw new ConversionError(
          "Zero-ADA withdrawal uses a key credential; only script credentials are supported as observer scripts in Midgard",
        );
      }

      if (required_observers === undefined) required_observers = [];
      required_observers.push(scriptHash.to_raw_bytes());
    }
  }

  // --- Inputs ---
  const cmlInputs = body.inputs();
  const inputs: OutputReference[] = [];
  for (let i = 0; i < cmlInputs.len(); i++) {
    inputs.push(cmlInputToOutputRef(cmlInputs.get(i)));
  }

  // --- Outputs ---
  const cmlOutputs = body.outputs();
  const outputs: TransactionOutput[] = [];
  for (let i = 0; i < cmlOutputs.len(); i++) {
    outputs.push(cmlOutputToMidgard(cmlOutputs.get(i)));
  }

  // --- Mandatory fields ---
  const fee = Number(body.fee());

  // --- Optional fields ---
  const cmlTtl = body.ttl();
  const ttl = cmlTtl !== undefined ? Number(cmlTtl) : undefined;

  const cmlAuxHash = body.auxiliary_data_hash();
  const auxiliary_data_hash = cmlAuxHash
    ? cmlAuxHash.to_raw_bytes()
    : undefined;

  const cmlVis = body.validity_interval_start();
  const validity_interval_start =
    cmlVis !== undefined ? Number(cmlVis) : undefined;

  const cmlMint = body.mint();
  const mint = cmlMint ? cmlMintToMidgard(cmlMint) : undefined;

  const cmlSdh = body.script_data_hash();
  const script_data_hash = cmlSdh ? cmlSdh.to_raw_bytes() : undefined;

  const cmlReqSigners = body.required_signers();
  let required_signers: Uint8Array[] | undefined;
  if (cmlReqSigners !== undefined) {
    required_signers = [];
    for (let i = 0; i < cmlReqSigners.len(); i++) {
      required_signers.push(cmlReqSigners.get(i).to_raw_bytes());
    }
  }

  const cmlNetId = body.network_id();
  const network_id = cmlNetId ? Number(cmlNetId.network()) : undefined;

  const cmlRefInputs = body.reference_inputs();
  let reference_inputs: OutputReference[] | undefined;
  if (cmlRefInputs !== undefined) {
    reference_inputs = [];
    for (let i = 0; i < cmlRefInputs.len(); i++) {
      reference_inputs.push(cmlInputToOutputRef(cmlRefInputs.get(i)));
    }
  }

  const midgardBody: TransactionBody = {
    inputs,
    outputs,
    fee,
    ttl,
    auxiliary_data_hash,
    validity_interval_start,
    mint,
    script_data_hash,
    required_signers,
    network_id,
    reference_inputs,
    required_observers,
  };

  const witness_set = cmlWitnessSetToMidgard(cmlTx.witness_set());
  return { body: midgardBody, witness_set, is_valid: cmlTx.is_valid() };
}

// ===========================================================================
// Midgard → CML
// ===========================================================================

/**
 * Convert a Midgard Transaction into a Cardano CML Transaction.
 *
 * @param cardanoNetwork  Cardano network byte used when reconstructing
 *   zero-ADA withdrawals for required_observers.  1 = mainnet, 0 = testnet.
 *   Defaults to 1 (mainnet).
 */
export function midgardToCml(
  tx: Transaction,
  cardanoNetwork: number = 1,
): CML.Transaction {
  const b = tx.body;

  // Inputs
  const cmlInputs = CML.TransactionInputList.new();
  for (const ref of b.inputs) {
    cmlInputs.add(outputRefToCmlInput(ref));
  }

  // Outputs
  const cmlOutputs = CML.TransactionOutputList.new();
  for (const o of b.outputs) {
    cmlOutputs.add(midgardOutputToCml(o));
  }

  const cmlBody = CML.TransactionBody.new(
    cmlInputs,
    cmlOutputs,
    BigInt(b.fee),
  );

  if (b.ttl !== undefined) cmlBody.set_ttl(BigInt(b.ttl));

  if (b.auxiliary_data_hash !== undefined)
    cmlBody.set_auxiliary_data_hash(
      CML.AuxiliaryDataHash.from_raw_bytes(b.auxiliary_data_hash),
    );

  if (b.validity_interval_start !== undefined)
    cmlBody.set_validity_interval_start(BigInt(b.validity_interval_start));

  if (b.mint !== undefined)
    cmlBody.set_mint(midgardMintToCml(b.mint));


  if (b.script_data_hash !== undefined)
    cmlBody.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(b.script_data_hash),
    );

  if (b.required_signers !== undefined) {
    const list = CML.Ed25519KeyHashList.new();
    for (const h of b.required_signers)
      list.add(CML.Ed25519KeyHash.from_raw_bytes(h));
    cmlBody.set_required_signers(list);
  }

  if (b.network_id !== undefined)
    cmlBody.set_network_id(CML.NetworkId.new(BigInt(b.network_id)));

  if (b.reference_inputs !== undefined) {
    const refs = CML.TransactionInputList.new();
    for (const r of b.reference_inputs) refs.add(outputRefToCmlInput(r));
    cmlBody.set_reference_inputs(refs);
  }

  // required_observers → zero-ADA withdrawals (reverse of the withdraw-zero trick)
  if (b.required_observers !== undefined && b.required_observers.length > 0) {
    const withdrawals = CML.MapRewardAccountToCoin.new();
    for (const scriptHashBytes of b.required_observers) {
      const scriptHash = CML.ScriptHash.from_raw_bytes(scriptHashBytes);
      const cred = CML.Credential.new_script(scriptHash);
      const rewardAddr = CML.RewardAddress.new(cardanoNetwork, cred);
      withdrawals.insert(rewardAddr, 0n);
    }
    cmlBody.set_withdrawals(withdrawals);
  }

  const cmlWitnessSet = midgardWitnessSetToCml(tx.witness_set);
  return CML.Transaction.new(cmlBody, cmlWitnessSet, tx.is_valid);
}

// ===========================================================================
// TransactionInput / OutputReference
// ===========================================================================

function cmlInputToOutputRef(input: CML.TransactionInput): OutputReference {
  return {
    tx_id: input.transaction_id().to_raw_bytes(),
    index: Number(input.index()),
  };
}

function outputRefToCmlInput(ref: OutputReference): CML.TransactionInput {
  return CML.TransactionInput.new(
    CML.TransactionHash.from_raw_bytes(ref.tx_id),
    BigInt(ref.index),
  );
}

// ===========================================================================
// TransactionOutput
// ===========================================================================

function cmlOutputToMidgard(out: CML.TransactionOutput): TransactionOutput {
  // Midgard only supports post-Alonzo (Conway-format) outputs.
  if (out.kind() !== CML.TransactionOutputKind.ConwayFormatTxOut) {
    throw new ConversionError(
      "Only Conway-format (post-Alonzo) transaction outputs are supported in Midgard",
    );
  }
  const conway = out.as_conway_format_tx_out()!;

  const address = conway.address().to_raw_bytes();
  const value = cmlValueToMidgard(conway.amount());

  let datum: Uint8Array | undefined;
  const datumOpt = conway.datum_option();
  if (datumOpt !== undefined) {
    if (datumOpt.kind() === CML.DatumOptionKind.Hash) {
      throw new ConversionError(
        "Datum hashes in transaction outputs are not supported in Midgard; only inline datums are allowed",
      );
    }
    // Inline datum: store raw PlutusData CBOR bytes.
    datum = datumOpt.as_datum()!.to_cbor_bytes();
  }

  let script_ref: Uint8Array | undefined;
  const scriptRef = conway.script_reference();
  if (scriptRef !== undefined) {
    script_ref = scriptRef.to_cbor_bytes();
  }

  return { address, value, datum, script_ref };
}

function midgardOutputToCml(o: TransactionOutput): CML.TransactionOutput {
  const address = CML.Address.from_raw_bytes(o.address);
  const amount = midgardValueToCml(o.value);
  const conway = CML.ConwayFormatTxOut.new(address, amount);

  if (o.datum !== undefined) {
    conway.set_datum_option(
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_bytes(o.datum)),
    );
  }

  if (o.script_ref !== undefined) {
    conway.set_script_reference(CML.Script.from_cbor_bytes(o.script_ref));
  }

  return CML.TransactionOutput.new_conway_format_tx_out(conway);
}

// ===========================================================================
// Value / Multiasset
// ===========================================================================

function cmlValueToMidgard(v: CML.Value): Value {
  if (!v.has_multiassets()) {
    return { type: "Coin", coin: Number(v.coin()) };
  }
  return {
    type: "MultiAsset",
    coin: Number(v.coin()),
    assets: cmlMultiassetToMidgard(v.multi_asset()),
  };
}

function midgardValueToCml(v: Value): CML.Value {
  if (v.type === "Coin") return CML.Value.from_coin(BigInt(v.coin));
  return CML.Value.new(BigInt(v.coin), midgardMultiassetToCml(v.assets));
}

function cmlMultiassetToMidgard(ma: CML.MultiAsset): Multiasset {
  const result: Multiasset = [];
  const policyIds = ma.keys();
  for (let i = 0; i < policyIds.len(); i++) {
    const pid = policyIds.get(i);
    const assets = ma.get_assets(pid)!;
    const assetNames = assets.keys();
    const assetEntries: Array<[Uint8Array, number]> = [];
    for (let j = 0; j < assetNames.len(); j++) {
      const name = assetNames.get(j);
      assetEntries.push([name.to_raw_bytes(), Number(assets.get(name)!)]);
    }
    result.push([pid.to_raw_bytes(), assetEntries]);
  }
  return result;
}

function midgardMultiassetToCml(ma: Multiasset): CML.MultiAsset {
  const cmlMa = CML.MultiAsset.new();
  for (const [pidBytes, assetList] of ma) {
    const pid = CML.ScriptHash.from_raw_bytes(pidBytes);
    const assets = CML.MapAssetNameToCoin.new();
    for (const [nameBytes, amount] of assetList) {
      assets.insert(
        CML.AssetName.from_raw_bytes(nameBytes),
        BigInt(amount),
      );
    }
    cmlMa.insert_assets(pid, assets);
  }
  return cmlMa;
}

// ===========================================================================
// Mint
//
// CML's Mint class exposes neither to_cbor_bytes nor from_cbor_bytes, but it
// does expose keys()/get_assets() for iteration and insert_assets() for
// construction — the same API as MultiAsset. Midgard's Mint type mirrors
// Multiasset with signed i64 amounts (positive = mint, negative = burn).
// ===========================================================================

function cmlMintToMidgard(cmlMint: CML.Mint): Mint {
  const result: Mint = [];
  const policyIds = cmlMint.keys();
  for (let i = 0; i < policyIds.len(); i++) {
    const pid = policyIds.get(i);
    const assets = cmlMint.get_assets(pid)!;
    const assetNames = assets.keys();
    const assetEntries: Array<[Uint8Array, number]> = [];
    for (let j = 0; j < assetNames.len(); j++) {
      const name = assetNames.get(j);
      assetEntries.push([name.to_raw_bytes(), Number(assets.get(name)!)]);
    }
    result.push([pid.to_raw_bytes(), assetEntries]);
  }
  return result;
}

function midgardMintToCml(mint: Mint): CML.Mint {
  const cmlMint = CML.Mint.new();
  for (const [pidBytes, assetList] of mint) {
    const pid = CML.ScriptHash.from_raw_bytes(pidBytes);
    const assetMap = CML.MapAssetNameToNonZeroInt64.new();
    for (const [nameBytes, amount] of assetList)
      assetMap.insert(CML.AssetName.from_raw_bytes(nameBytes), BigInt(amount));
    cmlMint.insert_assets(pid, assetMap);
  }
  return cmlMint;
}

// ===========================================================================
// WitnessSet
// ===========================================================================

function cmlWitnessSetToMidgard(
  ws: CML.TransactionWitnessSet,
): TransactionWitnessSet {
  let vkey_witnesses: VKeyWitness[] | undefined;
  const cmlVkeys = ws.vkeywitnesses();
  if (cmlVkeys !== undefined) {
    vkey_witnesses = [];
    for (let i = 0; i < cmlVkeys.len(); i++) {
      const vk = cmlVkeys.get(i);
      vkey_witnesses.push({
        vkey: vk.vkey().to_raw_bytes(),
        signature: vk.ed25519_signature().to_raw_bytes(),
      });
    }
  }

  let native_scripts: Uint8Array[] | undefined;
  const cmlNs = ws.native_scripts();
  if (cmlNs !== undefined) {
    native_scripts = [];
    for (let i = 0; i < cmlNs.len(); i++) {
      native_scripts.push(cmlNs.get(i).to_cbor_bytes());
    }
  }

  let redeemers: Uint8Array | undefined;
  const cmlRdm = ws.redeemers();
  if (cmlRdm !== undefined) {
    redeemers = cmlRdm.to_cbor_bytes();
  }

  let plutus_v3_scripts: Uint8Array[] | undefined;
  const cmlPv3 = ws.plutus_v3_scripts();
  if (cmlPv3 !== undefined) {
    plutus_v3_scripts = [];
    for (let i = 0; i < cmlPv3.len(); i++) {
      plutus_v3_scripts.push(cmlPv3.get(i).to_cbor_bytes());
    }
  }

  return { vkey_witnesses, native_scripts, redeemers, plutus_v3_scripts };
}

function midgardWitnessSetToCml(
  ws: TransactionWitnessSet,
): CML.TransactionWitnessSet {
  const cmlWs = CML.TransactionWitnessSet.new();

  if (ws.vkey_witnesses !== undefined) {
    const list = CML.VkeywitnessList.new();
    for (const vk of ws.vkey_witnesses) {
      list.add(
        CML.Vkeywitness.new(
          CML.PublicKey.from_bytes(vk.vkey),
          CML.Ed25519Signature.from_raw_bytes(vk.signature),
        ),
      );
    }
    cmlWs.set_vkeywitnesses(list);
  }

  if (ws.native_scripts !== undefined) {
    const list = CML.NativeScriptList.new();
    for (const ns of ws.native_scripts) {
      list.add(CML.NativeScript.from_cbor_bytes(ns));
    }
    cmlWs.set_native_scripts(list);
  }

  if (ws.redeemers !== undefined) {
    cmlWs.set_redeemers(CML.Redeemers.from_cbor_bytes(ws.redeemers));
  }

  if (ws.plutus_v3_scripts !== undefined) {
    const list = CML.PlutusV3ScriptList.new();
    for (const s of ws.plutus_v3_scripts) {
      list.add(CML.PlutusV3Script.from_cbor_bytes(s));
    }
    cmlWs.set_plutus_v3_scripts(list);
  }

  return cmlWs;
}
