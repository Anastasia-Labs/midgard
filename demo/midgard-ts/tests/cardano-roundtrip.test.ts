/**
 * Cardano CBOR round-trip tests.
 *
 * Transactions are constructed directly with CML so all outputs are
 * Conway-format and only fields Midgard supports are included.
 *
 * Flow per test:
 *   CML.Transaction → cmlToMidgard → encodeTransaction
 *     → decodeTransaction → midgardToCml → to_cbor_hex()
 * The resulting CBOR hex must equal the original.
 */

import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { cmlToMidgard, midgardToCml } from "../src/cardano";
import { encodeTransaction, decodeTransaction } from "../src/index";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function b(n: number, fill: number): Uint8Array {
  return new Uint8Array(n).fill(fill);
}

function txHash(fill: number): CML.TransactionHash {
  return CML.TransactionHash.from_raw_bytes(b(32, fill));
}

function address(fill: number): CML.Address {
  const cred = CML.Credential.new_pub_key(
    CML.Ed25519KeyHash.from_raw_bytes(b(28, fill)),
  );
  return CML.EnterpriseAddress.new(1, cred).to_address();
}

function input(hashFill: number, index: number): CML.TransactionInput {
  return CML.TransactionInput.new(txHash(hashFill), BigInt(index));
}

function inputList(...inputs: CML.TransactionInput[]): CML.TransactionInputList {
  const list = CML.TransactionInputList.new();
  for (const i of inputs) list.add(i);
  return list;
}

function conwayOutput(
  addr: CML.Address,
  value: CML.Value,
  buildOut?: (out: CML.ConwayFormatTxOut) => void,
): CML.TransactionOutput {
  const out = CML.ConwayFormatTxOut.new(addr, value);
  if (buildOut) buildOut(out);
  return CML.TransactionOutput.new_conway_format_tx_out(out);
}

function outputList(...outputs: CML.TransactionOutput[]): CML.TransactionOutputList {
  const list = CML.TransactionOutputList.new();
  for (const o of outputs) list.add(o);
  return list;
}

function buildTx(
  ins: CML.TransactionInputList,
  outs: CML.TransactionOutputList,
  fee: bigint,
  buildBody?: (body: CML.TransactionBody) => void,
  buildWitnesses?: (ws: CML.TransactionWitnessSet) => void,
  isValid = true,
): CML.Transaction {
  const body = CML.TransactionBody.new(ins, outs, fee);
  if (buildBody) buildBody(body);
  const ws = CML.TransactionWitnessSet.new();
  if (buildWitnesses) buildWitnesses(ws);
  return CML.Transaction.new(body, ws, isValid);
}

function roundTrip(cmlTx: CML.Transaction): string {
  const midgard = cmlToMidgard(cmlTx);
  const bytes = encodeTransaction(midgard);
  const midgard2 = decodeTransaction(bytes);
  return midgardToCml(midgard2).to_cbor_hex();
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("Cardano CBOR round-trip", () => {
  it("simple ADA transfer", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("multiple inputs and outputs", () => {
    const tx = buildTx(
      inputList(input(0x11, 0), input(0x22, 1)),
      outputList(
        conwayOutput(address(0xab), CML.Value.from_coin(1_000_000n)),
        conwayOutput(address(0xcd), CML.Value.from_coin(500_000n)),
      ),
      170_000n,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("multiasset output", () => {
    const policyId = CML.ScriptHash.from_raw_bytes(b(28, 0xca));
    const assetName = CML.AssetName.from_raw_bytes(b(10, 0x01));
    const assets = CML.MapAssetNameToCoin.new();
    assets.insert(assetName, 500n);
    const ma = CML.MultiAsset.new();
    ma.insert_assets(policyId, assets);

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.new(2_000_000n, ma))),
      170_000n,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("output with inline datum", () => {
    const datum = CML.PlutusData.from_cbor_bytes(
      Buffer.from("182a", "hex"), // integer 42
    );
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(
        conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n), (out) =>
          out.set_datum_option(CML.DatumOption.new_datum(datum)),
        ),
      ),
      170_000n,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("output with script reference", () => {
    const script = CML.Script.from_cbor_bytes(
      // [3, h'0000...'] — PlutusV3 script with 4 dummy bytes
      Buffer.from("8203440000cafe", "hex"),
    );
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(
        conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n), (out) =>
          out.set_script_reference(script),
        ),
      ),
      170_000n,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with ttl and validity_interval_start", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => {
        body.set_ttl(1_000_000n);
        body.set_validity_interval_start(900_000n);
      },
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with auxiliary_data_hash", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) =>
        body.set_auxiliary_data_hash(
          CML.AuxiliaryDataHash.from_raw_bytes(b(32, 0xad)),
        ),
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with mint (native token)", () => {
    const policyId = CML.ScriptHash.from_raw_bytes(b(28, 0xaa));
    const assetName = CML.AssetName.from_raw_bytes(b(4, 0x01));
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(assetName, 100n);
    const mint = CML.Mint.new();
    mint.insert_assets(policyId, mintAssets);

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => {
        body.set_mint(mint);
        body.set_script_data_hash(
          CML.ScriptDataHash.from_raw_bytes(b(32, 0xdd)),
        );
      },
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with burn (negative mint)", () => {
    const policyId = CML.ScriptHash.from_raw_bytes(b(28, 0xbb));
    const assetName = CML.AssetName.from_raw_bytes(b(4, 0x02));
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(assetName, -50n);
    const mint = CML.Mint.new();
    mint.insert_assets(policyId, mintAssets);

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => {
        body.set_mint(mint);
        body.set_script_data_hash(
          CML.ScriptDataHash.from_raw_bytes(b(32, 0xdd)),
        );
      },
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with required_signers and script_data_hash", () => {
    const signers = CML.Ed25519KeyHashList.new();
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(b(28, 0x55)));
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(b(28, 0x66)));

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => {
        body.set_required_signers(signers);
        body.set_script_data_hash(
          CML.ScriptDataHash.from_raw_bytes(b(32, 0xdd)),
        );
      },
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with network_id", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => body.set_network_id(CML.NetworkId.new(1n)),
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with reference inputs", () => {
    const refInputs = inputList(input(0xee, 0), input(0xff, 1));

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => body.set_reference_inputs(refInputs),
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with required_observers (zero-ADA withdrawal trick)", () => {
    const scriptHash = CML.ScriptHash.from_raw_bytes(b(28, 0x77));
    const withdrawals = CML.MapRewardAccountToCoin.new();
    withdrawals.insert(
      CML.RewardAddress.new(1, CML.Credential.new_script(scriptHash)),
      0n,
    );

    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      (body) => body.set_withdrawals(withdrawals),
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("with vkey witness", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      undefined,
      (ws) => {
        const list = CML.VkeywitnessList.new();
        list.add(
          CML.Vkeywitness.new(
            CML.PublicKey.from_bytes(b(32, 0xaa)),
            CML.Ed25519Signature.from_raw_bytes(b(64, 0xbb)),
          ),
        );
        ws.set_vkeywitnesses(list);
      },
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });

  it("is_valid false", () => {
    const tx = buildTx(
      inputList(input(0x11, 0)),
      outputList(conwayOutput(address(0xab), CML.Value.from_coin(2_000_000n))),
      170_000n,
      undefined,
      undefined,
      false,
    );
    expect(roundTrip(tx)).toBe(tx.to_cbor_hex());
  });
});
