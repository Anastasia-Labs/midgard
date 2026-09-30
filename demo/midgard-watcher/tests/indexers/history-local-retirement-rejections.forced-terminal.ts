import {
  ForcedInclusionTxV1,
  HubOracleDatum,
  MerkleRoot,
  Proof,
  RootDomain,
  SettlementDatum,
  TxOrderDatum,
  TxOrderMintRedeemer,
  TxOrderSpendRedeemer,
  UserEventWitnessPublishRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { WatcherIndexedUserEvent } from "../../src/indexers/user-event-indexer.js";
import type { WatcherUserEventOriginFacts } from "../../src/indexers/user-event-origin.js";
import {
  ledgerReferenceIndex,
  syntheticUserEventTransaction,
  transactionInput,
} from "../support/local-user-event-authority-fixture.js";

// Ports the former nonDepositSpendBundle forced-order branch, using the current
// local origin's deployed hub and a referenced settlement creating transaction.
export const forcedTerminal = (
  facts: WatcherUserEventOriginFacts,
  event: WatcherIndexedUserEvent,
) => {
  if (event.witnessScriptHash === undefined)
    throw new Error("forced witness missing");
  const hub = Data.from(facts.activation.hubDatumCbor, HubOracleDatum);
  const datum = Data.from(event.datumCborHex, TxOrderDatum);
  const phasRoot = "a4".repeat(32);
  const domain = "ForcedTransactionsV1RootDomain" as const;
  const root = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1"),
        Buffer.from(Data.to(domain, RootDomain), "hex"),
        Buffer.from(phasRoot, "hex"),
        Buffer.from(Data.to(1n), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
  if (!("ScriptCredential" in hub.settlement_addr.paymentCredential))
    throw new Error("settlement script missing");
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(hub.settlement),
    CML.AssetName.from_hex(""),
    1n,
  );
  const settlementOutputs = CML.TransactionOutputList.new();
  settlementOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(
        `70${hub.settlement_addr.paymentCredential.ScriptCredential[0]}`,
      ),
      CML.Value.new(5_000_000n, assets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              deposits_root: "a5".repeat(32),
              withdrawals_root: "a6".repeat(32),
              forced_transactions_root: root,
              transactions_root: "a7".repeat(32),
              resolution_claim: null,
            },
            SettlementDatum,
          ),
        ),
      ),
    ),
  );
  const settlementInputs = CML.TransactionInputList.new();
  settlementInputs.add(transactionInput(`${"a8".repeat(32)}#0`));
  const settlement = CML.TransactionBody.new(
    settlementInputs,
    settlementOutputs,
    200_000n,
  );
  const settlementBody = settlement.to_canonical_cbor_hex();
  const settlementRef = `${CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(settlementBody),
  ).to_hex()}#0`;
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  refs.add(transactionInput(settlementRef));
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(event.outRef));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(`60${"88".repeat(28)}`),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(refs);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    -1n,
  );
  body.set_mint(mint);
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(event.witnessScriptHash),
      ),
      0n,
    ),
  );
  body.set_certs(certificates);
  const proof = {
    domain,
    root,
    phas_root: phasRoot,
    count: 1n,
    key: CML.PlutusData.from_cbor_hex(event.eventCborHex)
      .as_constr_plutus_data()!
      .fields()
      .get(0)
      .to_cbor_hex(),
    value: Data.to(
      {
        tx_id: datum.event.tx.tx_id,
        submitted_source: datum.event.tx.submitted_source,
        verdict: "ForcedTxValid",
      },
      ForcedInclusionTxV1,
    ),
    proof: [],
  };
  const membership = CML.PlutusDataList.new();
  membership.add(CML.PlutusData.from_cbor_hex(Data.to(phasRoot, MerkleRoot)));
  membership.add(CML.PlutusData.new_bytes(Buffer.from(proof.key, "hex")));
  membership.add(CML.PlutusData.new_bytes(Buffer.from(proof.value, "hex")));
  membership.add(CML.PlutusData.from_cbor_hex(Data.to([], Proof)));
  return {
    settlementBody,
    consume: syntheticUserEventTransaction(body, [
      {
        tag: CML.RedeemerTag.Spend,
        index: 0n,
        cbor: Data.to(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: ledgerReferenceIndex(
              refs,
              facts.activation.hubOutRef,
            ),
            settlement_ref_input_index: ledgerReferenceIndex(
              refs,
              settlementRef,
            ),
            burn_redeemer_index: 1n,
            membership_proof: proof,
            inclusion_proof_script_withdraw_redeemer_index: 3n,
            validity_override: "ForcedTxValid",
          },
          TxOrderSpendRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Mint,
        index: 0n,
        cbor: Data.to(
          {
            event: {
              BurnEventNFT: {
                nonce_asset_name: event.assetNameHex,
                witness_unregistration_redeemer_index: 2n,
              },
            },
            material_carriage: [],
          },
          TxOrderMintRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Cert,
        index: 0n,
        cbor: Data.to(
          { MintOrBurn: { targetPolicy: event.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: CML.PlutusData.new_list(membership).to_cbor_hex(),
      },
    ]),
  };
};
