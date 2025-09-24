import { Data, fromText, LucidEvolution, mintingPolicyToId, Script, TxBuilder, validatorToAddress, validatorToScriptHash } from "@lucid-evolution/lucid";
import { DepositDatum, DepositDatumSchema, DepositEvent, MintRedeemer } from "../deposit/types.js";
import { Network } from "inspector/promises";
import { OutputReferenceSchema } from "../common.js";

export type DepositParams = {};

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
const mintingPolicy : Script = {
    type: "PlutusV3",
    script: ""
};
const policyId = mintingPolicyToId(mintingPolicy);

// Define your staking witness script
const witnessScript : Script = {
    type: "PlutusV3",
    script: ""
};
const witnessScriptHash = validatorToScriptHash(witnessScript);

const depositEvent :DepositEvent = {
    id: {
        txHash: {
            hash: ""
        },
        outputIndex: 1n
    },
    info: {
        l2_address: "addr1"
    }
};

const currDatum: DepositDatum = {
  event:depositEvent,
  inclusion_time: 900000n,
  witness: witnessScriptHash,
  refund_address: "addr1...",
  refund_datum: ""
};
const depositDatum = Data.to<DepositDatum>(currDatum, DepositDatum)
const depositNFT = policyId + fromText("H(l1_nonce)");
const deposit_address= "hub oracle deposit address"
const stakingscriptaddress = validatorToAddress("Custom",witnessScript);
const scriptRedeemer = "";

const mintRedeemer: MintRedeemer = { AuthenticateEvent: { nonce_input_index: 100n, event_output_index: 10n,hub_ref_input_index:1n,witness_registration_redeemer_index:1n } };
const authenticateEvent = Data.to(mintRedeemer, MintRedeemer);

export const depositTxBuilder = (
  lucid: LucidEvolution,
  params: DepositParams,
): TxBuilder => {
  const tx = lucid
    .newTx()
    .mintAssets({
      [depositNFT]: 1n
    },authenticateEvent)
    .pay.ToAddressWithData(deposit_address,{
      kind: "inline",
      value: depositDatum,
    },{lovelace:100000n,[depositNFT]: 1n })
    .registerStake(stakingscriptaddress)
    .validTo(Date.now() + 8000000)
    .attach.MintingPolicy(mintingPolicy);
    return tx;
};
