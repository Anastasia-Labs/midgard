/** Raw synthetic history outputs for exact-byte golden tests, not admission evidence. */
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const historyWitnessFixture = async (
  payload: SDK.EventHistoryPayload,
  inclusionTime: bigint,
  observation: SDK.AuthenticatedL1Observation,
) => {
  const policy = "18".repeat(28);
  const hubPolicy = "16".repeat(28);
  const addr: SDK.AddressData = {
    paymentCredential: { ScriptCredential: [policy] },
    stakeCredential: null,
  };
  const hubDatum: SDK.HubOracleDatum = {
    registered_operators: policy,
    active_operators: policy,
    retired_operators: policy,
    scheduler: policy,
    state_queue: "15".repeat(28),
    fraud_proof_catalogue: policy,
    fraud_proof: policy,
    deposit: policy,
    withdrawal: policy,
    tx_order: policy,
    settlement: policy,
    payout: policy,
    registered_operators_addr: addr,
    active_operators_addr: addr,
    retired_operators_addr: addr,
    scheduler_addr: addr,
    state_queue_addr: addr,
    fraud_proof_catalogue_addr: addr,
    fraud_proof_addr: addr,
    deposit_addr: addr,
    withdrawal_addr: addr,
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: policy,
  };
  const event =
    "DepositPayload" in payload
      ? payload.DepositPayload.event
      : payload.WithdrawalPayload.event;
  const key = await Effect.runPromise(SDK.eventHistoryKey(event.id));
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: event.id,
          inclusion_time: inclusionTime,
          location: { Inline: { payload } },
          structural_lovelace: "DepositPayload" in payload ? 2_000_000n : 0n,
          structural_refund_key: "44".repeat(28),
        },
      },
    },
  };
  const anchor: UTxO = {
    txHash: "a2".repeat(32),
    outputIndex: 0,
    address: credentialToAddress("Preview", { type: "Script", hash: policy }),
    assets: { lovelace: 5_000_000n, [policy + key]: 1n },
    datum: Data.to(node, SDK.EventHistoryNode),
  };
  const hubOracleUtxo: UTxO = {
    txHash: "a3".repeat(32),
    outputIndex: 0,
    address: credentialToAddress("Preview", {
      type: "Script",
      hash: hubPolicy,
    }),
    assets: {
      lovelace: 10_000_000n,
      [hubPolicy + SDK.HUB_ORACLE_ASSET_NAME]: 1n,
    },
    datum: Data.to(hubDatum, SDK.HubOracleDatum),
  };
  return {
    observation,
    anchor,
    hubOracleUtxo,
    hubOraclePolicyId: hubPolicy,
    network: "Preview" as const,
    history: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
      retentionAddress: credentialToAddress("Preview", {
        type: "Script",
        hash: "ee".repeat(28),
      }),
    },
  };
};
