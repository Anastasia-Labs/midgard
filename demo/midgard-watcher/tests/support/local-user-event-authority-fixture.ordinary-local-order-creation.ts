import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core";
import {
  type AddressData,
  EventHistoryNode,
  EventHistoryObserve,
  outputReferenceToPlutusDataCbor,
  resolveEventInclusionTime,
  TxOrderDatum,
  TxOrderMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { WatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { type WatcherUserEventOriginFacts } from "../../src/indexers/user-event-origin.js";
import { type WatcherRuleBundle } from "../../src/verification/rule-bundle.js";
import {
  h32,
  syntheticUserEventTransaction,
  transactionInput,
} from "./local-user-event-authority-fixture.synthetic-user-event-transaction.js";
import {
  type GenuineUserEventForcedPayload,
  genuineUserEventForcedPayloadForCanonicalTx,
} from "./user-event-forced-order-fixture.js";

export const ordinaryLocalOrderCreation = (
  facts: WatcherUserEventOriginFacts,
  kind: "withdrawal" | "forced_order",
  nativeCbor: Uint8Array,
  options: Readonly<{
    nonceByte?: string;
    withdrawalInfo?: WithdrawalInfo;
    structuralLovelace?: bigint;
    withdrawalL2OutRef?: Readonly<{
      transactionId: string;
      outputIndex: bigint;
    }>;
    forcedPayload?: GenuineUserEventForcedPayload;
  }> = {},
) => {
  const eventId = {
    transactionId: h32(
      options.nonceByte ?? (kind === "withdrawal" ? "c2" : "c3"),
    ),
    outputIndex: 0n,
  };
  const eventIdCborHex = outputReferenceToPlutusDataCbor({
    txHash: eventId.transactionId,
    outputIndex: 0,
  });
  const assetName = Buffer.from(
    blake2b(Buffer.from(eventIdCborHex, "hex"), { dkLen: 32 }),
  ).toString("hex");
  const witness = userEventWitnessScriptHash(assetName);
  const scripts =
    kind === "withdrawal"
      ? facts.scripts.withdrawal
      : facts.scripts.forcedOrder;
  const address: {
    paymentCredential: { PublicKeyCredential: [string] };
    stakeCredential: null;
  } = {
    paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
    stakeCredential: null,
  };
  const common = {
    inclusion_time: BigInt(
      resolveEventInclusionTime(
        slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
        "Preprod",
      ),
    ),
    witness,
    refund_address: address,
    refund_datum: "NoDatum" as const,
  };
  const payload =
    options.forcedPayload ??
    genuineUserEventForcedPayloadForCanonicalTx(
      encodeMidgardForcedTxCanonical(
        decodeMidgardNativeTxFullFromCanonicalCbor(nativeCbor),
      ),
    );
  const datum =
    kind === "withdrawal"
      ? Data.to(
          {
            position: { Key: [assetName] },
            next: null,
            protected_until: common.inclusion_time,
            payload: {
              Order: {
                facts: {
                  event_id: eventId,
                  inclusion_time: common.inclusion_time,
                  structural_lovelace: options.structuralLovelace ?? 0n,
                  structural_refund_key: "88".repeat(28),
                  location: {
                    Inline: {
                      payload: {
                        WithdrawalPayload: {
                          event: {
                            id: eventId,
                            info: options.withdrawalInfo ?? {
                              body: {
                                l2_outref:
                                  options.withdrawalL2OutRef ?? eventId,
                                l2_owner: "89".repeat(28),
                                l2_value: new Map(),
                                l1_address: address,
                                l1_datum: "NoDatum",
                              },
                              signature: ["aa", "bb"],
                              validity: "WithdrawalIsValid",
                            },
                          },
                          refund_address: address,
                          refund_datum: "NoDatum",
                        },
                      },
                    },
                  },
                },
              },
            },
          },
          EventHistoryNode,
        )
      : Data.to(
          {
            ...common,
            event: {
              id: eventId,
              tx: {
                tx_id: payload.tx_id,
                transaction_commitment: payload.transaction_commitment,
                submitted_source: payload.submitted_source,
              },
            },
          },
          TxOrderDatum,
        );
  const policy = CML.ScriptHash.from_hex(scripts.policyId);
  const assets = CML.MultiAsset.new();
  assets.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scripts.addressHex),
      CML.Value.new(
        3_000_000n +
          (kind === "withdrawal" ? (options.structuralLovelace ?? 0n) : 0n),
        assets,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(`${eventId.transactionId}#0`));
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  if (kind === "withdrawal") {
    inputs.add(transactionInput(`${h32("ff")}#1`));
    const rootAssets = CML.MultiAsset.new();
    rootAssets.set(policy, CML.AssetName.from_hex(""), 1n);
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(scripts.addressHex),
        CML.Value.new(2_000_000n, rootAssets),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to(
              {
                position: "Root",
                next: assetName,
                protected_until: common.inclusion_time,
                payload: "RootContent",
              },
              EventHistoryNode,
            ),
          ),
        ),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    body.set_reference_inputs(refs);
    body.set_ttl(1_000n);
    const mint = CML.Mint.new();
    mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
    body.set_mint(mint);
    const withdrawals = CML.MapRewardAccountToCoin.new();
    withdrawals.insert(
      CML.RewardAddress.new(0, CML.Credential.new_script(policy)),
      0n,
    );
    body.set_withdrawals(withdrawals);
    const cbor = syntheticUserEventTransaction(body, [
      { tag: CML.RedeemerTag.Spend, index: 1n, cbor: Data.to(1n) },
      { tag: CML.RedeemerTag.Mint, index: 0n, cbor: Data.to(0n) },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: Data.to(
          {
            Apply: {
              hub_reference_index: 0n,
              operation: {
                InsertOrder: {
                  predecessor_input_index: 1n,
                  predecessor_output_index: 1n,
                  order_output_index: 0n,
                  nonce_input_index: 0n,
                  external_reference_index: null,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      },
    ]);
    return { cbor, eventId, eventIdCborHex, payload };
  }
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_reg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
      0n,
    ),
  );
  const mint = CML.Mint.new();
  mint.set(policy, CML.AssetName.from_hex(assetName), 1n);
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(refs);
  body.set_certs(certificates);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const mintEvent = {
    AuthenticateEvent: {
      nonce_input_index: 0n,
      event_output_index: 0n,
      hub_ref_input_index: 0n,
      witness_registration_redeemer_index: 1n,
    },
  };
  const materialCarriage = payload.carriage.map((entry) => {
    if (
      typeof entry !== "object" ||
      entry === null ||
      !("Inline" in entry) ||
      typeof entry.Inline !== "object" ||
      entry.Inline === null ||
      !("preimage" in entry.Inline) ||
      typeof entry.Inline.preimage !== "string"
    )
      throw new Error("ordinary forced fixture requires inline carriage");
    return { Inline: { preimage: entry.Inline.preimage } };
  });
  const cbor = syntheticUserEventTransaction(body, [
    {
      tag: CML.RedeemerTag.Mint,
      index: 0n,
      cbor: Data.to(
        { event: mintEvent, material_carriage: materialCarriage },
        TxOrderMintRedeemer,
      ),
    },
    {
      tag: CML.RedeemerTag.Cert,
      index: 0n,
      cbor: Data.to(
        { MintOrBurn: { targetPolicy: scripts.policyId } },
        UserEventWitnessPublishRedeemer,
      ),
    },
  ]);
  return { cbor, eventId, eventIdCborHex, payload };
};

export type LocalReplayUserEventRequest = Readonly<{
  deposit?: Readonly<{ nonceByte: string; l2Address: AddressData }>;
  withdrawals?: readonly Readonly<{
    key: string;
    nonceByte: string;
    l2OutRef: Readonly<{ transactionId: string; outputIndex: bigint }>;
    info?: WithdrawalInfo;
  }>[];
  forcedOrders?: readonly Readonly<{
    key: string;
    nonceByte: string;
    payload: GenuineUserEventForcedPayload;
  }>[];
}>;

export type LocalReplayUserEventAuthorities = Readonly<{
  deposit: WatcherLocalUserEventAuthority | null;
  withdrawals: Readonly<Record<string, WatcherLocalUserEventAuthority>>;
  forcedOrders: Readonly<Record<string, WatcherLocalUserEventAuthority>>;
  /** Rule bundle bound to the local publication's deployment identity. */
  ruleBundle: WatcherRuleBundle;
  ruleBundleCommitment: string;
  close: () => Promise<void>;
}>;
