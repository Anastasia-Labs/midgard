/**
 * Synthetic L1 transactions for the follower user-event fixture: list
 * deposit and withdrawal orders and forced orders on the synthetic origin
 * deployment, in the shapes the follower's event and watcher projections
 * admit.
 */
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";

import { WATCHER_EMULATOR_HISTORY_RECIPE } from "./deployment-authority-fixture.js";
import type { FollowerUserEventsDeployment } from "./follower-user-events-fixture.js";
import type { GenuineUserEventForcedPayload } from "./user-event-forced-order-fixture.js";

/** An address no projection follows: change and payouts go here. */
export const ELSEWHERE_ADDRESS_HEX = `60${"ee".repeat(28)}`;

const inputOf = (outRef: string): CML.TransactionInput => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

type Unit = readonly [policyId: string, assetName: string, quantity: bigint];

const multiAsset = (units: readonly Unit[]): CML.MultiAsset => {
  const assets = CML.MultiAsset.new();
  for (const [policyId, assetName, quantity] of units)
    assets.set(
      CML.ScriptHash.from_hex(policyId),
      CML.AssetName.from_hex(assetName),
      quantity,
    );
  return assets;
};

/** A plain synthetic transaction: no Plutus evaluation or signature claimed. */
export const syntheticTransaction = (
  input: Readonly<{
    inputs: readonly string[];
    outputs: readonly Readonly<{
      addressHex: string;
      lovelace: bigint;
      units?: readonly Unit[];
      datumCbor?: string;
    }>[];
    mint?: readonly Unit[];
  }>,
): string => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of input.inputs) inputs.add(inputOf(outRef));
  const outputs = CML.TransactionOutputList.new();
  for (const output of input.outputs)
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(output.addressHex),
        CML.Value.new(output.lovelace, multiAsset(output.units ?? [])),
        output.datumCbor === undefined
          ? undefined
          : CML.DatumOption.new_datum(
              CML.PlutusData.from_cbor_hex(output.datumCbor),
            ),
      ),
    );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  if (input.mint !== undefined && input.mint.length > 0) {
    const mint = CML.Mint.new();
    for (const [policyId, assetName, quantity] of input.mint)
      mint.set(
        CML.ScriptHash.from_hex(policyId),
        CML.AssetName.from_hex(assetName),
        quantity,
      );
    body.set_mint(mint);
  }
  return CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  ).to_canonical_cbor_hex();
};

/** A user event's id: the nonce outref its admitting transaction spends. */
export const userEventIdOf = (id: SDK.OutputReference) => {
  const cborHex = Data.to(id, SDK.OutputReference);
  return Object.freeze({
    id,
    cborHex,
    nonce: `${id.transactionId}#${id.outputIndex.toString()}`,
    key: eventKeyOfId(Buffer.from(cborHex, "hex")).toString("hex"),
  });
};

export type UserEventId = ReturnType<typeof userEventIdOf>;

/** The id whose nonce is output `outputIndex` of transaction `nonceByte`*32. */
export const userEventId = (nonceByte: string, outputIndex = 0n): UserEventId =>
  userEventIdOf({ transactionId: nonceByte.repeat(32), outputIndex });

const OWNER = "cd".repeat(28);
const OWNER_AUTH = { PublicKeyCredential: [OWNER] as [string] };
const OWNER_ADDRESS = { paymentCredential: OWNER_AUTH, stakeCredential: null };

/** Inclusion time every fixture list event carries. */
export const FIXTURE_LIST_INCLUSION_TIME = 1_000n;

/**
 * A deposit or withdrawal Order on the deployment's event list, admitted by
 * a transaction spending its nonce: the follower's event projection opens
 * it as the chain's (`events/derive.ts`).
 */
export const listOrderTransaction = (
  deployment: FollowerUserEventsDeployment,
  kind: "deposit" | "withdrawal",
  event: UserEventId,
): string => {
  const list = deployment.scripts.eventProjection.lists.find(
    (entry) => entry.kind === kind,
  );
  if (list === undefined) throw new Error(`the deployment has no ${kind} list`);
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id: event.id,
              info: {
                l2_address: OWNER_ADDRESS,
                l2_network_id: 0n,
                l2_datum: null,
              },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id: event.id,
              info: {
                body: {
                  l2_outref: event.id,
                  l2_owner: OWNER,
                  l2_value: new Map([["", new Map([["", 9_000_000n]])]]),
                  l1_address: OWNER_ADDRESS,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: OWNER_ADDRESS,
            refund_datum: "NoDatum",
          },
        };
  const bounds = WATCHER_EMULATOR_HISTORY_RECIPE.bounds;
  const plan = SDK.prepareEventHistoryPayload(payload, OWNER_AUTH, {
    inlineLimitBytes: BigInt(bounds.inlineLimitBytes),
    maxPayloadBytes: BigInt(bounds.maxPayloadBytes),
    maxPayloadNodes: BigInt(bounds.maxPayloadNodes),
  });
  if (plan.kind !== "Inline")
    throw new Error("fixture list events carry an inline payload");
  const datumCbor = Data.to(
    {
      position: { Key: [event.key] },
      next: null,
      protected_until: 0n,
      payload: {
        Order: {
          facts: {
            event_id: event.id,
            inclusion_time: FIXTURE_LIST_INCLUSION_TIME,
            location: plan.location,
            structural_lovelace: 2_000_000n,
            structural_refund_key: OWNER,
          },
        },
      },
    },
    SDK.EventHistoryNode,
  );
  return syntheticTransaction({
    inputs: [event.nonce],
    outputs: [
      {
        addressHex: list.listAddress,
        lovelace: 7_000_000n,
        units: [[list.policyId, event.key, 1n]],
        datumCbor,
      },
    ],
    mint: [[list.policyId, event.key, 1n]],
  });
};

/** The forced-order payload fields a datum carries, with its carriage. */
export type ForcedOrderPayload = Pick<
  GenuineUserEventForcedPayload,
  "tx_id" | "transaction_commitment" | "submitted_source" | "carriage"
>;

/** A well-formed payload over no particular native transaction. */
export const PLACEHOLDER_FORCED_PAYLOAD: ForcedOrderPayload = Object.freeze({
  tx_id: "71".repeat(32),
  transaction_commitment: "72".repeat(32),
  submitted_source: Object.freeze({
    compact_cbor: "80",
    witness_set_compact_cbor: "80",
    field_preimage_lengths_cbor: "80",
  }),
  carriage: Object.freeze([]),
});

/** The forced-order inclusion time the replay suites have always used. */
export const FORCED_ORDER_INCLUSION_TIME = BigInt(
  SDK.resolveEventInclusionTime(
    slotToBeginUnixTime(1_000, SLOT_CONFIG_NETWORK.Preprod),
    "Preprod",
  ),
);

/**
 * An ordinary forced-order mint under the deployment's tx-order scripts: it
 * spends the order's nonce, references the activation's hub-oracle output,
 * registers the per-order witness and mints the order token onto the
 * tx-order address with its `TxOrderDatum`. No Plutus evaluation claimed.
 */
export const forcedOrderTransaction = (
  deployment: FollowerUserEventsDeployment,
  event: UserEventId,
  payload: ForcedOrderPayload = PLACEHOLDER_FORCED_PAYLOAD,
): string => {
  const scripts = deployment.scripts.forcedOrder;
  const witness = SDK.userEventWitnessScriptHash(event.key);
  const datum = Data.to(
    {
      inclusion_time: FORCED_ORDER_INCLUSION_TIME,
      witness,
      refund_address: {
        paymentCredential: { PublicKeyCredential: ["88".repeat(28)] },
        stakeCredential: null,
      },
      refund_datum: "NoDatum",
      event: {
        id: event.id,
        tx: {
          tx_id: payload.tx_id,
          transaction_commitment: payload.transaction_commitment,
          submitted_source: payload.submitted_source,
        },
      },
    },
    SDK.TxOrderDatum,
  );
  const policy = CML.ScriptHash.from_hex(scripts.policyId);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(scripts.addressHex),
      CML.Value.new(
        3_000_000n,
        multiAsset([[scripts.policyId, event.key, 1n]]),
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(inputOf(event.nonce));
  const references = CML.TransactionInputList.new();
  references.add(inputOf(deployment.hubOutRef));
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_reg_cert(
      CML.Credential.new_script(CML.ScriptHash.from_hex(witness)),
      0n,
    ),
  );
  const mint = CML.Mint.new();
  mint.set(policy, CML.AssetName.from_hex(event.key), 1n);
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(references);
  body.set_certs(certificates);
  body.set_mint(mint);
  body.set_ttl(1_000n);
  const material = payload.carriage.map((entry) => {
    if (
      typeof entry !== "object" ||
      entry === null ||
      !("Inline" in entry) ||
      typeof entry.Inline !== "object" ||
      entry.Inline === null ||
      !("preimage" in entry.Inline) ||
      typeof entry.Inline.preimage !== "string"
    )
      throw new Error("a fixture forced order carries inline carriage only");
    return { Inline: { preimage: entry.Inline.preimage } };
  });
  const redeemers = CML.LegacyRedeemerList.new();
  const mintRedeemer = Data.to(
    {
      event: {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 1n,
        },
      },
      material_carriage: material,
    },
    SDK.TxOrderMintRedeemer,
  );
  const certRedeemer = Data.to(
    { MintOrBurn: { targetPolicy: scripts.policyId } },
    SDK.UserEventWitnessPublishRedeemer,
  );
  for (const [tag, cbor] of [
    [CML.RedeemerTag.Mint, mintRedeemer],
    [CML.RedeemerTag.Cert, certRedeemer],
  ] as const)
    redeemers.add(
      CML.LegacyRedeemer.new(
        tag,
        0n,
        CML.PlutusData.from_cbor_hex(cbor),
        CML.ExUnits.new(0n, 0n),
      ),
    );
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  body.set_script_data_hash(
    CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0x6a)),
  );
  return CML.Transaction.new(body, witnessSet, true).to_canonical_cbor_hex();
};

/** Spends a forced order's output and burns its token: the unit leaves L1. */
export const forcedOrderBurnTransaction = (
  deployment: FollowerUserEventsDeployment,
  event: UserEventId,
  orderTransactionCbor: string,
): string =>
  syntheticTransaction({
    inputs: [
      `${CML.hash_transaction(
        CML.Transaction.from_cbor_hex(orderTransactionCbor).body(),
      ).to_hex()}#0`,
    ],
    outputs: [{ addressHex: ELSEWHERE_ADDRESS_HEX, lovelace: 2_800_000n }],
    mint: [[deployment.scripts.forcedOrder.policyId, event.key, -1n]],
  });

export const transactionHash = (transactionCbor: string): string =>
  CML.hash_transaction(
    CML.Transaction.from_cbor_hex(transactionCbor).body(),
  ).to_hex();
