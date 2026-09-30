import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { midgardTxFieldCommitmentsFromSource } from "@al-ft/midgard-core/consensus-validation";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  HubOracleDatumSchema,
  TxOrderMintRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import {
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from ".././user-event-reference-authority.js";
import { isHexBytes } from "./policy.js";
import { type EventSchema, type WatcherUserEventKind } from "./types.js";

export const dataRoundTrip = <T>(
  cborHex: string,
  schema: EventSchema,
): T | null => {
  try {
    const value = Data.from(cborHex, schema) as T;
    const canonicalInput =
      CML.PlutusData.from_cbor_hex(cborHex).to_canonical_cbor_hex();
    const canonicalRoundTrip = CML.PlutusData.from_cbor_hex(
      Data.to(value as never, schema),
    ).to_canonical_cbor_hex();
    return canonicalRoundTrip === canonicalInput ? value : null;
  } catch {
    return null;
  }
};

export const outputReference = (input: CML.TransactionInput): string =>
  `${input.transaction_id().to_hex()}#${input.index().toString()}`;

export const mintPolicyIndex = (mint: CML.Mint, policyId: string): number => {
  const keys = mint.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    if (keys.get(index).to_hex() === policyId) {
      return index;
    }
  }
  return -1;
};

export const matchingRedeemer = (
  transaction: WatcherNormalizedL1Block["transactions"][number],
  purpose: "spend" | "mint" | "certificate",
  index: number,
) => {
  const matches = transaction.redeemers.filter(
    (redeemer) =>
      redeemer.purpose === purpose && redeemer.index === index.toString(),
  );
  return matches.length === 1 ? matches[0]! : null;
};

export const redeemerAtGlobalIndex = (
  transaction: WatcherNormalizedL1Block["transactions"][number],
  index: bigint,
) =>
  index >= 0n && index <= BigInt(Number.MAX_SAFE_INTEGER)
    ? (transaction.redeemers[Number(index)] ?? null)
    : null;

type UserEventMintRedeemerBody =
  | Readonly<{
      AuthenticateEvent: Readonly<{
        nonce_input_index: bigint;
        event_output_index: bigint;
        hub_ref_input_index: bigint;
        witness_registration_redeemer_index: bigint;
      }>;
    }>
  | Readonly<{
      BurnEventNFT: Readonly<{
        nonce_asset_name: string;
        witness_unregistration_redeemer_index: bigint;
      }>;
    }>;

type DecodedUserEventMintRedeemer = Readonly<{
  event: UserEventMintRedeemerBody;
  /**
   * The §8 carriage vector, present only at the tx-order policy. `null` at the
   * three policies whose redeemer is the bare enum — a distinction the type keeps
   * so a later reader cannot mistake "this policy carries no material" for "this
   * order declared no carriage".
   */
  materialCarriage: readonly unknown[] | null;
}>;

/**
 * Decodes a user-event mint redeemer at the spelling the policy in question
 * actually uses.
 *
 * Three of the four user-event policies take `user_events.MintRedeemer`
 * unchanged. **The tx-order policy does not**: #594's owner ruling gave it its own
 * `MintRedeemer`, which wraps that enum beside the §8 `FieldCarriageV1` vector for
 * the order's material. Its wire form is therefore `Constr 0 [<enum>, <list>]`,
 * and a bare-enum decode of it fails — which is why this takes `kind` and is not
 * one schema for all four.
 *
 * Discriminating on the policy rather than trying both spellings is deliberate.
 * The two forms are structurally distinguishable, so a permissive decoder is
 * writable, but it would index a forced order out of a redeemer shape the tx-order
 * mint cannot accept, and a verifier that accepts more than the validator does is
 * worse than no verifier. An old-shape redeemer at the tx-order policy is
 * therefore a decode failure, and its consequence is this module's ordinary one:
 * the containing block yields no observation at all (see `scanCreatedEvents`).
 */
export const decodeMintRedeemer = (
  bytesHex: string,
  kind: WatcherUserEventKind,
): DecodedUserEventMintRedeemer | null => {
  if (kind !== "forced_order") {
    const event = dataRoundTrip<UserEventMintRedeemerBody>(
      bytesHex,
      UserEventMintRedeemer as unknown as EventSchema,
    );
    return event === null ? null : { event, materialCarriage: null };
  }
  const wrapped = dataRoundTrip<
    Readonly<{
      event: UserEventMintRedeemerBody;
      material_carriage: readonly unknown[];
    }>
  >(bytesHex, TxOrderMintRedeemer as unknown as EventSchema);
  return wrapped === null
    ? null
    : { event: wrapped.event, materialCarriage: wrapped.material_carriage };
};

/**
 * How many of §2.5's nine slots a forced order's payload commits material to.
 *
 * This is the whole input the mint's **exhaustion** rule needs beyond the
 * redeemer itself: the vector is positional over the non-empty slots, so its
 * length must equal this count exactly. The nine commitments come out of the
 * payload's own compact structures positionally (§4 has no field-index domain
 * separation, so the slot has to come from the structure), and
 * `forcedPayloadMatchesSubmittedSource` has already bound those structures to the
 * carried `tx_id` and commitment by the time this is consulted.
 *
 * Returns `null` when the payload is not a decodable §4 binding, so a caller
 * cannot read a count off a payload nothing authenticated.
 */
export const forcedOrderMaterialFieldCount = (
  payload: unknown,
): number | null => {
  const candidate = payload as {
    submitted_source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
  };
  if (
    !isHexBytes(candidate.submitted_source?.compact_cbor) ||
    !isHexBytes(candidate.submitted_source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.submitted_source.field_preimage_lengths_cbor)
  ) {
    return null;
  }
  try {
    return midgardTxFieldCommitmentsFromSource(
      {
        compactCbor: Buffer.from(
          candidate.submitted_source.compact_cbor,
          "hex",
        ),
        witnessSetCompactCbor: Buffer.from(
          candidate.submitted_source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          candidate.submitted_source.field_preimage_lengths_cbor,
          "hex",
        ),
      },
      "forced",
    ).filter((commitment) => !commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT))
      .length;
  } catch {
    return null;
  }
};

export const decodeWitnessRedeemer = (
  bytesHex: string,
):
  | Readonly<{ MintOrBurn: Readonly<{ targetPolicy: string }> }>
  | Readonly<{
      RegisterToProveNotRegistered: Readonly<{
        registrationCertificateIndex: bigint;
      }>;
    }>
  | Readonly<{
      UnregisterToProveNotRegistered: Readonly<{
        registrationCertificateIndex: bigint;
      }>;
    }>
  | null =>
  dataRoundTrip(
    bytesHex,
    UserEventWitnessPublishRedeemer as unknown as EventSchema,
  );

export const registeredScriptHashAt = (
  body: CML.TransactionBody,
  index: number,
  registration: boolean,
): string | null => {
  const certificates = body.certs();
  if (certificates === undefined || index < 0 || index >= certificates.len()) {
    return null;
  }
  const certificate = certificates.get(index);
  const credential = registration
    ? (certificate.as_stake_registration()?.stake_credential() ??
      certificate.as_reg_cert()?.stake_credential())
    : (certificate.as_stake_deregistration()?.stake_credential() ??
      certificate.as_unreg_cert()?.stake_credential());
  return credential?.as_script()?.to_hex() ?? null;
};

export const referencedOutRefAt = (
  body: CML.TransactionBody,
  index: bigint,
): string | null => {
  const inputs = body.reference_inputs();
  if (
    inputs === undefined ||
    index < 0n ||
    index >= BigInt(inputs.len()) ||
    index > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return null;
  }
  // Plutus reference-input indices follow ledger ordering, independent of the
  // order in the transaction body's CBOR set.
  const ordered = Array.from({ length: inputs.len() }, (_, position) => {
    const input = inputs.get(position);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const input = ordered[Number(index)]!;
  return `${input.txHash}#${input.outputIndex.toString()}`;
};

export const inlineDatumCbor = (output: CML.TransactionOutput): string | null =>
  output.datum()?.as_datum()?.to_cbor_hex() ?? null;

export const decodeHubAt = (
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  body: CML.TransactionBody,
  index: bigint,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
) => {
  const outRef = referencedOutRefAt(body, index);
  const output = watcherUserEventReferenceOutput(
    referenceEvidence,
    transactionHash,
    outRef,
  );
  const datumHex = output === null ? null : inlineDatumCbor(output);
  return datumHex === null ||
    output!.script_ref() !== undefined ||
    exactlyOneAsset(output!, deployment.appliedScriptHashes.hubOracleMint ?? "")
      ?.quantity !== 1n ||
    output!.address().payment_cred()?.as_script()?.to_hex() !==
      deployment.appliedScriptHashes.hubOracleMint
    ? null
    : dataRoundTrip<Record<string, unknown>>(
        datumHex,
        asDataType<EventSchema>(HubOracleDatumSchema),
      );
};

export const canonicalBody = (bytesHex: string): CML.TransactionBody | null => {
  try {
    const body = CML.TransactionBody.from_cbor_hex(bytesHex);
    return body.to_cbor_hex() === bytesHex ? body : null;
  } catch {
    return null;
  }
};

export const exactlyOneAsset = (
  output: CML.TransactionOutput,
  policyId: string,
): Readonly<{ assetNameHex: string; quantity: bigint }> | null => {
  const assets = output
    .amount()
    .multi_asset()
    .get_assets(CML.ScriptHash.from_hex(policyId));
  if (assets === undefined || assets.len() !== 1) {
    return null;
  }
  const keys = assets.keys();
  const asset = keys.get(0);
  const quantity = assets.get(asset);
  return quantity === undefined
    ? null
    : Object.freeze({ assetNameHex: asset.to_hex(), quantity });
};
