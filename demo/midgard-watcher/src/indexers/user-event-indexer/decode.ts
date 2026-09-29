import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardNativeTxProofFieldLengths,
  verifyMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { midgardTxFieldCommitmentsFromSource } from "@al-ft/midgard-core/consensus-validation";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  ConfirmedState,
  DepositDatumSchema,
  DepositEventSchema,
  EventHistoryOperation,
  eventHistoryRetirementOperation,
  ForcedInclusionTxV1Schema,
  HubOracleDatumSchema,
  LinkedListDatum,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
  PayoutMintRedeemerSchema,
  Proof,
  resolveEventInclusionTime,
  RootDomainSchema,
  SettlementDatumSchema,
  TxOrderDatumSchema,
  TxOrderEventSchema,
  TxOrderMintRedeemer,
  TxOrderSpendRedeemerSchema,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalEventSchema,
  WithdrawalOrderDatumSchema,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import {
  historyCardanoDatumMatches,
  historyListObservation,
  historyNodeFromOutput,
  historyOrderContinuationMatches,
  historyPayloadFromNode,
  historyRawField,
  historyRetirementObservation,
  historyWithdrawalPayoutDatum,
} from ".././authenticated-event-history.js";
import {
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from ".././user-event-reference-authority.js";
import {
  eventPolicy,
  isHex28,
  isHex32,
  isHexBytes,
  isNatural,
  kindForPolicy,
  sha256Bytes,
  watcherForcedOperatorVerdict,
} from "./policy.js";
import {
  type EventSchema,
  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventKind,
  type WatcherUserEventTerminalStatus,
} from "./types.js";

const dataRoundTrip = <T>(cborHex: string, schema: EventSchema): T | null => {
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

const mintPolicyIndex = (mint: CML.Mint, policyId: string): number => {
  const keys = mint.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    if (keys.get(index).to_hex() === policyId) {
      return index;
    }
  }
  return -1;
};

const matchingRedeemer = (
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

const redeemerAtGlobalIndex = (
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
const decodeMintRedeemer = (
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
const forcedOrderMaterialFieldCount = (payload: unknown): number | null => {
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

const decodeWitnessRedeemer = (
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

const registeredScriptHashAt = (
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

const referencedOutRefAt = (
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

const inlineDatumCbor = (output: CML.TransactionOutput): string | null =>
  output.datum()?.as_datum()?.to_cbor_hex() ?? null;

const decodeHubAt = (
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

const canonicalBody = (bytesHex: string): CML.TransactionBody | null => {
  try {
    const body = CML.TransactionBody.from_cbor_hex(bytesHex);
    return body.to_cbor_hex() === bytesHex ? body : null;
  } catch {
    return null;
  }
};

const outputPolicies = (output: CML.TransactionOutput): readonly string[] => {
  const value = output.amount();
  if (!value.has_multiassets()) {
    return [];
  }
  const keys = value.multi_asset().keys();
  const result: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    result.push(keys.get(index).to_hex());
  }
  return result;
};

const exactlyOneAsset = (
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

const canonicalDatumForOutput = (
  transaction: WatcherNormalizedL1Block["transactions"][number],
  outputIndex: number,
  output: CML.TransactionOutput,
): Readonly<{ cborHex: string; digest: string }> | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined || output.script_ref() !== undefined) {
    return null;
  }
  const cborHex = datum.to_cbor_hex();
  const normalizedDatum = datum.to_canonical_cbor_hex();
  const l1Utxo = transaction.utxos.find(
    (candidate) => candidate.outputIndex === outputIndex.toString(),
  );
  if (
    l1Utxo === undefined ||
    l1Utxo.output.bytesHex !== output.to_canonical_cbor_hex() ||
    l1Utxo.datum === null ||
    l1Utxo.datum.bytes.bytesHex !== normalizedDatum ||
    l1Utxo.datum.datumHash !==
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(normalizedDatum),
      ).to_hex()
  ) {
    return null;
  }
  // The adapter descriptors are normalized; the event retains the original
  // datum from the authenticated transaction body, together with its own digest.
  return Object.freeze({
    cborHex,
    digest: sha256Bytes(Buffer.from(cborHex, "hex")),
  });
};

const nonceAssetName = (input: CML.TransactionInput): string => {
  const cbor = outputReferenceToPlutusDataCbor({
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
  });
  return Buffer.from(blake2b(Buffer.from(cbor, "hex"), { dkLen: 32 })).toString(
    "hex",
  );
};

const eventSchemas = (
  kind: WatcherUserEventKind,
): Readonly<{ datum: EventSchema; event: EventSchema }> =>
  kind === "deposit"
    ? { datum: DepositDatumSchema, event: DepositEventSchema }
    : kind === "withdrawal"
      ? { datum: WithdrawalOrderDatumSchema, event: WithdrawalEventSchema }
      : { datum: TxOrderDatumSchema, event: TxOrderEventSchema };

const parseEventDatum = (
  kind: WatcherUserEventKind,
  cborHex: string,
): Readonly<{
  event: unknown;
  eventCborHex: string;
  inclusionTime: bigint;
  witness: string;
}> | null => {
  const schemas = eventSchemas(kind);
  const datum = dataRoundTrip<{
    event: unknown;
    inclusion_time: bigint;
    witness: string;
  }>(cborHex, schemas.datum);
  if (
    datum === null ||
    typeof datum.inclusion_time !== "bigint" ||
    !isHex28(datum.witness)
  ) {
    return null;
  }
  try {
    return Object.freeze({
      event: datum.event,
      eventCborHex: Data.to(datum.event as never, schemas.event),
      inclusionTime: datum.inclusion_time,
      witness: datum.witness,
    });
  } catch {
    return null;
  }
};

const eventIdMatchesNonce = (
  kind: WatcherUserEventKind,
  event: unknown,
  input: CML.TransactionInput,
): boolean => {
  const record = event as {
    id?: { transactionId?: unknown; outputIndex?: unknown };
  };
  return (
    (kind === "forced_order" || kind === "deposit" || kind === "withdrawal") &&
    record.id?.transactionId === input.transaction_id().to_hex() &&
    record.id.outputIndex === input.index()
  );
};

const scanHistoryOutput = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  references: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  kind: "deposit" | "withdrawal",
  outputIndex: number,
): { event?: WatcherIndexedUserEvent } | null => {
  const fields = eventPolicy(policy, kind);
  const body = canonicalBody(transaction.body.bytesHex)!;
  const output = body.outputs().get(outputIndex);
  const authenticated = historyNodeFromOutput(output, fields.policyId);
  const observed = historyListObservation(transaction, fields.policyId);
  if (
    authenticated === null ||
    observed === null ||
    output.address().to_hex() !== fields.addressHex
  )
    return null;
  const { node, key } = authenticated;
  const observe = observed.observe;
  if ("Initialize" in observe)
    return node.payload === "RootContent" &&
      observe.Initialize.root_output_index === BigInt(outputIndex)
      ? {}
      : null;
  const hub = decodeHubAt(
    references,
    transaction.txHash,
    body,
    observe.Apply.hub_reference_index,
    deployment,
  );
  if (
    hub === null ||
    hub[kind] !== fields.policyId ||
    !addressMatchesData(output.address(), hub[kind + "_addr"])
  )
    return null;
  const operation = observe.Apply.operation;
  if (node.payload === "RootContent" || !("Order" in node.payload)) return {};
  const admission =
    "InsertOrder" in operation
      ? operation.InsertOrder
      : "PromoteFiller" in operation
        ? operation.PromoteFiller
        : null;
  if (
    admission === null ||
    admission.order_output_index !== BigInt(outputIndex)
  ) {
    return "predecessor_output_index" in Object.values(operation)[0]! &&
      (Object.values(operation)[0] as { predecessor_output_index: bigint })
        .predecessor_output_index === BigInt(outputIndex)
      ? {}
      : null;
  }
  const nonceIndex = admission.nonce_input_index;
  if (nonceIndex < 0n || nonceIndex >= BigInt(body.inputs().len())) return null;
  const nonce = body.inputs().get(Number(nonceIndex));
  const facts = node.payload.Order.facts;
  const external =
    admission.external_reference_index === null
      ? null
      : watcherUserEventReferenceOutput(
          references,
          transaction.txHash,
          referencedOutRefAt(body, admission.external_reference_index),
        );
  const opened = historyPayloadFromNode(
    authenticated,
    kind,
    key,
    external,
    deployment.appliedScriptHashes[kind + "HistoryRetentionSpend"],
  );
  const ttl = body.ttl();
  const mint = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(fields.policyId));
  const expectedMint = "PromoteFiller" in operation ? 0n : 1n;
  if (expectedMint === 1n) {
    const index =
      body.mint() === undefined
        ? -1
        : mintPolicyIndex(body.mint()!, fields.policyId);
    if (index < 0 || matchingRedeemer(transaction, "mint", index) === null)
      return null;
  }
  if (
    opened === null ||
    key !== nonceAssetName(nonce) ||
    ttl === undefined ||
    ttl > BigInt(Number.MAX_SAFE_INTEGER) ||
    facts.inclusion_time !==
      BigInt(
        resolveEventInclusionTime(
          slotToBeginUnixTime(
            Number(ttl),
            policy.customNetwork?.slotConfig ??
              SLOT_CONFIG_NETWORK[policy.network],
          ),
          policy.network,
        ),
      ) ||
    (expectedMint === 0n
      ? mint !== undefined && mint.len() !== 0
      : mint?.len() !== 1 || mint.get(CML.AssetName.from_hex(key)) !== 1n)
  )
    return null;
  const { payload, payloadCbor, eventCbor: eventCborHex } = opened;
  const event =
    "DepositPayload" in payload
      ? payload.DepositPayload.event
      : payload.WithdrawalPayload.event;
  if (
    !eventIdMatchesNonce(kind, event, nonce) ||
    output.amount().coin() < facts.structural_lovelace
  )
    return null;
  const datum = canonicalDatumForOutput(transaction, outputIndex, output);
  if (datum === null) return null;
  const outputCborHex = output.to_cbor_hex();
  return {
    event: Object.freeze({
      kind,
      eventId: outputReferenceToPlutusDataCbor({
        txHash: nonce.transaction_id().to_hex(),
        outputIndex: Number(nonce.index()),
      }),
      outRef: `${transaction.txHash}#${outputIndex}`,
      transactionHash: transaction.txHash,
      outputIndex: String(outputIndex),
      nonceOutRef: outputReference(nonce),
      policyId: fields.policyId,
      spendScriptHash: fields.spendScriptHash,
      addressHex: fields.addressHex,
      assetNameHex: key,
      inclusionTime: String(facts.inclusion_time),
      eventCborHex,
      historyPayloadCborHex: payloadCbor,
      datumCborHex: datum.cborHex,
      outputCborHex,
      eventContentDigest: sha256Bytes(Buffer.from(eventCborHex, "hex")),
      datumDigest: datum.digest,
      outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
      originPointDigest: block.chainPoint.pointDigest,
      originChainPointId: block.chainPoint.chainPointId,
      originBlockHash: block.chainPoint.blockHash,
      originSlot: block.chainPoint.slot,
      originBlockNo: block.chainPoint.blockNo,
      finalityStatus: "pending",
    }),
  };
};

export const scanCreatedTransactionEvents = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  events: WatcherIndexedUserEvent[],
): true | null => {
  if (!transaction.isValid) {
    return true;
  }
  const body = canonicalBody(transaction.body.bytesHex);
  if (body === null) {
    return null;
  }
  const outputs = body.outputs();
  const inputs = body.inputs();
  const mint = body.mint();
  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const output = outputs.get(outputIndex);
    const knownPolicies = outputPolicies(output)
      .map((policyId) => [policyId, kindForPolicy(policy, policyId)] as const)
      .filter(
        (entry): entry is readonly [string, WatcherUserEventKind] =>
          entry[1] !== null,
      );
    if (knownPolicies.length === 0) {
      continue;
    }
    if (knownPolicies.length !== 1) {
      return null;
    }
    const [policyId, kind] = knownPolicies[0]!;
    const fields = eventPolicy(policy, kind);
    if (kind !== "forced_order") {
      const admitted = scanHistoryOutput(
        policy,
        block,
        transaction,
        referenceEvidence,
        deployment,
        kind,
        outputIndex,
      );
      if (admitted === null) return null;
      if (admitted.event !== undefined) events.push(admitted.event);
      continue;
    }
    if (mint === undefined) return null;
    const nft = exactlyOneAsset(output, policyId);
    const policyIndex = mintPolicyIndex(mint, policyId);
    const redeemer =
      policyIndex < 0
        ? null
        : matchingRedeemer(transaction, "mint", policyIndex);
    const decoded =
      redeemer === null
        ? null
        : decodeMintRedeemer(redeemer.bytes.bytesHex, kind);
    if (
      nft === null ||
      nft.quantity !== 1n ||
      policyIndex < 0 ||
      mint.get(
        CML.ScriptHash.from_hex(policyId),
        CML.AssetName.from_hex(nft.assetNameHex),
      ) !== 1n ||
      mint.get_assets(CML.ScriptHash.from_hex(policyId))?.len() !== 1 ||
      decoded === null ||
      !("AuthenticateEvent" in decoded.event)
    ) {
      return null;
    }
    const auth = decoded.event.AuthenticateEvent;
    if (
      auth.event_output_index !== BigInt(outputIndex) ||
      auth.nonce_input_index < 0n ||
      auth.nonce_input_index >= BigInt(inputs.len()) ||
      auth.hub_ref_input_index < 0n ||
      auth.witness_registration_redeemer_index < 0n
    ) {
      return null;
    }
    const nonceInput = inputs.get(Number(auth.nonce_input_index));
    const expectedAssetName = nonceAssetName(nonceInput);
    const expectedWitness = userEventWitnessScriptHash(expectedAssetName);
    const certificateRedeemer = redeemerAtGlobalIndex(
      transaction,
      auth.witness_registration_redeemer_index,
    );
    const certificateIndex =
      certificateRedeemer?.purpose === "certificate" &&
      isNatural(certificateRedeemer.index) &&
      BigInt(certificateRedeemer.index) <= BigInt(Number.MAX_SAFE_INTEGER)
        ? Number(certificateRedeemer.index)
        : -1;
    const witnessRedeemer =
      certificateRedeemer === null
        ? null
        : decodeWitnessRedeemer(certificateRedeemer.bytes.bytesHex);
    const datum = canonicalDatumForOutput(transaction, outputIndex, output);
    const hubDatum = decodeHubAt(
      referenceEvidence,
      transaction.txHash,
      body,
      auth.hub_ref_input_index,
      deployment,
    );
    const expectedHubPolicy = hubDatum?.tx_order;
    const expectedHubAddress = hubDatum?.tx_order_addr;
    if (
      nft.assetNameHex !== expectedAssetName ||
      output.address().to_hex() !== fields.addressHex ||
      output.address().payment_cred()?.as_script()?.to_hex() !==
        fields.spendScriptHash ||
      datum === null ||
      hubDatum === null ||
      expectedHubPolicy !== policyId ||
      !addressMatchesData(output.address(), expectedHubAddress) ||
      registeredScriptHashAt(body, certificateIndex, true) !==
        expectedWitness ||
      witnessRedeemer === null ||
      !("MintOrBurn" in witnessRedeemer) ||
      witnessRedeemer.MintOrBurn.targetPolicy !== policyId
    ) {
      return null;
    }
    const parsedDatum = parseEventDatum(kind, datum.cborHex);
    const ttl = body.ttl();
    const forcedEvent = parsedDatum?.event as
      | {
          id?: { transactionId?: unknown; outputIndex?: unknown };
          tx?: unknown;
        }
      | undefined;
    if (
      parsedDatum === null ||
      ttl === undefined ||
      ttl > BigInt(Number.MAX_SAFE_INTEGER) ||
      parsedDatum.inclusionTime !==
        BigInt(
          resolveEventInclusionTime(
            slotToBeginUnixTime(
              Number(ttl),
              policy.customNetwork?.slotConfig ??
                SLOT_CONFIG_NETWORK[policy.network],
            ),
            policy.network,
          ),
        ) ||
      parsedDatum.witness !== expectedWitness ||
      !eventIdMatchesNonce(kind, parsedDatum.event, nonceInput) ||
      (kind === "forced_order" &&
        (!isHex32(forcedEvent?.id?.transactionId) ||
          typeof forcedEvent.id.outputIndex !== "bigint" ||
          !forcedPayloadMatchesSubmittedSource(forcedEvent.tx) ||
          // #594's exhaustion rule, re-derived. The redeemer's carriage vector
          // is positional over the payload's non-empty slots, so its length
          // must equal their count exactly — a short vector leaves a field's
          // material uncarried, a spare entry lets two distinct redeemers spell
          // one order (§8.11). Both inputs are in hand here: the vector came
          // out of the mint redeemer above and the count out of the payload
          // whose binding the previous clause just verified. The per-field
          // *hash* half is not reachable from this module — see
          // `forcedPayloadMatchesSubmittedSource` — but this half is, so it is
          // checked rather than deferred with it.
          decoded.materialCarriage === null ||
          decoded.materialCarriage.length !==
            forcedOrderMaterialFieldCount(forcedEvent.tx)))
    ) {
      return null;
    }
    const policies = outputPolicies(output);
    const nonNftAssetCount = policies.reduce((count, candidatePolicy) => {
      if (candidatePolicy === policyId) {
        return count;
      }
      return (
        count +
        (output
          .amount()
          .multi_asset()
          .get_assets(CML.ScriptHash.from_hex(candidatePolicy))
          ?.len() ?? 0)
      );
    }, 1);
    if (policies.length !== 1 || nonNftAssetCount !== 1) {
      return null;
    }
    const outRef = `${transaction.txHash}#${outputIndex.toString()}`;
    const outputCborHex = output.to_cbor_hex();
    const eventId = outputReferenceToPlutusDataCbor({
      txHash: nonceInput.transaction_id().to_hex(),
      outputIndex: Number(nonceInput.index()),
    });
    events.push(
      Object.freeze({
        kind,
        eventId,
        outRef,
        transactionHash: transaction.txHash,
        outputIndex: outputIndex.toString(),
        nonceOutRef: outputReference(nonceInput),
        policyId,
        spendScriptHash: fields.spendScriptHash,
        addressHex: fields.addressHex,
        assetNameHex: expectedAssetName,
        witnessScriptHash: expectedWitness,
        inclusionTime: parsedDatum.inclusionTime.toString(),
        eventCborHex: parsedDatum.eventCborHex,
        datumCborHex: datum.cborHex,
        outputCborHex,
        eventContentDigest: sha256Bytes(
          Buffer.from(parsedDatum.eventCborHex, "hex"),
        ),
        datumDigest: datum.digest,
        outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
        originPointDigest: block.chainPoint.pointDigest,
        originChainPointId: block.chainPoint.chainPointId,
        originBlockHash: block.chainPoint.blockHash,
        originSlot: block.chainPoint.slot,
        originBlockNo: block.chainPoint.blockNo,
        finalityStatus: "pending",
      }),
    );
  }
  return true;
};

type DecodedTerminalSpend = Readonly<{
  terminalStatus: WatcherUserEventTerminalStatus;
  outputIndex: bigint;
  hubRefInputIndex: bigint;
  settlementRefInputIndex: bigint;
  mintRedeemerIndex: bigint;
  payoutMintRedeemerIndex: bigint | null;
  membershipProof: Readonly<{
    domain: string;
    root: string;
    phas_root: string;
    count: bigint;
    key: string;
    value: string;
    proof: unknown;
  }>;
  inclusionProofRedeemerIndex: bigint;
  purpose: unknown;
}>;

const decodeTerminalSpend = (
  kind: WatcherUserEventKind,
  bytesHex: string,
  inputIndex: number,
): DecodedTerminalSpend | null => {
  if (kind === "forced_order") {
    const decoded = dataRoundTrip<{
      input_index: bigint;
      output_index: bigint;
      hub_ref_input_index: bigint;
      settlement_ref_input_index: bigint;
      burn_redeemer_index: bigint;
      membership_proof: DecodedTerminalSpend["membershipProof"];
      inclusion_proof_script_withdraw_redeemer_index: bigint;
      validity_override: unknown;
    }>(bytesHex, TxOrderSpendRedeemerSchema);
    return decoded?.input_index === BigInt(inputIndex)
      ? {
          terminalStatus: "processed",
          outputIndex: decoded.output_index,
          hubRefInputIndex: decoded.hub_ref_input_index,
          settlementRefInputIndex: decoded.settlement_ref_input_index,
          mintRedeemerIndex: decoded.burn_redeemer_index,
          payoutMintRedeemerIndex: null,
          membershipProof: decoded.membership_proof,
          inclusionProofRedeemerIndex:
            decoded.inclusion_proof_script_withdraw_redeemer_index,
          purpose: decoded.validity_override,
        }
      : null;
  }
  return null;
};

const outputValue = (
  output: CML.TransactionOutput,
): ReadonlyMap<string, bigint> => {
  const result = new Map<string, bigint>([
    ["lovelace", output.amount().coin()],
  ]);
  const multiAsset = output.amount().multi_asset();
  const policies = multiAsset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policyId = policies.get(policyIndex);
    const assets = multiAsset.get_assets(policyId);
    if (assets === undefined) {
      continue;
    }
    for (let assetIndex = 0; assetIndex < assets.len(); assetIndex += 1) {
      const assetName = assets.keys().get(assetIndex);
      result.set(
        `${policyId.to_hex()}${assetName.to_hex()}`,
        assets.get(assetName) ?? 0n,
      );
    }
  }
  return result;
};

const expectedTerminalValue = (
  event: WatcherIndexedUserEvent,
  input: CML.TransactionOutput,
  hubDatum: Record<string, unknown>,
  status: WatcherUserEventTerminalStatus,
): ReadonlyMap<string, bigint> => {
  const expected = new Map(outputValue(input));
  expected.delete(`${event.policyId}${event.assetNameHex}`);
  if (status === "payout_initialized" && isHex28(hubDatum.payout)) {
    expected.set(`${hubDatum.payout}${event.assetNameHex}`, 1n);
  }
  return expected;
};

const sameValue = (
  left: ReadonlyMap<string, bigint>,
  right: ReadonlyMap<string, bigint>,
): boolean =>
  left.size === right.size &&
  [...left].every(([unit, quantity]) => right.get(unit) === quantity);

const addressMatchesData = (address: CML.Address, value: unknown): boolean => {
  const candidate = value as {
    paymentCredential?:
      | { ScriptCredential?: [unknown] }
      | { PublicKeyCredential?: [unknown] };
    stakeCredential?: unknown;
  };
  const payment = address.payment_cred();
  const expectedScript =
    "ScriptCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            ScriptCredential: [unknown];
          }
        ).ScriptCredential[0]
      : null;
  const expectedKey =
    "PublicKeyCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            PublicKeyCredential: [unknown];
          }
        ).PublicKeyCredential[0]
      : null;
  return (
    candidate.stakeCredential === null &&
    ((isHex28(expectedScript) &&
      payment?.as_script()?.to_hex() === expectedScript) ||
      (isHex28(expectedKey) && payment?.as_pub_key()?.to_hex() === expectedKey))
  );
};

const eventKeyValueCbor = (
  event: WatcherIndexedUserEvent,
): Readonly<{ key: string; value: string; payload: unknown }> | null => {
  try {
    const parsed = Data.from(
      event.eventCborHex,
      eventSchemas(event.kind).event,
    ) as { id: unknown; info?: unknown; tx?: unknown };
    const plutus = CML.PlutusData.from_cbor_hex(event.eventCborHex)
      .as_constr_plutus_data()
      ?.fields();
    if (plutus === undefined || plutus.len() !== 2) {
      return null;
    }
    return {
      key: plutus.get(0).to_cbor_hex(),
      value: plutus.get(1).to_cbor_hex(),
      payload: parsed.info ?? parsed.tx,
    };
  } catch {
    return null;
  }
};

const countedRootMatches = (
  proof: DecodedTerminalSpend["membershipProof"],
  expectedDomain:
    | "DepositsRootDomain"
    | "WithdrawalsRootDomain"
    | "ForcedTransactionsV1RootDomain",
  expectedRoot: unknown,
): boolean => {
  if (
    proof.domain !== expectedDomain ||
    proof.root !== expectedRoot ||
    proof.count <= 0n ||
    !isHex32(proof.root) ||
    !isHex32(proof.phas_root)
  ) {
    return false;
  }
  const tag = Buffer.from("MidgardRootCountV1", "utf8");
  const domain = Buffer.from(
    Data.to(expectedDomain as never, RootDomainSchema as never),
    "hex",
  );
  const count = Buffer.from(
    Data.to(proof.count as never, Data.Integer() as never),
    "hex",
  );
  return (
    Buffer.from(
      blake2b(
        Buffer.concat([
          tag,
          domain,
          Buffer.from(proof.phas_root, "hex"),
          count,
        ]),
        { dkLen: 32 },
      ),
    ).toString("hex") === proof.root
  );
};

const membershipWithdrawalCbor = (
  proof: DecodedTerminalSpend["membershipProof"],
): string | null => {
  try {
    const values = CML.PlutusDataList.new();
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.phas_root as never, MerkleRoot as never),
      ),
    );
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.key, "hex")));
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.value, "hex")));
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.proof as never, Proof as never),
      ),
    );
    return CML.PlutusData.new_list(values).to_canonical_cbor_hex();
  } catch {
    return null;
  }
};

const authenticReferenceDatum = (
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  body: CML.TransactionBody,
  index: bigint,
  policyId: string,
  schema: EventSchema,
): Readonly<{
  output: CML.TransactionOutput;
  datum: Record<string, unknown>;
}> | null => {
  const output = watcherUserEventReferenceOutput(
    referenceEvidence,
    transactionHash,
    referencedOutRefAt(body, index),
  );
  const datumCbor = output === null ? null : inlineDatumCbor(output);
  if (
    output === null ||
    datumCbor === null ||
    exactlyOneAsset(output, policyId)?.quantity !== 1n
  ) {
    return null;
  }
  const datum = dataRoundTrip<Record<string, unknown>>(datumCbor, schema);
  return datum === null ? null : { output, datum };
};

/**
 * Whether a forced order's payload is a well-formed §4 binding to its own native
 * source.
 *
 * It used to take a `verification` bundle — the durable store, the transaction
 * body, the deployment's policy identities and the tx-order id — because it had to
 * resolve `terminal_receipt_reference` to a receipt UTxO in the store, match that
 * UTxO's script address and minted asset name against the deployment, and count
 * the transaction's reference inputs to it. #587 retired the receipt chain and
 * with it every one of those lookups, so the predicate is now a pure function of
 * the payload.
 *
 * ### What this deliberately does not re-derive, and why
 *
 * The tx-order mint runs `tx_order_v1.verify_order_material`, which is
 * `material_directory` — re-derived below in full — followed by a walk of §2.5's
 * nine slots that opens the §8.8 field-access door at every slot carrying
 * material, against a `FieldCarriageV1` vector the **mint redeemer** supplies
 * (#594's owner ruling). That walk is not re-derived here.
 *
 * The omission used to be about deployability: while §8's availability
 * re-expression was unwired the clause admitted only the canonically-empty
 * transaction, and mirroring a producer-side stopgap would have cost the watcher
 * the ability to observe any forced order that moves anything. #594 wired the
 * mechanism, so that reasoning is spent and this is the reason that replaces it.
 *
 * **The carriage is not in the payload, by design.** §8.7's mandatory content
 * addressing prohibits identifying carriage by UTxO identity, so
 * `TxOrderPayloadV1` deliberately carries no carriage reference — the nine
 * commitments *are* the material directory, and this predicate re-derives them in
 * full. There is therefore nothing in a payload for a payload-shaped predicate to
 * check the carriage against, and adding a field to the datum to give it one is
 * exactly what the ruling refused. (The ruling's own text cites §8.5 for the
 * content-addressing rule; §8.5 is _Custody_ and the rule is §8.7's. Corrected
 * here and in §8.11.)
 *
 * **The exhaustion half is reachable and is not omitted.** The walk's rule that
 * the redeemer's vector be exhausted exactly needs two things: the vector, which
 * `scanCreatedEvents` decodes out of the tx-order mint redeemer, and the count of
 * non-empty slots, which comes out of this payload's own compact structures. Both
 * are in this module, so that clause is re-derived — at the redeemer site rather
 * than here, because this predicate never sees a redeemer. See
 * `forcedOrderMaterialFieldCount` and its caller. The burn's empty-vector rule is
 * likewise re-derived, in `scanConsumedEvents`.
 *
 * Per-field material hashes are not re-derived by this payload predicate.
 * `Inline` preimages ride the mint redeemer, while `RawUtxo`/`Certified` bytes
 * live in reference-input datums. The indexer now admits resolved reference
 * evidence for its hub/settlement checks, but connecting material carriage to
 * those bytes and validating every field remains a separate verification step.
 * Admitting reference bytes does not itself establish those material hashes.
 *
 * **What this predicate is not.** It is not the first line of defence, but the
 * reason is narrower than "the mint already checked". The mint in *this tree*
 * hashes every non-empty field's preimage against its committed hash before the
 * NFT exists. The mint currently **deployed** is the receipt-era one behind the
 * frozen blueprint (#579 owns the regeneration): its per-item opening is
 * unsatisfiable for a payload whose commitments are §4 flat hashes of real
 * material, but a payload *declaring* counted roots in place of flat commitments
 * could satisfy it, so it can authenticate a material-bearing order. That
 * residual is real and is not covered here — what stood here before #587 was the
 * same receipt walk that mint gates on, accepting exactly the payloads it
 * accepted, so no version of this predicate ever closed it. It closes when the
 * blueprint is regenerated, not by anything written in this module.
 */
const forcedPayloadMatchesSubmittedSource = (payload: unknown): boolean => {
  const candidate = payload as {
    tx_id?: unknown;
    transaction_commitment?: unknown;
    submitted_source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
  };
  if (
    !isHex32(candidate.tx_id) ||
    !isHex32(candidate.transaction_commitment) ||
    !isHexBytes(candidate.submitted_source?.compact_cbor) ||
    !isHexBytes(candidate.submitted_source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.submitted_source.field_preimage_lengths_cbor)
  ) {
    return false;
  }
  try {
    const source = {
      compactCbor: Buffer.from(candidate.submitted_source.compact_cbor, "hex"),
      witnessSetCompactCbor: Buffer.from(
        candidate.submitted_source.witness_set_compact_cbor,
        "hex",
      ),
      fieldPreimageLengthsCbor: Buffer.from(
        candidate.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
    };
    verifyMidgardForcedTxProofSource({
      transactionId: Buffer.from(candidate.tx_id, "hex"),
      source,
    });
    if (
      computeMidgardForcedTxProofCommitment(source).toString("hex") !==
      candidate.transaction_commitment
    ) {
      return false;
    }
    // The committed field lengths are decoded, not merely present: a payload whose
    // length vector is malformed or is not nine entries is not a §4 binding, and
    // `decodeMidgardNativeTxProofFieldLengths` is the thing that says so.
    decodeMidgardNativeTxProofFieldLengths(source.fieldPreimageLengthsCbor);
    // What stood here walked the counted publication receipt chain: it resolved
    // `terminal_receipt_reference` out of the durable store, checked the receipt
    // datum's identity and its minted `deriveMidgardTxFieldReceiptAssetNameV1`
    // name, verified the `collection_proof` with
    // `verifyMidgardBoundedCollectionItemProofV1`, and re-derived the terminal
    // chunk and encoded-size arithmetic — decoding both compact structures to get
    // the nine commitments it needed for that. All of it retired in #587 with the
    // chain itself: under `docs/spec/midgard-tx.md` §4 a field commitment is one
    // flat hash over the whole preimage, so no per-item Merkle opening can be
    // checked against it and the receipt mint policy was unsatisfiable for any
    // payload whose commitments were the §4 flat hashes of real material — a
    // narrowing this walk inherited, not a closed door (see the docstring above
    // for the declaring-payload residual the mint left open and this walk shared).
    //
    // **The payload no longer carries availability evidence at all.**
    // `TxOrderPayloadV1` shed `terminal_receipt_reference` in the same change, so
    // what is left to verify from a forced order's datum is exactly what is
    // checked above: the proof source authenticates against the carried `tx_id`,
    // and the carried commitment is the one derived from that source. Availability
    // is enforced where the evidence for it lives — the tx-order mint's
    // `verify_order_material` — and the docstring above says why this predicate
    // does not mirror that function's temporary all-empty clause.
    return true;
  } catch {
    return false;
  }
};

const cardanoDatumMatches = (
  output: CML.TransactionOutput,
  expected: unknown,
): boolean => {
  if (expected === "NoDatum") {
    return output.datum() === undefined;
  }
  const candidate = expected as {
    DatumHash?: { hash?: unknown };
    InlineDatum?: { data?: unknown };
  };
  if (candidate.DatumHash !== undefined) {
    return (
      (
        output.datum() as
          | { as_hash?: () => { to_hex(): string } | undefined }
          | undefined
      )
        ?.as_hash?.()
        ?.to_hex() === candidate.DatumHash.hash
    );
  }
  if (candidate.InlineDatum !== undefined) {
    try {
      return (
        output.datum()?.as_datum()?.to_cbor_hex() ===
        Data.to(candidate.InlineDatum.data as never)
      );
    } catch {
      return false;
    }
  }
  return false;
};

const verifyTerminalSemantics = (
  event: WatcherIndexedUserEvent,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  body: CML.TransactionBody,
  spend: DecodedTerminalSpend,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): boolean => {
  const outputs = body.outputs();
  if (
    spend.outputIndex < 0n ||
    spend.outputIndex >= BigInt(outputs.len()) ||
    spend.outputIndex > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return false;
  }
  const produced = outputs.get(Number(spend.outputIndex));
  const input = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const hubDatum = decodeHubAt(
    referenceEvidence,
    transaction.txHash,
    body,
    spend.hubRefInputIndex,
    deployment,
  );
  const settlementPolicy = hubDatum?.settlement;
  const settlement = isHex28(settlementPolicy)
    ? authenticReferenceDatum(
        referenceEvidence,
        transaction.txHash,
        body,
        spend.settlementRefInputIndex,
        settlementPolicy,
        asDataType<EventSchema>(SettlementDatumSchema),
      )
    : null;
  const eventPair = eventKeyValueCbor(event);
  const forcedProofValue =
    event.kind === "forced_order" && eventPair !== null
      ? (() => {
          const tx = eventPair.payload as {
            tx_id?: unknown;
            submitted_source?: unknown;
          };
          try {
            return Data.to(
              {
                tx_id: tx.tx_id,
                submitted_source: tx.submitted_source,
                verdict: spend.purpose,
              } as never,
              ForcedInclusionTxV1Schema as never,
            );
          } catch {
            return null;
          }
        })()
      : eventPair?.value;
  if (
    hubDatum === null ||
    settlement === null ||
    eventPair === null ||
    settlement.output.script_ref() !== undefined ||
    !addressMatchesData(
      settlement.output.address(),
      hubDatum.settlement_addr,
    ) ||
    produced.script_ref() !== undefined ||
    spend.membershipProof.key !== eventPair.key ||
    forcedProofValue === null ||
    spend.membershipProof.value !== forcedProofValue
  ) {
    return false;
  }
  const eventPolicy =
    event.kind === "deposit"
      ? hubDatum.deposit
      : event.kind === "withdrawal"
        ? hubDatum.withdrawal
        : hubDatum.tx_order;
  const domain =
    event.kind === "deposit"
      ? "DepositsRootDomain"
      : event.kind === "withdrawal"
        ? "WithdrawalsRootDomain"
        : "ForcedTransactionsV1RootDomain";
  const root =
    event.kind === "deposit"
      ? settlement.datum.deposits_root
      : event.kind === "withdrawal"
        ? settlement.datum.withdrawals_root
        : settlement.datum.forced_transactions_root;
  const membershipRedeemer = redeemerAtGlobalIndex(
    transaction,
    spend.inclusionProofRedeemerIndex,
  );
  const mintRedeemer = redeemerAtGlobalIndex(
    transaction,
    spend.mintRedeemerIndex,
  );
  const policyIndex =
    body.mint() === undefined
      ? -1
      : mintPolicyIndex(body.mint()!, event.policyId);
  if (
    eventPolicy !== event.policyId ||
    !countedRootMatches(spend.membershipProof, domain, root) ||
    membershipRedeemer?.purpose !== "withdrawal" ||
    membershipRedeemer.bytes.bytesHex !==
      membershipWithdrawalCbor(spend.membershipProof) ||
    mintRedeemer?.purpose !== "mint" ||
    mintRedeemer.index !== policyIndex.toString() ||
    !sameValue(
      outputValue(produced),
      expectedTerminalValue(event, input, hubDatum, spend.terminalStatus),
    )
  ) {
    return false;
  }
  const datum = Data.from(
    event.datumCborHex,
    eventSchemas(event.kind).datum,
  ) as {
    event: {
      id?: { transactionId?: unknown; outputIndex?: unknown };
      info?: unknown;
      tx?: unknown;
    };
    refund_address?: unknown;
    refund_datum?: unknown;
  };
  return (
    event.kind === "forced_order" &&
    isHex32(datum.event.id?.transactionId) &&
    typeof datum.event.id.outputIndex === "bigint" &&
    forcedPayloadMatchesSubmittedSource(datum.event.tx) &&
    addressMatchesData(produced.address(), datum.refund_address) &&
    cardanoDatumMatches(produced, datum.refund_datum)
  );
};

/** Classify a consumed history order using the deployed observer. A consumed
 * predecessor continues the same event; it is never a settlement by itself. */
const consumeHistoryOrder = (
  event: WatcherIndexedUserEvent,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  references: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  inputIndex: number,
):
  | { continuation: WatcherIndexedUserEvent }
  | { terminalStatus: WatcherUserEventTerminalStatus }
  | null => {
  if (event.kind === "forced_order") return null;
  const body = canonicalBody(transaction.body.bytesHex)!;
  const observed = historyListObservation(transaction, event.policyId);
  const input = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const node = historyNodeFromOutput(input, event.policyId);
  if (
    observed === null ||
    !("Apply" in observed.observe) ||
    node === null ||
    node.node.payload === "RootContent" ||
    !("Order" in node.node.payload)
  ) {
    return null;
  }
  const spend = matchingRedeemer(transaction, "spend", inputIndex);
  if (
    spend === null ||
    dataRoundTrip<bigint>(
      spend.bytes.bytesHex,
      Data.Integer() as EventSchema,
    ) !== BigInt(inputIndex)
  )
    return null;
  const operation = observed.observe.Apply.operation;
  const operationFields = Object.values(operation)[0]!;
  const continuingIndex =
    "predecessor_output_index" in operationFields
      ? operationFields.predecessor_output_index
      : null;
  if (
    continuingIndex !== null &&
    continuingIndex >= 0n &&
    continuingIndex < BigInt(body.outputs().len())
  ) {
    const output = body.outputs().get(Number(continuingIndex));
    if (historyOrderContinuationMatches(input, output, event.policyId)) {
      const datum = canonicalDatumForOutput(
        transaction,
        Number(continuingIndex),
        output,
      );
      if (datum === null) return null;
      const outputCborHex = output.to_cbor_hex();
      return {
        continuation: Object.freeze({
          ...event,
          outRef: `${transaction.txHash}#${continuingIndex}`,
          transactionHash: transaction.txHash,
          outputIndex: String(continuingIndex),
          datumCborHex: datum.cborHex,
          datumDigest: datum.digest,
          outputCborHex,
          outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
        }),
      };
    }
  }
  if (!("RetireOrder" in operation)) return null;
  const retirementHash =
    deployment.appliedScriptHashes[event.kind + "HistoryRetirementWithdraw"];
  if (!isHex28(retirementHash)) return null;
  const retired = historyRetirementObservation(transaction, retirementHash);
  if (retired === null) return null;
  const { witness, hub_reference_index } = retired.args;
  if (
    witness.order_input_index !== BigInt(inputIndex) ||
    hub_reference_index !== observed.observe.Apply.hub_reference_index ||
    Data.to(eventHistoryRetirementOperation(witness), EventHistoryOperation) !==
      Data.to(operation, EventHistoryOperation)
  )
    return null;
  const hub = decodeHubAt(
    references,
    transaction.txHash,
    body,
    hub_reference_index,
    deployment,
  );
  if (
    hub === null ||
    hub[event.kind] !== event.policyId ||
    !addressMatchesData(input.address(), hub[event.kind + "_addr"])
  )
    return null;
  const facts = node.node.payload.Order.facts;
  const external =
    witness.external_reference_index === null
      ? null
      : watcherUserEventReferenceOutput(
          references,
          transaction.txHash,
          referencedOutRefAt(body, witness.external_reference_index),
        );
  const opened = historyPayloadFromNode(
    node,
    event.kind,
    node.key,
    external,
    deployment.appliedScriptHashes[event.kind + "HistoryRetentionSpend"],
  );
  if (opened === null || event.historyPayloadCborHex !== opened.payloadCbor)
    return null;
  const { payload, payloadCbor } = opened;
  const confirmedOutput = watcherUserEventReferenceOutput(
    references,
    transaction.txHash,
    referencedOutRefAt(body, witness.confirmed_reference_index),
  );
  try {
    if (
      confirmedOutput === null ||
      !isHex28(hub.state_queue) ||
      !addressMatchesData(confirmedOutput.address(), hub.state_queue_addr) ||
      confirmedOutput.script_ref() !== undefined ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.assetNameHex !==
        Buffer.from("MIDGARD_CONFIRMED_STATE").toString("hex") ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.quantity !== 1n
    )
      return null;
    const confirmedNode = Data.from(
      inlineDatumCbor(confirmedOutput)!,
      LinkedListDatum,
    );
    if (!("Root" in confirmedNode.data)) return null;
    const confirmed = Data.castFrom(
      confirmedNode.data.Root.data,
      ConfirmedState,
    );
    if (facts.inclusion_time <= 0n || facts.inclusion_time > confirmed.endTime)
      return null;
  } catch {
    return null;
  }
  const settlement = isHex28(hub.settlement)
    ? authenticReferenceDatum(
        references,
        transaction.txHash,
        body,
        witness.settlement_reference_index,
        hub.settlement,
        asDataType<EventSchema>(SettlementDatumSchema),
      )
    : null;
  const root =
    settlement?.datum[
      event.kind === "deposit" ? "deposits_root" : "withdrawals_root"
    ];
  if (
    settlement === null ||
    !addressMatchesData(settlement.output.address(), hub.settlement_addr) ||
    settlement.output.script_ref() !== undefined ||
    !countedRootMatches(
      {
        ...witness.membership,
        domain:
          event.kind === "deposit"
            ? "DepositsRootDomain"
            : "WithdrawalsRootDomain",
        root: root as string,
        key: "",
        value: "",
      },
      event.kind === "deposit" ? "DepositsRootDomain" : "WithdrawalsRootDomain",
      root,
    )
  )
    return null;
  const mint = body.mint()?.get_assets(CML.ScriptHash.from_hex(event.policyId));
  if (
    mint?.len() !== 1 ||
    mint.get(CML.AssetName.from_hex(event.assetNameHex)) !== -1n ||
    witness.funds_output_index < 0n ||
    witness.funds_output_index >= BigInt(body.outputs().len()) ||
    witness.funds_output_index === witness.predecessor_output_index
  )
    return null;
  const output = body.outputs().get(Number(witness.funds_output_index));
  const original = new Map(outputValue(input));
  original.delete(event.policyId + event.assetNameHex);
  original.set(
    "lovelace",
    (original.get("lovelace") ?? 0n) - facts.structural_lovelace,
  );
  if (
    (original.get("lovelace") ?? -1n) < 0n ||
    output.script_ref() !== undefined
  )
    return null;
  if (facts.structural_lovelace === 0n) {
    if (witness.structural_refund_output_index !== null) return null;
  } else {
    const index = witness.structural_refund_output_index;
    if (
      index === null ||
      index < 0n ||
      index >= BigInt(body.outputs().len()) ||
      index === witness.funds_output_index ||
      index === witness.predecessor_output_index
    )
      return null;
    const refund = body.outputs().get(Number(index));
    if (
      CML.EnterpriseAddress.from_address(refund.address()) === undefined ||
      refund.address().payment_cred()?.as_pub_key()?.to_hex() !==
        facts.structural_refund_key ||
      refund.datum() !== undefined ||
      refund.script_ref() !== undefined ||
      refund.amount().has_multiassets() ||
      refund.amount().coin() < facts.structural_lovelace
    )
      return null;
  }
  if (witness.purpose === "AbsorbDeposit")
    return event.kind === "deposit" &&
      "DepositPayload" in payload &&
      addressMatchesData(output.address(), hub.reserve_addr) &&
      output.datum() === undefined &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "absorbed" }
      : null;
  if (event.kind !== "withdrawal" || !("WithdrawalPayload" in payload))
    return null;
  const withdrawal = payload.WithdrawalPayload;
  if (typeof witness.purpose === "object")
    return witness.purpose.RefundInvalidWithdrawal.validity !==
      "WithdrawalIsValid" &&
      addressMatchesData(output.address(), withdrawal.refund_address) &&
      historyCardanoDatumMatches(output, historyRawField(payloadCbor, [2])) &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "refunded" }
      : null;
  if (witness.purpose !== "InitializeWithdrawalPayout" || !isHex28(hub.payout))
    return null;
  const payoutIndex =
    body.mint() === undefined ? -1 : mintPolicyIndex(body.mint()!, hub.payout);
  const payoutRedeemer =
    payoutIndex < 0 ? null : matchingRedeemer(transaction, "mint", payoutIndex);
  const payout =
    payoutRedeemer === null
      ? null
      : dataRoundTrip<{
          MintPayout: {
            withdrawal_utxo_out_ref: {
              transactionId: string;
              outputIndex: bigint;
            };
            withdrawal_input_index: bigint;
            retirement_withdraw_redeemer_index: bigint;
            hub_ref_input_index: bigint;
          };
        }>(
          payoutRedeemer.bytes.bytesHex,
          asDataType<EventSchema>(PayoutMintRedeemerSchema),
        );
  if (
    payout === null ||
    !("MintPayout" in payout) ||
    payout.MintPayout.retirement_withdraw_redeemer_index !==
      BigInt(retired.globalIndex) ||
    payout.MintPayout.withdrawal_input_index !== BigInt(inputIndex) ||
    payout.MintPayout.hub_ref_input_index !== hub_reference_index ||
    payout.MintPayout.withdrawal_utxo_out_ref.transactionId !==
      event.transactionHash ||
    payout.MintPayout.withdrawal_utxo_out_ref.outputIndex !==
      BigInt(event.outputIndex) ||
    withdrawal.event.info.validity !== "WithdrawalIsValid" ||
    !addressMatchesData(output.address(), hub.payout_addr)
  )
    return null;
  original.set(hub.payout + event.assetNameHex, 1n);
  const payoutAssets = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(hub.payout));
  const datum = inlineDatumCbor(output);
  return payoutAssets?.len() === 1 &&
    payoutAssets.get(CML.AssetName.from_hex(event.assetNameHex)) === 1n &&
    sameValue(original, outputValue(output)) &&
    datum !== null &&
    historyRawField(datum, []) === historyWithdrawalPayoutDatum(payloadCbor)
    ? { terminalStatus: "payout_initialized" }
    : null;
};

export const scanConsumedTransactionEvents = (
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  active: Map<string, WatcherIndexedUserEvent>,
  terminal: WatcherTerminalUserEvent[],
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): true | null => {
  if (!transaction.isValid) {
    return true;
  }
  const body = canonicalBody(transaction.body.bytesHex);
  if (body === null) {
    return null;
  }
  const inputs = body.inputs();
  const mint = body.mint();
  for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
    const event = active.get(outputReference(inputs.get(inputIndex)));
    if (event === undefined) {
      continue;
    }
    if (event.kind !== "forced_order") {
      const disposition = consumeHistoryOrder(
        event,
        transaction,
        referenceEvidence,
        deployment,
        inputIndex,
      );
      if (disposition === null) return null;
      active.delete(event.outRef);
      if ("continuation" in disposition)
        active.set(disposition.continuation.outRef, disposition.continuation);
      else
        terminal.push(
          Object.freeze({
            ...event,
            terminalStatus: disposition.terminalStatus,
            terminalTransactionHash: transaction.txHash,
            terminalPointDigest: block.chainPoint.pointDigest,
            terminalBlockHash: block.chainPoint.blockHash,
            terminalSlot: block.chainPoint.slot,
            terminalBlockNo: block.chainPoint.blockNo,
            terminalFinalityStatus: "pending",
          }),
        );
      continue;
    }
    const spendRedeemer = matchingRedeemer(transaction, "spend", inputIndex);
    if (spendRedeemer === null || mint === undefined) {
      return null;
    }
    const policyIndex = mintPolicyIndex(mint, event.policyId);
    const terminalSpend =
      policyIndex < 0
        ? null
        : decodeTerminalSpend(
            event.kind,
            spendRedeemer.bytes.bytesHex,
            inputIndex,
          );
    const mintRedeemer =
      terminalSpend === null
        ? null
        : redeemerAtGlobalIndex(transaction, terminalSpend.mintRedeemerIndex);
    const decodedMint =
      mintRedeemer === null
        ? null
        : decodeMintRedeemer(mintRedeemer.bytes.bytesHex, event.kind);
    if (terminalSpend === null) {
      return null;
    }
    if (
      !verifyTerminalSemantics(
        event,
        referenceEvidence,
        transaction,
        body,
        terminalSpend,
        deployment,
      )
    ) {
      return null;
    }
    if (
      policyIndex < 0 ||
      mint.get(
        CML.ScriptHash.from_hex(event.policyId),
        CML.AssetName.from_hex(event.assetNameHex),
      ) !== -1n ||
      mint.get_assets(CML.ScriptHash.from_hex(event.policyId))?.len() !== 1 ||
      decodedMint === null ||
      !("BurnEventNFT" in decodedMint.event) ||
      decodedMint.event.BurnEventNFT.nonce_asset_name !== event.assetNameHex ||
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index <
        0n ||
      // #594: the tx-order policy requires a burn's carriage vector to be
      // empty, because a burn reads no material and an unread wire field is a
      // second spelling of the same transaction (§8.11, §6.1). `null` here is
      // the three unwrapped policies, which have no vector to constrain.
      (decodedMint.materialCarriage !== null &&
        decodedMint.materialCarriage.length !== 0)
    ) {
      return null;
    }
    const certificateRedeemer = redeemerAtGlobalIndex(
      transaction,
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index,
    );
    const certificateIndex =
      certificateRedeemer?.purpose === "certificate" &&
      isNatural(certificateRedeemer.index) &&
      BigInt(certificateRedeemer.index) <= BigInt(Number.MAX_SAFE_INTEGER)
        ? Number(certificateRedeemer.index)
        : -1;
    const witnessRedeemer =
      certificateRedeemer === null
        ? null
        : decodeWitnessRedeemer(certificateRedeemer.bytes.bytesHex);
    if (
      registeredScriptHashAt(body, certificateIndex, false) !==
        event.witnessScriptHash ||
      witnessRedeemer === null ||
      !("MintOrBurn" in witnessRedeemer) ||
      witnessRedeemer.MintOrBurn.targetPolicy !== event.policyId
    ) {
      return null;
    }
    const forcedOperatorValidity =
      event.kind === "forced_order"
        ? watcherForcedOperatorVerdict(terminalSpend.purpose)
        : null;
    if (event.kind === "forced_order" && forcedOperatorValidity === null) {
      return null;
    }
    active.delete(event.outRef);
    terminal.push(
      Object.freeze({
        ...event,
        terminalStatus: terminalSpend.terminalStatus,
        terminalTransactionHash: transaction.txHash,
        terminalPointDigest: block.chainPoint.pointDigest,
        terminalBlockHash: block.chainPoint.blockHash,
        terminalSlot: block.chainPoint.slot,
        terminalBlockNo: block.chainPoint.blockNo,
        terminalFinalityStatus: "pending",
        ...(forcedOperatorValidity === null
          ? {}
          : {
              terminalClassification: Object.freeze({
                schemaVersion:
                  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
                operatorValidity: forcedOperatorValidity,
                terminalTransactionHash: transaction.txHash,
                terminalPointDigest: block.chainPoint.pointDigest,
              }),
            }),
      }),
    );
  }
  return true;
};
