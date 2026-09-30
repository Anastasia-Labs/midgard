import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardNativeTxProofFieldLengths,
  verifyMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  outputReferenceToPlutusDataCbor,
  resolveEventInclusionTime,
} from "@al-ft/midgard-sdk";
import {
  CML,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import {
  historyListObservation,
  historyNodeFromOutput,
  historyPayloadFromNode,
} from ".././authenticated-event-history.js";
import {
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from ".././user-event-reference-authority.js";
import {
  addressMatchesData,
  canonicalDatumForOutput,
  eventIdMatchesNonce,
  nonceAssetName,
} from "./decode.canonical-datum-for-output.js";
import {
  canonicalBody,
  decodeHubAt,
  matchingRedeemer,
  mintPolicyIndex,
  outputReference,
  referencedOutRefAt,
} from "./decode.forced-order-material-field-count.js";
import { eventPolicy, isHex32, isHexBytes, sha256Bytes } from "./policy.js";
import {
  type WatcherIndexedUserEvent,
  type WatcherUserEventIndexerPolicy,
} from "./types.js";

export const scanHistoryOutput = (
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
export const forcedPayloadMatchesSubmittedSource = (
  payload: unknown,
): boolean => {
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
