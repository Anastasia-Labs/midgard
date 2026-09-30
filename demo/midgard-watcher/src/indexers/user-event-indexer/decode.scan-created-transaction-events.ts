import {
  outputReferenceToPlutusDataCbor,
  resolveEventInclusionTime,
  TxOrderSpendRedeemerSchema,
  userEventWitnessScriptHash,
} from "@al-ft/midgard-sdk";
import {
  CML,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import { type WatcherUserEventReferenceEvidence } from ".././user-event-reference-authority.js";
import {
  addressMatchesData,
  canonicalDatumForOutput,
  eventIdMatchesNonce,
  nonceAssetName,
  outputPolicies,
  parseEventDatum,
} from "./decode.canonical-datum-for-output.js";
import {
  canonicalBody,
  dataRoundTrip,
  decodeHubAt,
  decodeMintRedeemer,
  decodeWitnessRedeemer,
  exactlyOneAsset,
  forcedOrderMaterialFieldCount,
  matchingRedeemer,
  mintPolicyIndex,
  outputReference,
  redeemerAtGlobalIndex,
  registeredScriptHashAt,
} from "./decode.forced-order-material-field-count.js";
import {
  forcedPayloadMatchesSubmittedSource,
  scanHistoryOutput,
} from "./decode.scan-history-output.js";
import {
  eventPolicy,
  isHex32,
  isNatural,
  kindForPolicy,
  sha256Bytes,
} from "./policy.js";
import {
  type WatcherIndexedUserEvent,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventKind,
  type WatcherUserEventTerminalStatus,
} from "./types.js";

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

export type DecodedTerminalSpend = Readonly<{
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

export const decodeTerminalSpend = (
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
