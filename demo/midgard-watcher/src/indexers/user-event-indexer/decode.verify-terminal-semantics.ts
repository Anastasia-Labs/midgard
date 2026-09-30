import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  ForcedInclusionTxV1Schema,
  SettlementDatumSchema,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import { type WatcherUserEventReferenceEvidence } from ".././user-event-reference-authority.js";
import {
  addressMatchesData,
  eventSchemas,
} from "./decode.canonical-datum-for-output.js";
import {
  authenticReferenceDatum,
  cardanoDatumMatches,
  countedRootMatches,
  eventKeyValueCbor,
  expectedTerminalValue,
  membershipWithdrawalCbor,
  outputValue,
  sameValue,
} from "./decode.counted-root-matches.js";
import {
  decodeHubAt,
  mintPolicyIndex,
  redeemerAtGlobalIndex,
} from "./decode.forced-order-material-field-count.js";
import { type DecodedTerminalSpend } from "./decode.scan-created-transaction-events.js";
import { forcedPayloadMatchesSubmittedSource } from "./decode.scan-history-output.js";
import { isHex28, isHex32 } from "./policy.js";
import { type EventSchema, type WatcherIndexedUserEvent } from "./types.js";

export const verifyTerminalSemantics = (
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
