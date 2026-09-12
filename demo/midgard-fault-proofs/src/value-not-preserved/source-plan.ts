import { computeHash28, computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec/forced";
import {
  encodeHeaderCbor,
  type ForcedTransactionSourceMembershipProof,
  type Header,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import type { ConservationSubmissionAction } from "./submit-union.js";
import {
  ConservationClaim,
  ConservationForcedSourceArgs,
  ConservationSource,
} from "./union-schemas.js";

/** Submitted DA is retained verbatim; the authenticated leaf supplies its verdict. */
export const conservationForcedSource = ({
  header,
  membership,
  transactionCbor,
}: {
  readonly header: Header;
  readonly membership: ForcedTransactionSourceMembershipProof;
  readonly transactionCbor: string;
}) => {
  const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
    Buffer.from(transactionCbor, "hex"),
  );
  if (
    membership.value.verdict === "ForcedTxValid" ||
    membership.value.verdict.ForcedTxInvalid.reason !== "ValueNotPreserved"
  )
    throw new Error(
      "value conservation: wrong forced verdict or submitted validity",
    );
  if (
    computeMidgardNativeTxId(tx.compact).toString("hex") !==
    membership.value.tx_id
  )
    throw new Error("value conservation: forced transaction identity differs");
  const expected = deriveMidgardForcedTxProofSource(tx);
  const actual = membership.value.submitted_source;
  if (
    actual.compact_cbor !== expected.compactCbor.toString("hex") ||
    actual.witness_set_compact_cbor !==
      expected.witnessSetCompactCbor.toString("hex") ||
    actual.field_preimage_lengths_cbor !==
      expected.fieldPreimageLengthsCbor.toString("hex")
  )
    throw new Error(
      "value conservation: retained DA differs from authenticated forced source",
    );
  const source: ConservationSource = {
    transaction_id: membership.value.tx_id,
    claim: "ForcedConservation",
    fee: tx.body.fee,
    event_key: { ForcedTransactionEventKey: { tx_order_id: membership.key } },
    event_root: header.eventToStepRoot,
    event_count: header.totalEventCount,
    trace_root: header.transitionTraceRoot,
    trace_count: header.transitionStepCount,
  };
  const claim = Data.to("ForcedConservation", ConservationClaim);
  const actions: readonly ConservationSubmissionAction[] = [
    {
      position: "entry",
      inputState: null,
      nextPosition: "unionForcedSource",
      outputState: claim,
      args: Data.to(new Constr(1, [0n, 0n, Data.from(claim)])),
    },
    {
      position: "unionForcedSource",
      inputState: claim,
      nextPosition: "unionEvent",
      outputState: Data.to(source, ConservationSource),
      args: Data.to(
        { input_index: 0n, output_index: 0n, header, membership },
        ConservationForcedSourceArgs,
      ),
    },
  ];
  return {
    headerHash: computeHash28(encodeHeaderCbor(header)).toString("hex"),
    transaction: tx,
    source,
    actions,
    nativeTxCompactCbor: actual.compact_cbor,
  };
};

export const conservationAcceptedSource = ({
  header,
  transactionId,
  fee,
  claim,
}: {
  readonly header: Header;
  readonly transactionId: string;
  readonly fee: bigint;
  readonly claim: Exclude<ConservationClaim, string>;
}) => {
  const source: ConservationSource = {
    transaction_id: transactionId,
    claim,
    fee,
    event_key: { L2TransactionEventKey: { tx_id: transactionId } },
    event_root: header.eventToStepRoot,
    event_count: header.totalEventCount,
    trace_root: header.transitionTraceRoot,
    trace_count: header.transitionStepCount,
  };
  const claimCbor = Data.to(claim, ConservationClaim);
  const actions: readonly ConservationSubmissionAction[] = [
    {
      position: "entry",
      inputState: null,
      nextPosition: "unionAcceptedSource",
      outputState: claimCbor,
      args: Data.to(new Constr(1, [0n, 0n, Data.from(claimCbor)])),
    },
    {
      position: "unionAcceptedSource",
      inputState: claimCbor,
      nextPosition: "unionEvent",
      outputState: Data.to(source, ConservationSource),
      args: Data.to(new Constr(0, [])),
    },
  ];
  return { source, actions };
};
