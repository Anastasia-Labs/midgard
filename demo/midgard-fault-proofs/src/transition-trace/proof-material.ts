import { isDeepStrictEqual } from "node:util";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { mpfProofFromWitness, normalizedMpfRoot } from "./detect.js";
import type { TransitionTraceReconstruction } from "./reconstruct.js";

/**
 * The ledger replay an accepted-transaction proof steps through. The proof
 * commits only the transaction's source leaf; the two field preimages and the
 * ledger witnesses travel beside it, and the thread authenticates each piece
 * against its committed field or root as it is used.
 */
export type TransitionL2ReplayMaterial = Readonly<{
  /** The spend-inputs field (§2.5 field 0) items, in field order. */
  spendInputCbors: readonly string[];
  /** The outputs field (§2.5 field 2) items, in field order. */
  outputCbors: readonly string[];
  /** One delete witness per spend input, in field order. */
  spentUtxos: readonly SDK.LedgerDeleteWitness[];
  /** One insert witness per output, in field order. */
  producedUtxos: readonly SDK.LedgerInsertWitness[];
}>;

/** A serialization source, not an authority token. Installed workflows create
 * this from freshly admitted reconstruction and rederive it on resume. */
export type TransitionProofMaterial = Readonly<{
  proofCbor: string;
  l2Replay?: TransitionL2ReplayMaterial;
}>;

/** Typed callers author new Data; material callers retain already committed
 * Data. A typed view cannot recover a raw leaf's map order or repeated pairs. */
export type TransitionProofInput =
  | SDK.TransitionFaultProof
  | TransitionProofMaterial;

export const transitionProofCbor = (input: TransitionProofInput): string => {
  if (!("proofCbor" in input)) return Data.to(input, SDK.TransitionFaultProof);
  Data.from(input.proofCbor, SDK.TransitionFaultProof);
  return input.proofCbor;
};

/** The accepted-transaction replay material beside `input`, if it has any. */
export const transitionL2Replay = (
  input: TransitionProofInput,
): TransitionL2ReplayMaterial | undefined =>
  "proofCbor" in input ? input.l2Replay : undefined;

/**
 * The transaction an accepted-transaction proof replays. Kind 0 is an L2
 * transaction, opened from its `transactions_root` leaf; kind 2 is an accepted
 * forced transaction, opened from its `forced_transactions_root` leaf, whose
 * id and compact structure the leaf carries. Both take the same phases.
 */
export type TransitionReplayedTransaction = Readonly<{
  /** The final-state kind. */
  kind: 0n | 2n;
  /** The source kind the anchor re-derives the id under: normal or forced. */
  sourceKind: 0n | 1n;
  traceProof: SDK.IndexedTraceProof;
  txId: string;
  compactCbor: string;
  witnessSetCompactCbor: string;
}>;

export const transitionReplayedTransaction = (
  witness: SDK.InvalidOneStepTransitionWitness,
): TransitionReplayedTransaction | null => {
  if ("L2TransactionTransition" in witness) {
    const { trace_proof, source_membership } = witness.L2TransactionTransition;
    const { source } = Data.from(
      source_membership.value,
      SDK.L2TransactionSource,
    );
    return {
      kind: 0n,
      sourceKind: 0n,
      traceProof: trace_proof,
      txId: source_membership.key,
      compactCbor: source.compact_cbor,
      witnessSetCompactCbor: source.witness_set_compact_cbor,
    };
  }
  if ("ValidForcedTransactionTransition" in witness) {
    const { trace_proof, source_membership } =
      witness.ValidForcedTransactionTransition;
    // The leaf's verdict is the thread's to check (phase 0 refuses anything
    // but ForcedTxValid); the witness builder selects only accepted leaves.
    const leaf = Data.from(source_membership.value, SDK.ForcedInclusionTxV1);
    return {
      kind: 2n,
      sourceKind: 1n,
      traceProof: trace_proof,
      txId: leaf.tx_id,
      compactCbor: leaf.submitted_source.compact_cbor,
      witnessSetCompactCbor: leaf.submitted_source.witness_set_compact_cbor,
    };
  }
  return null;
};

export const readTransitionProof = (
  input: TransitionProofInput,
): SDK.TransitionFaultProof =>
  Data.from(transitionProofCbor(input), SDK.TransitionFaultProof);

type HistorySource = (
  | { kind: "Deposit"; membership: SDK.DepositSourceLeafMembershipProof }
  | { kind: "Withdrawal"; membership: SDK.WithdrawalSourceMembershipProof }
) & { valuePath: readonly number[] };

/** A deposit source leaf's typed view beside its committed bytes. */
export const depositSourceView = (
  membership: SDK.DepositSourceLeafMembershipProof,
) => ({
  key: membership.key,
  value: Data.from(membership.value, SDK.DepositInfo),
  valueCbor: membership.value,
});

const historySource = (
  proof: SDK.TransitionFaultProof,
): HistorySource | null => {
  const fault = proof.fault;
  if ("InvalidOneStepTransition" in fault) {
    const witness = fault.InvalidOneStepTransition.witness;
    const valuePath = [2, 0, 2, 5];
    if ("ValidDepositTransition" in witness)
      return {
        kind: "Deposit",
        membership: witness.ValidDepositTransition.source_membership,
        valuePath,
      };
    if ("ValidWithdrawalTransition" in witness)
      return {
        kind: "Withdrawal",
        membership: witness.ValidWithdrawalTransition.source_membership,
        valuePath,
      };
    if ("InvalidWithdrawalNoOpTransition" in witness)
      return {
        kind: "Withdrawal",
        membership: witness.InvalidWithdrawalNoOpTransition.source_membership,
        valuePath,
      };
  }
  if ("OutOfWindowSourceEvent" in fault) {
    const witness = fault.OutOfWindowSourceEvent.witness;
    const valuePath = [2, 0, 0, 5];
    if ("OutOfWindowDeposit" in witness)
      return {
        kind: "Deposit",
        membership: witness.OutOfWindowDeposit.source_membership,
        valuePath,
      };
    if ("OutOfWindowWithdrawal" in witness)
      return {
        kind: "Withdrawal",
        membership: witness.OutOfWindowWithdrawal.source_membership,
        valuePath,
      };
  }
  return null;
};

/** The proof with a withdrawal source value's user datum dropped, so the typed
 * encoder frames every other field; the raw leaf then fills the value field.
 * A deposit source carries its leaf as bytes and needs no frame. */
const withSourceValueFrame = (
  proof: SDK.TransitionFaultProof,
  source: HistorySource,
): SDK.TransitionFaultProof => {
  if (source.kind === "Deposit") return proof;
  const value = {
    ...source.membership.value,
    body: { ...source.membership.value.body, l1_datum: "NoDatum" },
  };
  const [arm, fault] = Object.entries(proof.fault)[0]! as [
    string,
    { witness: Record<string, { source_membership: object }> },
  ];
  const [kind, witness] = Object.entries(fault.witness)[0]!;
  return {
    ...proof,
    fault: {
      [arm]: {
        ...fault,
        witness: {
          [kind]: {
            ...witness,
            source_membership: { ...witness.source_membership, value },
          },
        },
      },
    } as SDK.TransitionFaultProof["fault"],
  };
};

export const transitionProofHistorySource = (input: TransitionProofInput) => {
  const cbor = transitionProofCbor(input);
  const source = historySource(Data.from(cbor, SDK.TransitionFaultProof));
  return source === null
    ? null
    : {
        ...source,
        valueCbor:
          source.kind === "Deposit"
            ? source.membership.value
            : plutusConstrFieldCbor(cbor, source.valuePath),
      };
};

/** Bind the carried leaf to header/DA authority, independently of the authentic
 * L1 opening. A fabricated committed leaf must remain exactly what L2 committed.
 * This verifies encoding/membership, not the claimed transition fault itself. */
export const makeTransitionProofMaterial = (
  reconstruction: TransitionTraceReconstruction,
  proof: SDK.TransitionFaultProof,
  l2Replay?: TransitionL2ReplayMaterial,
): TransitionProofMaterial => {
  if (
    proof.challenged_header_hash !== reconstruction.headerHash ||
    Data.to(proof.header, SDK.Header) !==
      Data.to(reconstruction.header, SDK.Header)
  )
    throw new Error("Transition proof header differs from its reconstruction");
  const source = historySource(proof);
  let proofCbor = Data.to(
    source === null ? proof : withSourceValueFrame(proof, source),
    SDK.TransitionFaultProof,
  );
  if (source !== null) {
    const root =
      source.kind === "Deposit"
        ? reconstruction.rootData.deposits
        : reconstruction.rootData.withdrawals;
    const entries =
      source.kind === "Deposit"
        ? reconstruction.deposits
        : reconstruction.withdrawals;
    const membership = source.membership;
    const key = Data.to(membership.key, SDK.OutputReference);
    const entry = entries.find(
      (candidate) => candidate.keyBytes.toString("hex") === key,
    );
    if (
      entry === undefined ||
      membership.domain !== root.domain ||
      membership.root !== root.root ||
      membership.phas_root !== root.phasRoot ||
      membership.count !== root.count
    )
      throw new Error("Transition proof source differs from its counted root");
    const valueCbor = entry.valueBytes.toString("hex");
    // The raw leaf decides. A deposit carries it as bytes; a withdrawal's
    // typed view must be its decoded view. Neither is re-encoded, since user
    // datum Data need not survive a typed encoder.
    if (
      aikenSerialisedPlutusDataCborPreservingMapOrder(valueCbor) !==
        valueCbor ||
      (source.kind === "Deposit"
        ? source.membership.value !== valueCbor
        : !isDeepStrictEqual(
            Data.from(valueCbor, SDK.WithdrawalInfo),
            source.membership.value,
          ))
    )
      throw new Error("Transition proof source view differs from its raw leaf");
    const opened = mpfProofFromWitness({
      key: entry.keyBytes,
      value: entry.valueBytes,
      proof: membership.proof,
      label: "transition raw source",
    });
    if (
      normalizedMpfRoot(opened.verify(true), "transition raw source") !==
      root.phasRoot
    )
      throw new Error(
        "Transition proof raw source membership does not open its root",
      );
    if (source.kind === "Withdrawal")
      proofCbor = replacePlutusConstrFieldCbor(
        proofCbor,
        source.valuePath,
        valueCbor,
      );
  }
  return Object.freeze({
    proofCbor,
    ...(l2Replay === undefined ? {} : { l2Replay }),
  });
};
