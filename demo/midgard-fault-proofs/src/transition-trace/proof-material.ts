import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { mpfProofFromWitness, normalizedMpfRoot } from "./detect.js";
import type { TransitionTraceReconstruction } from "./reconstruct.js";

/** A serialization source, not an authority token. Installed workflows create
 * this from freshly admitted reconstruction and rederive it on resume. */
export type TransitionProofMaterial = Readonly<{ proofCbor: string }>;

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

export const readTransitionProof = (
  input: TransitionProofInput,
): SDK.TransitionFaultProof =>
  Data.from(transitionProofCbor(input), SDK.TransitionFaultProof);

type HistorySource = (
  | { kind: "Deposit"; membership: SDK.DepositSourceMembershipProof }
  | { kind: "Withdrawal"; membership: SDK.WithdrawalSourceMembershipProof }
) & { valuePath: readonly number[] };

const wrappedSource = (
  source: SDK.TransitionSourceMembershipProof,
  valuePath: readonly number[],
): HistorySource | null => {
  if ("DepositSourceMembership" in source)
    return {
      kind: "Deposit",
      membership: source.DepositSourceMembership.membership,
      valuePath,
    };
  if ("WithdrawalSourceMembership" in source)
    return {
      kind: "Withdrawal",
      membership: source.WithdrawalSourceMembership.membership,
      valuePath,
    };
  return null;
};

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
  if ("SourceMembershipMismatch" in fault) {
    const witness = fault.SourceMembershipMismatch.witness;
    if ("SourceEventMissingTrace" in witness)
      return wrappedSource(
        witness.SourceEventMissingTrace.source_membership,
        [2, 0, 0, 0, 5],
      );
    if ("SourcePhaseMismatch" in witness)
      return wrappedSource(
        witness.SourcePhaseMismatch.source_membership,
        [2, 0, 1, 0, 5],
      );
  }
  return null;
};

export const transitionProofHistorySource = (input: TransitionProofInput) => {
  const cbor = transitionProofCbor(input);
  const source = historySource(Data.from(cbor, SDK.TransitionFaultProof));
  return source === null
    ? null
    : {
        ...source,
        valueCbor: plutusConstrFieldCbor(cbor, source.valuePath),
      };
};

/** Bind the carried leaf to header/DA authority, independently of the authentic
 * L1 opening. A fabricated committed leaf must remain exactly what L2 committed.
 * This verifies encoding/membership, not the claimed transition fault itself. */
export const makeTransitionProofMaterial = (
  reconstruction: TransitionTraceReconstruction,
  proof: SDK.TransitionFaultProof,
): TransitionProofMaterial => {
  if (
    proof.challenged_header_hash !== reconstruction.headerHash ||
    Data.to(proof.header, SDK.Header) !==
      Data.to(reconstruction.header, SDK.Header)
  )
    throw new Error("Transition proof header differs from its reconstruction");
  let proofCbor = Data.to(proof, SDK.TransitionFaultProof);
  const source = historySource(proof);
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
    const schema =
      source.kind === "Deposit" ? SDK.DepositInfo : SDK.WithdrawalInfo;
    const shadow =
      source.kind === "Deposit"
        ? Data.to(source.membership.value, SDK.DepositInfo)
        : Data.to(source.membership.value, SDK.WithdrawalInfo);
    if (
      aikenSerialisedPlutusDataCborPreservingMapOrder(valueCbor) !==
        valueCbor ||
      Data.to(Data.from(valueCbor, schema as never), schema as never) !== shadow
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
    proofCbor = replacePlutusConstrFieldCbor(
      proofCbor,
      source.valuePath,
      valueCbor,
    );
  }
  return Object.freeze({ proofCbor });
};
