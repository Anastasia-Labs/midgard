/** The validation-trace dispute's reads of its cases and its record by their
 * policy-unique tokens: once our pending claim is spent, the case its
 * out-ref names is ours whoever verified it, and the record a close of our
 * case minted is ours whoever closed it. */
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import { selectOwnFraudProofRecord } from "../fraud-proof-record-selection.js";
import { outRefLabel, type ResolvedProverSigner } from "../runtime.js";
import { parseOwnOutRef } from "../workflow/own-outputs.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import { validationTraceFraudProofUnit } from "./close.js";
import type { ValidationTraceOwnView } from "./own-view.js";
import {
  isValidationTraceCaseContested as isContested,
  validationTraceRecordedThreads,
} from "./thread-observation.js";
import { validationTraceCaseExpiryMs } from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";
import { validationTraceDefenseDeadlineMs } from "./waiting.js";

/** A live case of ours on the block. */
export interface OurCase {
  readonly caseUnit: string;
  readonly outRef: string;
  readonly caseDatum: SDKDa.Case;
  readonly openerOutRef: string;
}

export const validationTraceCaseReads = ({
  lucid,
  signer,
  deployment,
  caseHistory,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** Where each case's threads are read from. */
  readonly caseHistory: ValidationTraceCaseHistory;
}) => {
  // Our record on the block, whoever closed the case that minted it: every
  // conviction on the block mints a record of the same name, each naming its
  // prover. One rule picks ours, as the raw L1 derivation does; this read
  // has no removal history, so it agrees with it while the block is live.
  const recordAt = async (headerHash: string) => {
    const unit = validationTraceFraudProofUnit(deployment, headerHash);
    const found = selectOwnFraudProofRecord({
      records: (
        await lucid.utxosAtWithUnit(
          deployment.fraudProof.spendingScriptAddress,
          unit,
        )
      ).filter((utxo) => utxo.assets[unit] === 1n),
      outRefOf: outRefLabel,
      datumOf: (utxo) => utxo.datum,
      prover: signer.paymentKeyHash,
    });
    return found === undefined
      ? undefined
      : { unit, outRef: outRefLabel(found) };
  };
  // Our cases once our pending claim is gone, by the case policy's unique
  // token the claim's out-ref names, whoever verified it; the claim is the
  // case's origin every move binds. Every live case of ours is played, the
  // one whose deadline comes first before the others (the newest on a tie):
  // its defense deadline while nobody has defended it, else its expiry.
  const ourCases = async (
    view: ValidationTraceOwnView,
  ): Promise<readonly OurCase[]> => {
    const artifact = view.artifact();
    const live: { readonly found: OurCase; readonly deadlineMs: number }[] = [];
    for (const openerOutRef of await view.cases()) {
      const caseUnit =
        deployment.chain.case.policyId +
        SDKDa.validationCaseId(
          artifact.headerHash,
          artifact.claim.claimWitness.event_to_step_membership.key,
          parseOwnOutRef(openerOutRef),
        );
      const found = await view.caseAt(caseUnit);
      if (found === undefined || typeof found.datum !== "string") continue;
      const caseDatum = Data.from(found.datum, SDKDa.Case);
      live.push({
        deadlineMs: isContested(caseDatum)
          ? validationTraceCaseExpiryMs(caseDatum)
          : validationTraceDefenseDeadlineMs(caseDatum),
        found: {
          caseUnit,
          outRef: `${found.txHash}#${found.outputIndex.toString()}`,
          caseDatum,
          openerOutRef,
        },
      });
    }
    return live
      .sort((left, right) => left.deadlineMs - right.deadlineMs)
      .map(({ found }) => found);
  };
  // The live case of ours a move spending `consumes` plays: the case output
  // itself, or a thread whose token the case's history minted. Without
  // `consumes`, the case whose deadline comes first.
  const ourCase = async (
    view: ValidationTraceOwnView,
    consumes?: string,
  ): Promise<OurCase | undefined> => {
    const cases = await ourCases(view);
    if (consumes === undefined || cases.length < 2) return cases[0];
    const direct = cases.find(({ outRef }) => outRef === consumes);
    if (direct !== undefined) return direct;
    const [spent] = await lucid.utxosByOutRef([parseOwnOutRef(consumes)]);
    if (spent === undefined) return undefined;
    const { headerHash } = view.artifact();
    for (const found of cases) {
      const threads = await validationTraceRecordedThreads({
        caseHistory,
        headerHash,
        caseUnit: found.caseUnit,
      });
      if (threads.some(({ unit }) => (spent.assets[unit] ?? 0n) === 1n))
        return found;
    }
    return undefined;
  };
  return { recordAt, ourCases, ourCase, isContested };
};
