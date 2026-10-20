/** The validation-trace dispute's reads of its case and its record by their
 * policy-unique tokens: once our pending claim is spent, the case its
 * out-ref names is ours whoever verified it, and the record a close of our
 * case minted is ours whoever closed it. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { parseOwnOutRef } from "../workflow/own-outputs.js";
import type { confirmedValidationTraceStages } from "./actions.js";
import type { ValidationTraceDisputeArtifact } from "./artifact.js";
import { validationTraceFraudProofUnit } from "./close.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** Whether anyone has defended the case. */
const isContested = (caseDatum: SDK.Case): boolean =>
  caseDatum.open_threads > 0n ||
  caseDatum.defender_won ||
  caseDatum.defenders_root !== SDK.EMPTY_MERKLE_TREE_ROOT;

export const validationTraceCaseReads = ({
  lucid,
  signer,
  deployment,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
}) => {
  // The record a close of our case minted, whoever closed it: the policy
  // mints it once per block, and it names its prover.
  const recordAt = async (headerHash: string) => {
    const unit = validationTraceFraudProofUnit(deployment, headerHash);
    const [found] = (
      await lucid.utxosAtWithUnit(
        deployment.fraudProof.spendingScriptAddress,
        unit,
      )
    ).filter(
      (utxo) =>
        utxo.assets[unit] === 1n &&
        typeof utxo.datum === "string" &&
        Data.from(utxo.datum, SDK.FraudProofTokenDatum).fraud_prover ===
          signer.paymentKeyHash,
    );
    return found === undefined
      ? undefined
      : { unit, outRef: `${found.txHash}#${found.outputIndex.toString()}` };
  };
  const caseAt = async (caseUnit: string) => {
    const [found] = (
      await lucid.utxosAtWithUnit(
        deployment.chain.case.spendingScriptAddress,
        caseUnit,
      )
    ).filter((utxo) => utxo.assets[caseUnit] === 1n);
    return found;
  };
  // Our case once our pending claim is gone, by the case policy's unique
  // token the claim's out-ref names, whoever verified it.
  const ourCase = async (
    artifact: ValidationTraceDisputeArtifact,
    stages: ReturnType<typeof confirmedValidationTraceStages>,
  ) => {
    for (const { outputs } of [...stages]
      .reverse()
      .filter(({ action }) => action.stage === "open")) {
      const caseUnit =
        deployment.chain.case.policyId +
        SDK.validationCaseId(
          artifact.headerHash,
          artifact.claim.claimWitness.event_to_step_membership.key,
          parseOwnOutRef(outputs[0]!.outRef),
        );
      const found = await caseAt(caseUnit);
      if (found === undefined || typeof found.datum !== "string") continue;
      return {
        caseUnit,
        outRef: `${found.txHash}#${found.outputIndex.toString()}`,
        caseDatum: Data.from(found.datum, SDK.Case),
        originOutRef: stages.find(
          ({ action, outputs: left }) =>
            action.stage === "verify_source" && left[0]!.unit === caseUnit,
        )?.outputs[0]!.outRef,
      };
    }
    return undefined;
  };
  return { recordAt, ourCase, isContested };
};
