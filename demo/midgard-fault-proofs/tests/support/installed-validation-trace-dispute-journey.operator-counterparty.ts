import {
  type MidgardValidationTraceProof,
  selectMidgardValidationDisputeReveal,
} from "@al-ft/midgard-core";
import {
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
} from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitValidationDisputeReveal } from "../../src/index.js";
import { network } from "./emulator/blueprints.js";

type Reveal = Parameters<typeof submitValidationDisputeReveal>[0];

/**
 * The installed journey's emulator-side operator: it reads the live game
 * thread and answers each bisection turn from its own committed trace.
 */
export const installedValidationOperatorCounterparty = ({
  lucid,
  threadUnit,
  proofs,
  realBlueprint,
  deploymentInfo,
  signer,
  gameReferenceScriptUtxo,
  validityRange,
}: {
  readonly lucid: LucidEvolution;
  readonly threadUnit: string;
  readonly proofs: DeterministicValidationMachineTrace["tree"]["proofs"];
  readonly realBlueprint: Reveal["blueprint"];
  readonly deploymentInfo: Reveal["deploymentInfo"];
  readonly signer: Reveal["signer"];
  readonly gameReferenceScriptUtxo: UTxO;
  readonly validityRange: () => NonNullable<Reveal["validityRange"]>;
}) => {
  const gameDispute = async () => {
    const thread = await lucid.utxoByUnit(threadUnit);
    if (thread.datum == null) {
      throw new Error("operator found the dispute thread without a datum");
    }
    const datum = Data.from(thread.datum, ValidationDisputeDatum);
    if (datum.data === null) {
      throw new Error("operator found a null dispute state");
    }
    return {
      threadOutRef: `${thread.txHash}#${thread.outputIndex.toString()}`,
      dispute: validationDisputeCoreFromData(datum.data.dispute),
    };
  };
  /** The counterparty: the operator answers with its own committed trace. */
  const operatorResponds = async (
    overrideProof?: MidgardValidationTraceProof,
  ) => {
    const { threadOutRef, dispute } = await gameDispute();
    const move = selectMidgardValidationDisputeReveal({
      dispute,
      role: "operator",
      proofs,
    });
    if (move.type !== "revealOperator") {
      throw new Error(
        `operator asked to respond while the dispute is ${move.type}`,
      );
    }
    return await submitValidationDisputeReveal({
      lucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer,
      threadOutRef,
      role: "operator",
      proof: overrideProof ?? move.proof,
      gameReferenceScriptUtxo,
      validityRange: validityRange(),
      awaitConfirmation: true,
    });
  };
  const honestOperatorMove = async () => {
    const { dispute } = await gameDispute();
    const move = selectMidgardValidationDisputeReveal({
      dispute,
      role: "operator",
      proofs,
    });
    if (move.type !== "revealOperator") {
      throw new Error(
        `operator asked for a move while the dispute is ${move.type}`,
      );
    }
    return move.proof;
  };
  return { operatorResponds, honestOperatorMove };
};
