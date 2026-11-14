import {
  advanceCommittedFieldShapeWalk,
  CommittedFieldShapeStep01Datum,
  CommittedFieldShapeStep02Datum,
  type CommittedFieldShapeStep02State,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { CommittedFieldShapeContracts } from "./contracts.js";
import { committedFieldShapeSubmitError } from "./submit-common.js";

export type CommittedFieldShapeNextOutput = Readonly<{
  /** Step 01 again while items remain, step 02 once the walk has a verdict. */
  address: string;
  datum: string;
  complete: boolean;
  /** The state step 02 receives; `null` while items remain. */
  step02State: CommittedFieldShapeStep02State | null;
}>;

/**
 * The successor one step-01 transaction must create, from the thread's
 * current state and the authenticated field bytes.
 */
export const deriveCommittedFieldShapeNextOutput = ({
  threadUtxo,
  signer,
  contracts,
  badTxId,
  fieldIndex,
  committedPreimage,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly contracts: CommittedFieldShapeContracts;
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
}): CommittedFieldShapeNextOutput => {
  if (threadUtxo.datum == null) {
    throw committedFieldShapeSubmitError(
      "step-01 thread UTxO is missing its inline datum.",
    );
  }
  const current = Data.from(threadUtxo.datum, CommittedFieldShapeStep01Datum);
  if (current.fraud_prover !== signer.paymentKeyHash) {
    throw committedFieldShapeSubmitError(
      `thread fraud_prover ${current.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  const transition = advanceCommittedFieldShapeWalk({
    badTxId,
    fieldIndex,
    committedPreimage,
    previous: current.data,
  });
  if (transition.kind === "continue") {
    return Object.freeze({
      address: contracts.steps[0].spendingScriptAddress,
      datum: Data.to(
        { fraud_prover: signer.paymentKeyHash, data: transition.state },
        CommittedFieldShapeStep01Datum,
      ),
      complete: false,
      step02State: null,
    });
  }
  return Object.freeze({
    address: contracts.steps[1].spendingScriptAddress,
    datum: Data.to(
      { fraud_prover: signer.paymentKeyHash, data: transition.state },
      CommittedFieldShapeStep02Datum,
    ),
    complete: true,
    step02State: transition.state,
  });
};
