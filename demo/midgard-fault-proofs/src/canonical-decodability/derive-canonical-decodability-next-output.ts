import {
  advanceCanonicalDecodabilityWalk,
  CanonicalDecodabilityStep01Datum,
  CanonicalDecodabilityStep02Datum,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalDecodabilityContracts } from "./contracts.js";
import { canonicalDecodabilitySubmitError } from "./submit-common.js";

export type CanonicalDecodabilityNextOutput = Readonly<{
  /** Step 01 again while items remain, step 02 once the walk has a verdict. */
  address: string;
  datum: string;
  complete: boolean;
  verdict: number | null;
}>;

/**
 * The successor one step-01 transaction must create, from the thread's
 * current state and the authenticated field bytes.
 */
export const deriveCanonicalDecodabilityNextOutput = ({
  threadUtxo,
  signer,
  contracts,
  badTxId,
  fieldIndex,
  committedPreimage,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
}): CanonicalDecodabilityNextOutput => {
  if (threadUtxo.datum == null) {
    throw canonicalDecodabilitySubmitError(
      "step-01 thread UTxO is missing its inline datum.",
    );
  }
  const current = Data.from(threadUtxo.datum, CanonicalDecodabilityStep01Datum);
  if (current.fraud_prover !== signer.paymentKeyHash) {
    throw canonicalDecodabilitySubmitError(
      `thread fraud_prover ${current.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  const transition = advanceCanonicalDecodabilityWalk({
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
        CanonicalDecodabilityStep01Datum,
      ),
      complete: false,
      verdict: null,
    });
  }
  return Object.freeze({
    address: contracts.steps[1].spendingScriptAddress,
    datum: Data.to(
      { fraud_prover: signer.paymentKeyHash, data: transition.state },
      CanonicalDecodabilityStep02Datum,
    ),
    complete: true,
    verdict: Number(transition.state.verdict),
  });
};
