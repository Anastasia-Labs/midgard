/** Reading a validation-trace (00000006) removal's outcome: the prover's
 * reward address and the outputs paying it the reward. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type TxSigned,
} from "@lucid-evolution/lucid";

import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.js";
import { network } from "./submit-init-emulator-shared.js";

const economics = SDK.getProtocolParameters(network);

export const proverEnterpriseAddress = (fixture: ProvedDoubleSpendFixture) =>
  credentialToAddress(network, {
    type: "Key",
    hash: fixture.proverPaymentKeyHash,
  });

/** The outputs of `tx` paying exactly the reward to the prover. */
export const rewardOutputs = (
  fixture: ProvedDoubleSpendFixture,
  tx: TxSigned,
): CML.TransactionOutput[] => {
  const outputs = tx.toTransaction().body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    outputs.get(index),
  ).filter(
    (output) =>
      output.amount().coin() === economics.fraud_prover_reward &&
      output.address().to_bech32() === proverEnterpriseAddress(fixture),
  );
};
