import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { restrictWorkflowFundingSigner } from "../src/workflow/funding-reservation-permit.js";
import {
  fundingAddress,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import { runtimeFunding } from "./workflow-runtime.runtime-funding.js";

const replacementInputs = async (
  supersededAttemptFundingOutRefs?: readonly (readonly string[])[],
  tokenFunding?: NonNullable<
    Parameters<typeof runtimeFunding>[1]
  >["tokenFunding"],
) => {
  const runtime = await runtimeFunding("step-one", {
    collateral: true,
    ...(supersededAttemptFundingOutRefs === undefined
      ? {}
      : { supersededAttemptFundingOutRefs }),
    ...(tokenFunding === undefined ? {} : { tokenFunding }),
  });
  const lucid = await Lucid(new Emulator([]), "Preprod");
  restrictWorkflowFundingSigner({
    permit: runtime.permit,
    signer: {
      source: "funding-test",
      address: fundingAddress,
      paymentKeyHash: fundingKey.to_public().hash().to_hex(),
      selectWallet: (instance) =>
        instance.selectWallet.fromPrivateKey(fundingKey.to_bech32()),
    },
  }).selectWallet(lucid);
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(fundingAddress, { lovelace: 5_000_000n })
    .complete();
  const inputs = unsigned.toTransaction().body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();
};

it("spends a superseded attempt's input even when it is too small, topping up from the reserved pool", async () => {
  const small = `${"72".repeat(32)}#0`;
  const large = `${"73".repeat(32)}#0`;
  // Largest-first selection alone never reaches the 2 ADA input.
  expect(await replacementInputs()).toEqual([large]);
  // The superseded attempt spent the 2 ADA input (and a gone one): the
  // replacement must share it, and the pool pays the 5 ADA output and fee.
  const shared = await replacementInputs([[`${"70".repeat(32)}#0`, small]]);
  expect(shared).toContain(small);
  expect(shared).toContain(large);
  // An attempt none of whose inputs is still reserved asks for nothing.
  expect(await replacementInputs([[`${"70".repeat(32)}#0`]])).toEqual([large]);
});

it("never offers a reserved coin that holds a token to coin selection", async () => {
  const large = `${"73".repeat(32)}#0`;
  // A 50 ADA reserved coin that also holds a token: largest-first selection
  // would reach for it first were tokens no bar.
  const inputs = await replacementInputs(undefined, {
    outRef: `${"75".repeat(32)}#0`,
    lovelace: 50_000_000n,
    unit: `${"aa".repeat(28)}cafe`,
  });
  expect(inputs).toEqual([large]);
});
