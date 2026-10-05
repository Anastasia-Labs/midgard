import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  assertWorkflowFundingReservationReadyToSubmit,
  restrictWorkflowFundingSigner,
} from "../src/workflow/funding-reservation-permit.js";
import {
  fundingAddress,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

it("rechecks durable funding authority before building, signing and submitting through a retained wallet", async () => {
  let held = false;
  const gate = vi.fn(async () => {
    if (held) throw new Error("legacy overlap requires reconciliation");
  });
  const runtime = await runtimeFunding("step-one", {
    assertSubmissionAuthority: gate,
  });
  const lucid = await Lucid(new Emulator([]), "Preprod");
  const restricted = restrictWorkflowFundingSigner({
    permit: runtime.permit,
    signer: {
      source: "funding-test",
      address: fundingAddress,
      paymentKeyHash: fundingKey.to_public().hash().to_hex(),
      selectWallet: (instance) =>
        instance.selectWallet.fromPrivateKey(fundingKey.to_bech32()),
    },
  });
  restricted.selectWallet(lucid);
  const wallet = lucid.wallet();
  const signed = signedFundingTransaction({
    inputOutRefs: [],
    outputLovelace: 1_000_000n,
  });
  await expect(wallet.signTx(signed.toTransaction())).resolves.toBeDefined();
  held = true;
  const reads = runtime.resolveInputs.mock.calls.length;
  await expect(runtime.begin()).rejects.toThrow("legacy overlap");
  expect(runtime.resolveInputs).toHaveBeenCalledTimes(reads);
  await expect(wallet.signTx(signed.toTransaction())).rejects.toThrow(
    "legacy overlap",
  );
  await expect(wallet.signMessage(fundingAddress, "abcd")).rejects.toThrow(
    "legacy overlap",
  );
  await expect(
    wallet.submitTx(signed.toTransaction().to_cbor_hex()),
  ).rejects.toThrow("legacy overlap");
  await expect(
    assertWorkflowFundingReservationReadyToSubmit({
      journal: runtime.journal,
      transactionHash: signed.toHash(),
    }),
  ).rejects.toThrow("legacy overlap");
  held = false;
  await expect(runtime.begin()).resolves.toBeUndefined();
  await expect(wallet.signTx(signed.toTransaction())).resolves.toBeDefined();
});

const replacementInputs = async (
  supersededAttemptFundingOutRefs?: readonly (readonly string[])[],
) => {
  const runtime = await runtimeFunding("step-one", {
    collateral: true,
    ...(supersededAttemptFundingOutRefs === undefined
      ? {}
      : { supersededAttemptFundingOutRefs }),
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
