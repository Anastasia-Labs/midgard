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
