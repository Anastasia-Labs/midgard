import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";
import { prepareRuntimeFunding } from "./workflow-runtime.slash-funding-fixture.js";

/** The no-change exception must remain specific to authenticated slashes. */
export const assertOrdinaryMissingChange = async () => {
  for (const includeChange of [true, false]) {
    const runtime = await runtimeFunding("step-one");
    const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
    const governed = CML.Address.from_bech32(policy.contracts[0]!.address);
    const minimum = CML.min_ada_required(
      CML.TransactionOutput.new(governed, CML.Value.from_coin(2_000_000n)),
      BigInt(policy.protocolParameters.coinsPerUtxoByte),
    );
    const change = 2_000_000n - minimum - 200_000n;
    const caller = CML.Address.from_bech32(runtime.snapshot.walletAddress);
    const changeOutput = CML.TransactionOutput.new(
      caller,
      CML.Value.from_coin(change),
    );
    expect(change).toBeGreaterThanOrEqual(
      CML.min_ada_required(
        changeOutput,
        BigInt(policy.protocolParameters.coinsPerUtxoByte),
      ),
    );
    const signed = signedFundingTransaction({
      inputOutRefs: [`${"72".repeat(32)}#0`],
      outputAddress: governed.to_bech32(),
      outputLovelace: minimum,
      fee: includeChange ? 200_000n : 2_000_000n - minimum,
      additionalOutputs: includeChange ? [changeOutput] : [],
    });
    if (includeChange) {
      await expect(
        prepareRuntimeFunding(runtime, signed),
      ).resolves.toBeUndefined();
      expect(runtime.prepare).toHaveBeenCalledTimes(1);
    } else {
      await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
        "production transaction omitted reserved-wallet change",
      );
      expect(runtime.prepare).not.toHaveBeenCalled();
    }
  }
};
