import type { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";

import { TRANSITION_TRACE_YIELD_REFERENCES } from "../../src/transition-trace/yield-references.js";
import { network } from "./emulator/blueprints.js";
import { type ReferenceScriptPublishingContracts } from "./emulator/reference-script-publisher.js";
import { publishAuthenticatedValidationDisputeControl } from "./emulator/reference-scripts.js";

export const publishTransitionTraceYields = async (
  lucid: LucidEvolution,
  contracts: ReferenceScriptPublishingContracts & {
    referenceScriptAuth: Awaited<
      ReturnType<typeof createReferenceScriptAuthPolicy>
    >;
  },
) => {
  const published: Record<string, { scriptHash: string; utxo: UTxO }> = {};
  for (const key of Object.keys(
    TRANSITION_TRACE_YIELD_REFERENCES,
  ) as (keyof typeof TRANSITION_TRACE_YIELD_REFERENCES)[]) {
    const identity = TRANSITION_TRACE_YIELD_REFERENCES[key];
    const validator = contracts.fraudProofContracts.transitionTrace.yields[key];
    const result = await publishAuthenticatedValidationDisputeControl({
      lucid,
      authPolicy: contracts.referenceScriptAuth,
      publisher: contracts.referenceScriptPublisher,
      target: {
        name: identity.role,
        control: identity.entry,
        script: validator.withdrawalScript,
      },
    });
    published[identity.entry] = {
      scriptHash: validator.withdrawalScriptHash,
      utxo: result.utxo,
    };
    const reward = validatorToRewardAddress(
      network,
      validator.withdrawalScript,
    );
    if (!(await lucid.rewardAccountAt(reward)).registered) {
      const signed = await (
        await lucid.newTx().register.Stake(reward).complete()
      ).sign
        .withWallet()
        .complete();
      await lucid.awaitTx(await signed.submit());
    }
  }
  return published;
};
