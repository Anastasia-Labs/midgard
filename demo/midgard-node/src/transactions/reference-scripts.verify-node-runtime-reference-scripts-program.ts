import * as SDK from "@al-ft/midgard-sdk";
import { type ReferenceScriptAuthPolicyRef } from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { nodeRuntimeReferenceScriptTargets } from "./reference-scripts.ensure-reference-script-wallet-working-capital.js";
import {
  fetchReferenceScriptUtxosAt,
  type ReferenceScriptResolved,
  resolveReferenceScriptUtxo,
} from "./reference-scripts.fetch-reference-script-utxos-program.js";

export const verifyNodeRuntimeReferenceScriptsProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  contracts: SDK.MidgardValidators,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const referenceScriptUtxos = yield* fetchReferenceScriptUtxosAt(
      lucid,
      referenceScriptsAddress,
      `node-runtime reference-script UTxO fetch at ${referenceScriptsAddress}`,
      `Failed to fetch node-runtime reference-script UTxOs at ${referenceScriptsAddress}`,
    );
    const resolved: ReferenceScriptResolved[] = [];
    const missing: string[] = [];
    for (const target of targets) {
      const utxo = resolveReferenceScriptUtxo(
        referenceScriptUtxos,
        referenceScriptsAddress,
        target,
        authPolicy,
      );
      if (utxo === undefined) {
        missing.push(target.name);
      } else {
        resolved.push({ name: target.name, utxo });
      }
    }
    if (missing.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Missing node-runtime reference scripts",
          cause: `address=${referenceScriptsAddress};missing=[${missing.join(",")}]`,
        }),
      );
    }
    return resolved;
  });
