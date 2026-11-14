import * as SDK from "@al-ft/midgard-sdk";

import { utxoOutRefKey } from "./reference-scripts.fetch-reference-script-utxos-program.js";

/**
 * Out-ref keys of the deployment's reserved initialization nonces, read from
 * the deployment's own contracts. Protocol initialization consumes them, so
 * every other operator-wallet spend leaves them out of the view it selects
 * from: the reference-script top-up and the script-ref strip both read them
 * here and nowhere else.
 */
export const deploymentInitializationNonceOutRefKeys = (
  contracts: Pick<SDK.MidgardValidators, "eventHistory">,
): ReadonlySet<string> =>
  new Set(
    Object.values(SDK.requireEventHistoryContracts(contracts)).map(
      ({ recipe }) =>
        utxoOutRefKey({
          txHash: recipe.initializationNonce.transactionId,
          outputIndex: Number(recipe.initializationNonce.outputIndex),
        }),
    ),
  );
