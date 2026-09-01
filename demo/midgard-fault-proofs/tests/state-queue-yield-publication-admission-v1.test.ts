import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  alwaysSucceedsBlueprintPath,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { buildMinimalFaultProofContracts } from "./support/emulator/contracts.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import {
  publishPlainReferenceScriptUtxo,
  publishStateQueueYieldReferenceScriptV1,
} from "./support/emulator/reference-scripts.js";

describe("state-queue withdraw-zero publication admission V1", () => {
  it("publishes the mint policy and every arm-specific rewarding script under Van Rossem limits", async () => {
    const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) throw new Error("missing publication nonce");

    const referenceScriptAuth = createReferenceScriptAuthPolicy(
      lucid,
      emulator.now(),
    );
    const contracts = {
      ...(await buildMinimalFaultProofContracts(
        readBlueprint(realBlueprintPath),
        readBlueprint(alwaysSucceedsBlueprintPath),
        nonceUtxo,
        { referenceScriptAuthPolicyId: referenceScriptAuth.policyId },
      )),
      referenceScriptAuth,
    };

    const mint = await publishPlainReferenceScriptUtxo({
      lucid,
      script: contracts.stateQueue.mintingScript,
      label: "state-queue mint publication admission",
    });
    expect(mint.publicationMeasurement.completeSignedBytes).toBeLessThan(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    for (const arm of [
      "commit",
      "unattestedTimeout",
      "unavailableTimeout",
      "fraudRemoval",
      "merge",
    ] as const) {
      const publication = await publishStateQueueYieldReferenceScriptV1({
        lucid,
        contracts,
        arm,
      });
      expect(
        publication.publicationMeasurement.completeSignedBytes,
        `${arm} publication bytes`,
      ).toBeLessThan(EMULATOR_PROTOCOL_PARAMETERS.maxTxSize);
    }
  });
});
