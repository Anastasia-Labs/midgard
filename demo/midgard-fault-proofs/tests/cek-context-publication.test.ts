import {
  cekContextReferenceScripts,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  alwaysSucceedsBlueprintPath,
  buildMinimalFaultProofContracts,
  EMULATOR_PROTOCOL_PARAMETERS,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

describe("CEK context tail publication", () => {
  it("publishes each parameterized continuation under the real L1 limit", async () => {
    const real = readBlueprint(realBlueprintPath);
    const publisher = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const emulator = new Emulator([publisher], {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    });
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const nonce = (await lucid.wallet().getUtxos())[0]!;
    const contracts = await buildMinimalFaultProofContracts(
      real,
      readBlueprint(alwaysSucceedsBlueprintPath),
      nonce,
      { realValidationTraceDispute: true, alwaysFraudProofCatalogue: true },
    );
    const family = (
      contracts as typeof contracts & ValidationTraceDisputeFaultProofContracts
    ).validationTraceDispute;
    const publications = cekContextReferenceScripts(
      family.cekContextStages,
      family.cekContextItemStages,
    );
    for (const { deploymentEntry: key, validator: contract } of publications) {
      const result = await publishPlainReferenceScriptUtxo({
        lucid,
        script: contract.spendingScript,
        label: `CEK context ${key}`,
      });
      const measurement = result.publicationMeasurement;
      expect(measurement.l1ByteMargin, key).toBeGreaterThanOrEqual(512);
    }
  }, 900_000);
});
