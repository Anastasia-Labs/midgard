import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  ensureReferenceScriptTargetsProgram,
  nodeRuntimeReferenceScriptTargets,
} from "../src/transactions/reference-scripts.js";
import {
  initEmulatorLucid,
  loadContracts,
} from "./initialization-emulator.init-emulator-lucid.js";
it("publishes large constructor via actual production authenticated publisher", async () => {
  const { emulator, referenceScriptsLucid, nonceUtxo, referenceScriptAuth } =
    await initEmulatorLucid();
  const contracts = await loadContracts(nonceUtxo, referenceScriptAuth);
  const targets = nodeRuntimeReferenceScriptTargets(contracts).filter(
    (t) =>
      t.name ===
      "V1 validation-trace ledger-output-proof datum large-constructor yield",
  );
  expect(targets).toHaveLength(1);
  let maximum = 0;
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    maximum = Math.max(maximum, cbor.length / 2);
    console.info("actual-production-publication", cbor.length / 2);
    return hash;
  };
  const published = await Effect.runPromise(
    ensureReferenceScriptTargetsProgram(
      referenceScriptsLucid,
      "large constructor fit",
      targets,
      referenceScriptAuth,
    ),
  );
  expect(published).toHaveLength(1);
  expect(maximum).toBeLessThanOrEqual(15872);
}, 300000);
