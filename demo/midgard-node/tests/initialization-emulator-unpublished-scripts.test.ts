import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "../src/deployment-manifest.js";
import { fetchProtocolDeploymentStatus } from "../src/transactions/initialization.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import {
  buildAtomicInitializationTx,
  initEmulatorLucid,
  loadContracts,
} from "./initialization-emulator.init-emulator-lucid.js";

/**
 * script_decode_v1 and the CEK core BLS budget step do not fit in one L1
 * transaction, so initialization publishes every reference script except
 * theirs. This pins both sides of that one exception set: nothing else is
 * skipped, and none of those scripts is published.
 */
const NOT_YET_PUBLISHED_CONTRACTS = new Set([
  "scriptDecodeSpend",
  "scriptDecodeMint",
  "validationTraceDisputeCekCoreBlsBudget",
]);

it("initializes and publishes every reference script outside the exception set", async () => {
  const {
    emulator,
    lucid,
    referenceScriptsLucid,
    nonceUtxo,
    operatorSeedPhrase,
    referenceScriptAuth,
  } = await initEmulatorLucid();
  const contracts = await loadContracts(
    { txHash: nonceUtxo.txHash, outputIndex: nonceUtxo.outputIndex },
    referenceScriptAuth,
  );
  emulator.awaitSlot(120);
  const initTx = await buildAtomicInitializationTx(
    lucid,
    referenceScriptsLucid,
    contracts,
    nonceUtxo,
    operatorSeedPhrase,
  );
  const signed = await (await initTx.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await signed.submit());
  const status = await Effect.runPromise(
    fetchProtocolDeploymentStatus(lucid, contracts),
  );
  expect(status.complete).toBe(true);

  const referenceScriptsAddress = await referenceScriptsLucid
    .wallet()
    .address();
  const published = await Effect.runPromise(
    verifyNodeRuntimeReferenceScriptsProgram(
      lucid,
      referenceScriptsAddress,
      contracts,
      contracts.referenceScriptAuth,
    ),
  );
  const roles = Object.entries(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  const exempt = roles
    .filter(([, contract]) => NOT_YET_PUBLISHED_CONTRACTS.has(contract))
    .map(([role]) => role);
  expect(exempt).toHaveLength(NOT_YET_PUBLISHED_CONTRACTS.size);
  expect(published.map(({ name }) => name).sort()).toEqual(
    roles
      .map(([role]) => role)
      .filter((role) => !exempt.includes(role))
      .sort(),
  );

  const excludedHashes = [
    contracts.scriptDecode.spendingScriptHash,
    contracts.scriptDecode.policyId,
    contracts.fraudProofContracts.validationTraceDispute.cekCoreStages.blsBudget
      .spendingScriptHash,
  ];
  const referenceHashes = (
    await lucid.utxosAt(referenceScriptsAddress)
  ).flatMap(({ scriptRef }) =>
    scriptRef == null ? [] : [validatorToScriptHash(scriptRef)],
  );
  expect(referenceHashes.length).toBeGreaterThanOrEqual(published.length);
  for (const hash of excludedHashes) {
    expect(referenceHashes).not.toContain(hash);
  }
}, 600_000);
