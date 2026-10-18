import { MIDGARD_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core/consensus-profile";
import {
  bindScriptDecodeRun,
  buildScriptDecodeWalk,
  buildScriptDecodeWalkTx,
  finishScriptDecodeRun,
  scriptDecodeRecordDatum,
  scriptDecodeWalkStakes,
  stageScriptDecodeBytes,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  nextScriptDecodeChainedWalk,
  scriptDecodeRecordOfScript,
  scriptDecodeScriptHash,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import { CML, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "../src/deployment-manifest.js";
import { fetchProtocolDeploymentStatus } from "../src/transactions/initialization.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import {
  ensureRuntimeRewardAccountsRegisteredProgram,
  ensureScriptRewardAccountRegisteredProgram,
} from "../src/transactions/script-reward-registration.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  buildAtomicInitializationTx,
  initEmulatorLucid,
  loadContracts,
} from "./initialization-emulator.init-emulator-lucid.js";
import {
  anotherRunStakes,
  brokenLinkStakes,
  expectSpendRefusal,
  gapStakes,
  OUTSIDE_STAKER,
  outsideStakes,
  payDecoy,
  publishOutsideStaker,
  scriptDecodeDeploymentAt,
} from "./initialization-emulator.script-decode-walks.js";

/**
 * Initialization publishes every reference script of every family, the
 * script-decode validator and its class stakers among them, each in a
 * transaction the ledger accepts, and registers the stakers' reward
 * accounts. The CEK core BLS budget step publishes under the publication
 * target. A script's decode then runs on that deployment.
 */
const PUBLICATION_TARGET_BYTES =
  MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationTransactionBytes;
const MAX_TX_BYTES = 16_384;
const PLUTUS_V3 = 3;
// A small compiled PlutusV3 script whose first Walk at this fuel steps the
// term class, then the value class, then the term class again.
const DECODED_SCRIPT = Buffer.from("5001010023259800b452689b2b20025735", "hex");
const WALK_FUEL = 32;

const publicationsOf = (submitted: readonly string[], hash: string) =>
  submitted.filter((cbor) => {
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const scriptRef = outputs.get(index).script_ref();
      if (scriptRef != null && scriptRef.hash().to_hex() === hash) {
        return true;
      }
    }
    return false;
  });

type Initialized = Awaited<ReturnType<typeof initEmulatorLucid>> & {
  readonly contracts: SDK.MidgardValidators;
  readonly submitted: readonly string[];
  readonly referenceScriptsAddress: string;
};

/** A genuine Init in a fresh emulator, recording every accepted tx. */
const initialize = async (): Promise<Initialized> => {
  const setup = await initEmulatorLucid();
  const { emulator, lucid, referenceScriptsLucid, nonceUtxo } = setup;
  const submitted: string[] = [];
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    submitted.push(cbor);
    return hash;
  };
  const contracts = await loadContracts(
    { txHash: nonceUtxo.txHash, outputIndex: nonceUtxo.outputIndex },
    setup.referenceScriptAuth,
  );
  emulator.awaitSlot(120);
  const initTx = await buildAtomicInitializationTx(
    lucid,
    referenceScriptsLucid,
    contracts,
    nonceUtxo,
    setup.operatorSeedPhrase,
  );
  const signed = await (await initTx.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await signed.submit());
  await runWithoutFollower(
    ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts),
  );
  return {
    ...setup,
    contracts,
    submitted,
    referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
  };
};

describe("initialization emulator: script decode", () => {
  let init: Initialized;

  beforeAll(async () => {
    init = await initialize();
  }, 600_000);

  it("initializes and publishes every family reference script, the BLS budget step under the publication target", async () => {
    const { lucid, contracts, submitted, referenceScriptsAddress } = init;
    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    expect(status.complete).toBe(true);

    const published = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucid,
        referenceScriptsAddress,
        contracts,
        contracts.referenceScriptAuth,
      ),
    );
    expect(published.map(({ name }) => name).sort()).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).sort(),
    );

    const referenceHashes = (
      await lucid.utxosAt(referenceScriptsAddress)
    ).flatMap(({ scriptRef }) =>
      scriptRef == null ? [] : [validatorToScriptHash(scriptRef)],
    );
    expect(referenceHashes.length).toBeGreaterThanOrEqual(published.length);
    const scriptDecodeHashes = [
      contracts.scriptDecode.spendingScriptHash,
      ...Object.values(contracts.scriptDecode.stakers).map(
        ({ withdrawalScriptHash }) => withdrawalScriptHash,
      ),
    ];
    for (const hash of scriptDecodeHashes) {
      expect(referenceHashes).toContain(hash);
      const publications = publicationsOf(submitted, hash);
      expect(publications.length).toBeGreaterThan(0);
      for (const cbor of publications) {
        expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_BYTES);
      }
    }
    const blsBudgetHash =
      contracts.fraudProofContracts.validationTraceDispute.cekCoreStages
        .blsBudget.spendingScriptHash;
    expect(referenceHashes).toContain(blsBudgetHash);

    // The CEK core BLS budget step publishes in a transaction under the target.
    const blsPublications = publicationsOf(submitted, blsBudgetHash);
    expect(blsPublications).toHaveLength(1);
    expect(blsPublications[0]!.length / 2).toBeLessThanOrEqual(
      PUBLICATION_TARGET_BYTES,
    );
  }, 120_000);

  it("decodes a script on the initialized deployment, refusing tampered Walks", async () => {
    const { lucid, contracts, referenceScriptsAddress } = init;
    const deployment = await scriptDecodeDeploymentAt(
      lucid,
      referenceScriptsAddress,
      contracts.scriptDecode,
    );
    await runWithoutFollower(
      ensureScriptRewardAccountRegisteredProgram(
        lucid,
        OUTSIDE_STAKER.withdrawalScript,
      ),
    );
    const scriptHashHex = scriptDecodeScriptHash(
      PLUTUS_V3,
      DECODED_SCRIPT,
    ).toString("hex");
    const staging = await stageScriptDecodeBytes({
      lucid,
      scriptBytes: DECODED_SCRIPT,
    });
    let cursor = startScriptDecodeWalk(PLUTUS_V3, DECODED_SCRIPT);
    let runUtxo = await bindScriptDecodeRun({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptHashHex,
      initialRun: cursor.run,
      staging,
    });
    const walkTx = (
      stakes: Parameters<typeof buildScriptDecodeWalkTx>[0]["stakes"],
      after: Parameters<typeof buildScriptDecodeWalkTx>[0]["after"],
      inputs: Parameters<typeof buildScriptDecodeWalkTx>[0]["inputs"] = [],
    ) =>
      buildScriptDecodeWalkTx({
        lucid,
        deployment,
        scriptHashHex,
        runUtxo,
        staging,
        after,
        stakes,
        inputs,
      });

    const outsideReference = await publishOutsideStaker(lucid);
    const first = nextScriptDecodeChainedWalk(cursor, WALK_FUEL).walk;
    const classes = first.segments.map(({ within }) => within);
    expect(new Set(classes).size).toBeGreaterThanOrEqual(2);
    expect(classes.length).toBeGreaterThanOrEqual(3);
    // The honest Walk builds; each tampered one is refused by the run's spend.
    await walkTx(
      scriptDecodeWalkStakes(deployment, runUtxo, first),
      first.after,
    );
    await expectSpendRefusal(() =>
      walkTx(outsideStakes(runUtxo, first, outsideReference), first.after),
    );
    const decoy = await payDecoy(lucid, runUtxo.datum!);
    await expectSpendRefusal(() =>
      walkTx(anotherRunStakes(deployment, runUtxo, first, decoy), first.after, [
        decoy,
      ]),
    );
    await expectSpendRefusal(() =>
      walkTx(gapStakes(deployment, runUtxo, first), first.after),
    );
    await expectSpendRefusal(() =>
      walkTx(brokenLinkStakes(deployment, runUtxo, first), first.after),
    );

    let crossed = false;
    while (scriptDecodeWalkRecord(cursor) === undefined) {
      const { walk, next } = nextScriptDecodeChainedWalk(cursor, WALK_FUEL);
      crossed ||= new Set(walk.segments.map(({ within }) => within)).size >= 2;
      const built = await buildScriptDecodeWalk({
        lucid,
        deployment,
        scriptHashHex,
        runUtxo,
        staging,
        walk,
      });
      expect(built.txBytes).toBeLessThanOrEqual(MAX_TX_BYTES);
      runUtxo = await built.submit();
      cursor = next;
    }
    expect(crossed).toBe(true);
    const record = scriptDecodeWalkRecord(cursor)!;
    expect(record).toEqual(
      scriptDecodeRecordOfScript(PLUTUS_V3, DECODED_SCRIPT),
    );
    const recordUtxo = await finishScriptDecodeRun({
      lucid,
      deployment,
      scriptHashHex,
      runUtxo,
      record,
    });
    expect(recordUtxo.datum).toBe(scriptDecodeRecordDatum(record));
  }, 300_000);
});
