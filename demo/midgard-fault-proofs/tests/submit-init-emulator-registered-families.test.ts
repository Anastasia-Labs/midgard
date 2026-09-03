/**
 * Emulator lifecycles for the three families registered by issue #547:
 * `no-reference-input` (Q18), `reference-input-no-idx` (Q31), and
 * `invalid-signature` (Q15).
 *
 * Each case drives the real Aiken validators through a Lucid emulator with the
 * production submitters: `init` (which spends the catalogue membership proof
 * for the family's newly registered category, mints the computation thread and
 * pays it to the family's real step-01 address) followed by `step-01` (which
 * re-derives the counted `transactions_root` from the on-chain header, binds
 * the committed native transaction, and forwards the family's opened field to
 * step 02).
 *
 * Scope is deliberately `init -> step-01` per family: that is the segment the
 * catalogue registration unblocks and the segment whose evidence these builders
 * can construct without a family-specific ledger/exclusion fixture. The later
 * steps of the four-step families need `pexcludes` non-membership material
 * against a populated prev-ledger trie, which is the Q18/Q31 rows' own
 * remaining work.
 *
 * Kept in its own file so the leaked wasm heap stays far below the ~4 GiB
 * wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { outRefLabel } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { type NativeTxWitnessSetCompact, Proof } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitInvalidSignatureStep01,
  submitNoReferenceInputStep01,
  submitReferenceInputNoIdxStep01,
} from "../src/index.js";
import type { SubmitStep01TxInclusion } from "../src/submit-step-01.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlock as setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  trieRootHex,
} from "./support/submit-init-emulator-shared.js";

const inputCbor = (txId: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    Data.to(
      { tx_id: txId, output_index: outputIndex } as never,
      Data.Object({
        tx_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
        output_index: Data.Integer(),
      }) as never,
    ),
    "hex",
  );

type SingleTxBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly inclusion: SubmitStep01TxInclusion;
};

/**
 * Commits one canonical native-V1 compact transaction as the sole leaf of a
 * block's raw transactions MPF and returns the step-01 inclusion evidence for
 * it. Every family under test opens a different field of the same transaction
 * shape, so the fixture is shared.
 */
const buildSingleTxBlockFixture = async (
  nativeTx: MidgardNativeTxFull,
): Promise<SingleTxBlockFixture> => {
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
  const l2TransactionSourceCbor = l2TransactionSourceCborV1(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  const transactionsRoot = trieRootHex(trie);
  return {
    transactionsRoot,
    l2TransactionCount: 1n,
    nativeTxId,
    inclusion: {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proofCbor, Proof),
      txMembershipProofCbor: proofCbor,
    },
  };
};

type FamilyFlag =
  | "realNoReferenceInput"
  | "realReferenceInputNoIdx"
  | "realInvalidSignature";

const makeEmulatorHarness = async (familyFlag: FamilyFlag) =>
  await makeFaultProofEmulatorHarness({
    contractOptions: { [familyFlag]: true, alwaysFraudProofCatalogue: true },
  });

describe("registered fraud-proof families emulator lifecycle", () => {
  it(
    "initialises and binds a no-reference-input thread",
    { timeout: 600_000 },
    async () => {
      const harness = await makeEmulatorHarness("realNoReferenceInput");
      const {
        realBlueprint,
        emulator,
        funderLucid,
        proverLucid,
        proverSigner,
        contracts,
        catalogue,
      } = harness;

      const badTx = makeNativeTx({
        spendInputCbors: [inputCbor("11".repeat(32), 0n)],
        fee: 9n,
        referenceByte: "22",
      });
      const fixture = await buildSingleTxBlockFixture(badTx);
      const setup = await setupFraudulentBlock({
        funderLucid,
        emulator,
        contracts,
        catalogue,
        fixture,
      });
      await expectStateQueueHeaderOrder({
        lucid: funderLucid,
        contracts,
        expectedHeaderHashes: [setup.headerHash],
      });
      const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

      const initResult = await submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "noReferenceInput",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(initResult.txHash).toHaveLength(64);
      expect(initResult.fraudCategoryName).toBe("noReferenceInput");
      expect(initResult.fraudCategoryId).toBe(
        catalogue.categories.noReferenceInput.categoryId,
      );
      expect(initResult.computationThreadAssetName).toBe(
        `${catalogue.categories.noReferenceInput.categoryId}${setup.headerHash}`,
      );
      expect(initResult.firstStepAddress).toBe(
        contracts.fraudProofs.noReferenceInput.spendingScriptAddress,
      );

      const firstStepUtxo = await expectSingleUtxoWithUnit(
        proverLucid,
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      );

      const step01Result = await submitNoReferenceInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.inclusion,
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofNoReferenceInput.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(step01Result.txHash).toHaveLength(64);
      // #604: the thread carries the §2.5 anchor. The field-1 commitment it
      // used to forward is still in the compact structure step-02 opens, and is
      // checked there against the slot rather than carried loose.
      expect(step01Result.badTxId).toBe(fixture.nativeTxId);
      await expect(
        proverLucid.utxosAtWithUnit(
          initResult.firstStepAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
      await expectSingleUtxoWithUnit(
        proverLucid,
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      );
    },
  );

  it(
    "initialises and binds a reference-input-no-idx thread",
    { timeout: 600_000 },
    async () => {
      const harness = await makeEmulatorHarness("realReferenceInputNoIdx");
      const {
        realBlueprint,
        emulator,
        funderLucid,
        proverLucid,
        proverSigner,
        contracts,
        catalogue,
      } = harness;

      const badTx = makeNativeTx({
        spendInputCbors: [inputCbor("33".repeat(32), 0n)],
        fee: 11n,
        referenceByte: "44",
      });
      const fixture = await buildSingleTxBlockFixture(badTx);
      const setup = await setupFraudulentBlock({
        funderLucid,
        emulator,
        contracts,
        catalogue,
        fixture,
      });
      const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

      const initResult = await submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "referenceInputNoIdx",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(initResult.fraudCategoryName).toBe("referenceInputNoIdx");
      expect(initResult.fraudCategoryId).toBe(
        catalogue.categories.referenceInputNoIdx.categoryId,
      );
      expect(initResult.firstStepAddress).toBe(
        contracts.fraudProofs.referenceInputNoIdx.spendingScriptAddress,
      );

      const firstStepUtxo = await expectSingleUtxoWithUnit(
        proverLucid,
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      );

      const step01Result = await submitReferenceInputNoIdxStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.inclusion,
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofReferenceInputNoIdx.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(step01Result.txHash).toHaveLength(64);
      expect(step01Result.badTxId).toBe(fixture.nativeTxId);
      // #604: the thread carries the §2.5 anchor. Field 1's commitment is
      // re-derived at step-02 from the compact structure that anchor
      // authenticates, so it is not forwarded here any more.
      expect(step01Result.verifiedTxId).toBe(fixture.nativeTxId);
      await expectSingleUtxoWithUnit(
        proverLucid,
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      );
    },
  );

  it(
    "initialises and binds an invalid-signature thread",
    { timeout: 600_000 },
    async () => {
      const harness = await makeEmulatorHarness("realInvalidSignature");
      const {
        realBlueprint,
        emulator,
        funderLucid,
        proverLucid,
        proverSigner,
        contracts,
        catalogue,
      } = harness;

      const badTx = makeNativeTx({
        spendInputCbors: [inputCbor("55".repeat(32), 0n)],
        fee: 13n,
        witnessByte: "66",
      });
      const fixture = await buildSingleTxBlockFixture(badTx);
      const witnessSetCompact = deriveMidgardNativeTxWitnessSetCompact(
        badTx.witnessSet,
      );
      const badTxWitnessSetCompact: NativeTxWitnessSetCompact = {
        addr_tx_wits_hash: witnessSetCompact.addrTxWitsHash.toString("hex"),
        script_tx_wits_hash: witnessSetCompact.scriptTxWitsHash.toString("hex"),
        redeemer_tx_wits_hash:
          witnessSetCompact.redeemerTxWitsHash.toString("hex"),
      };
      const setup = await setupFraudulentBlock({
        funderLucid,
        emulator,
        contracts,
        catalogue,
        fixture,
      });
      const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

      const initResult = await submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "invalidSignature",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(initResult.fraudCategoryName).toBe("invalidSignature");
      expect(initResult.fraudCategoryId).toBe(
        catalogue.categories.invalidSignature.categoryId,
      );
      expect(initResult.firstStepAddress).toBe(
        contracts.fraudProofs.invalidSignature.spendingScriptAddress,
      );

      const firstStepUtxo = await expectSingleUtxoWithUnit(
        proverLucid,
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      );

      const step01Result = await submitInvalidSignatureStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.inclusion,
        badTxWitnessSetCompact,
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofInvalidSignature.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      });
      expect(step01Result.txHash).toHaveLength(64);
      expect(step01Result.nativeTxId).toBe(fixture.nativeTxId);
      // #604: the thread carries `WitnessAnchor` — the id plus the committed
      // `witness_set_hash` — rather than field 7's own commitment. Field 7 is
      // re-derived at step-02 out of the witness set this hash authenticates.
      expect(step01Result.badTxWitnessSetHash).toBe(
        fixture.inclusion.nativeTx.witness_set_hash,
      );
      await expectSingleUtxoWithUnit(
        proverLucid,
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      );
    },
  );
});
