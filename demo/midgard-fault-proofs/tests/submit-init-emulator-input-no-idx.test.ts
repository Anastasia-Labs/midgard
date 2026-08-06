/**
 * `input-no-idx` emulator lifecycle (Goal task `Q13`, §9.1 output 9).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters: init -> step-01 -> step-02 -> step-03 -> step-04 ->
 * permanent fraud-proof token -> fraudulent block removal, plus the
 * valid-block negative on both reachable planes.
 *
 * The committed evidence is a two-transaction block: a **producing**
 * transaction and a **spender** that spends `(producer_tx_id, output_index)`
 * where `output_index` is at or past the end of the producer's canonical
 * outputs list. The producer's id preimage *is* in the block, which is exactly
 * what distinguishes this family from `non-existent-input`.
 *
 * Kept in its own file so the leaked wasm heap stays far below the ~4 GiB
 * wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */

import { createHash } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { outRefLabel } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCompactV1,
} from "@al-ft/midgard-core/codec";
import {
  FraudProofTokenDatum,
  inputNoIdxOutputsCommitmentV1,
  inputNoIdxSpendInputsCommitmentV1,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
  type MidgardTxInput,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  Emulator,
  type EmulatorAccount,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  midgardTxOutputFromCanonicalCborV1,
  publishInputNoIdxSpendInputsV1,
  resolveProverSigner,
  submitInputNoIdxStep01,
  submitInputNoIdxStep02,
  submitInputNoIdxStep02UntilTerminal,
  submitInputNoIdxStep03,
  submitInputNoIdxStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import type { SubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  nativeTxFromCoreCompact,
  submitInit,
} from "./support/legacy-submit-emulator.js";
import {
  countedTransactionsRoot,
  expectStateQueueHeaderOrder,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  makeHeader,
  makeNativeTx,
  network,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
  trieRootHex,
} from "./support/submit-init-emulator-shared.js";

/** The out-of-range output index the fraudulent spender claims. */
const CHALLENGED_OUTPUT_INDEX = 7n;

// Public BIP39 vectors used only by this emulator test. They are not secrets
// and must never fund a real wallet.
const TEST_ONLY_FUNDER_SEED =
  "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
const TEST_ONLY_PROVER_SEED =
  "test test test test test test test test test test test junk";
const FIXED_EMULATOR_UNIX_MS = 1_735_689_600_000;

const fixedBaseEmulatorAccount = (
  seedPhrase: string,
  lovelace: bigint,
): EmulatorAccount => ({
  seedPhrase,
  privateKey: "",
  address: walletFromSeed(seedPhrase, {
    addressType: "Base",
    accountIndex: 0,
    network: "Custom",
  }).address,
  assets: { lovelace },
});

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      outputIndex,
    ).to_cbor_bytes(),
  );

/** One canonical native output: enterprise pubkey address, lovelace only. */
const nativeOutputCbor = (paymentByte: number, lovelace: bigint): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

type InputNoIdxBlockFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly producingTxId: string;
  readonly producingTxOutputsHash: string;
  readonly producingOutputsCbor: readonly string[];
  readonly badTxId: string;
  readonly badInput: MidgardTxInput;
  readonly badInputs: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
  readonly verifiedTxInputsHash: string;
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly producingTxInclusion: SubmitStep01TxInclusion;
};

/**
 * Commits a producer with `producingOutputs` canonical outputs and a spender
 * of `(producerTxId, challengedOutputIndex)` as the two native-compact leaves
 * of one block's transactions MPF. The block carries the `input-no-idx`
 * violation exactly when `challengedOutputIndex >= producingOutputs.length`.
 */
const buildInputNoIdxBlockFixture = async ({
  producingOutputCount,
  challengedOutputIndex = CHALLENGED_OUTPUT_INDEX,
  badSpendInputCount = 1,
}: {
  readonly producingOutputCount: number;
  readonly challengedOutputIndex?: bigint;
  readonly badSpendInputCount?: number;
}): Promise<InputNoIdxBlockFixture> => {
  if (!Number.isSafeInteger(badSpendInputCount) || badSpendInputCount <= 0) {
    throw new Error("badSpendInputCount must be a positive safe integer");
  }
  const producingOutputs = Array.from(
    { length: producingOutputCount },
    (_, index) => nativeOutputCbor(0x40 + index, 5_000_000n + BigInt(index)),
  );
  const producingTx = makeNativeTx({
    spendInputCbors: [inputCbor("99".repeat(32), 0n)],
    fee: 7n,
    ...(producingOutputs[0] === undefined
      ? {}
      : { outputCbor: producingOutputs[0] }),
  });
  const producingTxId = computeMidgardNativeTxIdV1(producingTx).toString("hex");
  const challengedInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: challengedOutputIndex,
  };
  const badInputs = [
    challengedInput,
    ...Array.from({ length: badSpendInputCount - 1 }, (_, index) => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    })),
  ].sort((left, right) =>
    Buffer.compare(
      inputCbor(left.tx_id, left.output_index),
      inputCbor(right.tx_id, right.output_index),
    ),
  );
  const badInputsIndex = badInputs.findIndex(
    (input) =>
      input.tx_id === challengedInput.tx_id &&
      input.output_index === challengedInput.output_index,
  );
  if (badInputsIndex < 0) {
    throw new Error("Expected challenged input in canonical bad input list");
  }
  const badTx = makeNativeTx({
    spendInputCbors: badInputs.map((input) =>
      inputCbor(input.tx_id, input.output_index),
    ),
    fee: 9n,
  });
  const badTxId = computeMidgardNativeTxIdV1(badTx).toString("hex");

  const producingCompactCbor = encodeMidgardNativeTxCompactV1(
    producingTx.compact,
  );
  const badCompactCbor = encodeMidgardNativeTxCompactV1(badTx.compact);

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(Buffer.from(producingTxId, "hex"), producingCompactCbor);
  await trie.insert(Buffer.from(badTxId, "hex"), badCompactCbor);
  const producingProof = await trie.prove(Buffer.from(producingTxId, "hex"));
  const badProof = await trie.prove(Buffer.from(badTxId, "hex"));
  const transactionsRoot = trieRootHex(trie);
  const producingCompact = nativeTxFromCoreCompact(producingTx.compact);
  const badCompact = nativeTxFromCoreCompact(badTx.compact);

  const inclusionFor = (
    nativeTxId: string,
    compactCbor: Buffer,
    proofCbor: string,
    compact: typeof producingTx.compact,
  ): SubmitStep01TxInclusion => ({
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: compactCbor.toString("hex"),
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  });

  return {
    transactionsRoot,
    l2TransactionCount: 2n,
    producingTxId,
    producingTxOutputsHash: producingCompact.body.outputs_hash,
    producingOutputsCbor: producingOutputs.map((item) => item.toString("hex")),
    badTxId,
    badInput: challengedInput,
    badInputs,
    badInputsIndex,
    verifiedTxInputsHash: badCompact.body.spend_inputs_hash,
    badTxInclusion: inclusionFor(
      badTxId,
      badCompactCbor,
      badProof.toCBOR().toString("hex"),
      badTx.compact,
    ),
    producingTxInclusion: inclusionFor(
      producingTxId,
      producingCompactCbor,
      producingProof.toCBOR().toString("hex"),
      producingTx.compact,
    ),
  };
};

const makeEmulatorHarness = async () => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder = fixedBaseEmulatorAccount(
    TEST_ONLY_FUNDER_SEED,
    40_000_000_000n,
  );
  const prover = fixedBaseEmulatorAccount(
    TEST_ONLY_PROVER_SEED,
    20_000_000_000n,
  );
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  emulator.time = FIXED_EMULATOR_UNIX_MS;
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    { realInputNoIdx: true, alwaysFraudProofCatalogue: true },
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  return {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  };
};

const setupFraudulentBlock = async ({
  funderLucid,
  emulator,
  contracts,
  catalogue,
  fixture,
}: {
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly emulator: Emulator;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly fixture: InputNoIdxBlockFixture;
}) => {
  const funderPaymentCredential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (
    funderPaymentCredential === undefined ||
    funderPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const fraudulentHeader = makeHeader(
    funderPaymentCredential.hash,
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  return await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo: (await funderLucid.wallet().getUtxos())[0]!,
    catalogue,
    header: fraudulentHeader,
  });
};

const STEP02_RELEASE_MEMORY_LIMIT = 13_200_000n;
const STEP02_RELEASE_CPU_LIMIT = 8_000_000_000n;
const HALF_CANONICAL_MATURITY_MS = 302_400_000;
/**
 * Pinned `CompletePublished` measurement, re-derived at `7fd434a7` (#542).
 *
 * The previous pin (fee `542_885`, mem `521_130`, cpu `209_629_043`, CBOR sha
 * `8ec9d1d8…`) was taken against the **pre-#521** blueprint. #521's decoder
 * remediation (`a954669f`, blueprint re-pinned in `c682cc69`) renamed
 * `cek_machine_v1.ValueWitnessV1` -> `MachineValueWitnessV1` and
 * `user_events/deposit.Datum` -> `DepositDatum`, which moved the compiled
 * bytes of 8 validators; the applied step scripts therefore changed size
 * slightly while the transaction layout stayed identical — exactly the
 * observed signature of drifted ex-units/fee/tx-hash with an unchanged
 * `signedTxBytes` (7_771) and unchanged structural counts.
 *
 * Attribution (both `aiken v1.1.22+39d6b04`, `aiken build --env testnet`):
 *   - pre-rename blueprint built from `84aa1ce3` (plutus.json sha256
 *     `991da062…`) reproduces the OLD pin verbatim — this case passes when the
 *     suite is pointed at it via `MIDGARD_REAL_BLUEPRINT_PATH`;
 *   - post-rename blueprint built from `7fd434a7` (plutus.json sha256
 *     `76f9e53d…`, byte-identical to the `c682cc69` build) yields the values
 *     pinned below.
 *
 * Determinism (Q13 discipline — deterministic test wallets, fixed emulator
 * clock, two fresh `vitest run --pool=forks` processes on `7fd434a7` with the
 * `7fd434a7` blueprint): both producing runs emitted identical fee, ex-units,
 * and CBOR sha256 `e6936871…`.
 *
 * Serializer provenance (prose, deliberately not asserted anywhere): the
 * producing runs used `@anastasia-labs/cardano-multiplatform-lib-nodejs`
 * `6.2.0-1` with the locally shadow-stack-patched
 * `cardano_multiplatform_lib_bg.wasm` (sha256 `cd96b005…`; pristine npm
 * `6.2.0-1` is `91b38c8e…`). RE-VERIFIED 2026-08-05: the `6.2.0-2` bump
 * (source-built 16 MiB shadow stack, wasm sha256 `47e56638…`; patcher
 * retired) landed and this suite reproduced every pin below unchanged —
 * the shadow-stack size is serialization-invisible, as expected.
 *
 * Re-pinned at issue #545. Wiring the four foundational families onto
 * published-chunk proof carriage changed the Q00-owned shared binding
 * (`midgard/fraud_proofs/common`) and made
 * `midgard/common/utils.get_unique_withdraw_redeemer` public, so every applied
 * step script in the blueprint recompiled. The transaction layout is untouched
 * — `signedTxBytes` is still 7,771 and every structural count is unchanged —
 * and the drift is 0.4% of execution: memory 523,998 to 521,130, cpu
 * 210,521,290 to 209,629,043, fee 543,115 to 542,885, which moves the CBOR
 * sha256. That is the same signature this pin has drifted under before, and it
 * is re-pinned rather than relaxed.
 *
 * Re-pinned at issue #547. Registering `noReferenceInput`,
 * `referenceInputNoIdx` and `invalidSignature` grew the fraud-proof catalogue
 * from 8 to 11 leaves, so the `nonExistentInputNoIndex` membership proof this
 * transaction carries in its catalogue redeemer folds a different set of
 * neighbours. Every measured quantity is bit-for-bit unchanged — same 7,771
 * signed bytes, same fee, same memory, same cpu, same structural counts — so
 * the proof is the same length and only its content, and therefore the CBOR
 * sha256, moves: 2eae6308… to ae89c6c6….
 */
const COMPLETE_PUBLISHED_CANONICAL_PROOF = {
  signedTxBytes: 7_771,
  signedTxSha256:
    "ae89c6c6026e038f0bcf4e8f868e077c96f9eb0a71a604da121c93f813533654",
  txByteMargin: 8_613,
  fee: 542_885n,
  executionMemory: 521_130n,
  executionCpu: 209_629_043n,
  releaseMemoryMargin: 12_678_870n,
  releaseCpuMargin: 7_790_370_957n,
  inputCount: 2,
  referenceInputCount: 1,
  outputCount: 2,
  collateralInputCount: 1,
  vkeyWitnessCount: 1,
  redeemerCount: 1,
  outputLovelace: 1_512_810n,
  outputMinAda: 1_499_880n,
  outputValueBytes: 73,
  valueByteMargin: 4_927,
} as const;

const measureStep02ProofTransaction = ({
  transactionCbor,
  outputIndex,
  elapsedMs,
}: {
  readonly transactionCbor: string;
  readonly outputIndex: number;
  readonly elapsedMs?: number;
}) => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const output = body.outputs().get(outputIndex);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionCpu = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    executionMemory += units.mem();
    executionCpu += units.steps();
  }
  const signedTxBytes = transactionCbor.length / 2;
  const outputValueBytes = output.amount().to_cbor_bytes().length;
  const localBuildSubmitConfirmWallMs =
    elapsedMs === undefined ? undefined : Number(elapsedMs.toFixed(3));
  return {
    signedTxBytes,
    signedTxSha256: createHash("sha256")
      .update(Buffer.from(transactionCbor, "hex"))
      .digest("hex"),
    txByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - signedTxBytes,
    fee: body.fee(),
    executionMemory,
    executionCpu,
    releaseMemoryMargin: STEP02_RELEASE_MEMORY_LIMIT - executionMemory,
    releaseCpuMargin: STEP02_RELEASE_CPU_LIMIT - executionCpu,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    outputLovelace: output.amount().coin(),
    outputMinAda: CML.min_ada_required(
      output,
      BigInt(PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte),
    ),
    outputValueBytes,
    valueByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxValSize - outputValueBytes,
    ...(localBuildSubmitConfirmWallMs === undefined
      ? {}
      : {
          localBuildSubmitConfirmWallMs,
          localHalfMaturityMarginMs:
            HALF_CANONICAL_MATURITY_MS - localBuildSubmitConfirmWallMs,
        }),
  };
};

const expectStep02ProofFit = (
  measurement: ReturnType<typeof measureStep02ProofTransaction>,
): void => {
  expect(measurement.txByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.releaseMemoryMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.releaseCpuMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.outputLovelace).toBeGreaterThanOrEqual(
    measurement.outputMinAda,
  );
  expect(measurement.valueByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.vkeyWitnessCount).toBe(1);
  expect(measurement.redeemerCount).toBe(1);
  if (measurement.localBuildSubmitConfirmWallMs !== undefined) {
    expect(measurement.localBuildSubmitConfirmWallMs).toBeGreaterThan(0);
    expect(measurement.localHalfMaturityMarginMs).toBeGreaterThan(0);
  }
};

const startInputNoIdxStep02Thread = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<ReturnType<typeof makeEmulatorHarness>>;
  readonly fixture: InputNoIdxBlockFixture;
}) => {
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
  );
  const initResult = await submitInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "nonExistentInputNoIndex",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Result = await submitInputNoIdxStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.badTxInclusion,
    awaitConfirmation: true,
  });
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    step01Result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  return {
    deploymentInfo,
    initResult,
    secondStepUtxo,
    setup,
    step01Result,
  };
};

describe("input-no-idx fault-proof emulator lifecycle", () => {
  it("proves and removes an out-of-range spend-input block end to end", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });

    // The producer commits no outputs at all, so index 7 cannot exist.
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
    });
    expect(fixture.producingOutputsCbor).toHaveLength(0);
    expect(inputNoIdxOutputsCommitmentV1([])).toBe(
      fixture.producingTxOutputsHash,
    );
    expect(inputNoIdxSpendInputsCommitmentV1([fixture.badInput])).toBe(
      fixture.verifiedTxInputsHash,
    );

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    // ## init
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("nonExistentInputNoIndex");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.nonExistentInputNoIndex.categoryId,
    );

    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // ## step-01: bind the bad transaction to the committed header
    const step01Result = await submitInputNoIdxStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      awaitConfirmation: true,
    });
    expect(step01Result.badTxId).toBe(fixture.badTxId);
    expect(step01Result.verifiedTxInputsHash).toBe(
      fixture.verifiedTxInputsHash,
    );

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(secondStepUtxo.datum!, InputNoIdxStep02Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        Direct: { verified_tx_inputs_hash: fixture.verifiedTxInputsHash },
      },
    });

    // ## step-02: open the spend-inputs commitment and forward the input
    const directStartedAt = performance.now();
    const directCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: {
            inputsPreimage: [fixture.badInput],
            badInputsIndex: 0,
          },
          awaitConfirmation: true,
        }),
    );
    const directElapsedMs = performance.now() - directStartedAt;
    const step02Result = directCapture.result;
    const directProofMeasurement = measureStep02ProofTransaction({
      transactionCbor: directCapture.transactionCbors[0]!,
      outputIndex: step02Result.outputIndex,
      elapsedMs: directElapsedMs,
    });
    expectStep02ProofFit(directProofMeasurement);
    expect(directProofMeasurement.referenceInputCount).toBe(0);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          { q13DirectConsumingProof: directProofMeasurement },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
    expect(step02Result.badInputTxId).toBe(fixture.producingTxId);
    expect(step02Result.badInputOutputIndex).toBe(
      Number(CHALLENGED_OUTPUT_INDEX),
    );

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(thirdStepUtxo.datum!, InputNoIdxStep03Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        bad_input_tx_id: fixture.producingTxId,
        bad_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-03: bind the producing transaction from the same block
    const step03Result = await submitInputNoIdxStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      awaitConfirmation: true,
    });
    expect(step03Result.producingTxId).toBe(fixture.producingTxId);
    expect(step03Result.producingTxOutputsHash).toBe(
      fixture.producingTxOutputsHash,
    );

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(Data.from(fourthStepUtxo.datum!, InputNoIdxStep04Datum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
      data: {
        producing_tx_outputs_hash: fixture.producingTxOutputsHash,
        bad_input_output_index: CHALLENGED_OUTPUT_INDEX,
      },
    });

    // ## step-04: open the outputs commitment and mint the permanent token
    const step04Result = await submitInputNoIdxStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      outputsPreimage: { outputsPreimage: [] },
      awaitConfirmation: true,
    });
    expect(step04Result.producingTxOutputCount).toBe(0);
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step04Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step03Result.fourthStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    // ## removal: the proven block leaves the state queue, the token stays
    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("nonExistentInputNoIndex");
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step04Result.fraudProofUnit]).toBe(1n);
  }, 240_000);

  it("constructs and measures a genuine CompletePublished consuming proof", async () => {
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
    });
    const { deploymentInfo, initResult, secondStepUtxo } =
      await startInputNoIdxStep02Thread({ harness, fixture });
    const inputsPreimage = {
      inputsPreimage: fixture.badInputs,
      badInputsIndex: fixture.badInputsIndex,
    };

    const publicationStartedAt = performance.now();
    const publication = await publishInputNoIdxSpendInputsV1({
      lucid: harness.proverLucid,
      network,
      signer: harness.proverSigner,
      computationThreadPolicyId: initResult.computationThreadPolicyId,
      computationThreadAssetName: initResult.computationThreadAssetName,
      verifiedTxInputsHash: fixture.verifiedTxInputsHash,
      inputsPreimage,
    });
    const publicationBuildSubmitConfirmWallMs = Number(
      (performance.now() - publicationStartedAt).toFixed(3),
    );

    const proofStartedAt = performance.now();
    const proofCapture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitInputNoIdxStep02({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage,
          publicationReference: publication,
          awaitConfirmation: true,
        }),
    );
    const proofElapsedMs = performance.now() - proofStartedAt;
    const proofResult = proofCapture.result;
    const proofMeasurement = measureStep02ProofTransaction({
      transactionCbor: proofCapture.transactionCbors[0]!,
      outputIndex: proofResult.outputIndex,
      elapsedMs: proofElapsedMs,
    });

    expect(proofResult.step02Execution).toBe("published");
    expect(proofResult.terminal).toBe(true);
    expect(proofResult.publicationOutRef).toBe(publication.outRef);
    expect(proofMeasurement).toMatchObject(COMPLETE_PUBLISHED_CANONICAL_PROOF);
    expect(proofMeasurement.referenceInputCount).toBe(1);
    const signedProofTransaction = CML.Transaction.from_cbor_hex(
      proofCapture.transactionCbors[0]!,
    );
    const proofReferenceInputs = signedProofTransaction
      .body()
      .reference_inputs();
    expect(proofReferenceInputs?.len()).toBe(1);
    expect(proofReferenceInputs?.get(0).transaction_id().to_hex()).toBe(
      publication.utxo.txHash,
    );
    expect(proofReferenceInputs?.get(0).index()).toBe(
      BigInt(publication.utxo.outputIndex),
    );
    expectStep02ProofFit(proofMeasurement);
    await expect(
      harness.proverLucid.utxosByOutRef([
        {
          txHash: publication.utxo.txHash,
          outputIndex: publication.utxo.outputIndex,
        },
      ]),
    ).resolves.toHaveLength(1);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            q13CompletePublished: {
              publication: publication.measurement,
              publicationBuildSubmitConfirmWallMs,
              consumingProof: proofMeasurement,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 240_000);

  it("confirms every intermediate root in a true 20-input ordered fold", async () => {
    const harness = await makeEmulatorHarness();
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 0,
      badSpendInputCount: 20,
    });
    expect(fixture.badInputs).toHaveLength(20);
    expect(inputNoIdxSpendInputsCommitmentV1(fixture.badInputs)).toBe(
      fixture.verifiedTxInputsHash,
    );
    const { deploymentInfo, secondStepUtxo } =
      await startInputNoIdxStep02Thread({ harness, fixture });

    const foldStartedAt = performance.now();
    const foldCapture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitInputNoIdxStep02UntilTerminal({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          inputsPreimage: {
            inputsPreimage: fixture.badInputs,
            badInputsIndex: fixture.badInputsIndex,
          },
          awaitConfirmation: true,
        }),
    );
    const foldBuildSubmitConfirmWallMs = Number(
      (performance.now() - foldStartedAt).toFixed(3),
    );
    const { submissions } = foldCapture.result;
    expect(submissions).toHaveLength(20);
    expect(foldCapture.transactionCbors).toHaveLength(20);
    expect(submissions[0]!.step02Execution).toBe("fold-start");
    expect(
      submissions
        .slice(1)
        .every((item) => item.step02Execution === "fold-next"),
    ).toBe(true);
    expect(submissions.slice(0, -1).every((item) => !item.terminal)).toBe(true);
    expect(submissions.at(-1)!.terminal).toBe(true);

    const measurements = foldCapture.transactionCbors.map(
      (transactionCbor, index) => {
        const submission = submissions[index]!;
        const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
        const threadOutput = transaction
          .body()
          .outputs()
          .get(submission.outputIndex);
        const coreOutput = coreToTxOutput(threadOutput);
        if (index < 19) {
          const expectedSelected =
            fixture.badInputsIndex <= index ? fixture.badInput : null;
          expect(Data.from(coreOutput.datum!, InputNoIdxStep02Datum)).toEqual({
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: {
              Folding: {
                verified_tx_inputs_hash: fixture.verifiedTxInputsHash,
                item_count: 20n,
                next_item_index: BigInt(index + 1),
                bad_inputs_index: BigInt(fixture.badInputsIndex),
                selected_input: expectedSelected,
              },
            },
          });
          expect(submission.nextFoldItemIndex).toBe(index + 1);
        } else {
          expect(Data.from(coreOutput.datum!, InputNoIdxStep03Datum)).toEqual({
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: {
              bad_input_tx_id: fixture.badInput.tx_id,
              bad_input_output_index: fixture.badInput.output_index,
            },
          });
        }
        const measurement = measureStep02ProofTransaction({
          transactionCbor,
          outputIndex: submission.outputIndex,
        });
        expectStep02ProofFit(measurement);
        return { itemIndex: index, ...measurement };
      },
    );
    expect(foldBuildSubmitConfirmWallMs).toBeGreaterThan(0);
    expect(
      HALF_CANONICAL_MATURITY_MS - foldBuildSubmitConfirmWallMs,
    ).toBeGreaterThan(0);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            q13Fold20: {
              badInputsIndex: fixture.badInputsIndex,
              foldBuildSubmitConfirmWallMs,
              localHalfMaturityMarginMs:
                HALF_CANONICAL_MATURITY_MS - foldBuildSubmitConfirmWallMs,
              measurements,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 240_000);

  it("cannot finalize an input-no-idx thread against a valid block", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    // A valid block: the spender takes index 0 of a producer that really has
    // an output at index 0.
    const fixture = await buildInputNoIdxBlockFixture({
      producingOutputCount: 1,
      challengedOutputIndex: 0n,
    });
    const producingOutputs = fixture.producingOutputsCbor.map((item) =>
      midgardTxOutputFromCanonicalCborV1(Buffer.from(item, "hex")),
    );
    expect(inputNoIdxOutputsCommitmentV1(producingOutputs)).toBe(
      fixture.producingTxOutputsHash,
    );

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
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // Steps 01-03 carry no verdict: a valid block advances just as far, which
    // is why the family's adjudication lives in step 04.
    const step01Result = await submitInputNoIdxStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.badTxInclusion,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 1 — off-chain fail-closed at the preimage opening: a preimage that
    // does not open the committed spend-inputs hash is refused before any
    // transaction is built.
    await expect(
      submitInputNoIdxStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        inputsPreimage: {
          inputsPreimage: [{ tx_id: fixture.producingTxId, output_index: 7n }],
          badInputsIndex: 0,
        },
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/does not open the committed spend-inputs hash/u);

    const step02Result = await submitInputNoIdxStep02({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: {
        inputsPreimage: [fixture.badInput],
        badInputsIndex: 0,
      },
      awaitConfirmation: true,
    });
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Result = await submitInputNoIdxStep03({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.producingTxInclusion,
      awaitConfirmation: true,
    });
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 2 — the valid-block verdict: index 0 exists in the producing
    // transaction, so finalization is refused off-chain...
    await expect(
      submitInputNoIdxStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: producingOutputs },
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      /an existing transaction input cannot be proven non-existent/u,
    );

    // ...and on-chain, if a prover strips the producer's outputs to fake an
    // empty list, the outputs commitment no longer opens.
    await expect(
      submitInputNoIdxStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        outputsPreimage: { outputsPreimage: [] },
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/does not open the committed outputs hash/u);

    // The thread is stuck at step 04 and the valid block is still queued.
    const stillFourthStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(stillFourthStep)).toBe(outRefLabel(fourthStepUtxo));
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 240_000);
});
