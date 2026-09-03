/**
 * `fabricated-withdrawal` emulator lifecycle (Goal task `Q40`, §9.1 output 9).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters: init -> step-01 -> step-02 -> step-03 -> step-04 ->
 * permanent fraud-proof token, plus the valid-block negative on both planes
 * (off-chain fail-closed and on-chain membership rejection).
 *
 * The committed evidence is a `withdrawals_root` leaf that pairs the authentic
 * withdrawal identity with a **diverted** L1 payout address — exactly the fault
 * the family adjudicates (Q40's `MismatchedWithdrawalContent` shape). The
 * authentic L1 withdrawal event is exhibited as a reference input carrying the
 * hub-registered withdrawal event NFT, so step-02's verdict rests on
 * authenticated material.
 *
 * Until #614 regenerated the blueprint this file was a boundary tripwire that
 * measured the family's titles as absent (#482); the titles are present now,
 * so the tripwire is retired into this real lifecycle.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  deriveFabricatedWithdrawalStep01Handoff,
  parseSubmitFabricatedWithdrawalInclusion,
  submitFabricatedWithdrawalStep01,
} from "../src/submit-fabricated-withdrawal-step-01.js";
import { submitFabricatedWithdrawalStep02 } from "../src/submit-fabricated-withdrawal-step-02.js";
import {
  deriveFabricatedWithdrawalStep03Handoff,
  submitFabricatedWithdrawalStep03,
} from "../src/submit-fabricated-withdrawal-step-03.js";
import {
  assertFabricatedWithdrawalStep04Finalizable,
  submitFabricatedWithdrawalStep04,
} from "../src/submit-fabricated-withdrawal-step-04.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h28,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture-v1.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitFabricatedFamilyInit,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

// The `mismatched_content_block_v1` scenario, measured out of
// `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2}.ak`:
// the authentic withdrawal identity committed with a diverted L1 payout address,
// and the authentic event datum whose inclusion time falls inside the window.
const KEY_AUTHENTIC_WITHDRAWAL_ID =
  "d8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ff";
const VALUE_DIVERTED_WITHDRAWAL_INFO =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";
const DATUM_AUTHENTIC_WITHDRAWAL_EVENT =
  "d8799fd8799fd8799f58208b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b02ffd8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ffff0f581c57575757575757575757575757575757575757575757575757575757d8799fd8799f581c2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2bffd87a80ffd87980ff";
const HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM =
  "b5e4fa1c72a874ec61778f2e29dc4cc326313b3bc581bc64738fd45f1d9a9a70";
const HASH_AUTHENTIC_WITHDRAWAL_INFO =
  "f6b65e77ecfcfcaccba6fc17cf30e124829e93d60ef0e7259200316869ef38a0";
const HASH_DIVERTED_WITHDRAWAL_INFO =
  "6d8fd0959a65127c274f31b291d1ed97899bba0866c6945473ca7102a30de973";
const MM_WITHDRAWALS_ROOT =
  "ddf6c2b73b0a5be5c6afcb11cbb8c47ecec36a856231911288306a01e411bbed";
const NONCE_AUTHENTIC_WITHDRAWAL_ID =
  "630f633bd50fa6888cf4e56be119c4970c013d0c7a45216b7eed46960fac800b";
const AUTHENTIC_INCLUSION_TIME = 15n;
const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;

/** Stands in for the emulator prover's payment key hash. */
const FRAUD_PROVER = h28(0x77);

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

const L1_OBSERVATION: SDK.AuthenticatedL1Observation = {
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-local-node",
    grade: "security",
  },
  chainPoint: { slot: 4242n, blockHash: "09".repeat(32) },
  confirmationDepth: 12,
};

/** Commits the challenged block's single fabricated withdrawal leaf. */
const buildChallengedBlock = async () => {
  const base = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    startTime: HEADER_START_TIME,
    endTime: HEADER_END_TIME,
    transactionsRootMode: "nativeCompact",
  });
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    {
      key: Buffer.from(KEY_AUTHENTIC_WITHDRAWAL_ID, "hex"),
      value: Buffer.from(VALUE_DIVERTED_WITHDRAWAL_INFO, "hex"),
    },
  ]);
  const header: SDK.Header = {
    ...base.header,
    withdrawalsRoot: counted.root,
    withdrawalCount: counted.count,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const withdrawals: SDK.DaPayloadEntry[] = [
    [KEY_AUTHENTIC_WITHDRAWAL_ID, VALUE_DIVERTED_WITHDRAWAL_INFO],
  ];
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      withdrawals,
      counts: {
        ...base.payload.block_body.counts,
        withdrawalCount: counted.count,
      },
    },
  };
  return {
    header,
    headerHash,
    withdrawalsRoot: counted.root,
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    observation: authenticatedHeaderObservation({
      ...base,
      header,
      headerHash,
    }),
  };
};

/**
 * The harness every emulator scenario in this file opens with: the real
 * fabricated-withdrawal chain built from the regenerated blueprint and
 * registered in the canonical production catalogue.
 */
const makeEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realFabricatedWithdrawal: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const fabricatedWithdrawal = harness.contracts.fabricatedWithdrawal;
  const category = harness.catalogue.categories.fabricatedWithdrawal;
  if (fabricatedWithdrawal === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the fabricated-withdrawal contracts/category",
    );
  }
  expect(category.categoryId).toBe(SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID);
  expect(category.scriptHash).toBe(
    fabricatedWithdrawal.steps[0].spendingScriptHash,
  );
  return { ...harness, fabricatedWithdrawal, category };
};

/**
 * The challenged block committed on the emulator: the diverted leaf's counted
 * root under the funder-operated header whose window opens at the aligned
 * emulator clock. Returns everything the prover-side scenarios consume.
 */
const setupChallengedBlockOnEmulator = async (
  harness: Awaited<ReturnType<typeof makeEmulatorHarness>>,
  committedInfoCbor: string,
) => {
  const {
    emulator,
    funderLucid,
    contracts,
    catalogue,
    nonceUtxo,
    fabricatedWithdrawal,
  } = harness;
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    {
      key: Buffer.from(KEY_AUTHENTIC_WITHDRAWAL_ID, "hex"),
      value: Buffer.from(committedInfoCbor, "hex"),
    },
  ]);
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  // `header_v1_is_valid` (state-queue `CommitBlockHeader`) enforces the
  // transition-commitment identities: `total_event_count` must equal the sum
  // of the per-kind counts, `transition_step_count` must equal it, and the
  // transition-trace/event-to-step roots must be non-empty 32-byte roots
  // whenever that count is non-zero. Reusing the withdrawals counted root
  // keeps the header committable without touching validation traces (no
  // forced or L2 transactions in this block).
  const header: SDK.Header = {
    ...makeHeader(funderKeyHash, headerStartTime),
    withdrawalsRoot: counted.root,
    withdrawalCount: counted.count,
    totalEventCount: counted.count,
    transitionStepCount: counted.count,
    transitionTraceRoot: counted.root,
    eventToStepRoot: counted.root,
  };
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
  });
  const step01ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedWithdrawal.steps[0].spendingScript,
      label: "fabricated-withdrawal step-01",
    })
  ).utxo;
  const step02ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedWithdrawal.steps[1].spendingScript,
      label: "fabricated-withdrawal step-02",
    })
  ).utxo;
  const step03ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedWithdrawal.steps[2].spendingScript,
      label: "fabricated-withdrawal step-03",
    })
  ).utxo;
  const step04ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedWithdrawal.steps[3].spendingScript,
      label: "fabricated-withdrawal step-04",
    })
  ).utxo;
  // The authentic event's inclusion time, inside this header's establishment
  // window `(start_time, end_time]`.
  const eventInclusionTime = header.startTime + 500n;
  const authenticEventDatum: SDK.WithdrawalOrderDatum = {
    ...Data.from(DATUM_AUTHENTIC_WITHDRAWAL_EVENT, SDK.WithdrawalOrderDatum),
    inclusion_time: eventInclusionTime,
  };
  // Canonical (`serialiseData`-normalised) bytes: the datum carries token
  // maps, so plain `Data.to` output is not the classifier's canonical form.
  const eventDatumCbor = SDK.withdrawalEventDatumBytes(authenticEventDatum);
  const observedEventAssetName = await Effect.runPromise(
    SDK.withdrawalEventNonce(authenticEventDatum.event.id),
  );
  return {
    counted,
    header,
    setup,
    eventInclusionTime,
    authenticEventDatum,
    eventDatumCbor,
    observedEventAssetName,
    referenceScriptUtxos: [
      step01ReferenceScriptUtxo,
      step02ReferenceScriptUtxo,
      step03ReferenceScriptUtxo,
      step04ReferenceScriptUtxo,
    ] as const,
  };
};

describe("fabricated-withdrawal fault-proof emulator lifecycle", () => {
  it("admits retained-DA evidence and derives every thread handoff off-chain", async () => {
    // ## 1. Evidence admission over real retained-DA bytes.
    const block = await buildChallengedBlock();
    expect(block.withdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: block.observation,
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      },
    );
    expect(evidence.grade).toBe("security");
    expect(evidence.headerHash).toBe(block.headerHash);

    // ## 2. The proof plan the prover would submit.
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
      withdrawalCount: evidence.withdrawalCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: {
        kind: "present_event",
        observation: L1_OBSERVATION,
        withdrawalEventPolicyId: h28(0x19),
        observedEventAssetName: NONCE_AUTHENTIC_WITHDRAWAL_ID,
        eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
      },
    });
    expect(plan.threadTokenAssetName).toBe(
      `${SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID}${block.headerHash}`,
    );

    // ## 3. Every thread datum an emulator lifecycle places on chain.
    const step01Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: null },
      SDK.FabricatedWithdrawalStep01Datum,
    );
    const step01Handoff = await deriveFabricatedWithdrawalStep01Handoff({
      header: block.header,
      headerHash: block.headerHash,
      inclusion: parseSubmitFabricatedWithdrawalInclusion(
        plan.withdrawalInclusion,
      ),
    });
    const step02Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step01Handoff.step02State },
      SDK.FabricatedWithdrawalStep02Datum,
    );
    const step03State = SDK.fabricatedWithdrawalStep03State(
      step01Handoff.step02State,
      {
        WithdrawalEventObserved: {
          event_datum_hash: HASH_AUTHENTIC_WITHDRAWAL_EVENT_DATUM,
          event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
        },
      },
    );
    const step03Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03State },
      SDK.FabricatedWithdrawalStep03Datum,
    );
    const step03Handoff = await deriveFabricatedWithdrawalStep03Handoff({
      state: step03State,
      eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    });
    const step04Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03Handoff.step04State },
      SDK.FabricatedWithdrawalStep04Datum,
    );
    assertFabricatedWithdrawalStep04Finalizable({
      state: step03Handoff.step04State,
      fraudulentHeaderHash: block.headerHash,
    });
    expect(step01Handoff.step02State.committed_withdrawal_info_hash).toBe(
      HASH_DIVERTED_WITHDRAWAL_INFO,
    );
    expect(step03Handoff.step04State.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
        authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    for (const datum of [step01Datum, step02Datum, step03Datum, step04Datum]) {
      expect(datum).toMatch(/^[0-9a-f]+$/u);
    }
  }, 60_000);

  it("proves a fabricated withdrawal end-to-end, mints permanent evidence, and removes the fraudulent commitment", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      fabricatedWithdrawal,
      category,
    } = harness;

    const {
      counted,
      header,
      setup,
      eventInclusionTime,
      eventDatumCbor,
      observedEventAssetName,
      referenceScriptUtxos,
    } = await setupChallengedBlockOnEmulator(
      harness,
      VALUE_DIVERTED_WITHDRAWAL_INFO,
    );
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: proverLucid,
      contracts,
    });

    // ## The authentic L1 withdrawal event, minted under the hub-registered
    // withdrawal policy with the nonce asset name the committed identity
    // derives.
    const eventUnit = toUnit(
      contracts.withdrawal.policyId,
      observedEventAssetName,
    );
    const eventMintUnsigned = await funderLucid
      .newTx()
      .mintAssets({ [eventUnit]: 1n }, Data.void())
      .pay.ToContract(
        contracts.withdrawal.spendingScriptAddress,
        { kind: "inline", value: eventDatumCbor },
        { lovelace: 5_000_000n, [eventUnit]: 1n },
      )
      .attach.MintingPolicy(contracts.withdrawal.mintingScript)
      .complete({ localUPLCEval: true });
    const eventMintSigned = await eventMintUnsigned.sign
      .withWallet()
      .complete();
    await funderLucid.awaitTx(await eventMintSigned.submit());
    const eventUtxo = await expectSingleUtxoWithUnit(
      funderLucid,
      contracts.withdrawal.spendingScriptAddress,
      eventUnit,
    );

    // ## Evidence admission over the emulator block's retained-DA bytes.
    const base = await buildCanonicalBlockFixture({ transactions: [] });
    const payload: SDK.DaPayload = {
      ...base.payload,
      block_body: {
        ...base.payload.block_body,
        header,
        header_hash: headerHash,
        withdrawals: [
          [KEY_AUTHENTIC_WITHDRAWAL_ID, VALUE_DIVERTED_WITHDRAWAL_INFO],
        ],
        counts: {
          ...base.payload.block_body.counts,
          withdrawalCount: counted.count,
        },
      },
    };
    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: authenticatedHeaderObservation({
          ...base,
          header,
          headerHash,
        }),
        payloadEnvelopeCbor: await reencodeFixturePayload(payload),
        daProvenance: DA_PROVENANCE,
      },
    );
    expect(evidence.headerHash).toBe(headerHash);
    expect(evidence.committedWithdrawalsRoot).toBe(counted.root);

    // ## The proof plan, classified against the authentic event.
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
      withdrawalCount: evidence.withdrawalCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: {
        kind: "present_event",
        observation: L1_OBSERVATION,
        withdrawalEventPolicyId: contracts.withdrawal.policyId,
        observedEventAssetName,
        eventDatumCbor,
      },
    });
    expect(plan.threadTokenAssetName).toBe(
      `${SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID}${headerHash}`,
    );
    expect(plan.classification.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: HASH_DIVERTED_WITHDRAWAL_INFO,
        authentic_withdrawal_info_hash: HASH_AUTHENTIC_WITHDRAWAL_INFO,
        event_inclusion_time: eventInclusionTime,
      },
    });

    // ## init
    const initResult = await submitFabricatedFamilyInit({
      lucid: proverLucid,
      realBlueprint,
      contracts,
      catalogueRoot: catalogue.root,
      category,
      family: fabricatedWithdrawal,
      familyLabel: "fabricated-withdrawal",
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.computationThreadAssetName).toBe(
      plan.threadTokenAssetName,
    );
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(firstStepUtxo)).toBe(initResult.threadOutRef);
    expect(
      Data.from(firstStepUtxo.datum!, SDK.FabricatedWithdrawalStep01Datum),
    ).toEqual({ fraud_prover: proverSigner.paymentKeyHash, data: null });

    // ## step-01: bind the committed diverted leaf to the header
    const inclusion = parseSubmitFabricatedWithdrawalInclusion(
      plan.withdrawalInclusion,
    );
    const expectedHandoff = await deriveFabricatedWithdrawalStep01Handoff({
      header,
      headerHash,
      inclusion,
    });
    const step01Result = await submitFabricatedWithdrawalStep01({
      lucid: proverLucid,
      contracts: fabricatedWithdrawal,
      network,
      signer: proverSigner,
      threadOutRef: initResult.threadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      withdrawalInclusion: inclusion,
      referenceScriptUtxo: referenceScriptUtxos[0],
      awaitConfirmation: true,
    });
    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.committedWithdrawalInfoHash).toBe(
      HASH_DIVERTED_WITHDRAWAL_INFO,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(secondStepUtxo)).toBe(step01Result.nextThreadOutRef);
    // The handoff the L1 step-01 validator pinned is exactly the one the
    // off-chain rule derives from the committed bytes.
    expect(
      Data.from(secondStepUtxo.datum!, SDK.FabricatedWithdrawalStep02Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: expectedHandoff.step02State,
    });

    // ## step-02: authenticate the L1 withdrawal-event witness
    const step02Result = await submitFabricatedWithdrawalStep02({
      lucid: proverLucid,
      contracts: fabricatedWithdrawal,
      network,
      signer: proverSigner,
      threadOutRef: step01Result.nextThreadOutRef,
      evidence: { kind: "present_event", eventOutRef: outRefLabel(eventUtxo) },
      referenceScriptUtxo: referenceScriptUtxos[1],
      awaitConfirmation: true,
    });
    expect(step02Result.verdict).toEqual({
      WithdrawalEventObserved: {
        event_datum_hash: plan.classification.eventDatumHash!,
        event_inclusion_time: eventInclusionTime,
      },
    });
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(thirdStepUtxo)).toBe(step02Result.nextThreadOutRef);
    const step03State = SDK.fabricatedWithdrawalStep03State(
      expectedHandoff.step02State,
      step02Result.verdict,
    );
    expect(
      Data.from(thirdStepUtxo.datum!, SDK.FabricatedWithdrawalStep03Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: step03State,
    });

    // ## step-03: re-open the authenticated event datum and pin the fault
    const step03Result = await submitFabricatedWithdrawalStep03({
      lucid: proverLucid,
      contracts: fabricatedWithdrawal,
      signer: proverSigner,
      threadOutRef: step02Result.nextThreadOutRef,
      eventDatumCbor,
      referenceScriptUtxo: referenceScriptUtxos[2],
      awaitConfirmation: true,
    });
    expect(step03Result.fault).toEqual(plan.classification.fault);
    await expect(
      proverLucid.utxosAtWithUnit(
        step02Result.thirdStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(fourthStepUtxo)).toBe(step03Result.nextThreadOutRef);
    const step03Handoff = await deriveFabricatedWithdrawalStep03Handoff({
      state: step03State,
      eventDatumCbor,
    });
    expect(
      Data.from(fourthStepUtxo.datum!, SDK.FabricatedWithdrawalStep04Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: step03Handoff.step04State,
    });

    // ## step-04: adjudicate and mint the permanent fraud-proof token
    const step04Result = await submitFabricatedWithdrawalStep04({
      lucid: proverLucid,
      contracts: fabricatedWithdrawal,
      signer: proverSigner,
      threadOutRef: step03Result.nextThreadOutRef,
      referenceScriptUtxo: referenceScriptUtxos[3],
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    expect(step04Result.fault).toEqual(plan.classification.fault);
    expect(step04Result.fraudProofAssetName).toBe(plan.threadTokenAssetName);
    // The computation thread is burned; the fraud-proof token is permanent.
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
    expect(outRefLabel(fraudProofUtxo)).toBe(step04Result.fraudProofOutRef);
    expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
    expect(Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum)).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
    });

    // ## removal: consume the convicted state-queue node while retaining the
    // permanent proof token at its original out-ref.
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const removeNow = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "fabricatedWithdrawal",
      fraudulentHeaderHash: headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("fabricatedWithdrawal");
    expect(removal.transactions).toHaveLength(1);
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(step04Result.fraudProofOutRef);
  }, 240_000);

  it("cannot advance a fabricated-withdrawal thread against a valid block", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      fabricatedWithdrawal,
      category,
    } = harness;

    // An honest block: the committed leaf's content IS the authentic event's.
    const authenticInfoCbor = SDK.committedWithdrawalValueBytes(
      Data.from(DATUM_AUTHENTIC_WITHDRAWAL_EVENT, SDK.WithdrawalOrderDatum)
        .event.info,
    );
    const {
      counted,
      header,
      setup,
      eventDatumCbor,
      observedEventAssetName,
      referenceScriptUtxos,
    } = await setupChallengedBlockOnEmulator(harness, authenticInfoCbor);

    const initResult = await submitFabricatedFamilyInit({
      lucid: proverLucid,
      realBlueprint,
      contracts,
      catalogueRoot: catalogue.root,
      category,
      family: fabricatedWithdrawal,
      familyLabel: "fabricated-withdrawal",
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 1 — off-chain fail-closed: the committed content hash equals the
    // authentic event's, so the classifier refuses to build a plan at all.
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: setup.headerHash,
        committedWithdrawalsRoot: counted.root,
        withdrawalCount: counted.count,
        headerStartTime: header.startTime,
        headerEndTime: header.endTime,
        entries: [[KEY_AUTHENTIC_WITHDRAWAL_ID, authenticInfoCbor]],
        witness: {
          kind: "present_event",
          observation: L1_OBSERVATION,
          withdrawalEventPolicyId: contracts.withdrawal.policyId,
          observedEventAssetName,
          eventDatumCbor,
        },
      }),
    ).rejects.toThrow(/authentic_content_matches_commitment/u);

    // Plane 2 — on-chain: substituting diverted content for the honest leaf
    // passes the local counted-root equality (root and count are the header's
    // own), but the L1 membership proof cannot open the committed root over a
    // value the block never committed. The inline MPF verification in step-01's
    // spend handler is what refuses it.
    const honestProof = await keyValuePhasProof(
      { ...counted, root: counted.phasRoot },
      Buffer.from(KEY_AUTHENTIC_WITHDRAWAL_ID, "hex"),
      Buffer.from(authenticInfoCbor, "hex"),
    );
    const divertedInclusion = parseSubmitFabricatedWithdrawalInclusion({
      committedWithdrawalIdCbor: KEY_AUTHENTIC_WITHDRAWAL_ID,
      committedWithdrawalInfoCbor: VALUE_DIVERTED_WITHDRAWAL_INFO,
      withdrawalsPhasRoot: counted.phasRoot,
      withdrawalMembershipProofCbor: Data.to(honestProof, SDK.Proof),
    });
    await expect(
      submitFabricatedWithdrawalStep01({
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        withdrawalInclusion: divertedInclusion,
        referenceScriptUtxo: referenceScriptUtxos[0],
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/failed script execution.*Spend/su);

    // The thread is untouched: no step-02 output exists and the valid block is
    // still in the state queue.
    const stillFirstStep = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(stillFirstStep)).toBe(outRefLabel(firstStepUtxo));
    await expect(
      proverLucid.utxosAtWithUnit(
        fabricatedWithdrawal.steps[1].spendingScriptAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 240_000);
});
