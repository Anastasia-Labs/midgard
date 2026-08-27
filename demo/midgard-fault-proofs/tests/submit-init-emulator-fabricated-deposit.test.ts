/**
 * `fabricated-deposit` emulator lifecycle (Goal task `Q39`, §9.1 output 9).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters: init -> step-01 -> step-02 -> step-03 -> step-04 ->
 * permanent fraud-proof token, plus the valid-block negative on both planes
 * (off-chain fail-closed and on-chain membership rejection).
 *
 * The committed evidence is a `deposits_root` leaf that pairs the authentic
 * deposit identity with **diverted** content — exactly the fault the family
 * adjudicates (Q39's `MismatchedDepositContent` shape). The authentic L1
 * deposit event is exhibited as a reference input carrying the hub-registered
 * deposit event NFT, so step-02's verdict rests on authenticated material.
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

import {
  fabricatedDepositBlockEvidenceFromVerifiedPayloadV1,
  prepareFabricatedDepositFromCommittedLeavesV1,
} from "../src/prepare-fabricated-deposit.js";
import {
  deriveFabricatedDepositStep01HandoffV1,
  parseSubmitFabricatedDepositInclusion,
  submitFabricatedDepositStep01,
} from "../src/submit-fabricated-deposit-step-01.js";
import { submitFabricatedDepositStep02 } from "../src/submit-fabricated-deposit-step-02.js";
import {
  deriveFabricatedDepositStep03HandoffV1,
  submitFabricatedDepositStep03,
} from "../src/submit-fabricated-deposit-step-03.js";
import {
  assertFabricatedDepositStep04FinalizableV1,
  submitFabricatedDepositStep04,
} from "../src/submit-fabricated-deposit-step-04.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  h28,
  outRefCbor,
  reencodeFixturePayloadV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  submitFabricatedFamilyInitV1,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

// The `mismatched_content_block_v1` scenario, measured out of
// `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2}.ak`:
// the authentic deposit identity committed with diverted content, and the
// authentic event datum whose inclusion time falls inside the block's window.
const KEY_AUTHENTIC_DEPOSIT_ID =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";
const VALUE_DIVERTED_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";
const DATUM_AUTHENTIC_DEPOSIT_EVENT =
  "d8799fd8799fd8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ffff0f581c57575757575757575757575757575757575757575757575757575757ff";
const HASH_AUTHENTIC_DEPOSIT_EVENT_DATUM =
  "2538e7986f6a3468a1dd016318a82d3dd4f60d55f6e688e164dd35564c4a85b4";
const HASH_AUTHENTIC_DEPOSIT_INFO =
  "89ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863";
const HASH_DIVERTED_DEPOSIT_INFO =
  "0ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335";
const MM_DEPOSITS_ROOT =
  "880ba7ceb072fce058c5e8f9adbbe9b5bcc3efdcb53ec82039f142f577c47ab4";
const AUTHENTIC_INCLUSION_TIME = 15n;
const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;

/** Stands in for the emulator prover's payment key hash. */
const FRAUD_PROVER = h28(0x77);

const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

const L1_OBSERVATION: SDK.AuthenticatedL1ObservationV1 = {
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-local-node",
    grade: "security",
  },
  chainPoint: { slot: 4242n, blockHash: "09".repeat(32) },
  confirmationDepth: 12,
};

/** Commits the challenged block's single fabricated deposit leaf. */
const buildChallengedBlockV1 = async () => {
  const base = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    startTime: HEADER_START_TIME,
    endTime: HEADER_END_TIME,
    transactionsRootMode: "nativeCompact",
  });
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.deposits, [
    {
      key: Buffer.from(KEY_AUTHENTIC_DEPOSIT_ID, "hex"),
      value: Buffer.from(VALUE_DIVERTED_DEPOSIT_INFO, "hex"),
    },
  ]);
  const header: SDK.HeaderV1 = {
    ...base.header,
    depositsRoot: counted.root,
    depositCount: counted.count,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const deposits: SDK.DaPayloadEntry[] = [
    [KEY_AUTHENTIC_DEPOSIT_ID, VALUE_DIVERTED_DEPOSIT_INFO],
  ];
  const payload: SDK.DaPayloadV1 = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      deposits,
      counts: {
        ...base.payload.block_body.counts,
        depositCount: counted.count,
      },
    },
  };
  return {
    header,
    headerHash,
    depositsRoot: counted.root,
    payloadEnvelopeCbor: await reencodeFixturePayloadV1(payload),
    observation: authenticatedHeaderObservationV1({
      ...base,
      header,
      headerHash,
    }),
  };
};

/**
 * The harness every emulator scenario in this file opens with: the real
 * fabricated-deposit chain built from the regenerated blueprint and registered
 * in the canonical production catalogue.
 */
const makeEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realFabricatedDeposit: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const fabricatedDeposit = harness.contracts.fabricatedDeposit;
  const category = harness.catalogue.categories.fabricatedDeposit;
  if (fabricatedDeposit === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the fabricated-deposit contracts/category",
    );
  }
  expect(category.categoryId).toBe(SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1);
  expect(category.scriptHash).toBe(
    fabricatedDeposit.steps[0].spendingScriptHash,
  );
  return { ...harness, fabricatedDeposit, category };
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
    fabricatedDeposit,
  } = harness;
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.deposits, [
    {
      key: Buffer.from(KEY_AUTHENTIC_DEPOSIT_ID, "hex"),
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
  // whenever that count is non-zero. Reusing the deposits counted root keeps
  // the header committable without touching validation traces (no forced or
  // L2 transactions in this block).
  const header: SDK.HeaderV1 = {
    ...makeHeader(funderKeyHash, headerStartTime),
    depositsRoot: counted.root,
    depositCount: counted.count,
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
      script: fabricatedDeposit.steps[0].spendingScript,
      label: "fabricated-deposit step-01",
    })
  ).utxo;
  const step02ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedDeposit.steps[1].spendingScript,
      label: "fabricated-deposit step-02",
    })
  ).utxo;
  const step03ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedDeposit.steps[2].spendingScript,
      label: "fabricated-deposit step-03",
    })
  ).utxo;
  const step04ReferenceScriptUtxo = (
    await publishPlainReferenceScriptUtxo({
      lucid: funderLucid,
      script: fabricatedDeposit.steps[3].spendingScript,
      label: "fabricated-deposit step-04",
    })
  ).utxo;
  // The authentic event's inclusion time, inside this header's establishment
  // window `(start_time, end_time]`.
  const eventInclusionTime = header.startTime + 500n;
  const authenticEventDatum: SDK.DepositDatum = {
    ...Data.from(DATUM_AUTHENTIC_DEPOSIT_EVENT, SDK.DepositDatum),
    inclusion_time: eventInclusionTime,
  };
  const eventDatumCbor = Data.to(authenticEventDatum, SDK.DepositDatum);
  const observedEventAssetName = await Effect.runPromise(
    SDK.depositEventNonceV1(authenticEventDatum.event.id),
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

describe("fabricated-deposit fault-proof emulator lifecycle", () => {
  it("admits retained-DA evidence and derives every thread handoff off-chain", async () => {
    // ## 1. Evidence admission over real retained-DA bytes.
    const block = await buildChallengedBlockV1();
    expect(block.depositsRoot).toBe(MM_DEPOSITS_ROOT);
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayloadV1({
      observation: block.observation,
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.headerHash).toBe(block.headerHash);

    // ## 2. The proof plan the prover would submit.
    const plan = await prepareFabricatedDepositFromCommittedLeavesV1({
      headerHash: evidence.headerHash,
      committedDepositsRoot: evidence.committedDepositsRoot,
      depositCount: evidence.depositCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: {
        kind: "present_event",
        observation: L1_OBSERVATION,
        depositEventPolicyId: h28(0x18),
        observedEventAssetName:
          "db496846395df718772b56f398cc7c7882869ddc0154fd035d63da1c3e95dd06",
        eventDatumCbor: DATUM_AUTHENTIC_DEPOSIT_EVENT,
      },
    });
    expect(plan.threadTokenAssetName).toBe(
      `${SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1}${block.headerHash}`,
    );

    // ## 3. Every thread datum an emulator lifecycle places on chain.
    const step01Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: null },
      SDK.FabricatedDepositStep01Datum,
    );
    const step01Handoff = await deriveFabricatedDepositStep01HandoffV1({
      header: block.header,
      headerHash: block.headerHash,
      inclusion: parseSubmitFabricatedDepositInclusion(plan.depositInclusion),
    });
    const step02Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step01Handoff.step02State },
      SDK.FabricatedDepositStep02Datum,
    );
    const step03State = SDK.fabricatedDepositStep03StateV1(
      step01Handoff.step02State,
      {
        DepositEventObserved: {
          event_datum_hash: HASH_AUTHENTIC_DEPOSIT_EVENT_DATUM,
          event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
        },
      },
    );
    const step03Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03State },
      SDK.FabricatedDepositStep03Datum,
    );
    const step03Handoff = await deriveFabricatedDepositStep03HandoffV1({
      state: step03State,
      eventDatumCbor: DATUM_AUTHENTIC_DEPOSIT_EVENT,
    });
    const step04Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03Handoff.step04State },
      SDK.FabricatedDepositStep04Datum,
    );
    assertFabricatedDepositStep04FinalizableV1({
      state: step03Handoff.step04State,
      fraudulentHeaderHash: block.headerHash,
    });
    expect(step01Handoff.step02State.committed_deposit_info_hash).toBe(
      HASH_DIVERTED_DEPOSIT_INFO,
    );
    expect(step03Handoff.step04State.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    for (const datum of [step01Datum, step02Datum, step03Datum, step04Datum]) {
      expect(datum).toMatch(/^[0-9a-f]+$/u);
    }
  }, 60_000);

  it("proves a fabricated deposit end-to-end and mints the permanent fraud-proof token", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      fabricatedDeposit,
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
      VALUE_DIVERTED_DEPOSIT_INFO,
    );
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    // ## The authentic L1 deposit event, minted under the hub-registered
    // deposit policy with the nonce asset name the committed identity derives.
    const eventUnit = toUnit(
      contracts.deposit.policyId,
      observedEventAssetName,
    );
    const eventMintUnsigned = await funderLucid
      .newTx()
      .mintAssets({ [eventUnit]: 1n }, Data.void())
      .pay.ToContract(
        contracts.deposit.spendingScriptAddress,
        { kind: "inline", value: eventDatumCbor },
        { lovelace: 5_000_000n, [eventUnit]: 1n },
      )
      .attach.MintingPolicy(contracts.deposit.mintingScript)
      .complete({ localUPLCEval: true });
    const eventMintSigned = await eventMintUnsigned.sign
      .withWallet()
      .complete();
    await funderLucid.awaitTx(await eventMintSigned.submit());
    const eventUtxo = await expectSingleUtxoWithUnit(
      funderLucid,
      contracts.deposit.spendingScriptAddress,
      eventUnit,
    );

    // ## Evidence admission over the emulator block's retained-DA bytes.
    const base = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const payload: SDK.DaPayloadV1 = {
      ...base.payload,
      block_body: {
        ...base.payload.block_body,
        header,
        header_hash: headerHash,
        deposits: [[KEY_AUTHENTIC_DEPOSIT_ID, VALUE_DIVERTED_DEPOSIT_INFO]],
        counts: {
          ...base.payload.block_body.counts,
          depositCount: counted.count,
        },
      },
    };
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1({
        ...base,
        header,
        headerHash,
      }),
      payloadEnvelopeCbor: await reencodeFixturePayloadV1(payload),
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.headerHash).toBe(headerHash);
    expect(evidence.committedDepositsRoot).toBe(counted.root);

    // ## The proof plan, classified against the authentic event.
    const plan = await prepareFabricatedDepositFromCommittedLeavesV1({
      headerHash: evidence.headerHash,
      committedDepositsRoot: evidence.committedDepositsRoot,
      depositCount: evidence.depositCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: {
        kind: "present_event",
        observation: L1_OBSERVATION,
        depositEventPolicyId: contracts.deposit.policyId,
        observedEventAssetName,
        eventDatumCbor,
      },
    });
    expect(plan.threadTokenAssetName).toBe(
      `${SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1}${headerHash}`,
    );
    expect(plan.classification.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: eventInclusionTime,
      },
    });

    // ## init
    const initResult = await submitFabricatedFamilyInitV1({
      lucid: proverLucid,
      realBlueprint,
      contracts,
      catalogueRoot: catalogue.root,
      category,
      family: fabricatedDeposit,
      familyLabel: "fabricated-deposit",
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
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
      Data.from(firstStepUtxo.datum!, SDK.FabricatedDepositStep01Datum),
    ).toEqual({ fraud_prover: proverSigner.paymentKeyHash, data: null });

    // ## step-01: bind the committed diverted leaf to the header
    const inclusion = parseSubmitFabricatedDepositInclusion(
      plan.depositInclusion,
    );
    const expectedHandoff = await deriveFabricatedDepositStep01HandoffV1({
      header,
      headerHash,
      inclusion,
    });
    const step01Result = await submitFabricatedDepositStep01({
      lucid: proverLucid,
      contracts: fabricatedDeposit,
      network,
      signer: proverSigner,
      threadOutRef: initResult.threadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      depositInclusion: inclusion,
      referenceScriptUtxo: referenceScriptUtxos[0],
      awaitConfirmation: true,
    });
    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.committedDepositInfoHash).toBe(
      HASH_DIVERTED_DEPOSIT_INFO,
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
      Data.from(secondStepUtxo.datum!, SDK.FabricatedDepositStep02Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: expectedHandoff.step02State,
    });

    // ## step-02: authenticate the L1 deposit-event witness
    const step02Result = await submitFabricatedDepositStep02({
      lucid: proverLucid,
      contracts: fabricatedDeposit,
      network,
      signer: proverSigner,
      threadOutRef: step01Result.nextThreadOutRef,
      evidence: { kind: "present_event", eventOutRef: outRefLabel(eventUtxo) },
      referenceScriptUtxo: referenceScriptUtxos[1],
      awaitConfirmation: true,
    });
    expect(step02Result.verdict).toEqual({
      DepositEventObserved: {
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
    const step03State = SDK.fabricatedDepositStep03StateV1(
      expectedHandoff.step02State,
      step02Result.verdict,
    );
    expect(
      Data.from(thirdStepUtxo.datum!, SDK.FabricatedDepositStep03Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: step03State,
    });

    // ## step-03: re-open the authenticated event datum and pin the fault
    const step03Result = await submitFabricatedDepositStep03({
      lucid: proverLucid,
      contracts: fabricatedDeposit,
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
    const step03Handoff = await deriveFabricatedDepositStep03HandoffV1({
      state: step03State,
      eventDatumCbor,
    });
    expect(
      Data.from(fourthStepUtxo.datum!, SDK.FabricatedDepositStep04Datum),
    ).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: step03Handoff.step04State,
    });

    // ## step-04: adjudicate and mint the permanent fraud-proof token
    const step04Result = await submitFabricatedDepositStep04({
      lucid: proverLucid,
      contracts: fabricatedDeposit,
      signer: proverSigner,
      threadOutRef: step03Result.nextThreadOutRef,
      referenceScriptUtxo: referenceScriptUtxos[3],
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
  }, 240_000);

  it("cannot advance a fabricated-deposit thread against a valid block", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      fabricatedDeposit,
      category,
    } = harness;

    // An honest block: the committed leaf's content IS the authentic event's.
    const authenticInfoCbor = SDK.committedDepositValueBytesV1(
      Data.from(DATUM_AUTHENTIC_DEPOSIT_EVENT, SDK.DepositDatum).event.info,
    );
    const {
      counted,
      header,
      setup,
      eventDatumCbor,
      observedEventAssetName,
      referenceScriptUtxos,
    } = await setupChallengedBlockOnEmulator(harness, authenticInfoCbor);

    const initResult = await submitFabricatedFamilyInitV1({
      lucid: proverLucid,
      realBlueprint,
      contracts,
      catalogueRoot: catalogue.root,
      category,
      family: fabricatedDeposit,
      familyLabel: "fabricated-deposit",
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 1 — off-chain fail-closed: the committed content hash equals the
    // authentic event's, so the classifier refuses to build a plan at all.
    await expect(
      prepareFabricatedDepositFromCommittedLeavesV1({
        headerHash: setup.headerHash,
        committedDepositsRoot: counted.root,
        depositCount: counted.count,
        headerStartTime: header.startTime,
        headerEndTime: header.endTime,
        entries: [[KEY_AUTHENTIC_DEPOSIT_ID, authenticInfoCbor]],
        witness: {
          kind: "present_event",
          observation: L1_OBSERVATION,
          depositEventPolicyId: contracts.deposit.policyId,
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
      Buffer.from(KEY_AUTHENTIC_DEPOSIT_ID, "hex"),
      Buffer.from(authenticInfoCbor, "hex"),
    );
    const divertedInclusion = parseSubmitFabricatedDepositInclusion({
      committedDepositIdCbor: KEY_AUTHENTIC_DEPOSIT_ID,
      committedDepositInfoCbor: VALUE_DIVERTED_DEPOSIT_INFO,
      depositsPhasRoot: counted.phasRoot,
      depositMembershipProofCbor: Data.to(honestProof, SDK.Proof),
    });
    await expect(
      submitFabricatedDepositStep01({
        lucid: proverLucid,
        contracts: fabricatedDeposit,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        depositInclusion: divertedInclusion,
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
        fabricatedDeposit.steps[1].spendingScriptAddress,
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
