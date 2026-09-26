import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

/**
 * Complete fabricated-withdrawal proof fixture: real list initialization and
 * nonce admission, retained-DA preparation, catalogue-member CT initialization,
 * production stages 01-04, atomic queue marking, and fraud removal. Both inline
 * and separately prepublished external payloads are exercised. Content-matching
 * eligible events refuse this family off chain, cannot open a substituted
 * committed leaf, and cannot advance the authentic stage03 thread on chain.
 * Unrelated L2 validity is outside this fixture.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import { fabricatedProofValidity } from "../src/fabricated-proof-validity.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../src/prepare-fabricated-withdrawal.js";
import { selectFeeInput } from "../src/step-support.js";
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
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h28,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import { historyWitnessFixture } from "./helpers/history-witness-fixture.js";
import {
  prepareFamilyHistory,
  recordFamilyTransaction,
} from "./support/emulator/family-history.js";
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

const historyRecords: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  const blueprint = readFileSync(
    new URL("../../../onchain/aiken/plutus.json", import.meta.url),
  );
  writeFileSync(
    join(directory, "full-withdrawal-history.json"),
    JSON.stringify(
      {
        scope:
          "Real history admission, catalogue-member CT initialization adapter, production stages 01-04 and fraud removal; fixture catalogue governance, not live acceptance",
        blueprintSha256: createHash("sha256").update(blueprint).digest("hex"),
        records: historyRecords,
      },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

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
const HASH_AUTHENTIC_WITHDRAWAL_CONTENT =
  "283ad237b5850498ff2cc5e4c2017d6129a3d955265f5e3a889387776716d12f";
const HASH_DIVERTED_WITHDRAWAL_CONTENT =
  "8e8f341a7ae7b42e43bf1b09b3e63c742979eb2ada627a417d7af04be1dbe2a3";
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
  const history = await prepareFamilyHistory(harness, historyRecords);
  return {
    ...harness,
    contracts: history.contracts,
    history,
    fabricatedWithdrawal,
    category,
  };
};

/**
 * The challenged block committed on the emulator: the diverted leaf's counted
 * root under the funder-operated header whose window opens at the aligned
 * emulator clock. Returns everything the prover-side scenarios consume.
 */
const fundedWithdrawalInfo = (
  info: SDK.WithdrawalInfo,
): SDK.WithdrawalInfo => ({
  ...info,
  body: {
    ...info.body,
    l2_value: new Map([
      ...info.body.l2_value,
      ["", new Map([["", 50_000_000n]])],
    ]),
  },
});
const fundedWithdrawalValue = (cbor: string) =>
  SDK.committedWithdrawalValueBytes(
    fundedWithdrawalInfo(Data.from(cbor, SDK.WithdrawalInfo)),
  );

const setupChallengedBlockOnEmulator = async (
  harness: Awaited<ReturnType<typeof makeEmulatorHarness>>,
  committedInfoCbor: string,
  mode: "inline" | "external" = "inline",
  absent = false,
) => {
  const {
    emulator,
    funderLucid,
    contracts,
    catalogue,
    nonceUtxo,
    fabricatedWithdrawal,
  } = harness;
  const nonce = harness.history.nonce("Withdrawal");
  const eventId = {
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  };
  // The absent case commits an arbitrary identity that was never admitted;
  // the distinct live neighbor still exercises a nonempty authenticated list.
  const committedId = absent
    ? { transactionId: "fe".repeat(32), outputIndex: 65535n }
    : eventId;
  if (absent) expect(committedId).not.toEqual(eventId);
  const keyCbor = SDK.committedWithdrawalKeyBytes(committedId);
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    {
      key: Buffer.from(keyCbor, "hex"),
      value: Buffer.from(committedInfoCbor, "hex"),
    },
  ]);
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 240_000) -
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
  const legacy = Data.from(
    DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    SDK.WithdrawalOrderDatum,
  );
  const authenticEvent = {
    ...legacy.event,
    id: eventId,
    info: fundedWithdrawalInfo(legacy.event.info),
  };
  if (mode === "external")
    authenticEvent.info.body.l1_datum = {
      InlineDatum: { data: "cd".repeat(1600) },
    };
  let admitted: Awaited<ReturnType<typeof harness.history.admit>> | undefined;
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
    beforeHeaderCommit: async (hub) => {
      admitted = await harness.history.admit(
        hub,
        {
          WithdrawalPayload: {
            event: authenticEvent,
            refund_address: legacy.refund_address,
            refund_datum: legacy.refund_datum,
          },
        },
        header,
      );
    },
  });
  if (admitted === undefined) throw new Error("History admission did not run");
  const applied = harness.history.applied[1]!;
  const current = await SDK.fetchEventHistoryWitness(
    { utxosAt: (address) => funderLucid.utxosAt(address) },
    {
      policyId: applied.policyId,
      address: applied.address,
      retentionAddress: applied.retention.address,
      inlineLimitBytes: harness.history.recipes[1]!.inlineLimitBytes,
    },
    committedId,
  );
  expect(current.kind).toBe(absent ? "Absent" : "Present");
  const raw = absent
    ? {
        ...admitted.raw,
        anchor: current.anchor.utxo,
        retainedDataUtxo: undefined,
      }
    : admitted.raw;

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
  const eventInclusionTime = admitted.captured.commitment.inclusion_time;
  expect(eventInclusionTime).toBe(header.endTime);
  return {
    counted,
    header,
    setup,
    eventInclusionTime,
    keyCbor,
    admitted,
    raw,
    eventUtxo: admitted.witness.anchor.utxo,
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
    const legacy = Data.from(
      DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
      SDK.WithdrawalOrderDatum,
    );
    expect(
      await Effect.runPromise(SDK.withdrawalEventNonce(legacy.event.id)),
    ).toBe(NONCE_AUTHENTIC_WITHDRAWAL_ID);
    const witness = await historyWitnessFixture(
      {
        WithdrawalPayload: {
          event: legacy.event,
          refund_address: legacy.refund_address,
          refund_datum: legacy.refund_datum,
        },
      },
      AUTHENTIC_INCLUSION_TIME,
      L1_OBSERVATION,
    );
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
      withdrawalCount: evidence.withdrawalCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness,
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
      stateQueuePolicyId: "15".repeat(28),
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
      plan.classification.verdict,
    );
    const step03Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03State },
      SDK.FabricatedWithdrawalStep03Datum,
    );
    const step03Handoff = await deriveFabricatedWithdrawalStep03Handoff({
      state: step03State,
      openingCbor: plan.classification.openingCbor!,
    });
    const step04Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03Handoff.step04State },
      SDK.FabricatedWithdrawalStep04Datum,
    );
    assertFabricatedWithdrawalStep04Finalizable({
      state: step03Handoff.step04State,
      fraudulentHeaderHash: block.headerHash,
    });
    expect(step01Handoff.step02State.committed_withdrawal_content_hash).toBe(
      HASH_DIVERTED_WITHDRAWAL_CONTENT,
    );
    expect(step03Handoff.step04State.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    for (const datum of [step01Datum, step02Datum, step03Datum, step04Datum]) {
      expect(datum).toMatch(/^[0-9a-f]+$/u);
    }
  }, 60_000);

  it.each(
    (["inline", "external"] as const).flatMap((mode) =>
      (["diverted-content", "absent-identity", "signature-only"] as const).map(
        (scenario) => ({ mode, scenario }),
      ),
    ),
  )(
    "proves $scenario withdrawal with $mode history, mints permanent evidence, and removes the fraudulent commitment",
    async ({ mode, scenario }) => {
      const absent = scenario === "absent-identity";
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

      const authenticInfo = fundedWithdrawalInfo(
        Data.from(DATUM_AUTHENTIC_WITHDRAWAL_EVENT, SDK.WithdrawalOrderDatum)
          .event.info,
      );
      if (mode === "external")
        authenticInfo.body.l1_datum = {
          InlineDatum: { data: "cd".repeat(1600) },
        };
      const signatureSubstitution: SDK.WithdrawalInfo = {
        ...authenticInfo,
        signature: [authenticInfo.signature[0], "ee".repeat(64)],
      };
      const committedInfoCbor =
        scenario === "signature-only"
          ? SDK.committedWithdrawalValueBytes(signatureSubstitution)
          : fundedWithdrawalValue(VALUE_DIVERTED_WITHDRAWAL_INFO);
      const committedContentHash = await Effect.runPromise(
        SDK.withdrawalContentCommitment(
          Data.from(committedInfoCbor, SDK.WithdrawalInfo),
        ),
      );
      const {
        counted,
        header,
        setup,
        eventInclusionTime,
        keyCbor,
        admitted,
        raw,
        eventUtxo,
        referenceScriptUtxos,
      } = await setupChallengedBlockOnEmulator(
        harness,
        committedInfoCbor,
        mode,
        absent,
      );
      if (!("WithdrawalPayload" in admitted.captured.payload))
        throw new Error("Wrong payload kind");
      if (scenario === "signature-only") {
        const committed = Data.from(committedInfoCbor, SDK.WithdrawalInfo);
        const actual = admitted.captured.payload.WithdrawalPayload.event.info;
        expect(Data.to(committed.body, SDK.WithdrawalBody)).toBe(
          Data.to(actual.body, SDK.WithdrawalBody),
        );
        expect(committed.validity).toEqual(actual.validity);
        expect(committed.signature[0]).toBe(actual.signature[0]);
        expect(committed.signature[1]).not.toBe(actual.signature[1]);
      }
      const authenticContentHash = await Effect.runPromise(
        SDK.withdrawalContentCommitment(
          admitted.captured.payload.WithdrawalPayload.event.info,
        ),
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

      // ## Evidence admission over the emulator block's retained-DA bytes.
      const base = await buildCanonicalBlockFixture({ transactions: [] });
      const payload: SDK.DaPayload = {
        ...base.payload,
        block_body: {
          ...base.payload.block_body,
          header,
          header_hash: headerHash,
          withdrawals: [[keyCbor, committedInfoCbor]],
          counts: {
            ...base.payload.block_body.counts,
            withdrawalCount: counted.count,
          },
        },
      };
      const evidence =
        await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation({
            ...base,
            header,
            headerHash,
          }),
          payloadEnvelopeCbor: await reencodeFixturePayload(payload),
          daProvenance: DA_PROVENANCE,
        });
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
        witness: { ...raw, observation: L1_OBSERVATION },
      });
      expect(plan.threadTokenAssetName).toBe(
        `${SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID}${headerHash}`,
      );
      expect(plan.classification.fault).toEqual(
        absent
          ? "NonexistentWithdrawalIdentity"
          : {
              MismatchedWithdrawalContent: {
                committed_withdrawal_content_hash: committedContentHash,
                authentic_withdrawal_content_hash: authenticContentHash,
                event_inclusion_time: eventInclusionTime,
              },
            },
      );

      // ## init
      const initResult = await submitFabricatedFamilyInit({
        onSigned: (signed) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-init-${mode}-${scenario}`,
            signed,
          ),
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
        stateQueuePolicyId: contracts.stateQueue.policyId,
        header,
        headerHash,
        inclusion,
      });
      const step01Result = await submitFabricatedWithdrawalStep01({
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-step-01-${mode}-${scenario}`,
            signed,
          ),
        now: () => harness.emulator.now(),
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
      expect(step01Result.committedWithdrawalContentHash).toBe(
        committedContentHash,
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
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-step-02-${mode}-${scenario}`,
            signed,
          ),
        now: () => harness.emulator.now(),
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        network,
        signer: proverSigner,
        threadOutRef: step01Result.nextThreadOutRef,
        evidence: absent
          ? { kind: "absent_identity" }
          : { kind: "present_event", eventOutRef: outRefLabel(eventUtxo) },
        referenceScriptUtxo: referenceScriptUtxos[1],
        awaitConfirmation: true,
      });
      expect(step02Result.verdict).toEqual(
        absent
          ? "WithdrawalIdentityAbsent"
          : {
              WithdrawalEventObserved: {
                commitment: admitted.captured.commitment,
              },
            },
      );
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
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-step-03-${mode}-${scenario}`,
            signed,
          ),
        now: () => harness.emulator.now(),
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        signer: proverSigner,
        threadOutRef: step02Result.nextThreadOutRef,
        openingCbor: absent ? undefined : admitted.openingCbor,
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
        openingCbor: absent ? undefined : admitted.openingCbor,
      });
      expect(
        Data.from(fourthStepUtxo.datum!, SDK.FabricatedWithdrawalStep04Datum),
      ).toEqual({
        fraud_prover: proverSigner.paymentKeyHash,
        data: step03Handoff.step04State,
      });

      // ## step-04: adjudicate and mint the permanent fraud-proof token
      const step04Result = await submitFabricatedWithdrawalStep04({
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-step-04-${mode}-${scenario}`,
            signed,
          ),
        now: () => harness.emulator.now(),
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        signer: proverSigner,
        threadOutRef: step03Result.nextThreadOutRef,
        referenceScriptUtxo: referenceScriptUtxos[3],
        witnessReferenceScripts: {
          ...harness.witnessReferenceScripts,
          stateQueueSpend: removalReferences.published.stateQueueSpend,
        },
        awaitConfirmation: true,
      });
      expect(step04Result.fault).toEqual(plan.classification.fault);
      expect(step04Result.fraudProofAssetName).toBe(plan.threadTokenAssetName);
      const markedQueue = await proverLucid.utxoByUnit(
        setup.stateQueueBlockUnit,
      );
      expect(markedQueue.txHash).toBe(step04Result.txHash);
      expect(outRefLabel(markedQueue)).not.toBe(setup.fraudulentBlockOutRef);
      const markedView = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(markedQueue),
      );
      expect(
        Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(markedView))
          .proven_fraud,
      ).toBe(plan.threadTokenAssetName);
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
      expect(
        Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
      ).toEqual({
        fraud_prover: proverSigner.paymentKeyHash,
      });

      // ## removal: consume the convicted state-queue node while retaining the
      // permanent proof token at its original out-ref.
      const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
        removalReferenceScripts: removalReferences.published,
      });
      const removeNow = BigInt(harness.emulator.now());
      const removal = await submitRemoveFraudulentBlock({
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-removal-${mode}-${scenario}`,
            signed,
          ),
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
      expect(outRefLabel(retainedFraudProof)).toBe(
        step04Result.fraudProofOutRef,
      );
      // Adjudicating an unrelated committed identity/content must not consume
      // or rewrite the authentic neighbor or its original locked Value.
      expect(
        await proverLucid.utxosByOutRef([
          {
            txHash: admitted.witness.anchor.utxo.txHash,
            outputIndex: admitted.witness.anchor.utxo.outputIndex,
          },
        ]),
      ).toEqual([admitted.witness.anchor.utxo]);
      if (admitted.witness.retainedDataUtxo !== undefined) {
        const retained = admitted.witness.retainedDataUtxo;
        expect(
          await proverLucid.utxosByOutRef([
            {
              txHash: retained.txHash,
              outputIndex: retained.outputIndex,
            },
          ]),
        ).toEqual([retained]);
      }
      historyRecords.push({
        label: "completed-family-scenario",
        kind: "Withdrawal",
        mode,
        scenario,
        headerHash,
        committedIdCbor: keyCbor,
        authenticCommitment: admitted.captured.commitment,
        originalAssetsCbor: Data.to(
          admitted.captured.originalAssets,
          SDK.Value,
        ),
        authenticOrder: admitted.witness.anchor.utxo,
        retainedData: admitted.witness.retainedDataUtxo,
        fault: step04Result.fault,
        permanentProofOutRef: step04Result.fraudProofOutRef,
        removal,
      });
    },
    240_000,
  );

  it.each(["inline", "external"] as const)(
    "cannot advance a fabricated-withdrawal thread against a valid %s block",
    async (mode) => {
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
      const authenticInfo = fundedWithdrawalInfo(
        Data.from(DATUM_AUTHENTIC_WITHDRAWAL_EVENT, SDK.WithdrawalOrderDatum)
          .event.info,
      );
      if (mode === "external")
        authenticInfo.body.l1_datum = {
          InlineDatum: { data: "cd".repeat(1600) },
        };
      const authenticInfoCbor =
        SDK.committedWithdrawalValueBytes(authenticInfo);
      const {
        counted,
        header,
        setup,
        keyCbor,
        admitted,
        referenceScriptUtxos,
      } = await setupChallengedBlockOnEmulator(
        harness,
        authenticInfoCbor,
        mode,
      );

      const initResult = await submitFabricatedFamilyInit({
        onSigned: (signed) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-honest-init-${mode}`,
            signed,
          ),
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
          entries: [[keyCbor, authenticInfoCbor]],
          witness: { ...admitted.raw, observation: L1_OBSERVATION },
        }),
      ).rejects.toThrow(/authentic_content_matches_commitment/u);

      // Plane 2 — on-chain: substituting diverted content for the honest leaf
      // passes the local counted-root equality (root and count are the header's
      // own), but the L1 membership proof cannot open the committed root over a
      // value the block never committed. The inline MPF verification in step-01's
      // spend handler is what refuses it.
      const honestProof = await keyValuePhasProof(
        { ...counted, root: counted.phasRoot },
        Buffer.from(keyCbor, "hex"),
        Buffer.from(authenticInfoCbor, "hex"),
      );
      const divertedInclusion = parseSubmitFabricatedWithdrawalInclusion({
        committedWithdrawalIdCbor: keyCbor,
        committedWithdrawalInfoCbor: fundedWithdrawalValue(
          VALUE_DIVERTED_WITHDRAWAL_INFO,
        ),
        withdrawalsPhasRoot: counted.phasRoot,
        withdrawalMembershipProofCbor: Data.to(honestProof, SDK.Proof),
      });
      await expect(
        submitFabricatedWithdrawalStep01({
          preSubmitBoundary: async ({ signed }) =>
            recordFamilyTransaction(
              historyRecords,
              `withdrawal-refused-step-01-${mode}`,
              signed,
            ),
          now: () => harness.emulator.now(),
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

      // Advance the unchanged honest source through genuine stages01/02. The
      // next refusal must come from stage03's applied script, not its off-chain
      // content-matches preflight and not a fabricated thread or commitment.
      const first = await submitFabricatedWithdrawalStep01({
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-honest-step-01-${mode}`,
            signed,
          ),
        now: () => harness.emulator.now(),
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(stillFirstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        withdrawalInclusion: parseSubmitFabricatedWithdrawalInclusion({
          committedWithdrawalIdCbor: keyCbor,
          committedWithdrawalInfoCbor: authenticInfoCbor,
          withdrawalsPhasRoot: counted.phasRoot,
          withdrawalMembershipProofCbor: Data.to(honestProof, SDK.Proof),
        }),
        referenceScriptUtxo: referenceScriptUtxos[0],
        awaitConfirmation: true,
      });
      const second = await submitFabricatedWithdrawalStep02({
        preSubmitBoundary: async ({ signed }) =>
          recordFamilyTransaction(
            historyRecords,
            `withdrawal-honest-step-02-${mode}`,
            signed,
          ),
        now: () => harness.emulator.now(),
        lucid: proverLucid,
        contracts: fabricatedWithdrawal,
        network,
        signer: proverSigner,
        threadOutRef: first.nextThreadOutRef,
        evidence: {
          kind: "present_event",
          eventOutRef: outRefLabel(admitted.witness.anchor.utxo),
        },
        referenceScriptUtxo: referenceScriptUtxos[1],
        awaitConfirmation: true,
      });
      expect(second.verdict).toEqual({
        WithdrawalEventObserved: { commitment: admitted.captured.commitment },
      });
      const third = await expectSingleUtxoWithUnit(
        proverLucid,
        second.thirdStepAddress,
        initResult.computationThreadUnit,
      );
      const state = Data.from(
        third.datum!,
        SDK.FabricatedWithdrawalStep03Datum,
      ).data;
      if (state === null) throw new Error("Missing honest stage03 state");
      expect(state.verdict).toEqual(second.verdict);
      expect(state.challenged_header_hash).toBe(setup.headerHash);
      expect(state.state_queue_policy).toBe(contracts.stateQueue.policyId);
      expect(state.committed_withdrawal_id).toEqual(
        Data.from(keyCbor, SDK.OutputReference),
      );
      const inclusion = admitted.captured.commitment.inclusion_time;
      expect(state.header_start_time).toBeLessThan(inclusion);
      expect(inclusion).toBe(state.header_end_time);
      expect(
        SDK.opensEventHistoryCommitmentCbor(
          admitted.captured.commitment,
          plutusConstrFieldCbor(admitted.openingCbor, [0]),
          plutusConstrFieldCbor(admitted.openingCbor, [1]),
        ),
      ).toBe(true);
      const matchingHash = await Effect.runPromise(
        SDK.withdrawalContentCommitmentCbor(authenticInfoCbor),
      );
      expect(state.committed_withdrawal_content_hash).toBe(matchingHash);
      await expect(
        deriveFabricatedWithdrawalStep03Handoff({
          state,
          openingCbor: admitted.openingCbor,
        }),
      ).rejects.toThrow(
        "Authentic eligible event content matches the header commitment",
      );

      const queueBefore = await proverLucid.utxoByUnit(
        setup.stateQueueBlockUnit,
      );
      const claimedFault: SDK.FabricatedWithdrawalFault = {
        MismatchedWithdrawalContent: {
          committed_withdrawal_content_hash: matchingHash,
          authentic_withdrawal_content_hash: matchingHash,
          event_inclusion_time: inclusion,
        },
      };
      const claimedDatum = Data.to(
        {
          fraud_prover: proverSigner.paymentKeyHash,
          data: SDK.fabricatedWithdrawalStep04State(state, claimedFault),
        },
        SDK.FabricatedWithdrawalStep04Datum,
      );
      const matches = computationThreadOutputPredicate({
        address: fabricatedWithdrawal.steps[3].spendingScriptAddress,
        datum: claimedDatum,
        unit: initResult.computationThreadUnit,
      });
      let layout:
        | { inputIndex: bigint; outputIndex: bigint; redeemerCbor: string }
        | undefined;
      const redeemer = ((ctx) => {
        SDK.requireOwnSpendPurpose(ctx, third, "honest stage03 refusal");
        const inputIndex = SDK.requireInputIndex(
          ctx,
          third,
          "honest stage03 input",
        );
        const outputIndex = SDK.requireUniqueOutputIndex(
          ctx.outputs,
          matches,
          "claimed stage04 output",
        );
        const redeemerCbor = replacePlutusConstrFieldCbor(
          Data.to(
            {
              Continue: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  authentic_content: Data.from(
                    admitted.openingCbor,
                    SDK.FabricatedWithdrawalAuthenticContentOpening,
                  ),
                },
              ],
            },
            SDK.FabricatedWithdrawalStep03SpendRedeemer,
          ),
          [0, 2],
          admitted.openingCbor,
        );
        layout = { inputIndex, outputIndex, redeemerCbor };
        return redeemerCbor;
      }) satisfies BuildTxWithRedeemer;
      proverSigner.selectWallet(proverLucid);
      const fee = selectFeeInput(await proverLucid.wallet().getUtxos());
      expect(fee.scriptRef == null && fee.datum == null).toBe(true);
      const validity = fabricatedProofValidity(
        state.header_end_time,
        harness.emulator.now(),
      );
      // Test-only construction deliberately bypasses only the honest-content
      // preflight. Funding, indices, reference script, signer, time bounds and
      // local UPLC evaluation follow the production stage03 builder.
      let refusal: string | undefined;
      await expect(
        (async () => {
          try {
            await proverLucid
              .newTx()
              .validFrom(validity.validFrom)
              .validTo(validity.validTo)
              .collectFrom([fee])
              .collectFrom([third], redeemer)
              .readFrom([referenceScriptUtxos[2]])
              .pay.ToContract(
                fabricatedWithdrawal.steps[3].spendingScriptAddress,
                { kind: "inline", value: claimedDatum },
                {
                  lovelace: third.assets.lovelace ?? 0n,
                  [initResult.computationThreadUnit]: 1n,
                },
              )
              .addSignerKey(proverSigner.paymentKeyHash)
              .complete({ localUPLCEval: true });
          } catch (error) {
            refusal = error instanceof Error ? error.message : String(error);
            throw error;
          }
        })(),
      ).rejects.toThrow(/failed script execution.*Spend/su);
      expect(layout).toBeDefined();
      expect(
        await proverLucid.utxosByOutRef([
          { txHash: third.txHash, outputIndex: third.outputIndex },
        ]),
      ).toEqual([third]);
      expect(await proverLucid.utxoByUnit(setup.stateQueueBlockUnit)).toEqual(
        queueBefore,
      );
      expect(
        await proverLucid.utxosByOutRef([admitted.witness.anchor.utxo]),
      ).toEqual([admitted.witness.anchor.utxo]);
      if (admitted.witness.retainedDataUtxo !== undefined)
        expect(
          await proverLucid.utxosByOutRef([admitted.witness.retainedDataUtxo]),
        ).toEqual([admitted.witness.retainedDataUtxo]);
      expect(
        await proverLucid.utxosAtWithUnit(
          fabricatedWithdrawal.steps[3].spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).toHaveLength(0);
      expect(
        await proverLucid.utxosAtWithUnit(
          fabricatedWithdrawal.fraudProof.spendingScriptAddress,
          fabricatedWithdrawal.fraudProof.policyId +
            initResult.computationThreadAssetName,
        ),
      ).toHaveLength(0);
      historyRecords.push({
        label: "honest-stage03-script-refusal",
        kind: "Withdrawal",
        mode,
        scope:
          "Actual live stage03 script input and authentic eligible opening; rejected during local evaluation before signing/submission",
        headerHash: setup.headerHash,
        thread: third,
        queue: queueBefore,
        authenticOrder: admitted.witness.anchor.utxo,
        retainedData: admitted.witness.retainedDataUtxo,
        originalAssetsCbor: Data.to(
          admitted.captured.originalAssets,
          SDK.Value,
        ),
        openingCbor: admitted.openingCbor,
        claimedDatum,
        layout,
        validity,
        referenceScript: referenceScriptUtxos[2],
        refusal,
      });
    },
    240_000,
  );
});
