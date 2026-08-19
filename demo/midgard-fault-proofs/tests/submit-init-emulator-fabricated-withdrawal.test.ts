/**
 * `fabricated-withdrawal` emulator lifecycle (Goal task `Q40`, §9.1 output 9) — at
 * the boundary the frozen blueprint imposes.
 *
 * The four step validators exist in `onchain/aiken` and pass `aiken check`, but
 * the blueprint in the tree does **not** contain them: regenerating
 * `onchain/aiken/plutus.json` changes deployed script hashes, which is
 * owner-gated (#510, owner ruling R5 / #617). Until that regeneration lands there
 * is no `compiledCode` for `fraud_proofs/fabricated_withdrawal/step_0{1..4}`, so
 * no emulator can deploy the family and no on-chain execution can be measured.
 * This file therefore does everything short of execution **for real** and then
 * skips with the measured reason:
 *
 * - it reads the same frozen blueprint the emulator harness reads
 *   (`realBlueprintPath`, honouring `MIDGARD_REAL_BLUEPRINT_PATH`) and measures
 *   exactly which of the family's eight titles are missing, with a live control
 *   title proving the read itself works;
 * - it drives the whole off-chain lifecycle over a real withdrawals-bearing
 *   `DaPayloadV1` — evidence admission, proof plan, and the step-01 → step-02 →
 *   step-03 → step-04 handoffs — and encodes each thread datum the emulator
 *   would place on chain, so the only unmeasured link is the UPLC evaluation
 *   itself.
 *
 * The skip is self-retiring: if the blueprint gains the family's titles, the
 * absence assertion fails and names what has to replace this file — a real
 * lifecycle driven through `makeFaultProofEmulatorHarnessV1`, which needs a
 * `realFabricatedWithdrawal` contract option in the (parent-owned)
 * `tests/support/submit-init-emulator-shared.ts`.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayloadV1,
  prepareFabricatedWithdrawalFromCommittedLeavesV1,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  deriveFabricatedWithdrawalStep01HandoffV1,
  parseSubmitFabricatedWithdrawalInclusion,
} from "../src/submit-fabricated-withdrawal-step-01.js";
import { deriveFabricatedWithdrawalStep03HandoffV1 } from "../src/submit-fabricated-withdrawal-step-03.js";
import { assertFabricatedWithdrawalStep04FinalizableV1 } from "../src/submit-fabricated-withdrawal-step-04.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  h28,
  outRefCbor,
  reencodeFixturePayloadV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";
import {
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

/** Every blueprint entry the family's four steps would contribute. */
const FABRICATED_WITHDRAWAL_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/fabricated_withdrawal/step_01.main.spend",
  "fraud_proofs/fabricated_withdrawal/step_01.main.else",
  "fraud_proofs/fabricated_withdrawal/step_02.main.spend",
  "fraud_proofs/fabricated_withdrawal/step_02.main.else",
  "fraud_proofs/fabricated_withdrawal/step_03.main.spend",
  "fraud_proofs/fabricated_withdrawal/step_03.main.else",
  "fraud_proofs/fabricated_withdrawal/step_04.main.spend",
  "fraud_proofs/fabricated_withdrawal/step_04.main.else",
] as const;

/**
 * A title the frozen blueprint does carry, so "the family is missing" can never
 * be satisfied by a failed or empty blueprint read.
 */
const CONTROL_BLUEPRINT_TITLE_V1 =
  "fraud_proofs/da_hash_preimage/step_01.main.spend";

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

/** Commits the challenged block's single fabricated withdrawal leaf. */
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
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, [
    {
      key: Buffer.from(KEY_AUTHENTIC_WITHDRAWAL_ID, "hex"),
      value: Buffer.from(VALUE_DIVERTED_WITHDRAWAL_INFO, "hex"),
    },
  ]);
  const header: SDK.HeaderV1 = {
    ...base.header,
    withdrawalsRoot: counted.root,
    withdrawalCount: counted.count,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const withdrawals: SDK.DaPayloadEntry[] = [
    [KEY_AUTHENTIC_WITHDRAWAL_ID, VALUE_DIVERTED_WITHDRAWAL_INFO],
  ];
  const payload: SDK.DaPayloadV1 = {
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
    payloadEnvelopeCbor: await reencodeFixturePayloadV1(payload),
    observation: authenticatedHeaderObservationV1({
      ...base,
      header,
      headerHash,
    }),
  };
};

describe("fabricated-withdrawal fault-proof emulator lifecycle", () => {
  it("measures the whole off-chain lifecycle and stops at the frozen blueprint", async (ctx) => {
    // ## 1. The blueprint boundary, measured.
    const blueprint = readBlueprint(realBlueprintPath);
    const titles = new Set(blueprint.validators.map((entry) => entry.title));
    expect(blueprint.validators.length).toBeGreaterThan(0);
    // Control: the read works and this is the real frozen blueprint.
    expect(titles.has(CONTROL_BLUEPRINT_TITLE_V1)).toBe(true);
    const presentFamilyTitles =
      FABRICATED_WITHDRAWAL_BLUEPRINT_TITLES_V1.filter((title) =>
        titles.has(title),
      );
    // A failure here is the retirement signal, not a regression: the blueprint
    // now carries the family, so this boundary file must be replaced by a real
    // lifecycle driven through `makeFaultProofEmulatorHarnessV1` with a
    // `realFabricatedWithdrawal` contract option.
    expect(presentFamilyTitles).toEqual([]);

    // ## 2. Evidence admission over real retained-DA bytes.
    const block = await buildChallengedBlockV1();
    expect(block.withdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);
    const evidence =
      await fabricatedWithdrawalBlockEvidenceFromVerifiedPayloadV1({
        observation: block.observation,
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      });
    expect(evidence.grade).toBe("security");
    expect(evidence.headerHash).toBe(block.headerHash);

    // ## 3. The proof plan the prover would submit.
    const plan = await prepareFabricatedWithdrawalFromCommittedLeavesV1({
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
      `${SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1}${block.headerHash}`,
    );

    // ## 4. Every thread datum the emulator would place on chain.
    const step01Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: null },
      SDK.FabricatedWithdrawalStep01Datum,
    );
    const step01Handoff = await deriveFabricatedWithdrawalStep01HandoffV1({
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
    const step03State = SDK.fabricatedWithdrawalStep03StateV1(
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
    const step03Handoff = await deriveFabricatedWithdrawalStep03HandoffV1({
      state: step03State,
      eventDatumCbor: DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
    });
    const step04Datum = Data.to(
      { fraud_prover: FRAUD_PROVER, data: step03Handoff.step04State },
      SDK.FabricatedWithdrawalStep04Datum,
    );
    assertFabricatedWithdrawalStep04FinalizableV1({
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

    // ## 5. The only unmeasured link.
    ctx.skip(
      `on-chain execution is unmeasurable until onchain/aiken/plutus.json is regenerated (#510, owner ruling R5 / #617): ${realBlueprintPath} carries ${blueprint.validators.length.toString()} validators and none of the family's ${FABRICATED_WITHDRAWAL_BLUEPRINT_TITLES_V1.length.toString()} titles — ${FABRICATED_WITHDRAWAL_BLUEPRINT_TITLES_V1.join(", ")} — so the four step scripts have no compiledCode to deploy. Everything short of evaluation is measured above: evidence admission, the proof plan, and all four thread datums.`,
    );
  });
});
