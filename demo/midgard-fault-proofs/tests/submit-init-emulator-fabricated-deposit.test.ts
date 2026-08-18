/**
 * `fabricated-deposit` emulator lifecycle (Goal task `Q39`, §9.1 output 9) — at
 * the boundary the frozen blueprint imposes.
 *
 * The four step validators exist in `onchain/aiken` and pass `aiken check`, but
 * the blueprint in the tree does **not** contain them: regenerating
 * `onchain/aiken/plutus.json` changes deployed script hashes, which is
 * owner-gated (#510, owner ruling R5 / #617). Until that regeneration lands there
 * is no `compiledCode` for `fraud_proofs/fabricated_deposit/step_0{1..4}`, so no
 * emulator can deploy the family and no on-chain execution can be measured. This
 * file therefore does everything short of execution **for real** and then skips
 * with the measured reason:
 *
 * - it reads the same frozen blueprint the emulator harness reads
 *   (`realBlueprintPath`, honouring `MIDGARD_REAL_BLUEPRINT_PATH`) and measures
 *   exactly which of the family's eight titles are missing, with a live control
 *   title proving the read itself works;
 * - it drives the whole off-chain lifecycle over a real deposits-bearing
 *   `DaPayloadV1` — evidence admission, proof plan, and the step-01 → step-02 →
 *   step-03 → step-04 handoffs — and encodes each thread datum the emulator
 *   would place on chain, so the only unmeasured link is the UPLC evaluation
 *   itself.
 *
 * The skip is self-retiring: if the blueprint gains the family's titles, the
 * absence assertion fails and names what has to replace this file — a real
 * lifecycle driven through `makeFaultProofEmulatorHarnessV1`, which needs a
 * `realFabricatedDeposit` contract option in the (parent-owned)
 * `tests/support/submit-init-emulator-shared.ts`.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  fabricatedDepositBlockEvidenceFromVerifiedPayloadV1,
  prepareFabricatedDepositFromCommittedLeavesV1,
} from "../src/prepare-fabricated-deposit.js";
import {
  deriveFabricatedDepositStep01HandoffV1,
  parseSubmitFabricatedDepositInclusion,
} from "../src/submit-fabricated-deposit-step-01.js";
import { deriveFabricatedDepositStep03HandoffV1 } from "../src/submit-fabricated-deposit-step-03.js";
import { assertFabricatedDepositStep04FinalizableV1 } from "../src/submit-fabricated-deposit-step-04.js";
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
const FABRICATED_DEPOSIT_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/fabricated_deposit/step_01.main.spend",
  "fraud_proofs/fabricated_deposit/step_01.main.else",
  "fraud_proofs/fabricated_deposit/step_02.main.spend",
  "fraud_proofs/fabricated_deposit/step_02.main.else",
  "fraud_proofs/fabricated_deposit/step_03.main.spend",
  "fraud_proofs/fabricated_deposit/step_03.main.else",
  "fraud_proofs/fabricated_deposit/step_04.main.spend",
  "fraud_proofs/fabricated_deposit/step_04.main.else",
] as const;

/**
 * A title the frozen blueprint does carry, so "the family is missing" can never
 * be satisfied by a failed or empty blueprint read.
 */
const CONTROL_BLUEPRINT_TITLE_V1 =
  "fraud_proofs/da_hash_preimage/step_01.main.spend";

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

describe("fabricated-deposit fault-proof emulator lifecycle", () => {
  it("measures the whole off-chain lifecycle and stops at the frozen blueprint", async (ctx) => {
    // ## 1. The blueprint boundary, measured.
    const blueprint = readBlueprint(realBlueprintPath);
    const titles = new Set(blueprint.validators.map((entry) => entry.title));
    expect(blueprint.validators.length).toBeGreaterThan(0);
    // Control: the read works and this is the real frozen blueprint.
    expect(titles.has(CONTROL_BLUEPRINT_TITLE_V1)).toBe(true);
    const presentFamilyTitles = FABRICATED_DEPOSIT_BLUEPRINT_TITLES_V1.filter(
      (title) => titles.has(title),
    );
    // A failure here is the retirement signal, not a regression: the blueprint
    // now carries the family, so this boundary file must be replaced by a real
    // lifecycle driven through `makeFaultProofEmulatorHarnessV1` with a
    // `realFabricatedDeposit` contract option.
    expect(presentFamilyTitles).toEqual([]);

    // ## 2. Evidence admission over real retained-DA bytes.
    const block = await buildChallengedBlockV1();
    expect(block.depositsRoot).toBe(MM_DEPOSITS_ROOT);
    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayloadV1({
      observation: block.observation,
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.headerHash).toBe(block.headerHash);

    // ## 3. The proof plan the prover would submit.
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

    // ## 4. Every thread datum the emulator would place on chain.
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
        authentic_deposit_info_hash:
          "89ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863",
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    for (const datum of [step01Datum, step02Datum, step03Datum, step04Datum]) {
      expect(datum).toMatch(/^[0-9a-f]+$/u);
    }

    // ## 5. The only unmeasured link.
    ctx.skip(
      `on-chain execution is unmeasurable until onchain/aiken/plutus.json is regenerated (#510, owner ruling R5 / #617): ${realBlueprintPath} carries ${blueprint.validators.length.toString()} validators and none of the family's ${FABRICATED_DEPOSIT_BLUEPRINT_TITLES_V1.length.toString()} titles — ${FABRICATED_DEPOSIT_BLUEPRINT_TITLES_V1.join(", ")} — so the four step scripts have no compiledCode to deploy. Everything short of evaluation is measured above: evidence admission, the proof plan, and all four thread datums.`,
    );
  });
});
