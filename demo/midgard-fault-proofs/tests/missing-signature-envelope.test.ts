/** Missing-signature compiled-size and three-axis carriage frontier gate. */
import {
  MIDGARD_CONSENSUS_LIMITS,
  midgardFieldCarriageBounds,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import {
  buildMissingSignatureFaultProofContracts,
  MissingSignatureStep01SpendRedeemer,
  type MissingSignatureStep01SpendRedeemer as MissingSignatureStep01SpendRedeemerType,
  MissingSignatureStep02SpendRedeemer,
  type MissingSignatureStep02SpendRedeemer as MissingSignatureStep02SpendRedeemerType,
  MissingSignatureStep04SpendRedeemer,
  type MissingSignatureStep04SpendRedeemer as MissingSignatureStep04SpendRedeemerType,
  parseFaultProofBlueprint,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  proveMissingSignatureFault,
  submitMissingSignatureInit,
  submitMissingSignatureStep01,
} from "../src/missing-signature/index.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { parseSubmitStep01TxInclusion } from "./support/legacy-submit-emulator.js";
import {
  makeMissingSignatureEmulatorHarness,
  MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT,
  MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT,
  missingSignatureFinding,
  missingSignatureProverDeps,
  publishMissingSignatureField07Certificate,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
} from "./support/missing-signature-emulator.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  buildTransactionInclusionFixture,
  countedTransactionsRoot,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeHeader,
  network,
  readBlueprint,
  realBlueprintPath,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const OWNER = Buffer.alloc(28, 0x11);
const TX_ID = Buffer.alloc(32, 0x22);
const overhead = 2_048;
const bytes = (hex: string) => hex.length / 2;

/**
 * The release ExUnits basis every missing-signature transaction is held to:
 * the mainnet-equivalent per-transaction budget the family is allowed to
 * consume, stated once here so a measurement that outgrows it fails rather
 * than being re-pinned.
 */
const RELEASE_MAX_EXECUTION_MEMORY = 13_200_000n;
const RELEASE_MAX_EXECUTION_STEPS = 8_000_000_000n;

/**
 * The chain is four distinct spending validators (step-01 through step-04);
 * each step is parameterised by the next step's applied hash, so the four
 * applied hashes must differ and every parameter must be load-bearing.
 */
const MISSING_SIGNATURE_STEP_COUNT = 4;

/**
 * Transaction counts for the two prover journeys. These are the exact number
 * of L1 transactions the prover is allowed to submit end to end -- one per
 * proof step plus the carriage transactions the witness tier forces. They are
 * pinned exactly (not as an upper bound) because an extra submission is a
 * cost and liveness regression the reserve checks cannot see, and a missing
 * one means a step was skipped.
 *
 * First automatic tier-2 frontier: 4 proof steps + init + the tier-2 raw-utxo
 * carriage publications the frontier witness count forces = 9.
 * Maximum admissible field-7 vector: the same journey with certified carriage
 * for field 7, which adds five further publications = 14.
 */
const TIER2_FRONTIER_JOURNEY_TX_COUNT = 9;
const MAX_FIELD07_JOURNEY_TX_COUNT = 14;

describe("missing-signature compiled envelope", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("applies four parameter-distinct steps that fit reference-script deployment", async () => {
    const {
      missingSignature: { steps: chain },
    } = await Effect.runPromise(
      buildMissingSignatureFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprint),
        network,
        hubOraclePolicyId: "55".repeat(28),
        fraudProofCataloguePolicyId: "66".repeat(28),
      }),
    );
    expect(chain, "the missing-signature chain length").toHaveLength(
      MISSING_SIGNATURE_STEP_COUNT,
    );
    const hashes = chain.map(({ spendingScriptHash }) => spendingScriptHash);
    expect(
      new Set(hashes).size,
      "each step must apply to a distinct script hash",
    ).toBe(MISSING_SIGNATURE_STEP_COUNT);
    for (const step of chain) {
      expect(
        step.spendingScript.script.length / 2 + overhead,
        "applied step plus deployment overhead must fit a reference-script output",
      ).toBeLessThan(65_536);
    }
  });

  it("fits a deep step-01 inclusion under reference-script deployment", async () => {
    const fixture = await buildTransactionInclusionFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const inclusion = fixture.tx1.inclusion as {
      readonly nativeTxId: string;
      readonly nativeTxCompactCbor: string;
      readonly l2TransactionSourceCbor: string;
      readonly transactionsPhasRoot: string;
      readonly txMembershipProofCbor: string;
    };
    const redeemer: MissingSignatureStep01SpendRedeemerType = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 0n,
          state_queue_node_ref_input_index: 1n,
          native_tx_id: inclusion.nativeTxId,
          l2_transaction_source_cbor: inclusion.l2TransactionSourceCbor,
          transactions_phas_root: inclusion.transactionsPhasRoot,
          tx_membership_proof: Data.from(
            inclusion.txMembershipProofCbor,
            Proof,
          ),
          inclusion_proof_script_withdraw_redeemer_index: 0n,
        },
      ],
    };
    expect(
      bytes(Data.to(redeemer, MissingSignatureStep01SpendRedeemer)) + overhead,
      "ESCALATE: the bare step-01 subject/inclusion no longer fits L1",
    ).toBeLessThanOrEqual(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes);
  });

  it("executes the worst-depth step-01 inclusion transaction within the release envelope", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    const fixture = await buildTransactionInclusionFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const startTime =
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1;
    const header = makeHeader(
      await funderPaymentKeyHash(harness.funderLucid),
      startTime,
      await countedTransactionsRoot(
        fixture.transactionsRoot,
        fixture.l2TransactionCount,
      ),
      fixture.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header,
    });
    const [step01] = await publishMissingSignatureReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.missingSignature,
    });
    const initialized = await submitMissingSignatureInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingSignature,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const capture = await captureEmulatorSubmission(harness.emulator, () =>
      submitMissingSignatureStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: initialized.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(fixture.tx1.inclusion),
        referenceScriptUtxo: step01,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(capture.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
    );
    expect(capture.measurement.executionMemory).toBeLessThanOrEqual(
      RELEASE_MAX_EXECUTION_MEMORY,
    );
    expect(capture.measurement.executionSteps).toBeLessThanOrEqual(
      RELEASE_MAX_EXECUTION_STEPS,
    );
  }, 600_000);

  it("charts field-4 and field-7 at Inline/RawUtxo/Certified boundaries", () => {
    const plan = (fieldIndex: number, length: number, publish = false) =>
      planMidgardFieldCarriage({
        owner: OWNER,
        txId: TX_ID,
        fieldIndex,
        preimage: Buffer.alloc(length, 0x80),
        publish,
      });
    const { maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes } =
      midgardFieldCarriageBounds;
    expect(plan(4, 29).tier).toBe("Inline");
    expect(plan(4, 29, true).tier).toBe("RawUtxo");
    expect(plan(4, maxTier1RedeemerPreimageBytes).tier).toBe("Inline");
    expect(plan(4, maxTier1RedeemerPreimageBytes + 1).tier).toBe("RawUtxo");
    expect(plan(4, maxPublishableCarriageBytes + 1).tier).toBe("Certified");

    // Field 7's fixed stride is 103 bytes. The exact witness-count crossings
    // are pinned so a change in §5.3 encoding cannot silently move the route.
    const firstRawWitnessCount =
      Math.floor((maxTier1RedeemerPreimageBytes - 3) / 103) + 1;
    const firstCertifiedWitnessCount =
      Math.floor((maxPublishableCarriageBytes - 3) / 103) + 1;
    expect(plan(7, 3 + (firstRawWitnessCount - 1) * 103).tier).toBe("Inline");
    expect(plan(7, 3 + firstRawWitnessCount * 103).tier).toBe("RawUtxo");
    expect(plan(7, 3 + firstCertifiedWitnessCount * 103).tier).toBe(
      "Certified",
    );
  });

  it("keeps boundary-inline field openings inside the redeemer envelope", () => {
    const inline = {
      Inline: {
        preimage: Buffer.alloc(
          midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes,
          0x80,
        ).toString("hex"),
      },
    } as const;
    const step02: MissingSignatureStep02SpendRedeemerType = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          required_signers_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "80",
              carriage: inline,
            },
          },
          bad_required_signer_hash_index: 0n,
        },
      ],
    };
    expect(
      bytes(Data.to(step02, MissingSignatureStep02SpendRedeemer)),
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes);
    const step04: MissingSignatureStep04SpendRedeemerType = {
      Continue: [
        {
          Finalize: {
            input_index: 0n,
            output_index: 0n,
            fraud_proof_mint_redeemer_index: 0n,
            addr_tx_wits_opening: {
              WitnessFieldOpening: {
                native_tx_compact_cbor: "80",
                witness_set: {
                  addr_tx_wits_hash: "11".repeat(32),
                  script_tx_wits_hash: "22".repeat(32),
                  redeemer_tx_wits_hash: "33".repeat(32),
                },
                carriage: inline,
              },
            },
            checkpoint_cbor: null,
          },
        },
      ],
    };
    expect(
      bytes(Data.to(step04, MissingSignatureStep04SpendRedeemer)),
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes);
  });

  it("proves the first automatic tier-2 witness frontier within the release ExUnits basis", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    const scenario = await setupMissingSignatureScenario({
      harness,
      decoyWitnessCount: MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT,
    });
    const [step01, step02, step03, step04] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const capture = await captureEmulatorSubmission(harness.emulator, () =>
      Effect.runPromise(
        proveMissingSignatureFault(
          missingSignatureFinding(scenario),
          missingSignatureProverDeps({
            harness,
            scenario,
            referenceScriptUtxos: { step01, step02, step03, step04 },
          }),
        ),
      ),
    );
    if (capture.result.kind !== "proven") {
      throw new Error(
        `tier-2 frontier did not prove: ${capture.result.kind} ${capture.result.reason}${
          capture.result.kind === "stalled"
            ? `; cause=${String(capture.result.cause)}`
            : ""
        }`,
      );
    }
    expect(
      capture.result.txHashes,
      "tier-2 frontier journey transaction count",
    ).toHaveLength(TIER2_FRONTIER_JOURNEY_TX_COUNT);
    expect(capture.measurements.length).toBeGreaterThanOrEqual(
      TIER2_FRONTIER_JOURNEY_TX_COUNT,
    );
    for (const measurement of capture.measurements) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        RELEASE_MAX_EXECUTION_MEMORY,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        RELEASE_MAX_EXECUTION_STEPS,
      );
    }
  }, 600_000);

  it("proves the maximum admissible field-7 vector through certified carriage and bounded scans", async () => {
    const harness = await makeMissingSignatureEmulatorHarness();
    const scenario = await setupMissingSignatureScenario({
      harness,
      decoyWitnessCount: MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT,
    });
    const [step01, step02, step03, step04] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const certificateUtxo = await publishMissingSignatureField07Certificate({
      harness,
      scenario,
    });
    const deps = missingSignatureProverDeps({
      harness,
      scenario,
      referenceScriptUtxos: { step01, step02, step03, step04 },
    });
    const capture = await captureEmulatorSubmission(harness.emulator, () =>
      Effect.runPromise(
        proveMissingSignatureFault(missingSignatureFinding(scenario), {
          ...deps,
          fieldCertificates: { step04: certificateUtxo },
        }),
      ),
    );
    if (capture.result.kind !== "proven") {
      throw new Error(
        `maximum field-7 subject did not prove: ${capture.result.kind} ${capture.result.reason}${
          capture.result.kind === "stalled"
            ? `; cause=${String(capture.result.cause)}`
            : ""
        }`,
      );
    }
    expect(
      capture.result.txHashes,
      "maximum field-7 journey transaction count",
    ).toHaveLength(MAX_FIELD07_JOURNEY_TX_COUNT);
    for (const measurement of capture.measurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
      );
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        RELEASE_MAX_EXECUTION_MEMORY,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        RELEASE_MAX_EXECUTION_STEPS,
      );
    }
  }, 600_000);
});
