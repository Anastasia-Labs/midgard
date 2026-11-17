/**
 * A forced `SpendInputSignerMissing` rejection naming a negative input
 * position, or one past any field (up to the rejection codec's 64-bit
 * bounds), is wrongful: the position names no input, so canonical validation
 * requires no signer there. Step 02 takes the direct route to the terminal
 * without resolving an input. A claim that a held input is absent is refused
 * at step 02, and the route convicts over field 0 at its byte cap.
 */
import { sign } from "node:crypto";

import {
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  SPEND_INPUT_SIGNER_MISSING_ID,
  type SpendInputSignerMissingEvidence,
} from "../src/spend-input-signer-missing/index.js";
import {
  commitForcedBlock,
  publishReferences,
} from "./spend-input-signer-missing-lifecycle.commit-forced-block.js";
import { familyDriver } from "./spend-input-signer-missing-lifecycle.family-driver.js";
import {
  ed25519Keypair,
  FAMILY,
  newHarness,
  prepareSpendInputSignerMissingEvidence,
  priorLedgerFor,
  registeredContracts,
  signedNativeTx,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import { MAX_SLOT0_ITEM_COUNT } from "./support/committed-field-shape-emulator.committed-field-shape-scenario-material.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

type NativeTx = ReturnType<typeof makeNativeTx>;

/**
 * A forced block rejecting `nativeTx` with `SpendInputSignerMissing {
 * inputIndex }`, a thread bound to it through step 01 with `evidence`, and
 * the family driver.
 */
const boundThread = async ({
  nativeTx,
  inputIndex,
  priorRoot,
  evidenceFor,
}: {
  readonly nativeTx: NativeTx;
  readonly inputIndex: bigint;
  readonly priorRoot: string;
  readonly evidenceFor: (
    block: Awaited<ReturnType<typeof commitForcedBlock>>,
  ) => SpendInputSignerMissingEvidence;
}) => {
  const harness = await newHarness();
  const family = await registeredContracts(harness);
  const block = await commitForcedBlock(
    harness,
    family,
    nativeTx,
    priorRoot,
    { SpendInputSignerMissing: { input_index: inputIndex } },
    "f7",
  );
  const evidence = evidenceFor(block);
  const { references, certificateReference } = await publishReferences(
    harness,
    family,
    `${FAMILY}-absent-input`,
    false,
  );
  const run = familyDriver(harness, family, references, certificateReference);
  const thread = await run.initThread(block.blockOutRef);
  const bound = await captureEmulatorSubmission(harness.emulator, () =>
    run.step01Forced(thread.threadOutRef, evidence, block.forcedSource),
  );
  return { harness, block, evidence, run, bound };
};

/** Step 02's direct route, the proof mint and the block's removal. */
const convict = async ({
  harness,
  block,
  evidence,
  run,
  bound,
}: Awaited<ReturnType<typeof boundThread>>) => {
  expect(evidence.route).toBe("coordinate_out_of_range");
  const direct = await captureEmulatorSubmission(harness.emulator, () =>
    run.step02(
      bound.result.nextThreadOutRef,
      evidence,
      block.compactCbor,
      block.witnessSetCompactCbor,
    ),
  );
  expect(direct.result.stage).toBe("terminal");
  const minted = await captureEmulatorSubmission(harness.emulator, () =>
    run.terminal(direct.result.nextThreadOutRef, evidence),
  );
  expect(minted.result.fraudProofUnit).toBeTruthy();
  const removal = await run.removal(block.headerHash);
  expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
  return { direct, minted };
};

/** A forced-rejection evidence for `inputIndex`; the fixture wrapper projects
 * `nativeTx` into its forced form. */
const forcedEvidence =
  (nativeTx: NativeTx, inputIndex: bigint, priorRoot: string) =>
  (block: Awaited<ReturnType<typeof commitForcedBlock>>) =>
    prepareSpendInputSignerMissingEvidence({
      subject: block.subject,
      inputIndex,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      priorRoot,
    });

describe("spendInputSignerMissing forced rejection of a coordinate naming no input", () => {
  it("convicts a negative input coordinate", async () => {
    const signerKey = ed25519Keypair(21);
    const prior = await priorLedgerFor(signerKey.keyHash, "2a".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 19n,
      witnesses: () => [],
    });
    await convict(
      await boundThread({
        nativeTx,
        inputIndex: -1n,
        priorRoot: prior.priorRoot,
        evidenceFor: forcedEvidence(nativeTx, -1n, prior.priorRoot),
      }),
    );
  }, 600_000);

  it.each([
    { label: "2^53 + 1", inputIndex: 2n ** 53n + 1n },
    { label: "the codec's largest, 2^64 - 1", inputIndex: 2n ** 64n - 1n },
    { label: "the codec's smallest, -2^64", inputIndex: -(2n ** 64n) },
  ])(
    "convicts an input coordinate at $label",
    async ({ inputIndex }) => {
      const signerKey = ed25519Keypair(25);
      const prior = await priorLedgerFor(signerKey.keyHash, "2d".repeat(32));
      const nativeTx = signedNativeTx({
        outRefBytes: prior.outRefBytes,
        fee: 29n,
        witnesses: () => [],
      });
      await convict(
        await boundThread({
          nativeTx,
          inputIndex,
          priorRoot: prior.priorRoot,
          evidenceFor: forcedEvidence(nativeTx, inputIndex, prior.priorRoot),
        }),
      );
    },
    600_000,
  );

  it("refuses on chain a claim that a held input is absent", async () => {
    // The operator's rejection is right: input 0's signer never signs.
    const signerKey = ed25519Keypair(22);
    const strangerKey = ed25519Keypair(23);
    const prior = await priorLedgerFor(signerKey.keyHash, "2b".repeat(32));
    const nativeTx = signedNativeTx({
      outRefBytes: prior.outRefBytes,
      fee: 23n,
      witnesses: (txId) => [
        encodeMidgardAddressWitnessItem({
          verificationKey: strangerKey.verificationKey,
          signature: sign(null, txId, strangerKey.privateKey),
        }),
      ],
    });
    const thread = await boundThread({
      nativeTx,
      inputIndex: 0n,
      priorRoot: prior.priorRoot,
      // The prover's builder refuses evidence that agrees with the verdict,
      // so the evidence is prepared as a wrongful-acceptance claim and bound
      // to the forced leaf: the thread carries the written coordinate.
      evidenceFor: (block) => ({
        ...prepareSpendInputSignerMissingEvidence({
          subject: acceptedVerdictSubject(block.nativeTxId),
          inputIndex: 0n,
          canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
          resolved: prior.resolved,
        }),
        subject: block.subject,
        canonicalTransactionCborHex: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(nativeTx),
        ).toString("hex"),
      }),
    });
    expect(thread.evidence.signerPresent).toBe(false);
    // Step 02 routes a held position to the ledger membership proof, which the
    // out-of-range route does not carry.
    await expectOnchainRefusal(
      () =>
        thread.run.step02(
          thread.bound.result.nextThreadOutRef,
          {
            ...thread.evidence,
            route: "coordinate_out_of_range",
            signerRequired: false,
            signerPresent: false,
          },
          thread.block.compactCbor,
          thread.block.witnessSetCompactCbor,
        ),
      {
        refusedBy: "fraud_proofs/spend_input_signer_missing/step_02",
        check: /^expect \[Pair\(_, redeemer\)\] = redeemers/u,
      },
    );
  }, 600_000);

  it("convicts a negative coordinate over field 0 at its byte cap", async () => {
    const inputs = Array.from({ length: MAX_SLOT0_ITEM_COUNT }, (_, index) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, (index % 0xff) + 1).fill(index >> 8, 0, 1),
        outputIndex: index % 0x100,
      }),
    );
    const fieldBytes = encodeCbor(inputs).length;
    expect(fieldBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes,
    );
    const nativeTx = makeNativeTx({ spendInputCbors: inputs, fee: 0n });
    const { priorRoot } = await priorLedgerFor(
      ed25519Keypair(24).keyHash,
      "2c".repeat(32),
    );
    const thread = await boundThread({
      nativeTx,
      inputIndex: -1n,
      priorRoot,
      evidenceFor: forcedEvidence(nativeTx, -1n, priorRoot),
    });
    expect(thread.evidence.inputCarriage).toBe("Certified");
    const { direct, minted } = await convict(thread);
    for (const { measurement } of [thread.bound, direct, minted]) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
    }
    console.info(
      `max-shape negative input coordinate (${inputs.length.toString()} inputs, ${fieldBytes.toString()} bytes): step-01 mem=${thread.bound.measurement.executionMemory.toString()} cpu=${thread.bound.measurement.executionSteps.toString()}; step-02 mem=${direct.measurement.executionMemory.toString()} cpu=${direct.measurement.executionSteps.toString()}; step-05 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
    );
  }, 1_800_000);
});
