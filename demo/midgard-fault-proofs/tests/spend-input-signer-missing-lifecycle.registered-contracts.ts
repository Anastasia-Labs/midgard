import { createPrivateKey, createPublicKey } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  AddressData,
  addressDataFromBech32,
  missingSignatureVkeyHash,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  applySpendInputSignerMissingScripts,
  prepareSpendInputSignerMissingEvidence as prepareSpendEvidence,
  SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  type SpendInputSignerMissingContracts,
} from "../src/spend-input-signer-missing/index.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

// These fixtures construct native transactions, then explicitly project them
// into the source kind of the claim they are testing.
export const prepareSpendInputSignerMissingEvidence = (
  input: Parameters<typeof prepareSpendEvidence>[0],
) =>
  prepareSpendEvidence({
    ...input,
    canonicalTransactionCbor:
      input.subject.source_kind === 1n
        ? encodeMidgardForcedTxCanonical(
            materializeMidgardForcedTxFromCanonical(
              decodeMidgardNativeTxFullFromCanonicalCbor(
                input.canonicalTransactionCbor,
              ),
            ),
          )
        : input.canonicalTransactionCbor,
  });

export const network = "Custom" as const;

export const FAMILY = "spend-input-signer-missing" as const;

export const REASON = "SpendInputSignerMissing" as const;

export const MAXIMUM_WITNESSES = 318;

export const MAXIMUM_SHAPE =
  "318 address witnesses; 32,757-byte Certified field; 16-witness scan batches";

/**
 * The smallest witness field the honest refusal run needs so that field 7
 * rides tier-3 certified carriage: 160 witnesses encode to 16,483 bytes, past
 * the raw-carriage bound, and the valid signature sits in the last batch so
 * the scan resumes nine times before it terminates.
 */
export const HONEST_ACCEPTED_WITNESSES = 160;

export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "prior_output_membership",
  "field_certificate",
  "forced_leaf",
  "credential",
] as const;

export const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

/**
 * Asserts that a submission was refused by a validator during local UPLC
 * evaluation, not by a builder precondition. Lucid's default evaluator reports
 * `failed script execution`; the scalus evaluator this suite runs under
 * reports the evaluation error together with the budget spent before the
 * abort. Anything else is a non-validator failure and fails the test.
 */
export const expectRefusedOnChain = async (
  build: () => Promise<unknown>,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined)
    throw new Error(
      "expected the validator to refuse this transaction, but it succeeded",
    );
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution|Error evaluated at/u.test(text))
    throw new Error(
      `expected an on-chain validator refusal, got a non-validator failure: ${text}`,
    );
  return text;
};

export const coverage = createLifecycleCoverageRecorder();

/** Every submitted transaction of the maximum and adjacent runs, by name. */
export const measurements: VanRossemFitMeasurement[] = [];

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export type Captured = Awaited<ReturnType<typeof captureEmulatorSubmission>>;

export type Family = Awaited<ReturnType<typeof registeredContracts>>;

export const recordMeasurements = (
  name: string,
  kind: VanRossemFitMeasurement["kind"],
  maximumShape: string,
  captured: Captured,
): void => {
  captured.measurements.forEach((measurement, index) => {
    const measurementName =
      captured.measurements.length === 1
        ? name
        : `${name}-${index.toString().padStart(2, "0")}`;
    expect(measurement.l1ByteMargin, measurementName).toBeGreaterThan(0);
    measurements.push({
      name: measurementName,
      kind,
      maximumShape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  });
};

export const ed25519Keypair = (seedByte: number) => {
  const privateKey = createPrivateKey({
    key: Buffer.concat([
      Buffer.from("302e020100300506032b657004220420", "hex"),
      Buffer.alloc(32, seedByte),
    ]),
    format: "der",
    type: "pkcs8",
  });
  const verificationKey = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .subarray(-32);
  return {
    privateKey,
    verificationKey,
    keyHash: missingSignatureVkeyHash(verificationKey.toString("hex")),
  };
};

/** A witness whose signature cannot verify over any transaction id. */
export const garbageWitness = (index: number): Buffer => {
  const verificationKey = Buffer.alloc(32);
  verificationKey.writeUInt32BE(index + 1, 28);
  return encodeMidgardAddressWitnessItem({
    verificationKey,
    signature: Buffer.alloc(64, 0xff),
  });
};

export const priorLedgerFor = async (
  paymentCredentialHex: string,
  priorTxId: string,
  /** Address header: `0x60` pub-key enterprise, `0x70` script enterprise. */
  addressHeader = 0x60,
) => {
  const priorOutput = encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([addressHeader]),
      Buffer.from(paymentCredentialHex, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const outRefBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from(priorTxId, "hex"),
    outputIndex: 0,
  });
  const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: priorOutput,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outRefBytes, outputMaterial.descriptorCbor);
  const proof = await trie.prove(outRefBytes);
  const priorRoot = Buffer.from(trie.hash).toString("hex");
  return {
    outRefBytes,
    priorRoot,
    resolved: {
      priorRoot,
      transactionId: priorTxId,
      outputIndex: 0,
      descriptorCborHex: outputMaterial.descriptorCbor.toString("hex"),
      outputCborHex: priorOutput.toString("hex"),
      membershipProofCborHex: proof.toCBOR().toString("hex"),
      membershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
    },
  };
};

/** Signs the body-only transaction id with `keypair` and rebuilds the
 * transaction with the given witness items in field 7. */
export const signedNativeTx = ({
  outRefBytes,
  fee,
  witnesses,
}: {
  readonly outRefBytes: Buffer;
  readonly fee: bigint;
  readonly witnesses: (txId: Buffer) => readonly Buffer[];
}): MidgardNativeTxFull => {
  const unsigned = makeNativeTx({ spendInputCbors: [outRefBytes], fee });
  const txId = computeMidgardNativeTxId(unsigned);
  const nativeTx = makeNativeTx({
    spendInputCbors: [outRefBytes],
    fee,
    addrTxWitsPreimageCbor: encodeCbor([...witnesses(txId)]),
  });
  expect(computeMidgardNativeTxId(nativeTx)).toEqual(txId);
  return nativeTx;
};

export const witnessSetCompactHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.spendInputSignerMissing;
  const category = harness.catalogue.categories.spendInputSignerMissing;
  expectRegisteredChainParity({
    registered,
    applied: applySpendInputSignerMissingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const contracts: SpendInputSignerMissingContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { steps, contracts, catalogue: harness.catalogue, category };
};

export const newHarness = async () =>
  makeFaultProofEmulatorHarness({
    contractOptions: {
      realSpendInputSignerMissing: true,
      alwaysFraudProofCatalogue: true,
    },
  });
