import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
  type MidgardNativeScript,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  Proof,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  applyOutputReferenceScriptDecodingScripts,
  OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  type OutputReferenceScriptDecodingContracts,
} from "../../src/output-reference-script-decoding/index.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { type CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import { makeNativeTx } from "./submit-init-emulator-shared.js";

export const network = "Custom" as const;

export const OUTPUT_REFERENCE_CATEGORY_ID = "0000002a";

/** `ledger_output_v1.max_output_canonical_cbor_bytes`: the consensus bound. */
export const OUTPUT_REFERENCE_MAX_OUTPUT_BYTES = 16_384;

export const OUTPUT_REFERENCE_REASON_ARMS = [
  "OutputReferenceScriptMalformed",
  "OutputReferenceScriptNodeLimit",
  "OutputReferenceScriptDepthLimit",
] as const;

export type OutputReferenceReasonArm =
  (typeof OUTPUT_REFERENCE_REASON_ARMS)[number];

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export type Measurement = CompleteSignedTransactionMeasurement;

// ## Output fixtures

const subjectAddress = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 1),
]);

const subjectValue = { lovelace: 2_000_000n, assets: new Map() };

export const signatureScript = (fill = 2): MidgardNativeScript => ({
  type: "sig",
  keyHash: Buffer.alloc(28, fill),
});

/** `all [all [... sig]]` nested `depth` containers deep. */
export const nestedScript = (depth: number): MidgardNativeScript =>
  depth === 0
    ? signatureScript(3)
    : { type: "all", scripts: [nestedScript(depth - 1)] };

/** `all [sig × count]`. */
export const wideScript = (count: number): MidgardNativeScript => ({
  type: "all",
  scripts: Array.from({ length: count }, (_, index) =>
    signatureScript(4 + (index % 200)),
  ),
});

export const outputWithNativeScript = (script: MidgardNativeScript): Buffer =>
  Buffer.from(
    encodeMidgardTxOutput({
      address: subjectAddress,
      value: subjectValue,
      script_ref: {
        language: "NativeCardano",
        scriptBytes: Buffer.alloc(0),
        nativeScript: script,
      },
    }),
  );

/**
 * A canonical output whose reference script is `[0, payload]` for arbitrary
 * payload bytes: encoded as PlutusV3 and re-tagged to the native language, so
 * malformed and empty native payloads reach the descriptor unchanged.
 */
export const outputWithRawNativePayload = (payload: Buffer): Buffer => {
  const output = Buffer.from(
    encodeMidgardTxOutput({
      address: subjectAddress,
      value: subjectValue,
      script_ref: { language: "PlutusV3", scriptBytes: payload },
    }),
  );
  const marker = output.indexOf(Buffer.from("8203", "hex"));
  if (marker < 0) throw new Error("versioned script marker absent");
  output[marker + 1] = 0;
  return output;
};

/** The zero-payload output of exactly `length` bytes (malformed at token 0). */
export const rawNativeOutputOfLength = (length: number): Buffer => {
  for (let payload = length; payload > length - 64; payload -= 1) {
    const candidate = outputWithRawNativePayload(Buffer.alloc(payload, 0));
    if (candidate.length === length) return candidate;
  }
  throw new Error(`no zero-payload output of ${length.toString()} bytes`);
};

/** The widest `all [sig × n]` output that still fits the consensus bound. */
export const maximumWideScriptOutput = (): {
  readonly output: Buffer;
  readonly childCount: number;
} => {
  for (let count = 520; count > 0; count -= 1) {
    const output = outputWithNativeScript(wideScript(count));
    if (output.length <= OUTPUT_REFERENCE_MAX_OUTPUT_BYTES)
      return { output, childCount: count };
  }
  throw new Error("no wide script output fits the bound");
};

export const subjectTransaction = (
  outputs: readonly Buffer[],
  fee = 7n,
): MidgardNativeTxFull =>
  makeNativeTx({ spendInputCbors: [], fee, outputCbors: outputs });

// ## Registered chain

export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.outputReferenceScriptDecoding;
  const category = harness.catalogue.categories.outputReferenceScriptDecoding;
  expectRegisteredChainParity({
    registered,
    applied: applyOutputReferenceScriptDecodingScripts({
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
  const validators = familyStepsFromRegisteredChain(
    registered.steps,
    OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  );
  const contracts: OutputReferenceScriptDecodingContracts = {
    steps: validators,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { validators, contracts, catalogue: harness.catalogue, category };
};

export const makeOutputReferenceHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realOutputReferenceScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  return { harness, ...(await registeredContracts(harness)) };
};

export type OutputReferenceContext = Awaited<
  ReturnType<typeof makeOutputReferenceHarness>
>;

// ## Fit-ledger recorder

export const MAXIMUM_SHAPE =
  "16,384-byte accepted output (zero native payload) with Certified field-2 carriage and eight resumable descriptor windows; widest all-of native script that fits the bound (16-step resumable scans, adjacent chunk windows); nested containers; cancellation from every step; mint and leased removal";

export const createMeasurementRecorder = () => {
  const measurements: VanRossemFitMeasurement[] = [];
  const names = new Set<string>();
  const record = (
    name: string,
    measurement: Measurement,
    { runsScripts = true, maximumShape = MAXIMUM_SHAPE } = {},
  ) => {
    expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    if (runsScripts) {
      expect(measurement.executionMemory, name).toBeGreaterThan(0n);
      expect(measurement.executionSteps, name).toBeGreaterThan(0n);
    }
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "lifecycle",
      maximumShape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  const recordPublication = (stepIndex: number, measurement: Measurement) => {
    const name = `publish-step0${(stepIndex + 1).toString()}`;
    expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "publication",
      maximumShape: "fully applied testnet validator",
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  /** Step 02/04 carriage publications and the certificate mint. */
  const recordCarriage = (
    prefix: string,
    captured: { readonly measurements: readonly Measurement[] },
  ) => {
    const auxiliary = captured.measurements.slice(0, -1);
    auxiliary.forEach((measurement, index) => {
      const last = index === auxiliary.length - 1;
      record(
        last
          ? `${prefix}-carriage-certificate`
          : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
        { runsScripts: last },
      );
    });
  };
  return { measurements, record, recordPublication, recordCarriage };
};

export type MeasurementRecorder = ReturnType<typeof createMeasurementRecorder>;

// ## Blocks

export type AcceptedSubject = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxId: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly canonicalCbor: Buffer;
  readonly txInclusion: SubmitStep01TxInclusion;
};

/** Commits accepted transactions under one fraudulent state-queue block. */
export const commitAcceptedBlock = async (
  { harness, catalogue }: OutputReferenceContext,
  nativeTxs: readonly MidgardNativeTxFull[],
) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const prepared = nativeTxs.map((nativeTx) => {
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    return {
      nativeTx,
      nativeTxId,
      sourceCbor: l2TransactionSourceCbor(nativeTx),
    };
  });
  for (const { nativeTxId, sourceCbor } of prepared)
    await trie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const subjects: AcceptedSubject[] = [];
  for (const { nativeTx, nativeTxId, sourceCbor } of prepared) {
    const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
    subjects.push({
      nativeTx,
      nativeTxId,
      compactCborHex: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
        "hex",
      ),
      witnessSetCompactCborHex: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
      ).toString("hex"),
      canonicalCbor: Buffer.from(encodeMidgardNativeTxCanonical(nativeTx)),
      txInclusion: {
        nativeTxId,
        nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          nativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: sourceCbor,
        transactionsPhasRoot: transactionsRoot,
        txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
    });
  }
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: BigInt(nativeTxs.length),
    },
  });
  return { subjects, setup, transactionsRoot };
};

export type ForcedLeafSpec = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly reason: RejectionReason;
};
