import {
  encodeCbor,
  encodeMidgardVersionedScriptListPreimage,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  type RejectCode,
} from "@al-ft/midgard-validation";
import { nativeScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  applyExecutionSourceScriptDecodingScripts,
  EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  type ExecutionSourceScriptDecodingContracts,
} from "../../src/execution-source-script-decoding/index.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { type CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import {
  makeFaultProofEmulatorHarness,
  network,
} from "./submit-init-emulator-shared.js";

export const EXECUTION_SOURCE_CATEGORY_ID = "00000031";

/** The §5.4 aggregate cap on the field-6 preimage the source item lives in. */
export const EXECUTION_SOURCE_MAX_FIELD_BYTES = 32_768;

export const EXECUTION_SOURCE_REASON_ARMS = [
  "ExecutionNativeScriptMalformed",
  "ExecutionNativeScriptNodeLimit",
  "ExecutionNativeScriptDepthLimit",
] as const;

export type ExecutionSourceReasonArm =
  (typeof EXECUTION_SOURCE_REASON_ARMS)[number];

/** Twin of `rejection_code_of` for this family's three arms. */
export const EXECUTION_SOURCE_REJECTION_CODES: Record<
  ExecutionSourceReasonArm,
  RejectCode
> = {
  ExecutionNativeScriptMalformed: "E_INVALID_FIELD_TYPE",
  ExecutionNativeScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  ExecutionNativeScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
};

export const forcedReason = (arm: ExecutionSourceReasonArm): RejectionReason =>
  ({ [arm]: { execution_index: 0n } }) as never;

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export type Measurement = CompleteSignedTransactionMeasurement;

// ## Script shapes

export const signatureScript = (fill = 2): MidgardNativeScript => ({
  type: "sig",
  keyHash: Buffer.alloc(28, fill),
});

/** `all []`: the trivially satisfied container every fixture can execute. */
export const emptyAllScript = (): MidgardNativeScript => ({
  type: "all",
  scripts: [],
});

/** `all [all [... all []]]` nested `depth` containers deep. */
export const nestedScript = (depth: number): MidgardNativeScript =>
  depth === 0
    ? emptyAllScript()
    : { type: "all", scripts: [nestedScript(depth - 1)] };

/**
 * `any [all [], sig × count]`: satisfied by its first child whatever the
 * signer set, so the canonical machine accepts the execution, while the
 * structural scan still walks every signature node.
 */
export const wideScript = (
  count: number,
  emptyContainers = 0,
): MidgardNativeScript => ({
  type: "any",
  scripts: [
    emptyAllScript(),
    ...Array.from({ length: count }, (_, index) =>
      signatureScript(4 + (index % 200)),
    ),
    ...Array.from({ length: emptyContainers }, emptyAllScript),
  ],
});

const field6Bytes = (script: MidgardNativeScript): number =>
  encodeMidgardVersionedScriptListPreimage([nativeScriptWitness(script)])
    .length;

/**
 * The widest `any [all [], sig × n, all [] × k]` whose field-6 preimage fits
 * the cap: as many signature nodes as fit, then three-byte containers up to
 * the exact cap, so the item spans all nine bounded chunks.
 */
export const maximumWideScript = (): {
  readonly script: MidgardNativeScript;
  readonly childCount: number;
  readonly fieldBytes: number;
} => {
  let low = 1;
  let high = 2_000;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (field6Bytes(wideScript(middle)) <= EXECUTION_SOURCE_MAX_FIELD_BYTES)
      low = middle;
    else high = middle - 1;
  }
  let fill = 0;
  while (
    field6Bytes(wideScript(low, fill + 1)) <= EXECUTION_SOURCE_MAX_FIELD_BYTES
  )
    fill += 1;
  return {
    script: wideScript(low, fill),
    childCount: low + fill + 1,
    fieldBytes: field6Bytes(wideScript(low, fill)),
  };
};

const cborBytesHead = (length: number): Buffer => {
  if (length < 24) return Buffer.from([0x40 | length]);
  if (length <= 0xff) return Buffer.from([0x58, length]);
  const head = Buffer.alloc(3);
  head[0] = 0x59;
  head.writeUInt16BE(length, 1);
  return head;
};

/** The versioned native item `[0, payload]` for arbitrary payload bytes. */
export const rawNativeItem = (payload: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from([0x82, 0x00]),
    cborBytesHead(payload.length),
    payload,
  ]);

export const rawNativePayload = (item: Buffer): Buffer => {
  if (item[0] !== 0x82 || item[1] !== 0x00 || item[2] === undefined)
    throw new Error("raw item is not a tag-0 versioned script");
  const additional = item[2] & 0x1f;
  if (additional < 24) return item.subarray(3, 3 + additional);
  if (additional === 24) return item.subarray(4, 4 + item[3]!);
  if (additional === 25) return item.subarray(5, 5 + item.readUInt16BE(3));
  throw new Error("raw item payload head is unsupported");
};

/** The zero-payload item whose single-item field-6 preimage is `fieldBytes`. */
export const malformedItemOfFieldBytes = (fieldBytes: number): Buffer => {
  for (let payload = fieldBytes; payload > fieldBytes - 16; payload -= 1) {
    const item = rawNativeItem(Buffer.alloc(payload, 0));
    if (encodeCbor([item]).length === fieldBytes) return item;
  }
  throw new Error(
    `no zero-payload item has a ${fieldBytes.toString()}-byte field`,
  );
};

/** The zero-payload item whose field-6 preimage is exactly the cap. */
export const maximumMalformedItem = (): Buffer =>
  malformedItemOfFieldBytes(EXECUTION_SOURCE_MAX_FIELD_BYTES);

// ## Registered chain

export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.executionSourceScriptDecoding;
  const category = harness.catalogue.categories.executionSourceScriptDecoding;
  expectRegisteredChainParity({
    registered,
    applied: applyExecutionSourceScriptDecodingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  expect(category.categoryId).toBe(EXECUTION_SOURCE_CATEGORY_ID);
  const validators = familyStepsFromRegisteredChain(
    registered.steps,
    EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  );
  const contracts: ExecutionSourceScriptDecodingContracts = {
    steps: validators,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  return { validators, contracts, catalogue: harness.catalogue, category };
};

export const makeExecutionSourceHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realExecutionSourceScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  return { harness, ...(await registeredContracts(harness)) };
};

export type ExecutionSourceContext = Awaited<
  ReturnType<typeof makeExecutionSourceHarness>
>;

// ## Fit-ledger recorder

export const MAXIMUM_SHAPE =
  "32,768-byte field-6 preimage: zero-payload malformed item refused at its first token over the two-chunk window; widest any-of native script that fits the cap (16-step resumable scans across nine chunk windows); nested containers through the frame stack; cancellation from every step; mint and leased removal";

export const createMeasurementRecorder = () => {
  const measurements: VanRossemFitMeasurement[] = [];
  const names = new Set<string>();
  const record = (
    name: string,
    measurement: Measurement,
    { maximumShape = MAXIMUM_SHAPE } = {},
  ) => {
    expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
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
  return { measurements, record, recordPublication };
};

export type MeasurementRecorder = ReturnType<typeof createMeasurementRecorder>;

// ## Subjects

export type SubjectItem =
  | { readonly kind: "script"; readonly script: MidgardNativeScript }
  /** The exact versioned item bytes committed in field 6 (`[0, payload]`). */
  | { readonly kind: "raw"; readonly item: Buffer };

/**
 * The canonical machine's own verdict on the transaction: the deterministic
 * trace is rebuilt with whatever verdict and code the replay reports, so no
 * fixture ever asserts a classification the machine did not produce.
 */
export const buildCanonicalTrace = async (
  input: Omit<
    Parameters<typeof buildDeterministicValidationMachineTrace>[0],
    | "expectedVerdict"
    | "expectedRejectionCode"
    | "expectedLedgerOps"
    | "ledgerMutationSteps"
    | "postUtxosRoot"
  > & {
    readonly accepted: {
      readonly expectedLedgerOps: Parameters<
        typeof buildDeterministicValidationMachineTrace
      >[0]["expectedLedgerOps"];
      readonly ledgerMutationSteps: Parameters<
        typeof buildDeterministicValidationMachineTrace
      >[0]["ledgerMutationSteps"];
      readonly postUtxosRoot: string;
    };
  },
) => {
  const { accepted, ...rest } = input;
  const attempt = (verdict: "accepted" | "rejected", code: RejectCode | null) =>
    Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...rest,
        expectedVerdict: verdict,
        expectedRejectionCode: code,
        ...(verdict === "accepted"
          ? accepted
          : {
              expectedLedgerOps: [],
              ledgerMutationSteps: [],
              postUtxosRoot: rest.priorUtxosRoot,
            }),
      }),
    );
  try {
    return {
      verdict: "accepted" as const,
      code: null,
      trace: await attempt("accepted", null),
    };
  } catch (error) {
    const match = /actual=rejected\/([A-Z_]+)/u.exec(
      error instanceof Error ? error.message : String(error),
    );
    if (match?.[1] === undefined) throw error;
    const code = match[1] as RejectCode;
    return {
      verdict: "rejected" as const,
      code,
      trace: await attempt("rejected", code),
    };
  }
};
