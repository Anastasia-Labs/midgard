import {
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  encodeMidgardForcedTxCanonical,
  encodeMidgardNativeScript,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_PROFILE,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { aikenSerialisedPlutusDataBytes } from "@al-ft/midgard-core/plutus-data-cbor";
import { EventKeySchema } from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { encodeData } from "../../../src/index.js";
import { cekBuiltinFailureProgram } from "./cek-builtin-failure-program.js";
import { cekSelectionProgram } from "./cek-selection-program.js";
import { transitionTraceOutRef } from "./header-fixtures.js";
import { honestTraceFor, spendingKeyFor } from "./native-trace-memo.js";
import { makeNativeTx } from "./native-tx.js";
import { outRefCbor } from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { nativeTraceAssets } from "./validation-dispute-fixtures.native-trace-assets.js";
import { signedAddressWitnessesCbor } from "./validation-dispute-fixtures.signature-witnesses.js";

type NativeTransactionTraceParams = {
  readonly now: number;
  readonly addressWitnessCount?: number;
  /** Missing required signers drive the signature rejection fixture. */
  readonly requiredSignerHashes?: readonly string[];
  readonly txOrderSeed: string;
  /**
   * Inline datum attached to the produced output, as Aiken-canonical Plutus
   * data CBOR. Drives the ledger-output-proof traversal through its datum
   * stages (and their scalar/span attestation yields), which a plain
   * address+value output never reaches.
   */
  readonly outputDatumCbor?: Buffer;
  readonly outputLovelace?: bigint;
  readonly assetCount?: number;
  readonly mintAsset?: boolean;
  readonly plutusSelection?: boolean;
  /**
   * The Plutus script also mints the transaction's assets, and its Mint
   * redeemer precedes its Spend redeemer in the witness list. Ledger order,
   * (tag, index) ascending, puts the Spend first.
   */
  readonly cekPlutusMint?: boolean;
  readonly cekProgramLambdaCount?: number;
  readonly cekDataGraph?: boolean;
  readonly redeemerDataCbor?: Uint8Array;
  readonly nativeItemWidth?: number;
  readonly cekDirectBuiltin?: boolean;
  readonly cekBlsFinal?: boolean;
  readonly cekMaximumDirect?: boolean;
  readonly cekSemanticTag?: number;
  readonly cekBuiltinFailureTag?: 12 | 21 | 52 | 82 | 83;
  readonly observerCount?: number;
  readonly rejectAfterPreconditions?: boolean;
  readonly resolveMissingInput?: boolean;
  readonly descriptorMaximum?: boolean;
  readonly scriptSourcesRejection?:
    | "missingRedeemer"
    | "missingObserver"
    | "missingReceive"
    | "unusedRedeemer";
  readonly preconditionsRejection?:
    | "missingIntegrity"
    | "untaggedObservers"
    | "observerOrder";
  readonly cekObserverCount?: number;
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
/**
 * One honest, valid, signed native transaction (one spend, one output) and
 * its accepted deterministic trace, shared by the honest-operator mirror
 * fixture below and the forged-operator-successor fixtures that dispute one
 * of its steps. `txOrderSeed` keeps the forced-event keys of the fixtures
 * distinct.
 */
export const buildNativeTransactionTrace = async (
  params: NativeTransactionTraceParams,
) => {
  const {
    now,
    addressWitnessCount = 1,
    requiredSignerHashes = [],
    txOrderSeed,
    assetCount = 0,
    mintAsset = false,
    plutusSelection = false,
    cekPlutusMint = false,
    cekProgramLambdaCount = 1,
    cekDataGraph = false,
    redeemerDataCbor,
    nativeItemWidth = 0,
    cekDirectBuiltin = false,
    cekBlsFinal = false,
    cekMaximumDirect = false,
    cekSemanticTag,
    cekBuiltinFailureTag,
    observerCount = 0,
    preconditionsRejection,
    rejectAfterPreconditions = false,
    resolveMissingInput = false,
    scriptSourcesRejection,
    descriptorMaximum = false,
    cekObserverCount = 0,
    outputDatumCbor,
    outputLovelace,
  } = params;
  const txOrderId = transitionTraceOutRef(txOrderSeed);
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = spendingKeyFor(params);
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8a);
  const nativeScript = {
    type: "all" as const,
    scripts: Array.from({ length: nativeItemWidth }, () => ({
      type: "all" as const,
      scripts: [],
    })),
  };
  const script = {
    language: "NativeCardano" as const,
    scriptBytes: encodeMidgardNativeScript(nativeScript),
    nativeScript,
  };
  const program = plutusSelection
    ? cekBuiltinFailureTag !== undefined
      ? cekBuiltinFailureProgram(cekBuiltinFailureTag)
      : cekSelectionProgram(
          cekProgramLambdaCount,
          cekDataGraph,
          cekDirectBuiltin,
          cekBlsFinal,
          cekMaximumDirect,
          cekSemanticTag,
        )
    : undefined;
  const plutusScript =
    program === undefined
      ? undefined
      : { language: "PlutusV3" as const, scriptBytes: program.envelopeCbor };
  if (cekPlutusMint && plutusScript === undefined)
    throw new Error("a Plutus mint needs the Plutus selection script");
  const policyId = mintAsset
    ? hashMidgardVersionedScript(script)
    : cekPlutusMint
      ? hashMidgardVersionedScript(plutusScript!)
      : "aa".repeat(28);
  const { txAssets, mintFields } = nativeTraceAssets({
    assetCount,
    policyId,
    mintScript: mintAsset || cekPlutusMint ? script : undefined,
  });
  // 496 full 64-byte chunks and one 8-byte chunk encode to 32,747 bytes.
  // The redeemer field adds 21 bytes, preserving its exact 32,768-byte maximum.
  // A single long definite CBOR byte string is not admissible Plutus Data.
  const maximumDescriptorRedeemer = descriptorMaximum
    ? aikenSerialisedPlutusDataBytes(Buffer.alloc(31752))
    : undefined;
  if (maximumDescriptorRedeemer !== undefined) {
    expect(Data.from(maximumDescriptorRedeemer.toString("hex"))).toBe(
      "00".repeat(31752),
    );
  }
  const redeemerTxWitsPreimageCbor = encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items:
      scriptSourcesRejection === "missingRedeemer"
        ? []
        : [
            ...(scriptSourcesRejection === "unusedRedeemer"
              ? [
                  {
                    purpose: "Mint" as const,
                    index: 0n,
                    redeemerCbor:
                      maximumDescriptorRedeemer ??
                      Buffer.from(Data.void(), "hex"),
                    executionUnits: {
                      memory: 1_000_000_000n,
                      steps: 1_000_000_000n,
                    },
                  },
                ]
              : []),
            ...(cekPlutusMint
              ? [
                  {
                    purpose: "Mint" as const,
                    index: 0n,
                    redeemerCbor: Buffer.from(Data.void(), "hex"),
                    executionUnits: {
                      memory: 1_000_000_000n,
                      steps: 1_000_000_000n,
                    },
                  },
                ]
              : []),
            {
              purpose: "Spend",
              index: 0n,
              redeemerCbor:
                maximumDescriptorRedeemer ??
                redeemerDataCbor ??
                Buffer.from(Data.void(), "hex"),
              executionUnits: {
                memory: 1_000_000_000n,
                steps: cekBlsFinal ? 10_000_000_000n : 1_000_000_000n,
              },
            },
          ],
  });
  if (descriptorMaximum) expect(redeemerTxWitsPreimageCbor.length).toBe(32768);
  const observerScripts = Array.from({ length: observerCount }, (_, i) => {
    const nativeScript = {
      type: "before" as const,
      slot: BigInt(now + 2_000_000 + i),
    };
    return {
      language: "NativeCardano" as const,
      scriptBytes: encodeMidgardNativeScript(nativeScript),
      nativeScript,
    };
  });
  const observerFields =
    observerCount === 0
      ? {}
      : {
          requiredObserverHashes:
            preconditionsRejection === "observerOrder"
              ? observerScripts.map(hashMidgardVersionedScript).sort().reverse()
              : observerScripts.map(hashMidgardVersionedScript).sort(),
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            ...(mintAsset ? [script] : []),
            ...(plutusScript === undefined ? [] : [plutusScript]),
            ...(scriptSourcesRejection === "missingObserver"
              ? []
              : observerScripts),
          ]),
          validityIntervalEnd: BigInt(now + 1_000_000),
        };
  const cekObserverScripts = Array.from(
    { length: cekObserverCount },
    (_, index) => {
      const nativeScript = {
        type: "any" as const,
        scripts: [
          { type: "all" as const, scripts: [] },
          { type: "after" as const, slot: BigInt(index) },
        ],
      };
      return {
        language: "NativeCardano" as const,
        nativeScript,
        scriptBytes: encodeMidgardNativeScript(nativeScript),
      };
    },
  );
  const cekRequiredObserverHashes = cekObserverScripts
    .map(hashMidgardVersionedScript)
    .sort();
  const scriptFields =
    plutusScript === undefined
      ? mintFields
      : {
          ...mintFields,
          requiredObserverHashes: cekRequiredObserverHashes,
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            ...(mintAsset ? [script] : []),
            plutusScript,
            ...cekObserverScripts,
          ]),
          redeemerTxWitsPreimageCbor,
          scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
            midgardFieldCommitment(redeemerTxWitsPreimageCbor),
            ["PlutusV3"],
          ),
        };
  const rejectionFields = rejectAfterPreconditions
    ? { validityIntervalStart: 1n }
    : preconditionsRejection === "missingIntegrity"
      ? { scriptIntegrityHash: EMPTY_NULL_ROOT }
      : preconditionsRejection === "untaggedObservers"
        ? { networkId: 255n }
        : {};
  const spentOutput = encodeMidgardTxOutput({
    address:
      plutusScript === undefined
        ? spendingAddress
        : Buffer.concat([
            Buffer.from([0x70]),
            Buffer.from(hashMidgardVersionedScript(plutusScript), "hex"),
          ]),
    value: {
      lovelace:
        outputLovelace ?? (assetCount > 100 ? 100_000_000n : 10_000_000n),
      assets:
        assetCount === 0 || mintAsset || cekPlutusMint ? new Map() : txAssets,
    },
    ...(outputDatumCbor === undefined
      ? {}
      : { datum: { kind: "inline" as const, cbor: outputDatumCbor } }),
  });
  const producedOutput = encodeMidgardTxOutput({
    address:
      scriptSourcesRejection === "missingReceive"
        ? Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0xaa)])
        : spendingAddress,
    value: {
      lovelace:
        outputLovelace ?? (assetCount > 100 ? 100_000_000n : 10_000_000n),
      assets: assetCount === 0 ? new Map() : txAssets,
    },
    ...(outputDatumCbor === undefined
      ? {}
      : { datum: { kind: "inline" as const, cbor: outputDatumCbor } }),
  });
  if (assetCount === 1304) {
    expect(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: 0,
        outputCbor: producedOutput,
      }).descriptor.cardanoValueSize,
    ).toBe(5000);
  }
  const unsignedTx = makeNativeTx({
    ...scriptFields,
    ...observerFields,
    ...rejectionFields,
    requiredSignerHashes,
    spendInputCbors: [resolveMissingInput ? outRefCbor(0x8b) : spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxId(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    ...scriptFields,
    ...observerFields,
    ...rejectionFields,
    requiredSignerHashes,
    spendInputCbors: [resolveMissingInput ? outRefCbor(0x8b) : spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: signedAddressWitnessesCbor(
      transactionId,
      spendingKey,
      addressWitnessCount,
    ),
  });
  const forcedCanonicalCbor = encodeMidgardForcedTxCanonical(forcedNativeTx);
  const forcedSource =
    deriveMidgardForcedTxProofSourceFromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    submitted_source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const producedOutRef = encodeMidgardSpendInputItem({
    txId: transactionId,
    outputIndex: 0,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spentOutRef },
    buildValidationMachineLedgerInsertOp({
      key: producedOutRef,
      outputCbor: producedOutput,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  const preUtxosRoot = ledgerMutationSteps[0]!.preRoot.toString("hex");
  const accepted =
    preconditionsRejection === undefined &&
    !rejectAfterPreconditions &&
    !resolveMissingInput &&
    scriptSourcesRejection === undefined &&
    cekBuiltinFailureTag === undefined &&
    requiredSignerHashes.length === 0;
  const postUtxosRoot = accepted
    ? ledgerMutationSteps.at(-1)!.postRoot.toString("hex")
    : preUtxosRoot;
  const challengerReplayInput: Parameters<
    typeof buildDeterministicValidationMachineTrace
  >[0] = {
    ...(program === undefined
      ? {}
      : {
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
            ...program.material.values(),
          ]),
        }),
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    eventKeyCbor: encodeData(eventKey, EventKeySchema),
    sourceKind: "forced",
    blockEndTimeMs: now + 1_000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    transactionId,
    canonicalTransactionCbor: forcedCanonicalCbor,
    priorUtxosRoot: preUtxosRoot,
    postUtxosRoot,
    ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
    expectedLedgerOps: accepted ? expectedLedgerOps : [],
    ledgerMutationSteps: accepted ? ledgerMutationSteps : [],
    expectedVerdict: accepted ? "accepted" : "rejected",
    expectedRejectionCode:
      requiredSignerHashes.length > 0
        ? RejectCodes.MissingRequiredWitness
        : cekBuiltinFailureTag !== undefined
          ? RejectCodes.PlutusScriptInvalid
          : scriptSourcesRejection !== undefined
            ? scriptSourcesRejection === "unusedRedeemer"
              ? RejectCodes.InvalidFieldType
              : RejectCodes.MissingRequiredWitness
            : resolveMissingInput
              ? RejectCodes.InputNotFound
              : rejectAfterPreconditions
                ? RejectCodes.ValidityIntervalMismatch
                : preconditionsRejection === undefined
                  ? null
                  : RejectCodes.InvalidFieldType,
  };
  const honestTrace = await honestTraceFor(challengerReplayInput);
  return {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace,
    challengerReplayInput,
    preUtxosRoot,
    postUtxosRoot,
  };
};
