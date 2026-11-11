import {
  appendMidgardValidationMerkleLeaf,
  attachMidgardLedgerOutputProofFacts,
  buildMidgardBlake2b224Trace,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputProofTrace,
  buildMidgardRedeemerDataUnreadableTrace,
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  encodeCbor,
  encodeMidgardBlake2b224TraceControl,
  encodeMidgardLedgerOutputProofControl,
  hashMidgardMintAssetLeaf,
  hashMidgardOutputDescriptorLeaf,
  hashMidgardOutputItemLeaf,
  hashMidgardRedeemerItemLeaf,
  hashMidgardRedeemerItemProofControl,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardResolvedContextItemLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardScriptSourceLeaf,
  hashMidgardSignerLeaf,
  inspectMidgardRedeemerData,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBlake2b,
  type MidgardBlake2b224TraceControl,
  MidgardBlake2b224TraceStages,
  midgardBoundedItemChunkCount,
  type MidgardBoundedItemChunkProof,
  type MidgardLedgerOutputProofControl,
  midgardLedgerOutputProofFactsComplete,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  midgardScriptSourcePhysicalIndex,
  type MidgardValidationMerkleFrontier,
  type MidgardValidationPhaseName,
  traceMidgardLedgerOutputProof,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeSingleCbor,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
} from "@al-ft/midgard-core/codec/cbor";
import { Effect } from "effect";

import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
} from "../ledger-output-descriptor.js";
import {
  initialInputSetMergeControl,
  inputSetMergeStep,
} from "../phase-a.input-set-merge.js";
import { UNSET_REASON_COORDINATE } from "../rejection-reason.js";
import {
  canonicalCborArgumentHeaderSize,
  MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
  MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
} from "./canonical-field-item.js";
import { canonicalOrderedItemEncodedLength } from "./canonical-field-order.js";
import {
  encodeScriptDiscoveryControlCbor,
  encodeValidationControlList,
  encodeValidationFrontierPeaks,
  type ScriptDiscoveryTraceControl,
} from "./control-encoding.js";
import { countedMachineFieldTrace } from "./field-carriage.js";
import {
  emptyMidgardInputResolutionSchedule,
  noExactLedgerDescriptor,
  prependMidgardInputResolutionSchedule,
} from "./input-resolution.js";
import {
  hashValidationMachineNativeScriptFrame,
  MAX_NATIVE_SCRIPT_SCAN_DEPTH,
  MAX_NATIVE_SCRIPT_SCAN_NODES,
  readValidationMachineNativeScriptPayload,
  readValidationMachineNativeScriptTokenHead,
  readValidationMachineVersionedScriptHeader,
  type ValidationMachineNativeScriptFrame,
  type ValidationMachineNativeScriptToken,
  type ValidationMachineNativeScriptTokenHead,
  type ValidationMachineVersionedScriptHeader,
} from "./native-script-frame.js";
import {
  preparedFieldPreimage,
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import { redeemerPointerMatchesPurpose } from "./redeemer-purpose.js";
import { refusedOutputStopReason } from "./trace-builder-prepare.refused-output-stop-reason.js";
import type {
  MintFoldTraceControl,
  PhaseANativeScriptsScanControl,
  ScriptExecutionProofEntry,
  ScriptPurposeProofEntry,
  ScriptSourceProofEntry,
  SignatureScanControl,
  ValidationTraceWalkState,
} from "./trace-builder-types.js";
import { phaseContinues, phaseStops, type TraceWalk } from "./trace-walk.js";
import {
  type ValidationMachineSignerSetProof,
  type ValidationMachineWorkWitness,
} from "./types.js";

/** `source` as the bounded item of its origin field, checked against the
 * descriptor facts it was authenticated with. */
export const boundedItemForScriptSource = (source: ScriptSourceProofEntry) => {
  // The two origin kinds carry two different keys, and each has exactly one
  // decoder. An inline key is a bare canonical CBOR index; a reference key
  // *is* the ledger out-ref, i.e. §5.3's fixed-index 38-byte item, whose
  // `19 0000` head a minimal-CBOR reader rejects — so it goes through the
  // §5.3 twin, never `decodeSingleCbor`. See `docs/spec/midgard-tx.md` §5.3.
  const itemIndexValue =
    source.originKind === "inline"
      ? decodeSingleCbor(source.sourceKey)
      : decodeMidgardSpendInputItem(source.sourceKey).outputIndex;
  const itemIndex =
    typeof itemIndexValue === "number"
      ? itemIndexValue
      : typeof itemIndexValue === "bigint" &&
          itemIndexValue <= BigInt(Number.MAX_SAFE_INTEGER)
        ? Number(itemIndexValue)
        : -1;
  if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
    throw new Error("V1 script source has a noncanonical item index");
  }
  const item = buildMidgardBoundedItem({
    fieldIndex:
      source.originKind === "inline" ? MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX : 2,
    itemIndex,
    bytes: source.authenticatedVersionedItemBytes,
  });
  if (
    item.bytes.length !== source.scriptTotalLength ||
    !item.commitment.equals(source.scriptItemCommitment)
  ) {
    throw new Error(
      "V1 script source bytes disagree with authenticated descriptor facts",
    );
  }
  return item;
};

/** The NativeScripts scan witness at `control`; a late execution scan
 * passes its NativeScripts continuation. */
export const phaseANativeScriptsScanWitnessCbor = (
  {
    proofSource,
    contextCbor,
    resolutionScheduleHash,
    signerFrontier,
  }: PreparedValidationTransaction,
  control: PhaseANativeScriptsScanControl,
  continuationCbor: Buffer = Buffer.alloc(0),
): Buffer =>
  encodeValidationControlList([
    proofSource.compactCbor,
    proofSource.witnessSetCompactCbor,
    proofSource.fieldPreimageLengthsCbor,
    contextCbor,
    // A late execution scan binds its NativeScripts continuation here.
    continuationCbor.length === 0
      ? resolutionScheduleHash
      : Buffer.from(midgardBlake2b(continuationCbor, { dkLen: 32 })),
    BigInt(control.stage),
    BigInt(control.scriptCount),
    BigInt(control.scriptSeen),
    BigInt(control.containsNonNativeScript),
    BigInt(control.itemLength),
    control.itemCommitment,
    BigInt(control.cursor),
    control.stackRoot,
    BigInt(control.stackDepth),
    BigInt(control.nodeCount),
    BigInt(control.result),
    BigInt(signerFrontier.count),
    [encodeValidationFrontierPeaks(signerFrontier), continuationCbor],
    control.purposeKind,
    control.purposeIndex,
  ]);

export const prepareValidationTrace = (
  prepared: PreparedValidationTransaction,
  walk: TraceWalk,
): Effect.Effect<ValidationTraceWalkState, Error> =>
  Effect.gen(function* () {
    const {
      input,
      contextCbor,
      phaseALedgerTx,
      ledgerState,
      ledgerDescriptorState,
      proofSource,
      compactProofTransaction,
      fieldPreimages,
      spendInputsCollection,
      referenceInputsCollection,
      outputsCollection,
      requiredObserversCollection,
      requiredSignersCollection,
      mintCollection,
      scriptWitnessesCollection,
      addressWitnessesCollection,
      redeemerWitnessesCollection,
      resolutionScheduleNodes,
      resolutionScheduleHash,
      transactionContextWitnessCbor,
      sourceContextWitnessCbor,
      addressWitnessScanItems,
      signerFrontier,
      signerFrontierCommitment,
      inlineScriptSourceLeafHashes,
      inlineScriptSourceFrontier,
      outputCbors,
      outputLeafHashes,
      outputFrontier,
      decodedProofRedeemers,
      canonicalRedeemerWitnessCbors,
      redeemerLeafHashes,
      redeemerFrontier,
      emptyValidationFrontier,
    } = prepared;
    const { push: pushWitness } = walk;
    const fieldPreimage = (fieldIndex: number): Buffer =>
      preparedFieldPreimage(prepared, fieldIndex);
    const signerProofForHash = (signerHash: Buffer) =>
      preparedSignerProof(prepared, signerHash);
    const scriptSourceEntries: ScriptSourceProofEntry[] = [
      ...prepared.inlineScriptSourceEntries,
    ];
    const scriptPurposeEntries: ScriptPurposeProofEntry[] = [];
    const scriptExecutionEntries: ScriptExecutionProofEntry[] = [];
    const outputMembership = (outputIndex: number) =>
      buildMidgardValidationMerkleMembership(outputLeafHashes, outputIndex);
    const admittedOutputDescriptorCbors: Buffer[] = [];
    const admittedOutputDescriptorLeafHashes: Buffer[] = [];
    const encodeFrontierPeaks = encodeValidationFrontierPeaks;
    const inputSetsWitnessCbor = (control: {
      readonly spendCount: number;
      readonly referenceCount: number;
      readonly spendSeen: number;
      readonly referenceSeen: number;
      readonly lastKey: Uint8Array;
      readonly lastField: number;
      readonly lastItem: number;
      readonly cachedOtherHead: Uint8Array;
      readonly resolutionScheduleHash: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(control.spendCount),
        BigInt(control.referenceCount),
        BigInt(control.spendSeen),
        BigInt(control.referenceSeen),
        Buffer.from(control.lastKey),
        BigInt(control.lastField),
        BigInt(control.lastItem),
        Buffer.from(control.cachedOtherHead),
        control.resolutionScheduleHash,
      ]);
    const signaturesScanWitnessCbor = (control: SignatureScanControl): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.addressCount),
        BigInt(control.requiredCount),
        BigInt(control.addressSeen),
        BigInt(control.requiredSeen),
        control.previousSignerHash,
        BigInt(control.signerFrontier.count),
        encodeFrontierPeaks(control.signerFrontier),
        BigInt(control.firstInvalidWitnessIndex),
      ]);
    const initialSignatureScanControl: SignatureScanControl = {
      stage: 0,
      addressCount: addressWitnessesCollection.items.length === 0 ? 0 : -1,
      requiredCount: requiredSignersCollection.items.length === 0 ? 0 : -1,
      addressSeen: 0,
      requiredSeen: 0,
      previousSignerHash: Buffer.alloc(0),
      signerFrontier: emptyValidationFrontier,
      firstInvalidWitnessIndex: -1,
    };
    const resetPhaseANativeScriptsScanControl = (input: {
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
    }): PhaseANativeScriptsScanControl => ({
      stage: 0,
      scriptCount: input.scriptCount,
      scriptSeen: input.scriptSeen,
      containsNonNativeScript: input.containsNonNativeScript,
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      cursor: 0,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: 0,
      result: -1,
      purposeKind: -1n,
      purposeIndex: -1n,
    });
    const initialPhaseANativeScriptsScanControl =
      resetPhaseANativeScriptsScanControl({
        scriptCount: scriptWitnessesCollection.items.length === 0 ? 0 : -1,
        scriptSeen: 0,
        containsNonNativeScript: 0,
      });
    let resolvedItemFrontier = emptyValidationFrontier;
    const emptyMintFoldControl: MintFoldTraceControl = {
      policyCount: -1,
      policyCursor: 0,
      previousPolicy: Buffer.alloc(0),
      activePolicy: Buffer.alloc(0),
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      itemCursor: 0,
      assetsRemaining: 0,
      policyAssetCursor: 0,
      previousAsset: Buffer.alloc(0),
      assetFrontier: emptyValidationFrontier,
    };
    let mintFoldControl = emptyMintFoldControl;
    const encodeMintFoldControl = (
      control: MintFoldTraceControl,
    ): readonly unknown[] => [
      BigInt(control.policyCount),
      BigInt(control.policyCursor),
      control.previousPolicy,
      control.activePolicy,
      BigInt(control.itemLength),
      control.itemCommitment,
      BigInt(control.itemCursor),
      BigInt(control.assetsRemaining),
      BigInt(control.policyAssetCursor),
      control.previousAsset,
      BigInt(control.assetFrontier.count),
      encodeFrontierPeaks(control.assetFrontier),
    ];
    const emptyScriptDiscoveryControl: ScriptDiscoveryTraceControl = {
      purposeCursor: 0,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: -1,
      currentPurposeIndex: -1n,
      currentScriptHash: Buffer.alloc(0),
      currentSubject: Buffer.alloc(0),
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
      usedInlineBitmap: 0n,
      usedRedeemerBitmap: 0n,
      executionFrontier: emptyValidationFrontier,
      redeemerItemControlHash: Buffer.alloc(0),
    };
    const scriptDiscoveryControlCbor = encodeScriptDiscoveryControlCbor;
    const scriptSourcesWitnessCbor = (input: {
      readonly resolvedInputCount: number;
      readonly stage: number;
      readonly sourceFrontier: MidgardValidationMerkleFrontier;
      readonly redeemerFrontier: MidgardValidationMerkleFrontier;
      readonly replayCursor?: number;
      readonly replayRemainingScheduleHash?: Buffer;
      readonly spendIndex?: number;
      readonly purposeFrontier?: MidgardValidationMerkleFrontier;
      readonly outputCursor?: number;
      readonly outputFrontier?: MidgardValidationMerkleFrontier;
      readonly outputTotalCount?: number;
      readonly receiveScan?: {
        readonly sourceFrontier: MidgardValidationMerkleFrontier;
        readonly receiveCount: number;
        readonly previousHash: Buffer;
        readonly candidateHash: Buffer;
        readonly descriptorFrontier: MidgardValidationMerkleFrontier;
      };
      readonly inlineSourceCount?: number;
      readonly redeemerTotalCount?: number;
      readonly observerScan?: {
        readonly totalCount: number;
        readonly seen: number;
        readonly previousHash: Buffer;
      };
      readonly outputProof?: MidgardLedgerOutputProofControl | null;
      readonly discovery?: ScriptDiscoveryTraceControl;
      readonly pendingSource?: {
        readonly sourceIndex: number;
        readonly inlineSourceCount: number;
        readonly languageTag: 0 | 3 | 128;
        readonly payloadOffset: number;
        readonly payloadLength: number;
        readonly itemLength: number;
        readonly itemCommitment: Buffer;
        readonly hashControl: MidgardBlake2b224TraceControl;
      } | null;
      readonly redeemerItemControlHash?: Buffer;
    }): Buffer => {
      const observerScan = input.observerScan ?? {
        totalCount: 0,
        seen: 0,
        previousHash: Buffer.alloc(0),
      };
      const receiveScan = input.receiveScan ?? {
        sourceFrontier: emptyValidationFrontier,
        receiveCount: 0,
        previousHash: Buffer.alloc(0),
        candidateHash: Buffer.alloc(0),
        descriptorFrontier: emptyValidationFrontier,
      };
      const fields: unknown[] = [
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(input.resolvedInputCount),
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        encodeFrontierPeaks(resolvedItemFrontier),
        BigInt(input.stage),
        BigInt(input.sourceFrontier.count),
        encodeFrontierPeaks(input.sourceFrontier),
        BigInt(input.redeemerFrontier.count),
        encodeFrontierPeaks(input.redeemerFrontier),
        BigInt(input.replayCursor ?? 0),
        input.replayRemainingScheduleHash ??
          emptyMidgardInputResolutionSchedule(),
        BigInt(input.spendIndex ?? 0),
        BigInt(input.purposeFrontier?.count ?? 0),
        encodeFrontierPeaks(input.purposeFrontier ?? emptyValidationFrontier),
        BigInt(input.outputCursor ?? 0),
        BigInt(input.outputFrontier?.count ?? 0),
        encodeFrontierPeaks(input.outputFrontier ?? emptyValidationFrontier),
        BigInt(input.outputTotalCount ?? input.outputFrontier?.count ?? 0),
        [
          BigInt(receiveScan.sourceFrontier.count),
          encodeFrontierPeaks(receiveScan.sourceFrontier),
          BigInt(receiveScan.receiveCount),
          receiveScan.previousHash,
          receiveScan.candidateHash,
          encodeFrontierPeaks(receiveScan.descriptorFrontier),
        ],
        BigInt(input.inlineSourceCount ?? inlineScriptSourceFrontier.count),
        BigInt(input.redeemerTotalCount ?? input.redeemerFrontier.count),
        [
          BigInt(observerScan.totalCount),
          observerScan.previousHash,
          BigInt(observerScan.seen),
        ],
        encodeMintFoldControl(mintFoldControl),
        resolutionScheduleHash,
      ];
      if (
        input.stage === 0 &&
        input.pendingSource !== undefined &&
        input.pendingSource !== null
      ) {
        fields.push(
          encodeCbor([
            1n,
            BigInt(input.pendingSource.sourceIndex),
            BigInt(input.pendingSource.inlineSourceCount),
            BigInt(input.pendingSource.languageTag),
            BigInt(input.pendingSource.payloadOffset),
            BigInt(input.pendingSource.payloadLength),
            BigInt(input.pendingSource.itemLength),
            input.pendingSource.itemCommitment,
            encodeMidgardBlake2b224TraceControl(
              input.pendingSource.hashControl,
            ),
          ]),
        );
      } else if (
        input.stage === 1 &&
        input.redeemerItemControlHash !== undefined &&
        input.redeemerItemControlHash.length > 0
      ) {
        fields.push(input.redeemerItemControlHash);
      } else if (
        input.stage === 5 &&
        input.outputProof !== undefined &&
        input.outputProof !== null
      ) {
        fields.push(encodeMidgardLedgerOutputProofControl(input.outputProof));
      } else if (input.stage >= 8) {
        fields.push(
          scriptDiscoveryControlCbor(
            input.discovery ?? emptyScriptDiscoveryControl,
          ),
        );
      }
      return encodeCbor(fields);
    };
    const signerSetProof = (
      sourceKind: "spend" | "reference",
      value: Buffer | null,
    ): ValidationMachineSignerSetProof => {
      if (sourceKind === "reference" || value === null) {
        return { kind: "none" };
      }
      let signerHash: Buffer;
      try {
        const output = decodeMidgardTxOutput(value);
        const credential = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;
        if (credential.kind === "Script") return { kind: "none" };
        signerHash = Buffer.from(credential.hash);
      } catch {
        return { kind: "none" };
      }
      return signerProofForHash(signerHash);
    };
    const protectedOutputSignerProof = (
      outputCbor: Buffer,
    ): ValidationMachineSignerSetProof => {
      const output = decodeMidgardTxOutput(outputCbor);
      const address = decodeMidgardAddressBytes(output.address);
      if (!address.protected || address.paymentCredential.kind === "Script") {
        return { kind: "none" };
      }
      return signerProofForHash(Buffer.from(address.paymentCredential.hash));
    };
    const phaseAScriptPreconditionsWitnessCbor = (control: {
      readonly containsNonNativeScript: 0 | 1;
      readonly observerCount: number;
      readonly observerSeen: number;
      readonly previousObserver: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        BigInt(control.containsNonNativeScript),
        control.previousObserver,
        BigInt(control.observerCount),
        BigInt(control.observerSeen),
      ]);
    const macroWitnessByPhase = new Map<MidgardValidationPhaseName, Buffer>([
      [
        "compactBinding",
        encodeCbor([
          input.transactionId,
          walk.frame.transactionCommitment,
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
        ]),
      ],
      ["staticLedgerRules", sourceContextWitnessCbor],
      ["valueAndMint", transactionContextWitnessCbor],
      ["nativeScripts", transactionContextWitnessCbor],
      ["scriptIntegrity", transactionContextWitnessCbor],
      ["cek", transactionContextWitnessCbor],
    ]);
    const macroAuxiliaryByPhase = new Map<
      MidgardValidationPhaseName,
      ValidationMachineWorkWitness["auxiliary"]
    >([]);

    const macroWitness = (phase: MidgardValidationPhaseName): Buffer => {
      const witness = macroWitnessByPhase.get(phase);
      if (witness === undefined) {
        throw new Error(`missing macro witness for ${phase}`);
      }
      return witness;
    };
    const macroAuxiliary = (
      phase: MidgardValidationPhaseName,
    ): ValidationMachineWorkWitness["auxiliary"] =>
      macroAuxiliaryByPhase.get(phase) ?? null;

    let authenticatedNativeScriptsWitnessCbor: Buffer | null = null;
    let authenticatedNativeScriptsBaseFields: unknown[] | null = null;
    yield* walk.phase("canonicalDecode", () =>
      Effect.sync(() => {
        for (const field of fieldPreimages) {
          const collection = countedMachineFieldTrace(
            field.fieldIndex,
            field.preimageCbor,
          );
          if (collection.items.length === 0) {
            pushWitness(
              "canonicalDecode",
              encodeCbor([
                proofSource.compactCbor,
                proofSource.witnessSetCompactCbor,
                proofSource.fieldPreimageLengthsCbor,
                contextCbor,
                BigInt(field.fieldIndex),
                0n,
                0n,
                -1n,
                0n,
              ]),
            );
            continue;
          }
          let itemCount = -1;
          let encodedLength = 0;
          for (const item of collection.items) {
            pushWitness(
              "canonicalDecode",
              encodeCbor([
                proofSource.compactCbor,
                proofSource.witnessSetCompactCbor,
                proofSource.fieldPreimageLengthsCbor,
                contextCbor,
                BigInt(field.fieldIndex),
                BigInt(item.itemIndex),
                0n,
                BigInt(itemCount),
                BigInt(encodedLength),
              ]),
              {
                kind: "transactionFieldItem",
                fieldIndex: field.fieldIndex,
                fieldPreimage: fieldPreimage(field.fieldIndex),
              },
            );
            if (itemCount === -1) {
              itemCount = collection.items.length;
              encodedLength = canonicalCborArgumentHeaderSize(itemCount);
            }
            const itemEncodedLength = canonicalOrderedItemEncodedLength(
              field.fieldIndex,
              collection.items,
              item.itemIndex,
            );
            if (itemEncodedLength === null) {
              return phaseStops(
                {
                  FieldItemWidthIllegal: {
                    field_index: BigInt(field.fieldIndex),
                    item_index: BigInt(item.itemIndex),
                  },
                },
                "canonical decode found an item of illegal width",
              );
            }
            encodedLength += itemEncodedLength;
          }
        }
        return phaseContinues;
      }),
    );
    for (const phase of ["compactBinding", "staticLedgerRules"] as const) {
      yield* walk.phase(phase, () =>
        Effect.sync(() => {
          pushWitness(phase, macroWitness(phase), macroAuxiliary(phase));
          return phaseContinues;
        }),
      );
    }

    yield* walk.phase("inputSets", () =>
      Effect.gen(function* () {
        const mergeFields = [
          spendInputsCollection.items.map((item) => item.bytes),
          referenceInputsCollection.items.map((item) => item.bytes),
        ] as const;
        const mergeCollections = [
          spendInputsCollection,
          referenceInputsCollection,
        ] as const;
        let mergeControl = initialInputSetMergeControl(mergeFields);
        let inputScheduleHash = emptyMidgardInputResolutionSchedule();
        const currentInputSetsWitness = (): Buffer =>
          inputSetsWitnessCbor({
            ...mergeControl,
            resolutionScheduleHash: inputScheduleHash,
          });

        if (mergeControl.spendCount === 0) {
          pushWitness("inputSets", currentInputSetsWitness());
          return phaseStops(
            "EmptyInputs",
            "bounded input scan found no spend inputs",
          );
        } else {
          // The descending merge of fields 0 and 1 (`phase-a.input-set-merge.ts`):
          // the control fixes the item each step opens, so the auxiliary is the
          // opened field's carriage alone.
          for (;;) {
            const step = inputSetMergeStep(mergeControl, mergeFields);
            const fieldIndex =
              mergeCollections[step.opened.fieldIndex]!.fieldIndex;
            pushWitness("inputSets", currentInputSetsWitness(), {
              kind: "transactionFieldItem",
              fieldIndex,
              fieldPreimage: fieldPreimage(fieldIndex),
            });
            if (step.kind === "duplicate") {
              return phaseStops(
                {
                  DuplicateInput: {
                    first_field_index: UNSET_REASON_COORDINATE,
                    first_item_index: UNSET_REASON_COORDINATE,
                    second_field_index: UNSET_REASON_COORDINATE,
                    second_item_index: UNSET_REASON_COORDINATE,
                  },
                },
                "bounded input scan found a duplicate",
              );
            }
            mergeControl = step.next;
            if (step.kind === "consume") {
              inputScheduleHash = prependMidgardInputResolutionSchedule({
                sourceKind:
                  step.consumed.fieldIndex === 0 ? "spend" : "reference",
                itemIndex: step.consumed.itemIndex,
                key: Buffer.from(step.key),
                nextHash: inputScheduleHash,
              });
              if (step.complete) break;
            }
          }
          if (!inputScheduleHash.equals(resolutionScheduleHash)) {
            return yield* Effect.fail(
              new Error(
                `bounded input scan schedule ${inputScheduleHash.toString("hex")} differs from committed ${resolutionScheduleHash.toString("hex")}`,
              ),
            );
          }
        }
        return phaseContinues;
      }),
    );

    yield* walk.phase("signatures", () =>
      Effect.gen(function* () {
        let signatureControl = initialSignatureScanControl;
        const pushSignatureWitness = (
          auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
        ): void => {
          pushWitness(
            "signatures",
            signaturesScanWitnessCbor(signatureControl),
            auxiliary,
          );
        };
        if (signatureControl.addressCount === 0) {
          pushSignatureWitness();
          signatureControl = { ...signatureControl, stage: 1 };
        } else {
          for (
            let index = 0;
            index < addressWitnessScanItems.length;
            index += 1
          ) {
            const scan = addressWitnessScanItems[index]!;
            // The cursor opens the item at `addressSeen`.
            if (scan.item.itemIndex !== signatureControl.addressSeen) {
              return yield* Effect.fail(
                new Error("address-witness scan left physical order"),
              );
            }
            pushSignatureWitness({
              kind: "transactionFieldItem",
              fieldIndex: MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
              fieldPreimage: fieldPreimage(
                MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
              ),
            });
            const newSigner = !scan.decoded.signerHash.equals(
              signatureControl.previousSignerHash,
            );
            const signerFrontier = newSigner
              ? appendMidgardValidationMerkleLeaf(
                  signatureControl.signerFrontier,
                  hashMidgardSignerLeaf(scan.decoded.signerHash),
                )
              : signatureControl.signerFrontier;
            // The ledger's own check, as Phase A runs it. A failure of the
            // check itself is raised and stops the build.
            const signatureIsValid = verifyLedgerEd25519Signature(
              scan.decoded.verificationKey,
              input.transactionId,
              scan.decoded.signature,
            );
            const addressSeen = signatureControl.addressSeen + 1;
            const addressCount =
              signatureControl.addressCount === -1
                ? addressWitnessesCollection.items.length
                : signatureControl.addressCount;
            signatureControl =
              addressSeen === addressCount
                ? {
                    ...signatureControl,
                    stage: 1,
                    addressCount,
                    addressSeen,
                    previousSignerHash: Buffer.alloc(0),
                    signerFrontier,
                    firstInvalidWitnessIndex:
                      signatureControl.firstInvalidWitnessIndex === -1 &&
                      !signatureIsValid
                        ? scan.item.itemIndex
                        : signatureControl.firstInvalidWitnessIndex,
                  }
                : {
                    ...signatureControl,
                    addressCount,
                    addressSeen,
                    previousSignerHash: scan.decoded.signerHash,
                    signerFrontier,
                    firstInvalidWitnessIndex:
                      signatureControl.firstInvalidWitnessIndex === -1 &&
                      !signatureIsValid
                        ? scan.item.itemIndex
                        : signatureControl.firstInvalidWitnessIndex,
                  };
          }
        }
        if (signatureControl.stage !== 1) {
          return yield* Effect.fail(
            new Error("address-witness scan did not reach required signers"),
          );
        }
        if (signatureControl.requiredCount === 0) {
          pushSignatureWitness();
          if (signatureControl.firstInvalidWitnessIndex >= 0) {
            return phaseStops(
              {
                AddressWitnessSignatureInvalid: {
                  witness_index: BigInt(
                    signatureControl.firstInvalidWitnessIndex,
                  ),
                },
              },
              "signature scan found an invalid signature",
            );
          } else {
            signatureControl = { ...signatureControl, stage: 2 };
          }
        } else {
          for (
            let index = 0;
            index < requiredSignersCollection.items.length;
            index += 1
          ) {
            const item = requiredSignersCollection.items[index]!;
            const signerProof = signerProofForHash(item.bytes);
            pushSignatureWitness({
              kind: "requiredSignerItem",
              // No field or item index on the wire: the field is 4 by
              // construction and the item index is `control.required_seen`.
              fieldIndex: 4,
              fieldPreimage: fieldPreimage(4),
              signerProof,
            });
            if (signerProof.kind !== "membership") {
              return phaseStops(
                {
                  RequiredSignerUnsigned: {
                    signer_index: BigInt(signatureControl.requiredSeen),
                  },
                },
                "required signer is absent",
              );
            }
            const requiredSeen = signatureControl.requiredSeen + 1;
            const requiredCount =
              signatureControl.requiredCount === -1
                ? requiredSignersCollection.items.length
                : signatureControl.requiredCount;
            signatureControl = {
              ...signatureControl,
              requiredCount,
              requiredSeen,
            };
            if (
              requiredSeen === requiredCount &&
              signatureControl.firstInvalidWitnessIndex >= 0
            ) {
              return phaseStops(
                {
                  AddressWitnessSignatureInvalid: {
                    witness_index: BigInt(
                      signatureControl.firstInvalidWitnessIndex,
                    ),
                  },
                },
                "signature scan found an invalid signature",
              );
            }
            if (requiredSeen === requiredCount) {
              signatureControl = { ...signatureControl, stage: 2 };
            }
          }
        }
        if (signatureControl.stage !== 2) {
          return yield* Effect.fail(
            new Error("required-signer scan did not reach its handoff"),
          );
        }
        pushSignatureWitness();
        return phaseContinues;
      }),
    );

    let phaseANativeControl = initialPhaseANativeScriptsScanControl;
    yield* walk.phase("phaseANativeScripts", () =>
      Effect.sync(() => {
        const nativeScriptFrames: ValidationMachineNativeScriptFrame[] = [];
        const phaseANativeFault = (): string =>
          `bounded native-script scan stopped at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor}`;
        const pushPhaseANativeWitness = (
          auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
        ): void => {
          pushWitness(
            "phaseANativeScripts",
            phaseANativeScriptsScanWitnessCbor(prepared, phaseANativeControl),
            auxiliary,
          );
        };

        if (phaseANativeControl.scriptCount === 0) {
          pushPhaseANativeWitness();
        } else {
          for (const item of scriptWitnessesCollection.items) {
            const activeScriptCount =
              phaseANativeControl.scriptCount === -1
                ? scriptWitnessesCollection.items.length
                : phaseANativeControl.scriptCount;
            pushPhaseANativeWitness({
              kind: "transactionFieldChunk",
              fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
              itemIndex: item.itemIndex,
              fieldPreimage: fieldPreimage(
                MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
              ),
            });

            let header: ValidationMachineVersionedScriptHeader;
            try {
              header = readValidationMachineVersionedScriptHeader(item.bytes);
            } catch {
              return phaseStops(
                {
                  WitnessScriptHeaderMalformed: {
                    script_index: BigInt(phaseANativeControl.scriptSeen),
                  },
                },
                phaseANativeFault(),
              );
            }

            if (header.languageTag !== 0) {
              phaseANativeControl = resetPhaseANativeScriptsScanControl({
                scriptCount: activeScriptCount,
                scriptSeen: phaseANativeControl.scriptSeen + 1,
                containsNonNativeScript: 1,
              });
              continue;
            }

            phaseANativeControl = {
              ...phaseANativeControl,
              stage: 1,
              scriptCount: activeScriptCount,
              itemLength: item.bytes.length,
              itemCommitment: item.commitment,
              cursor: header.payloadOffset,
            };
            for (;;) {
              if (phaseANativeControl.stage === 1) {
                const chunkCount = midgardBoundedItemChunkCount(
                  item.bytes.length,
                );
                // An exhausted cursor authenticates an empty parser window on
                // chain. Carry the real last chunk; there is no chunk after it.
                const chunkIndex =
                  phaseANativeControl.cursor === item.bytes.length
                    ? chunkCount - 1
                    : Math.floor(
                        phaseANativeControl.cursor /
                          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
                      );
                let head: ValidationMachineNativeScriptTokenHead | null = null;
                try {
                  head = readValidationMachineNativeScriptTokenHead(
                    item.bytes,
                    phaseANativeControl.cursor,
                  );
                } catch {
                  // The authenticated token witness still proves the exact
                  // malformed bytes to the one-step resolver.
                }
                pushPhaseANativeWitness({
                  kind: "nativeScriptToken",
                  chunkProof: buildMidgardBoundedItemChunkProof(
                    item,
                    chunkIndex,
                  ),
                  nextChunkProof:
                    chunkIndex + 1 < chunkCount
                      ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                      : null,
                  signerProof: { kind: "none" },
                });
                if (head === null) {
                  return phaseStops(
                    {
                      WitnessNativeScriptMalformed: {
                        script_index: BigInt(phaseANativeControl.scriptSeen),
                      },
                    },
                    phaseANativeFault(),
                  );
                }

                const nextNodeCount = phaseANativeControl.nodeCount + 1;
                if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES) {
                  return phaseStops(
                    {
                      WitnessNativeScriptNodeLimit: {
                        script_index: BigInt(phaseANativeControl.scriptSeen),
                      },
                    },
                    phaseANativeFault(),
                  );
                }

                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                  cursor: head.payloadOffset,
                  nodeCount: nextNodeCount,
                };
                continue;
              }

              if (phaseANativeControl.stage >= 3) {
                const kind = (phaseANativeControl.stage - 3) as
                  | 0
                  | 1
                  | 2
                  | 3
                  | 4
                  | 5;
                const chunkCount = midgardBoundedItemChunkCount(
                  item.bytes.length,
                );
                // An exhausted cursor authenticates an empty parser window on
                // chain. Carry the real last chunk; there is no chunk after it.
                const chunkIndex =
                  phaseANativeControl.cursor === item.bytes.length
                    ? chunkCount - 1
                    : Math.floor(
                        phaseANativeControl.cursor /
                          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
                      );
                let token: ValidationMachineNativeScriptToken | null = null;
                let payloadParseFailure = "none";
                try {
                  token = readValidationMachineNativeScriptPayload(
                    item.bytes,
                    phaseANativeControl.cursor,
                    kind,
                  );
                } catch (cause) {
                  payloadParseFailure = String(cause);
                  // The authenticated payload witness proves the exact
                  // malformed bytes to the selected one-step resolver.
                }
                const signerProof =
                  token?.kind === 0
                    ? signerProofForHash(token.keyHash)
                    : ({ kind: "none" } as const);
                pushPhaseANativeWitness({
                  kind: "nativeScriptToken",
                  chunkProof: buildMidgardBoundedItemChunkProof(
                    item,
                    chunkIndex,
                  ),
                  nextChunkProof:
                    chunkIndex + 1 < chunkCount
                      ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                      : null,
                  signerProof,
                });
                if (token === null) {
                  return phaseStops(
                    {
                      WitnessNativeScriptMalformed: {
                        script_index: BigInt(phaseANativeControl.scriptSeen),
                      },
                    },
                    `bounded native-script payload failed at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor},bytes=${item.bytes.subarray(phaseANativeControl.cursor).toString("hex")}: ${payloadParseFailure}`,
                  );
                }

                if (
                  token.kind >= 1 &&
                  token.kind <= 3 &&
                  token.childCount > 0
                ) {
                  const nextDepth = phaseANativeControl.stackDepth + 1;
                  if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH) {
                    return phaseStops(
                      {
                        WitnessNativeScriptDepthLimit: {
                          script_index: BigInt(phaseANativeControl.scriptSeen),
                        },
                      },
                      phaseANativeFault(),
                    );
                  }
                  const frame: ValidationMachineNativeScriptFrame = {
                    tail: phaseANativeControl.stackRoot,
                    kind: token.kind as 1 | 2 | 3,
                    childCount: token.childCount,
                    remaining: token.childCount,
                    validCount: 0,
                    required: token.required,
                  };
                  nativeScriptFrames.push(frame);
                  phaseANativeControl = {
                    ...phaseANativeControl,
                    stage: 1,
                    cursor: token.nextOffset,
                    stackRoot: hashValidationMachineNativeScriptFrame(frame),
                    stackDepth: nextDepth,
                  };
                  continue;
                }

                let valid: boolean;
                if (token.kind === 0) {
                  valid = signerProof.kind === "membership";
                } else if (token.kind === 4) {
                  valid =
                    compactProofTransaction.transactionBody
                      .validityIntervalStart >= 0n &&
                    compactProofTransaction.transactionBody
                      .validityIntervalStart >= token.slot;
                } else if (token.kind === 5) {
                  valid =
                    compactProofTransaction.transactionBody
                      .validityIntervalEnd >= 0n &&
                    compactProofTransaction.transactionBody
                      .validityIntervalEnd <= token.slot;
                } else if (token.kind === 1) {
                  valid = true;
                } else if (token.kind === 2) {
                  valid = false;
                } else {
                  valid = token.required === 0n;
                }
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 2,
                  cursor: token.nextOffset,
                  result: valid ? 1 : 0,
                };
                continue;
              }

              const frame = nativeScriptFrames[nativeScriptFrames.length - 1];
              if (frame !== undefined) {
                pushPhaseANativeWitness({
                  kind: "nativeScriptFrame",
                  frame,
                });
                const validCount =
                  frame.validCount + (phaseANativeControl.result === 1 ? 1 : 0);
                if (frame.remaining === 1) {
                  nativeScriptFrames.pop();
                  const valid =
                    frame.kind === 1
                      ? validCount === frame.childCount
                      : frame.kind === 2
                        ? validCount > 0
                        : BigInt(validCount) >= frame.required;
                  phaseANativeControl = {
                    ...phaseANativeControl,
                    stackRoot: frame.tail,
                    stackDepth: phaseANativeControl.stackDepth - 1,
                    result: valid ? 1 : 0,
                  };
                } else {
                  const nextFrame: ValidationMachineNativeScriptFrame = {
                    ...frame,
                    remaining: frame.remaining - 1,
                    validCount,
                  };
                  nativeScriptFrames[nativeScriptFrames.length - 1] = nextFrame;
                  phaseANativeControl = {
                    ...phaseANativeControl,
                    stage: 1,
                    stackRoot:
                      hashValidationMachineNativeScriptFrame(nextFrame),
                    result: -1,
                  };
                }
                continue;
              }

              pushPhaseANativeWitness();
              if (
                phaseANativeControl.cursor !== phaseANativeControl.itemLength
              ) {
                return phaseStops(
                  {
                    WitnessNativeScriptMalformed: {
                      script_index: BigInt(phaseANativeControl.scriptSeen),
                    },
                  },
                  phaseANativeFault(),
                );
              }
              if (phaseANativeControl.result === 0) {
                return phaseStops(
                  {
                    WitnessNativeScriptFalse: {
                      script_index: BigInt(phaseANativeControl.scriptSeen),
                    },
                  },
                  phaseANativeFault(),
                );
              }
              phaseANativeControl = resetPhaseANativeScriptsScanControl({
                scriptCount: activeScriptCount,
                scriptSeen: phaseANativeControl.scriptSeen + 1,
                containsNonNativeScript:
                  phaseANativeControl.containsNonNativeScript,
              });
              break;
            }
          }
        }
        return phaseContinues;
      }),
    );

    yield* walk.phase("phaseAScriptPreconditions", () =>
      Effect.sync(() => {
        let observerCount = 0;
        let observerSeen = 0;
        let previousObserver = Buffer.alloc(0);
        const currentPreconditionsWitness = (): Buffer =>
          phaseAScriptPreconditionsWitnessCbor({
            containsNonNativeScript:
              phaseANativeControl.containsNonNativeScript,
            observerCount,
            observerSeen,
            previousObserver,
          });
        for (const observer of requiredObserversCollection.items) {
          pushWitness(
            "phaseAScriptPreconditions",
            currentPreconditionsWitness(),
            {
              kind: "transactionFieldChunk",
              fieldIndex: 3,
              itemIndex: observer.itemIndex,
              fieldPreimage: fieldPreimage(3),
            },
          );
          if (
            observerSeen > 0 &&
            Buffer.compare(previousObserver, observer.bytes) >= 0
          ) {
            return phaseStops(
              {
                ObserverOrderInvalid: { observer_index: BigInt(observerSeen) },
              },
              "bounded observer scan found a duplicate or noncanonical ordering",
            );
          }
          if (observerCount === 0) {
            observerCount = requiredObserversCollection.items.length;
          }
          observerSeen += 1;
          previousObserver = observer.bytes;
        }
        pushWitness("phaseAScriptPreconditions", currentPreconditionsWitness());
        return phaseContinues;
      }),
    );

    let remainingScheduleHash = resolutionScheduleHash;
    let resolutionCursor = 0;
    const resolutionWitnessCbor = (
      pending:
        | {
            readonly node: (typeof resolutionScheduleNodes)[number];
            readonly descriptorCbor: Buffer;
            readonly outputProof: MidgardLedgerOutputProofControl;
          }
        | undefined,
    ): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(resolutionCursor),
        remainingScheduleHash,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        pending === undefined
          ? Buffer.from([0])
          : encodeCbor([
              pending.node.sourceKind === "spend" ? 0n : 1n,
              BigInt(pending.node.itemIndex),
              pending.node.key,
              pending.node.nextScheduleHash,
              pending.descriptorCbor,
              encodeMidgardLedgerOutputProofControl(pending.outputProof),
            ]),
        resolutionScheduleHash,
      ]);

    yield* walk.phase("validityInterval", () =>
      Effect.gen(function* () {
        if (phaseALedgerTx === null) {
          return yield* Effect.fail(
            new Error(
              "V1 trace reached input resolution without a Phase A ledger transaction",
            ),
          );
        }
        pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
        return phaseContinues;
      }),
    );
    yield* walk.phase("resolveInputs", () =>
      Effect.gen(function* () {
        resolutionCursor = 1;

        for (
          let index = 0;
          index < resolutionScheduleNodes.length;
          index += 1
        ) {
          const item = resolutionScheduleNodes[index]!;
          if (!remainingScheduleHash.equals(item.scheduleHash)) {
            return yield* Effect.fail(
              new Error(
                "input-resolution schedule diverged from its committed hash chain",
              ),
            );
          }
          const outRefHex = item.key.toString("hex");
          const outputCbor = ledgerState.get(outRefHex);
          const descriptorCbor = ledgerDescriptorState.get(outRefHex);
          if (outputCbor === undefined || descriptorCbor === undefined) {
            pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
              kind: "scheduledLedgerLookup",
              sourceKind: item.sourceKind,
              itemIndex: item.itemIndex,
              key: item.key,
              nextScheduleHash: item.nextScheduleHash,
              value: null,
              proofCbor: item.proofCbor,
              signerProof: { kind: "none" },
            });
            return phaseStops(
              {
                InputNotFound: {
                  source_kind: item.sourceKind === "spend" ? 0n : 1n,
                  input_index: UNSET_REASON_COORDINATE,
                },
              },
              "input resolution found no ledger member",
            );
          }

          const outputIndex = yield* Effect.try({
            try: () =>
              buildCanonicalMidgardLedgerEntryOutputMaterial({
                outRef: item.key,
                outputCbor,
              }).descriptor.outputIndex,
            catch: () => noExactLedgerDescriptor(outRefHex),
          });
          const outputProof = buildMidgardLedgerOutputProofTrace({
            outputIndex,
            outputCbor,
          });
          pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
            kind: "scheduledLedgerLookup",
            sourceKind: item.sourceKind,
            itemIndex: item.itemIndex,
            key: item.key,
            nextScheduleHash: item.nextScheduleHash,
            value: descriptorCbor,
            proofCbor: item.proofCbor,
            signerProof: { kind: "none" },
          });
          for (const proofStep of outputProof.steps) {
            pushWitness(
              "resolveInputs",
              resolutionWitnessCbor({
                node: item,
                descriptorCbor,
                outputProof: proofStep.control,
              }),
              {
                kind: "ledgerOutputProofStep",
                witness: proofStep.witness,
              },
            );
          }
          // Chained descriptor-fact attachment: each canonical group is one
          // checkpointed machine step over the same finalize witness shape,
          // its commitments derived from the terminal control alone; the thin
          // terminal finalize below requires the scan fact to commit exactly
          // the resolved descriptor.
          let factsProof = outputProof.terminal;
          while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
            pushWitness(
              "resolveInputs",
              resolutionWitnessCbor({
                node: item,
                descriptorCbor,
                outputProof: factsProof,
              }),
              {
                kind: "ledgerOutputProofFinalize",
                signerProof: { kind: "none" },
              },
            );
            const attached = attachMidgardLedgerOutputProofFacts(factsProof);
            if (attached === null) {
              return yield* Effect.fail(
                new Error("ledger output proof fact attachment failed closed"),
              );
            }
            factsProof = attached;
          }
          const signerProof = signerSetProof(item.sourceKind, outputCbor);
          pushWitness(
            "resolveInputs",
            resolutionWitnessCbor({
              node: item,
              descriptorCbor,
              outputProof: factsProof,
            }),
            {
              kind: "ledgerOutputProofFinalize",
              signerProof,
            },
          );
          // A key-locked spend whose signer is absent stops the machine here,
          // so validation must have refused this spend for the same fault.
          if (
            signerProof.kind !== "membership" &&
            signerProof.kind !== "none"
          ) {
            return phaseStops(
              {
                SpendInputSignerMissing: {
                  input_index: UNSET_REASON_COORDINATE,
                },
              },
              "spend signer is missing",
            );
          }

          remainingScheduleHash = item.nextScheduleHash;
          resolutionCursor += 1;
        }

        pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
        return phaseContinues;
      }),
    );
    yield* walk.phase("scriptSources", () =>
      Effect.gen(function* () {
        const scriptSourceControl = {
          resolvedInputCount: resolutionScheduleNodes.length,
        };
        let authenticatedInlineSourceFrontier = emptyValidationFrontier;
        let inlineSourceTotalCount = 0;
        const currentInlineSourceWitness = (
          pendingSource?: {
            readonly sourceIndex: number;
            readonly inlineSourceCount: number;
            readonly languageTag: 0 | 3 | 128;
            readonly payloadOffset: number;
            readonly payloadLength: number;
            readonly itemLength: number;
            readonly itemCommitment: Buffer;
            readonly hashControl: MidgardBlake2b224TraceControl;
          } | null,
        ): Buffer =>
          scriptSourcesWitnessCbor({
            ...scriptSourceControl,
            stage: 0,
            sourceFrontier: authenticatedInlineSourceFrontier,
            redeemerFrontier: emptyValidationFrontier,
            inlineSourceCount: inlineSourceTotalCount,
            redeemerTotalCount: 0,
            pendingSource,
          });
        for (const item of scriptWitnessesCollection.items) {
          pushWitness("scriptSources", currentInlineSourceWitness(), {
            kind: "transactionFieldChunk",
            fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
            itemIndex: item.itemIndex,
            fieldPreimage: fieldPreimage(MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX),
          });
          if (inlineSourceTotalCount === 0) {
            inlineSourceTotalCount = scriptWitnessesCollection.items.length;
          }
          const source = scriptSourceEntries[item.itemIndex];
          if (source === undefined || source.originKind !== "inline") {
            return yield* Effect.fail(
              new Error(
                "bounded inline script item lost its canonical source entry",
              ),
            );
          }
          const scriptArray = readCborArrayHeader(
            item.bytes,
            0,
            "v1.script_source",
          );
          const scriptLanguage = readCborInteger(
            item.bytes,
            scriptArray.nextOffset,
            "v1.script_source.language",
          );
          const scriptPayload = readCborBytes(
            item.bytes,
            scriptLanguage.nextOffset,
            "v1.script_source.payload",
          );
          const payloadOffset =
            scriptPayload.nextOffset - scriptPayload.value.length;
          if (
            scriptArray.length !== 2 ||
            scriptPayload.nextOffset !== item.bytes.length ||
            scriptPayload.value.length !== source.script.scriptBytes.length ||
            !scriptPayload.value.equals(source.script.scriptBytes)
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded inline script item is not its exact canonical versioned-script encoding",
              ),
            );
          }
          const exactLanguageTag: 0 | 3 | 128 =
            source.script.language === "NativeCardano"
              ? 0
              : source.script.language === "PlutusV3"
                ? 3
                : 128;
          if (scriptLanguage.value !== BigInt(exactLanguageTag)) {
            return yield* Effect.fail(
              new Error(
                "bounded inline script language diverged from its canonical source",
              ),
            );
          }
          const hashMessage = Buffer.concat([
            Buffer.from([exactLanguageTag]),
            source.script.scriptBytes,
          ]);
          const hashTrace = buildMidgardBlake2b224Trace(hashMessage);
          let pendingSource = {
            sourceIndex: item.itemIndex,
            inlineSourceCount: inlineSourceTotalCount,
            languageTag: exactLanguageTag,
            payloadOffset,
            payloadLength: scriptPayload.value.length,
            itemLength: item.bytes.length,
            itemCommitment: item.commitment,
            hashControl: hashTrace[0]!.control,
          };
          for (const hashStep of hashTrace) {
            let auxiliary:
              | {
                  readonly kind: "scriptSourceHashBlock";
                  readonly chunkProof: MidgardBoundedItemChunkProof;
                  readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
                }
              | undefined;
            if (hashStep.block !== null) {
              const contentLength =
                hashStep.block.length - (hashStep.control.cursor === 0 ? 1 : 0);
              const itemCursor =
                hashStep.control.cursor === 0
                  ? payloadOffset
                  : payloadOffset + hashStep.control.cursor - 1;
              const chunkIndex =
                contentLength === 0
                  ? 0
                  : Math.floor(itemCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
              const chunkProof = buildMidgardBoundedItemChunkProof(
                item,
                chunkIndex,
              );
              const offset =
                contentLength === 0
                  ? payloadOffset
                  : itemCursor - chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
              const crossesChunk =
                contentLength > chunkProof.chunk.length - offset;
              auxiliary = {
                kind: "scriptSourceHashBlock",
                chunkProof,
                nextChunkProof: crossesChunk
                  ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                  : null,
              };
            }
            pushWitness(
              "scriptSources",
              currentInlineSourceWitness(pendingSource),
              auxiliary,
            );
            pendingSource = {
              ...pendingSource,
              hashControl: hashStep.next,
            };
          }
          if (
            pendingSource.hashControl.stage !==
              MidgardBlake2b224TraceStages.Terminal ||
            !pendingSource.hashControl.chainingValue
              .subarray(0, 28)
              .equals(source.scriptHash)
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded inline script hash trace diverged from its canonical identity",
              ),
            );
          }
          pushWitness(
            "scriptSources",
            currentInlineSourceWitness(pendingSource),
          );
          authenticatedInlineSourceFrontier = appendMidgardValidationMerkleLeaf(
            authenticatedInlineSourceFrontier,
            inlineScriptSourceLeafHashes[item.itemIndex]!,
          );
        }
        pushWitness("scriptSources", currentInlineSourceWitness());
        if (
          !commitMidgardValidationMerkleFrontier(
            authenticatedInlineSourceFrontier,
          ).equals(
            commitMidgardValidationMerkleFrontier(inlineScriptSourceFrontier),
          )
        ) {
          return yield* Effect.fail(
            new Error(
              "authenticated inline source fold diverged from the canonical source frontier",
            ),
          );
        }
        let authenticatedRedeemerFrontier = emptyValidationFrontier;
        let redeemerTotalCount = 0;
        const currentRedeemerWitness = (
          redeemerItemControlHash: Buffer = Buffer.alloc(0),
        ): Buffer =>
          scriptSourcesWitnessCbor({
            ...scriptSourceControl,
            stage: 1,
            sourceFrontier: inlineScriptSourceFrontier,
            redeemerFrontier: authenticatedRedeemerFrontier,
            inlineSourceCount: inlineSourceTotalCount,
            redeemerTotalCount,
            redeemerItemControlHash,
          });
        for (const item of redeemerWitnessesCollection.items) {
          const redeemer = decodedProofRedeemers[item.itemIndex];
          const canonicalRedeemerWitnessCbor =
            canonicalRedeemerWitnessCbors[item.itemIndex];
          if (
            redeemer === undefined ||
            canonicalRedeemerWitnessCbor === undefined ||
            !item.bytes.equals(canonicalRedeemerWitnessCbor)
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded redeemer item diverged from its canonical decoded witness",
              ),
            );
          }
          pushWitness("scriptSources", currentRedeemerWitness(), {
            kind: "transactionRedeemerItemBegin",
            // Stage 1: field 8, item index `control.redeemer_count`. Both are
            // fixed by the stage and its cursor, so the carriage is the whole
            // wire surface.
            fieldIndex: 8,
            fieldPreimage: fieldPreimage(8),
          });
          if (redeemerTotalCount === 0) {
            redeemerTotalCount = redeemerWitnessesCollection.items.length;
          }
          if (
            inspectMidgardRedeemerData(Buffer.from(redeemer.dataCborHex, "hex"))
              .kind === "refusal"
          ) {
            const refusal = buildMidgardRedeemerDataUnreadableTrace({
              itemIndex: item.itemIndex,
              itemCount: redeemerTotalCount,
              itemBytes: item.bytes,
            });
            for (const step of refusal.steps) {
              pushWitness(
                "scriptSources",
                currentRedeemerWitness(
                  hashMidgardRedeemerItemProofControl(step.control),
                ),
                {
                  kind: "redeemerItemStep",
                  redeemerControl: null,
                  control: step.control,
                  witness: step.witness,
                },
              );
            }
            pushWitness(
              "scriptSources",
              currentRedeemerWitness(
                hashMidgardRedeemerItemProofControl(refusal.control),
              ),
              {
                kind: "redeemerItemStep",
                redeemerControl: null,
                control: refusal.control,
                witness: refusal.witness,
              },
            );
            return phaseStops(
              { RedeemerMalformed: { redeemer_index: BigInt(item.itemIndex) } },
              "redeemer Data is unreadable",
            );
          }
          const itemTrace = buildMidgardRedeemerItemProofTrace({
            itemIndex: item.itemIndex,
            itemCount: redeemerTotalCount,
            itemBytes: item.bytes,
            mode: MidgardRedeemerItemProofModes.Data,
          });
          let activeItemControlHash = hashMidgardRedeemerItemProofControl(
            itemTrace.initial,
          );
          for (const itemStep of itemTrace.steps) {
            pushWitness(
              "scriptSources",
              currentRedeemerWitness(activeItemControlHash),
              {
                kind: "redeemerItemStep",
                redeemerControl: null,
                control: itemStep.control,
                witness: itemStep.witness,
              },
            );
            if (
              itemStep.next.stage === MidgardRedeemerItemProofStages.Terminal
            ) {
              authenticatedRedeemerFrontier = appendMidgardValidationMerkleLeaf(
                authenticatedRedeemerFrontier,
                hashMidgardRedeemerItemLeaf({
                  redeemerIndex: item.itemIndex,
                  itemLength: item.bytes.length,
                  itemCommitment: item.commitment,
                }),
              );
              activeItemControlHash = Buffer.alloc(0);
            } else {
              activeItemControlHash = hashMidgardRedeemerItemProofControl(
                itemStep.next,
              );
            }
          }
          if (activeItemControlHash.length !== 0) {
            return yield* Effect.fail(
              new Error(
                "redeemer item proof did not reach its terminal control",
              ),
            );
          }
        }
        pushWitness("scriptSources", currentRedeemerWitness());
        if (
          !commitMidgardValidationMerkleFrontier(
            authenticatedRedeemerFrontier,
          ).equals(commitMidgardValidationMerkleFrontier(redeemerFrontier))
        ) {
          return yield* Effect.fail(
            new Error(
              "authenticated redeemer fold diverged from the canonical redeemer frontier",
            ),
          );
        }
        {
          pushWitness(
            "scriptSources",
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 2,
              sourceFrontier: inlineScriptSourceFrontier,
              redeemerFrontier,
            }),
          );
          let replayCursor = 0;
          let replayRemainingScheduleHash = resolutionScheduleHash;
          let replaySpendIndex = 0;
          let replaySourceFrontier = inlineScriptSourceFrontier;
          let replayPurposeFrontier = emptyValidationFrontier;
          for (const node of resolutionScheduleNodes) {
            const outRefHex = node.key.toString("hex");
            const outputCbor = ledgerState.get(outRefHex);
            const descriptorCbor = ledgerDescriptorState.get(outRefHex);
            if (outputCbor === undefined || descriptorCbor === undefined) {
              return yield* Effect.fail(
                new Error(
                  "resolved-input replay lost previously authenticated output material",
                ),
              );
            }
            const outputMaterial =
              buildCanonicalMidgardLedgerEntryOutputMaterial({
                outRef: node.key,
                outputCbor,
              });
            if (!outputMaterial.descriptorCbor.equals(descriptorCbor)) {
              return yield* Effect.fail(
                new Error(
                  "resolved-input replay descriptor differs from retained output material",
                ),
              );
            }
            const descriptor = outputMaterial.descriptor;
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 3,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
              }),
              {
                kind: "resolvedInputReplay",
                sourceKind: node.sourceKind,
                itemIndex: node.itemIndex,
                key: node.key,
                nextScheduleHash: node.nextScheduleHash,
                value: descriptorCbor,
                // The prior-ledger membership proof the lookup step
                // carried for this node authenticates the replayed value.
                proofCbor: node.proofCbor,
                siblings: [],
              },
            );
            if (!replayRemainingScheduleHash.equals(node.scheduleHash)) {
              return yield* Effect.fail(
                new Error(
                  "resolved-input replay schedule diverged from its committed hash chain",
                ),
              );
            }
            if (descriptor.referenceScriptLanguage !== -1) {
              const output = decodeMidgardTxOutput(outputCbor);
              if (output.script_ref === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-output descriptor commits a missing retained reference script",
                  ),
                );
              }
              const leaf = hashMidgardReferenceScriptSourceLeaf({
                sourceKey: node.key,
                scriptLanguageTag: descriptor.referenceScriptLanguage,
                scriptHash: descriptor.referenceScriptHash,
                scriptTotalLength: descriptor.referenceScriptTotalLength,
                itemCommitment: descriptor.referenceScriptItemCommitment,
              });
              if (
                !leaf.equals(
                  hashMidgardScriptSourceLeaf({
                    originKind: "reference",
                    sourceKey: node.key,
                    script: output.script_ref,
                  }),
                )
              ) {
                return yield* Effect.fail(
                  new Error(
                    "retained reference script differs from its authenticated descriptor facts",
                  ),
                );
              }
              const sourceEntry: ScriptSourceProofEntry = {
                originKind: "reference",
                sourceKey: node.key,
                script: output.script_ref,
                authenticatedVersionedItemBytes: encodeMidgardVersionedScript(
                  output.script_ref,
                ),
                scriptLanguageTag: descriptor.referenceScriptLanguage,
                scriptHash: descriptor.referenceScriptHash,
                scriptTotalLength: descriptor.referenceScriptTotalLength,
                scriptItemCommitment: descriptor.referenceScriptItemCommitment,
                leaf,
              };
              scriptSourceEntries.push(sourceEntry);
              replaySourceFrontier = appendMidgardValidationMerkleLeaf(
                replaySourceFrontier,
                sourceEntry.leaf,
              );
            }
            if (node.sourceKind === "spend") {
              const credential = decodeMidgardAddressBytes(
                descriptor.address,
              ).paymentCredential;
              if (credential.kind === "Script") {
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 0,
                  purposeIndex: BigInt(replaySpendIndex),
                  scriptHash: Buffer.from(credential.hash),
                  subject: node.key,
                  leaf: hashMidgardScriptPurposeLeaf({
                    purposeKind: 0,
                    purposeIndex: BigInt(replaySpendIndex),
                    scriptHash: credential.hash,
                    subject: node.key,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                replayPurposeFrontier = appendMidgardValidationMerkleLeaf(
                  replayPurposeFrontier,
                  purposeEntry.leaf,
                );
              }
              replaySpendIndex += 1;
            }
            resolvedItemFrontier = appendMidgardValidationMerkleLeaf(
              resolvedItemFrontier,
              hashMidgardResolvedContextItemLeaf({
                sourceKind: node.sourceKind,
                itemIndex: replayCursor,
                key: node.key,
                outputCbor: descriptorCbor,
              }),
            );
            replayRemainingScheduleHash = node.nextScheduleHash;
            replayCursor += 1;
          }
          pushWitness(
            "scriptSources",
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 3,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: replayPurposeFrontier,
            }),
          );
          let authenticatedOutputFrontier = emptyValidationFrontier;
          let outputTotalCount = 0;
          const currentOutputCommitmentWitness = (): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 4,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: replayPurposeFrontier,
              outputFrontier: authenticatedOutputFrontier,
              outputTotalCount,
            });
          for (const item of outputsCollection.items) {
            const outputCbor = outputCbors[item.itemIndex];
            if (outputCbor === undefined || !item.bytes.equals(outputCbor)) {
              return yield* Effect.fail(
                new Error(
                  "bounded output item diverged from its canonical decoded output",
                ),
              );
            }
            // Stage 4 folds only the authenticated (field_index, item_index,
            // item_length, item_commitment) tuple, all derived by the door from the
            // authenticated preimage. The item bytes are not revealed: that re-proves
            // only that an authenticated commitment has a preimage (canonicalDecode and
            // the stage-5 output traversal establish it) while growing the one-step
            // evidence past the L1 envelope for legal 16,384-byte outputs
            // (C21-STAGE4-GAP, Option A). The carriage keeps this redeemer O(1) in
            // output size only under tiers 2-3, where the preimage rides reference
            // inputs. The step carries the *plan input* (field and bytes); the tier is
            // resolved at evidence commitment, where a transaction exists to index
            // reference inputs into (#600): above §8.3's 14,336-byte tier-1 cap it is
            // tier 2 or 3 and O(1), below it tier-1 `Inline`. The producer never
            // refuses and never names a tier.
            pushWitness("scriptSources", currentOutputCommitmentWitness(), {
              kind: "transactionRedeemerItemBegin",
              fieldIndex: 2,
              fieldPreimage: fieldPreimage(2),
            });
            if (outputTotalCount === 0) {
              outputTotalCount = outputsCollection.items.length;
            }
            authenticatedOutputFrontier = appendMidgardValidationMerkleLeaf(
              authenticatedOutputFrontier,
              hashMidgardOutputItemLeaf({
                outputIndex: item.itemIndex,
                itemLength: item.bytes.length,
                itemCommitment: item.commitment,
              }),
            );
          }
          pushWitness("scriptSources", currentOutputCommitmentWitness());
          if (
            !commitMidgardValidationMerkleFrontier(
              authenticatedOutputFrontier,
            ).equals(commitMidgardValidationMerkleFrontier(outputFrontier))
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated output fold diverged from the canonical output frontier",
              ),
            );
          }
          let outputCursor = 0;
          let receiveSourceFrontier = emptyValidationFrontier;
          let outputDescriptorFrontier = emptyValidationFrontier;
          const receiveSourceEntries: ScriptPurposeProofEntry[] = [];
          const receiveSourceScan = () => ({
            sourceFrontier: receiveSourceFrontier,
            receiveCount: 0,
            previousHash: Buffer.alloc(0),
            candidateHash: Buffer.alloc(0),
            descriptorFrontier: outputDescriptorFrontier,
          });
          const retainedOutputDescriptorScan = () => ({
            sourceFrontier: emptyValidationFrontier,
            receiveCount: 0,
            previousHash: Buffer.alloc(0),
            candidateHash: Buffer.alloc(0),
            descriptorFrontier: outputDescriptorFrontier,
          });
          const stageFiveWitness = (
            outputProof?: Parameters<
              typeof scriptSourcesWitnessCbor
            >[0]["outputProof"],
          ) =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 5,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: replayPurposeFrontier,
              outputCursor,
              outputFrontier,
              receiveScan: receiveSourceScan(),
              ...(outputProof === undefined ? {} : { outputProof }),
            });
          for (const outputCbor of outputCbors) {
            const outputItem = outputsCollection.items[outputCursor];
            if (
              outputItem === undefined ||
              !outputItem.bytes.equals(outputCbor)
            ) {
              return yield* Effect.fail(
                new Error(
                  "output admission lost its authenticated bounded item",
                ),
              );
            }
            const proofOutcome = traceMidgardLedgerOutputProof({
              outputIndex: outputCursor,
              outputCbor,
            });
            pushWitness("scriptSources", stageFiveWitness(), {
              kind: "ledgerOutputProofBegin",
              outputIndex: outputCursor,
              totalLength: outputItem.bytes.length,
              itemCommitment: outputItem.commitment,
              siblings: outputMembership(outputCursor).siblings,
            });
            for (const proofStep of proofOutcome.kind === "accepted"
              ? proofOutcome.trace.steps
              : [...proofOutcome.steps, proofOutcome.refusingStep]) {
              pushWitness(
                "scriptSources",
                stageFiveWitness(proofStep.control),
                {
                  kind: "ledgerOutputProofStep",
                  witness: proofStep.witness,
                },
              );
            }
            // The machine refuses the output at the refusing step.
            if (proofOutcome.kind === "refused") {
              return phaseStops(
                refusedOutputStopReason(outputCursor, proofOutcome.refusal),
                `output ${outputCursor} fails its output proof (${proofOutcome.refusal})`,
              );
            }
            const outputProof = proofOutcome.trace;
            const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
              outputIndex: outputCursor,
              outputCbor,
            });
            const signerProof = protectedOutputSignerProof(outputCbor);
            // Chained descriptor-fact attachment: each canonical group is
            // one checkpointed machine step over the same finalize witness
            // shape, its commitments derived from the terminal control
            // alone; the thin terminal finalize below requires the scan
            // fact to commit exactly the output's descriptor.
            let factsProof = outputProof.terminal;
            while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
              pushWitness("scriptSources", stageFiveWitness(factsProof), {
                kind: "ledgerOutputProofFinalize",
                signerProof: { kind: "none" },
              });
              const attached = attachMidgardLedgerOutputProofFacts(factsProof);
              if (attached === null) {
                return yield* Effect.fail(
                  new Error(
                    "ledger output proof fact attachment failed closed",
                  ),
                );
              }
              factsProof = attached;
            }
            pushWitness("scriptSources", stageFiveWitness(factsProof), {
              kind: "ledgerOutputProofFinalize",
              signerProof,
            });
            const output = decodeMidgardTxOutput(outputCbor);
            const address = decodeMidgardAddressBytes(output.address);
            // Stage 5 is where the machine refuses both output faults, so
            // validation must have refused this output for the same one.
            const networkFault =
              BigInt(address.networkId) !== input.expectedNetworkId;
            if (
              networkFault ||
              (address.protected &&
                address.paymentCredential.kind === "PubKey" &&
                signerProof.kind !== "membership")
            ) {
              return phaseStops(
                networkFault
                  ? "NetworkIdMismatch"
                  : {
                      ProtectedOutputSignerMissing: {
                        output_index: BigInt(outputCursor),
                      },
                    },
                `output ${outputCursor} fails at stage 5`,
              );
            }
            outputDescriptorFrontier = appendMidgardValidationMerkleLeaf(
              outputDescriptorFrontier,
              hashMidgardOutputDescriptorLeaf({
                outputIndex: outputCursor,
                descriptorCbor: outputMaterial.descriptorCbor,
              }),
            );
            admittedOutputDescriptorCbors.push(outputMaterial.descriptorCbor);
            admittedOutputDescriptorLeafHashes.push(
              hashMidgardOutputDescriptorLeaf({
                outputIndex: outputCursor,
                descriptorCbor: outputMaterial.descriptorCbor,
              }),
            );
            if (
              address.protected &&
              address.paymentCredential.kind === "Script"
            ) {
              const scriptHash = Buffer.from(address.paymentCredential.hash);
              const purposeEntry: ScriptPurposeProofEntry = {
                purposeKind: 3,
                purposeIndex: BigInt(receiveSourceFrontier.count),
                scriptHash,
                subject: scriptHash,
                leaf: hashMidgardScriptPurposeLeaf({
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveSourceFrontier.count),
                  scriptHash,
                  subject: scriptHash,
                }),
              };
              receiveSourceEntries.push(purposeEntry);
              receiveSourceFrontier = appendMidgardValidationMerkleLeaf(
                receiveSourceFrontier,
                purposeEntry.leaf,
              );
            }
            outputCursor += 1;
          }
          pushWitness("scriptSources", stageFiveWitness());
          let mintPurposeFrontier = replayPurposeFrontier;
          for (const policyItem of mintCollection.items) {
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 6,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: mintPurposeFrontier,
                outputCursor,
                outputFrontier,
                receiveScan: receiveSourceScan(),
              }),
              {
                kind: "transactionFieldChunk",
                fieldIndex: 5,
                itemIndex: policyItem.itemIndex,
                fieldPreimage: fieldPreimage(5),
              },
            );
            const itemHeader = readCborArrayHeader(
              policyItem.bytes,
              0,
              `v1.mint.policy[${policyItem.itemIndex}]`,
            );
            if (itemHeader.length !== 2) {
              throw new Error("V1 mint policy item must contain two fields");
            }
            const policy = readCborBytes(
              policyItem.bytes,
              itemHeader.nextOffset,
              `v1.mint.policy[${policyItem.itemIndex}].id`,
            );
            const assets = readCborMapHeader(
              policyItem.bytes,
              policy.nextOffset,
              `v1.mint.policy[${policyItem.itemIndex}].assets`,
            );
            const policyId = Buffer.from(policy.value);
            const purposeEntry: ScriptPurposeProofEntry = {
              purposeKind: 1,
              purposeIndex: BigInt(policyItem.itemIndex),
              scriptHash: policyId,
              subject: policyId,
              leaf: hashMidgardScriptPurposeLeaf({
                purposeKind: 1,
                purposeIndex: BigInt(policyItem.itemIndex),
                scriptHash: policyId,
                subject: policyId,
              }),
            };
            scriptPurposeEntries.push(purposeEntry);
            mintPurposeFrontier = appendMidgardValidationMerkleLeaf(
              mintPurposeFrontier,
              purposeEntry.leaf,
            );
            mintFoldControl = {
              ...mintFoldControl,
              policyCount: mintCollection.items.length,
              activePolicy: policyId,
              itemLength: policyItem.bytes.length,
              itemCommitment: Buffer.from(policyItem.commitment),
              itemCursor: assets.nextOffset,
              assetsRemaining: assets.length,
              policyAssetCursor: 0,
              previousAsset: Buffer.alloc(0),
            };
            let assetCursor = assets.nextOffset;
            for (
              let assetIndex = 0;
              assetIndex < assets.length;
              assetIndex += 1
            ) {
              const expectedChunkIndex = Math.floor(
                assetCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
              );
              const nextChunkIndex =
                expectedChunkIndex + 1 <
                midgardBoundedItemChunkCount(policyItem.bytes.length)
                  ? expectedChunkIndex + 1
                  : null;
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 6,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: mintPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
                {
                  kind: "mintFoldAsset",
                  chunkProof: buildMidgardBoundedItemChunkProof(
                    policyItem,
                    expectedChunkIndex,
                  ),
                  nextChunkProof:
                    nextChunkIndex === null
                      ? null
                      : buildMidgardBoundedItemChunkProof(
                          policyItem,
                          nextChunkIndex,
                        ),
                },
              );
              const asset = readCborBytes(
                policyItem.bytes,
                assetCursor,
                `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].name`,
              );
              const quantity = readCborInteger(
                policyItem.bytes,
                asset.nextOffset,
                `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].quantity`,
              );
              assetCursor = quantity.nextOffset;
              const nextAssetFrontier = appendMidgardValidationMerkleLeaf(
                mintFoldControl.assetFrontier,
                hashMidgardMintAssetLeaf({
                  policyId,
                  assetName: asset.value,
                  quantity: quantity.value,
                }),
              );
              const finishedPolicy = assetIndex + 1 === assets.length;
              mintFoldControl = finishedPolicy
                ? {
                    ...mintFoldControl,
                    policyCursor: mintFoldControl.policyCursor + 1,
                    previousPolicy: policyId,
                    activePolicy: Buffer.alloc(0),
                    itemLength: 0,
                    itemCommitment: Buffer.alloc(0),
                    itemCursor: 0,
                    assetsRemaining: 0,
                    policyAssetCursor: 0,
                    previousAsset: Buffer.alloc(0),
                    assetFrontier: nextAssetFrontier,
                  }
                : {
                    ...mintFoldControl,
                    itemCursor: assetCursor,
                    assetsRemaining: mintFoldControl.assetsRemaining - 1,
                    policyAssetCursor: mintFoldControl.policyAssetCursor + 1,
                    previousAsset: Buffer.from(asset.value),
                    assetFrontier: nextAssetFrontier,
                  };
            }
            if (assetCursor !== policyItem.bytes.length) {
              throw new Error("V1 mint policy item has trailing bytes");
            }
          }
          pushWitness(
            "scriptSources",
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 6,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: mintPurposeFrontier,
              outputCursor,
              outputFrontier,
              receiveScan: receiveSourceScan(),
            }),
          );
          // The empty-mint normalization belongs to the stage-six
          // transition. Its predecessor must retain the stage-five fold.
          if (mintCollection.items.length === 0) {
            mintFoldControl = {
              ...mintFoldControl,
              policyCount: 0,
            };
          }
          let observerPurposeFrontier = mintPurposeFrontier;
          let observerTotalCount = 0;
          let observerSeen = 0;
          let previousObserverHash = Buffer.alloc(0);
          const currentObserverPurposeWitness = (): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 7,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: observerPurposeFrontier,
              outputCursor: 0,
              outputFrontier,
              receiveScan: receiveSourceScan(),
              observerScan: {
                totalCount: observerTotalCount,
                seen: observerSeen,
                previousHash: previousObserverHash,
              },
            });
          for (const observer of requiredObserversCollection.items) {
            pushWitness("scriptSources", currentObserverPurposeWitness(), {
              kind: "transactionFieldChunk",
              fieldIndex: 3,
              itemIndex: observer.itemIndex,
              fieldPreimage: fieldPreimage(3),
            });
            if (observerTotalCount === 0) {
              observerTotalCount = requiredObserversCollection.items.length;
            }
            const observerHash = observer.bytes;
            const purposeEntry: ScriptPurposeProofEntry = {
              purposeKind: 2,
              purposeIndex: BigInt(observerSeen),
              scriptHash: observerHash,
              subject: observerHash,
              leaf: hashMidgardScriptPurposeLeaf({
                purposeKind: 2,
                purposeIndex: BigInt(observerSeen),
                scriptHash: observerHash,
                subject: observerHash,
              }),
            };
            scriptPurposeEntries.push(purposeEntry);
            observerPurposeFrontier = appendMidgardValidationMerkleLeaf(
              observerPurposeFrontier,
              purposeEntry.leaf,
            );
            observerSeen += 1;
            previousObserverHash = observerHash;
          }
          let allPurposeFrontier = observerPurposeFrontier;
          const receiveSourceLeaves = receiveSourceEntries.map(
            (entry) => entry.leaf,
          );
          const receiveSourceMembership = (sourceIndex: number) =>
            buildMidgardValidationMerkleMembership(
              receiveSourceLeaves,
              sourceIndex,
            );
          let receiveSourceCursor = 0;
          let receiveCount = 0;
          let receivePreviousHash = Buffer.alloc(0);
          let receiveCandidateHash = Buffer.alloc(0);
          const currentReceivePurposeWitness = (): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 7,
              sourceFrontier: replaySourceFrontier,
              redeemerFrontier,
              replayCursor,
              replayRemainingScheduleHash,
              spendIndex: replaySpendIndex,
              purposeFrontier: allPurposeFrontier,
              outputCursor: receiveSourceCursor,
              outputFrontier,
              receiveScan: {
                sourceFrontier: receiveSourceFrontier,
                receiveCount,
                previousHash: receivePreviousHash,
                candidateHash: receiveCandidateHash,
                descriptorFrontier: outputDescriptorFrontier,
              },
              observerScan: {
                totalCount: observerTotalCount,
                seen: observerSeen,
                previousHash: previousObserverHash,
              },
            });
          while (true) {
            if (receiveSourceCursor === receiveSourceEntries.length) {
              pushWitness("scriptSources", currentReceivePurposeWitness());
              if (receiveCandidateHash.length === 0) {
                break;
              }
              const scriptHash = receiveCandidateHash;
              const purposeEntry: ScriptPurposeProofEntry = {
                purposeKind: 3,
                purposeIndex: BigInt(receiveCount),
                scriptHash,
                subject: scriptHash,
                leaf: hashMidgardScriptPurposeLeaf({
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveCount),
                  scriptHash,
                  subject: scriptHash,
                }),
              };
              scriptPurposeEntries.push(purposeEntry);
              allPurposeFrontier = appendMidgardValidationMerkleLeaf(
                allPurposeFrontier,
                purposeEntry.leaf,
              );
              receiveCount += 1;
              receivePreviousHash = scriptHash;
              receiveCandidateHash = Buffer.alloc(0);
              receiveSourceCursor = 0;
              continue;
            }
            const receiveSource = receiveSourceEntries[receiveSourceCursor]!;
            pushWitness("scriptSources", currentReceivePurposeWitness(), {
              kind: "scriptPurposeScan",
              purposeKind: 3,
              purposeIndex: BigInt(receiveSourceCursor),
              scriptHash: receiveSource.scriptHash,
              subject: receiveSource.subject,
              siblings: receiveSourceMembership(receiveSourceCursor).siblings,
            });
            const scriptHash = receiveSource.scriptHash;
            if (
              (receivePreviousHash.length === 0 ||
                Buffer.compare(receivePreviousHash, scriptHash) < 0) &&
              (receiveCandidateHash.length === 0 ||
                Buffer.compare(scriptHash, receiveCandidateHash) < 0)
            ) {
              receiveCandidateHash = scriptHash;
            }
            receiveSourceCursor += 1;
          }
          {
            const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
            const purposeLeaves = scriptPurposeEntries.map(
              (entry) => entry.leaf,
            );
            const redeemerLeaves = redeemerLeafHashes;
            const discoveryWitnessCbor = (
              stage: number,
              discovery: ScriptDiscoveryTraceControl,
            ): Buffer =>
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: allPurposeFrontier,
                outputCursor: outputFrontier.count,
                outputFrontier,
                receiveScan: retainedOutputDescriptorScan(),
                discovery,
              });
            const sourceMembership = (sourceIndex: number) =>
              buildMidgardValidationMerkleMembership(sourceLeaves, sourceIndex);
            const purposeMembership = (purposeIndex: number) =>
              buildMidgardValidationMerkleMembership(
                purposeLeaves,
                purposeIndex,
              );
            const redeemerMembership = (redeemerIndex: number) =>
              buildMidgardValidationMerkleMembership(
                redeemerLeaves,
                redeemerIndex,
              );
            const setDiscoveryBit = (bitmap: bigint, index: number): bigint =>
              bitmap | (1n << BigInt(index));
            const resetCurrent = (
              discovery: ScriptDiscoveryTraceControl,
            ): ScriptDiscoveryTraceControl => ({
              ...discovery,
              sourceCursor: 0,
              redeemerCursor: 0,
              currentPurposeKind: -1,
              currentPurposeIndex: -1n,
              currentScriptHash: Buffer.alloc(0),
              currentSubject: Buffer.alloc(0),
              matchedSourceIndex: -1,
              matchedLanguageTag: -1,
              matchedSourceLeaf: Buffer.alloc(0),
              redeemerItemControlHash: Buffer.alloc(0),
            });

            let discovery = emptyScriptDiscoveryControl;
            for (
              let purposeCursor = 0;
              purposeCursor < scriptPurposeEntries.length;
              purposeCursor += 1
            ) {
              const purpose = scriptPurposeEntries[purposeCursor]!;
              pushWitness("scriptSources", discoveryWitnessCbor(8, discovery), {
                kind: "scriptPurposeScan",
                purposeKind: purpose.purposeKind,
                purposeIndex: purpose.purposeIndex,
                scriptHash: purpose.scriptHash,
                subject: purpose.subject,
                siblings: purposeMembership(purposeCursor).siblings,
              });
              discovery = {
                ...discovery,
                sourceCursor: 0,
                redeemerCursor: 0,
                currentPurposeKind: purpose.purposeKind,
                currentPurposeIndex: purpose.purposeIndex,
                currentScriptHash: purpose.scriptHash,
                currentSubject: purpose.subject,
                matchedSourceIndex: -1,
                matchedLanguageTag: -1,
                matchedSourceLeaf: Buffer.alloc(0),
              };

              let matchedSource:
                | {
                    readonly entry: ScriptSourceProofEntry;
                    readonly sourceIndex: number;
                    readonly languageTag: 0 | 3 | 128;
                  }
                | undefined;
              for (
                let sourceCursor = 0;
                sourceCursor < scriptSourceEntries.length;
                sourceCursor += 1
              ) {
                const sourceIndex = midgardScriptSourcePhysicalIndex(
                  inlineScriptSourceFrontier.count,
                  scriptSourceEntries.length,
                  sourceCursor,
                );
                const source = scriptSourceEntries[sourceIndex]!;
                pushWitness(
                  "scriptSources",
                  discoveryWitnessCbor(9, discovery),
                  {
                    kind: "scriptSourceScan",
                    sourceIndex,
                    originKind: source.originKind,
                    sourceKey: source.sourceKey,
                    scriptLanguageTag: source.scriptLanguageTag,
                    scriptHash: source.scriptHash,
                    scriptTotalLength: source.scriptTotalLength,
                    scriptItemCommitment: source.scriptItemCommitment,
                    siblings: sourceMembership(sourceIndex).siblings,
                  },
                );
                const sourceHash = source.scriptHash;
                discovery = {
                  ...discovery,
                  sourceCursor: sourceCursor + 1,
                };
                if (sourceHash.equals(purpose.scriptHash)) {
                  const exactLanguageTag = source.scriptLanguageTag;
                  discovery = {
                    ...discovery,
                    matchedSourceIndex: sourceIndex,
                    matchedLanguageTag: exactLanguageTag,
                    matchedSourceLeaf: source.leaf,
                    usedInlineBitmap:
                      source.originKind === "inline"
                        ? setDiscoveryBit(
                            discovery.usedInlineBitmap,
                            sourceIndex,
                          )
                        : discovery.usedInlineBitmap,
                  };
                  matchedSource = {
                    entry: source,
                    sourceIndex,
                    languageTag: exactLanguageTag,
                  };
                  break;
                }
              }
              if (matchedSource === undefined) {
                pushWitness(
                  "scriptSources",
                  discoveryWitnessCbor(9, discovery),
                );
                return phaseStops(
                  {
                    ScriptSourceMissing: {
                      purpose_kind: BigInt(discovery.currentPurposeKind),
                      purpose_index: BigInt(discovery.currentPurposeIndex),
                    },
                  },
                  "V1 source scan found no source for the purpose",
                );
              }

              if (matchedSource.languageTag === 0) {
                const executionLeaf = hashMidgardScriptExecutionLeaf({
                  languageTag: 0,
                  purposeLeaf: purpose.leaf,
                  sourceLeaf: matchedSource.entry.leaf,
                });
                scriptExecutionEntries.push({
                  purpose,
                  source: matchedSource.entry,
                  sourceIndex: matchedSource.sourceIndex,
                  languageTag: 0,
                  redeemerLeaf: Buffer.alloc(0),
                  leaf: executionLeaf,
                });
                discovery = resetCurrent({
                  ...discovery,
                  purposeCursor: purposeCursor + 1,
                  executionFrontier: appendMidgardValidationMerkleLeaf(
                    discovery.executionFrontier,
                    executionLeaf,
                  ),
                });
                continue;
              }

              let matchedRedeemerIndex = -1;
              for (
                let redeemerIndex = 0;
                redeemerIndex < decodedProofRedeemers.length;
                redeemerIndex += 1
              ) {
                const redeemer = decodedProofRedeemers[redeemerIndex]!;
                const item = redeemerWitnessesCollection.items[redeemerIndex]!;
                const itemTrace = buildMidgardRedeemerItemProofTrace({
                  itemIndex: redeemerIndex,
                  itemCount: decodedProofRedeemers.length,
                  itemBytes: item.bytes,
                  mode: MidgardRedeemerItemProofModes.Descriptor,
                });
                pushWitness(
                  "scriptSources",
                  discoveryWitnessCbor(10, discovery),
                  {
                    kind: "redeemerScanBegin",
                    itemIndex: redeemerIndex,
                    itemCount: decodedProofRedeemers.length,
                    totalLength: item.bytes.length,
                    itemCommitment: item.commitment,
                    siblings: redeemerMembership(redeemerIndex).siblings,
                  },
                );
                discovery = {
                  ...discovery,
                  redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
                    itemTrace.initial,
                  ),
                };
                for (const itemStep of itemTrace.steps) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(10, discovery),
                    {
                      kind: "redeemerItemStep",
                      redeemerControl: null,
                      control: itemStep.control,
                      witness: itemStep.witness,
                    },
                  );
                  if (
                    itemStep.next.stage !==
                    MidgardRedeemerItemProofStages.Terminal
                  ) {
                    discovery = {
                      ...discovery,
                      redeemerItemControlHash:
                        hashMidgardRedeemerItemProofControl(itemStep.next),
                    };
                    continue;
                  }
                  if (
                    redeemerPointerMatchesPurpose({
                      purposeKind: purpose.purposeKind,
                      purposeIndex: purpose.purposeIndex,
                      redeemerTag: redeemer.tag,
                      redeemerIndex: redeemer.index,
                    })
                  ) {
                    matchedRedeemerIndex = redeemerIndex;
                    const executionLeaf = hashMidgardScriptExecutionLeaf({
                      languageTag: matchedSource.languageTag,
                      purposeLeaf: purpose.leaf,
                      sourceLeaf: matchedSource.entry.leaf,
                      redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                    });
                    scriptExecutionEntries.push({
                      purpose,
                      source: matchedSource.entry,
                      sourceIndex: matchedSource.sourceIndex,
                      languageTag: matchedSource.languageTag,
                      redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                      leaf: executionLeaf,
                    });
                    discovery = resetCurrent({
                      ...discovery,
                      purposeCursor: purposeCursor + 1,
                      usedRedeemerBitmap: setDiscoveryBit(
                        discovery.usedRedeemerBitmap,
                        redeemerIndex,
                      ),
                      executionFrontier: appendMidgardValidationMerkleLeaf(
                        discovery.executionFrontier,
                        executionLeaf,
                      ),
                    });
                  } else {
                    discovery = {
                      ...discovery,
                      redeemerCursor: redeemerIndex + 1,
                      redeemerItemControlHash: Buffer.alloc(0),
                    };
                  }
                }
                if (matchedRedeemerIndex >= 0) {
                  break;
                }
              }
              if (matchedRedeemerIndex < 0) {
                pushWitness(
                  "scriptSources",
                  discoveryWitnessCbor(10, discovery),
                );
                return phaseStops(
                  {
                    RedeemerMissing: {
                      purpose_kind: BigInt(discovery.currentPurposeKind),
                      purpose_index: BigInt(discovery.currentPurposeIndex),
                    },
                  },
                  "V1 redeemer scan found no redeemer for the purpose",
                );
              }
            }

            pushWitness("scriptSources", discoveryWitnessCbor(8, discovery));
            discovery = resetCurrent({
              ...discovery,
              sourceCursor: 0,
            });
            for (
              let sourceIndex = 0;
              sourceIndex < scriptSourceEntries.length;
              sourceIndex += 1
            ) {
              const source = scriptSourceEntries[sourceIndex]!;
              pushWitness(
                "scriptSources",
                discoveryWitnessCbor(11, discovery),
                {
                  kind: "scriptSourceScan",
                  sourceIndex,
                  originKind: source.originKind,
                  sourceKey: source.sourceKey,
                  scriptLanguageTag: source.scriptLanguageTag,
                  scriptHash: source.scriptHash,
                  scriptTotalLength: source.scriptTotalLength,
                  scriptItemCommitment: source.scriptItemCommitment,
                  siblings: sourceMembership(sourceIndex).siblings,
                },
              );
              if (
                source.originKind === "inline" &&
                (discovery.usedInlineBitmap & (1n << BigInt(sourceIndex))) ===
                  0n
              ) {
                return phaseStops(
                  {
                    UnusedScriptWitness: {
                      script_index: BigInt(sourceIndex),
                    },
                  },
                  "V1 source audit found an extraneous inline script",
                );
              }
              discovery = {
                ...discovery,
                sourceCursor: sourceIndex + 1,
              };
            }

            pushWitness("scriptSources", discoveryWitnessCbor(11, discovery));
            discovery = {
              ...discovery,
              redeemerCursor: 0,
            };
            for (
              let redeemerIndex = 0;
              redeemerIndex < decodedProofRedeemers.length;
              redeemerIndex += 1
            ) {
              const item = redeemerWitnessesCollection.items[redeemerIndex]!;
              const itemTrace = buildMidgardRedeemerItemProofTrace({
                itemIndex: redeemerIndex,
                itemCount: decodedProofRedeemers.length,
                itemBytes: item.bytes,
                mode: MidgardRedeemerItemProofModes.Descriptor,
              });
              pushWitness(
                "scriptSources",
                discoveryWitnessCbor(12, discovery),
                {
                  kind: "redeemerScanBegin",
                  itemIndex: redeemerIndex,
                  itemCount: decodedProofRedeemers.length,
                  totalLength: item.bytes.length,
                  itemCommitment: item.commitment,
                  siblings: redeemerMembership(redeemerIndex).siblings,
                },
              );
              discovery = {
                ...discovery,
                redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
                  itemTrace.initial,
                ),
              };
              for (const itemStep of itemTrace.steps) {
                pushWitness(
                  "scriptSources",
                  discoveryWitnessCbor(12, discovery),
                  {
                    kind: "redeemerItemStep",
                    redeemerControl: null,
                    control: itemStep.control,
                    witness: itemStep.witness,
                  },
                );
                if (
                  itemStep.next.stage !==
                  MidgardRedeemerItemProofStages.Terminal
                ) {
                  discovery = {
                    ...discovery,
                    redeemerItemControlHash:
                      hashMidgardRedeemerItemProofControl(itemStep.next),
                  };
                }
              }
              if (
                (discovery.usedRedeemerBitmap &
                  (1n << BigInt(redeemerIndex))) ===
                0n
              ) {
                return phaseStops(
                  {
                    UnusedRedeemer: {
                      redeemer_index: BigInt(redeemerIndex),
                    },
                  },
                  "V1 redeemer audit found an extraneous redeemer",
                );
              }
              discovery = {
                ...discovery,
                redeemerCursor: redeemerIndex + 1,
                redeemerItemControlHash: Buffer.alloc(0),
              };
            }

            const nativeScriptBaseFields: unknown[] = [
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(scriptSourceControl.resolvedInputCount),
              BigInt(replaySpendIndex),
              encodeFrontierPeaks(resolvedItemFrontier),
              BigInt(signerFrontier.count),
              signerFrontierCommitment,
              BigInt(replaySourceFrontier.count),
              encodeFrontierPeaks(replaySourceFrontier),
              BigInt(redeemerFrontier.count),
              encodeFrontierPeaks(redeemerFrontier),
              BigInt(allPurposeFrontier.count),
              encodeFrontierPeaks(allPurposeFrontier),
              BigInt(outputFrontier.count),
              encodeFrontierPeaks(outputFrontier),
              encodeFrontierPeaks(outputDescriptorFrontier),
              BigInt(mintFoldControl.assetFrontier.count),
              encodeFrontierPeaks(mintFoldControl.assetFrontier),
              BigInt(discovery.executionFrontier.count),
              encodeFrontierPeaks(discovery.executionFrontier),
            ];
            const nativeScriptFields: unknown[] = [
              ...nativeScriptBaseFields,
              0n,
              0n,
              resolutionScheduleHash,
            ];
            authenticatedNativeScriptsBaseFields = nativeScriptBaseFields;
            authenticatedNativeScriptsWitnessCbor =
              encodeCbor(nativeScriptFields);
            pushWitness("scriptSources", discoveryWitnessCbor(12, discovery));
          }
        }
        return phaseContinues;
      }),
    );
    return {
      scriptSourceEntries,
      scriptPurposeEntries,
      scriptExecutionEntries,
      admittedOutputDescriptorCbors,
      admittedOutputDescriptorLeafHashes,
      mintFoldControl,
      authenticatedNativeScriptsWitnessCbor,
      authenticatedNativeScriptsBaseFields,
    };
  });
