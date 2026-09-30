import "node:fs";
import "node:util";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/cek-program.js";
import "../src/midgard-redeemers.js";
import "../src/validation-machine/index.js";
import "./helpers/ordered-collection-boundary.js";
import "./helpers/retained-da-boundary.js";
import "./data-breadth-boundary.assert-exact-fold-semantics.js";
import "./data-breadth-boundary.exact-broad-frontier-vector.js";

import {
  buildMidgardLedgerOutputProofTrace,
  buildMidgardRedeemerItemProofTrace,
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardCekProgramMaterialSidecar,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  finalizeMidgardCekDataTraverse,
  finalizeMidgardRedeemerItemProof,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  midgardFieldCommitment,
  midgardNativeTxFullToCardanoTxEncoding,
  midgardRedeemerItemDescriptor,
  MidgardRedeemerItemProofModes,
  nextMidgardRedeemerItemProofSpan,
  validateMidgardConsensusTx,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  Lucid,
  type SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalScriptArtifact } from "../src/cek-program.js";
import { decodeMidgardRedeemers } from "../src/midgard-redeemers.js";
import { countedMachineFieldTrace } from "../src/validation-machine/index.js";
import {
  alwaysSucceedsCompiledCode,
  assertExactBreadthSemantics,
  assertExactFoldSemantics,
  assertExactTerminalSummary,
  cardanoBreadthDataCbor,
  dataNodeCount,
} from "./data-breadth-boundary.assert-exact-fold-semantics.js";
import {
  exactBroadFrontierVector,
  exactTerminalVector,
  extractAuthenticatedLedgerOutputDataSteps,
  maximumDatumChunkBytes,
  maximumRedeemerChunkBytes,
  maximumSourceSpan,
  replayRedeemerItemProof,
} from "./data-breadth-boundary.exact-broad-frontier-vector.js";
import {
  buildCollateralFreeMidgardSchemaParallelCandidate,
  buildSignedCardanoNestedDatumCandidate,
  buildSignedCardanoSpendRedeemersCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  CARDANO_BOUNDARY_TOTAL_COLLATERAL,
  deterministicCardanoBoundaryPrivateKey,
  findSignedCardanoCollectionBoundary,
  measureCollateralizedPlutusFeasibilityCandidate,
  measureMidgardCompleteItemCarriageFit,
  measureSignedCardanoNestedDatum,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaCanonicalBoundary } from "./helpers/retained-da-boundary.js";

if (alwaysSucceedsCompiledCode === undefined) {
  throw new Error(
    "Missing always-succeeds blueprint entry midgard.deposit_spend.else",
  );
}

const spendingScript: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(alwaysSucceedsCompiledCode),
};

/**
 * The exact genuine signed-Cardano breadth boundaries, per Data kind and per
 * carriage path. Before these pins the two searches were only bounded relative
 * to `maxTxSize`, and the measured vectors were printed rather than asserted —
 * a silently shrunk collection kept the suite green. Each entry is the maximum
 * breadth the search must land on, its adjacent overflow, and both signed
 * transaction sizes and Data CBOR sizes.
 */
const MAXIMUM_DATUM_BREADTH_BOUNDARY = {
  constructor: {
    acceptedBreadth: 16_166,
    acceptedDataCborBytes: 16_173,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 16_167,
    adjacentDataCborBytes: 16_174,
    adjacentSignedBytes: 16_385,
  },
  list: {
    acceptedBreadth: 16_171,
    acceptedDataCborBytes: 16_173,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 16_172,
    adjacentDataCborBytes: 16_174,
    adjacentSignedBytes: 16_385,
  },
  map: {
    acceptedBreadth: 4_112,
    acceptedDataCborBytes: 16_171,
    acceptedSignedBytes: 16_382,
    adjacentBreadth: 4_113,
    adjacentDataCborBytes: 16_175,
    adjacentSignedBytes: 16_386,
  },
} as const;

const MAXIMUM_REDEEMER_BREADTH_BOUNDARY = {
  constructor: {
    acceptedBreadth: 15_977,
    acceptedDataCborBytes: 15_984,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 15_978,
    adjacentDataCborBytes: 15_985,
    adjacentSignedBytes: 16_385,
  },
  list: {
    acceptedBreadth: 15_982,
    acceptedDataCborBytes: 15_984,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 15_983,
    adjacentDataCborBytes: 15_985,
    adjacentSignedBytes: 16_385,
  },
  map: {
    acceptedBreadth: 4_065,
    acceptedDataCborBytes: 15_983,
    acceptedSignedBytes: 16_383,
    adjacentBreadth: 4_066,
    adjacentDataCborBytes: 15_987,
    adjacentSignedBytes: 16_387,
  },
} as const;

describe("canonical V1 Cardano Data breadth boundaries", () => {
  it.each(["constructor", "list", "map"] as const)(
    "retains maximum %s breadth through the inline-datum path",
    async (kind) => {
      const privateKey = deterministicCardanoBoundaryPrivateKey(0);
      const funder = {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(privateKey.to_public().hash()),
        )
          .to_address()
          .to_bech32(),
        assets: { lovelace: 40_000_000_000n },
      };

      const buildCandidate = (breadth: number) =>
        buildSignedCardanoNestedDatumCandidate({
          privateKeyBech32: funder.privateKey,
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedNestedLeafCount: breadth,
          nestedDatumCborHex: cardanoBreadthDataCbor(kind, breadth),
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        });
      const boundary = await findSignedCardanoCollectionBoundary({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
        buildSignedCandidate: buildCandidate,
      });
      const accepted = measureSignedCardanoNestedDatum(
        boundary.accepted.cborHex,
      );
      const adjacent = measureSignedCardanoNestedDatum(
        boundary.adjacent.cborHex,
      );
      const acceptedDataCborHex = cardanoBreadthDataCbor(
        kind,
        boundary.accepted.requestedItemCount,
      );
      const adjacentDataCborHex = cardanoBreadthDataCbor(
        kind,
        boundary.adjacent.requestedItemCount,
      );
      expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
        CARDANO_BOUNDARY_MAX_TX_SIZE,
      );
      expect(boundary.adjacent.signedBytes).toBeGreaterThan(
        CARDANO_BOUNDARY_MAX_TX_SIZE,
      );
      expect(boundary.adjacent.requestedItemCount).toBe(
        boundary.accepted.requestedItemCount + 1,
      );

      // The genuine maximum and its immediately adjacent control are exact, not
      // merely "whatever the search returned".
      expect({
        acceptedBreadth: boundary.accepted.requestedItemCount,
        acceptedDataCborBytes: acceptedDataCborHex.length / 2,
        acceptedSignedBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedBytes: boundary.adjacent.signedBytes,
      }).toEqual(MAXIMUM_DATUM_BREADTH_BOUNDARY[kind]);
      expect(accepted.datumCborHex).toBe(acceptedDataCborHex);
      expect(adjacent.datumCborHex).toBe(adjacentDataCborHex);
      assertExactBreadthSemantics(
        kind,
        boundary.accepted.requestedItemCount,
        acceptedDataCborHex,
      );
      assertExactBreadthSemantics(
        kind,
        boundary.adjacent.requestedItemCount,
        adjacentDataCborHex,
      );
      expect({
        outputCount: accepted.outputCount,
        hasWithdrawals: accepted.hasWithdrawals,
        hasMint: accepted.hasMint,
        hasPlutusScripts: accepted.hasPlutusScripts,
        hasRedeemers: accepted.hasRedeemers,
        collateralInputCount: accepted.collateralInputCount,
      }).toEqual({
        outputCount: 1,
        hasWithdrawals: false,
        hasMint: false,
        hasPlutusScripts: false,
        hasRedeemers: false,
        collateralInputCount: 0,
      });

      const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      );
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
      expect(validateMidgardConsensusTx(native, canonical.length)).toBeNull();
      const outputCbors = decodeMidgardNativeByteListPreimage(
        native.body.outputsPreimageCbor,
        "native.outputs",
      );
      expect(outputCbors).toHaveLength(1);
      const output = decodeMidgardTxOutput(outputCbors[0]!);
      expect(output.datum?.cbor.toString("hex")).toBe(acceptedDataCborHex);
      const outputTrace = buildMidgardLedgerOutputProofTrace({
        outputIndex: 0,
        outputCbor: outputCbors[0]!,
      });
      const dataSteps = extractAuthenticatedLedgerOutputDataSteps(outputTrace);
      expect(dataSteps.at(-1)?.action?.kind).toBe("finalizeFrame");
      assertExactFoldSemantics({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        steps: dataSteps,
      });
      const terminalSummary = finalizeMidgardCekDataTraverse(
        outputTrace.terminal.datum!,
      );
      expect(terminalSummary).not.toBeNull();
      assertExactTerminalSummary({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        dataCborHex: acceptedDataCborHex,
        summary: terminalSummary!,
      });
      expect(maximumSourceSpan(dataSteps)).toBeLessThanOrEqual(
        MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
      );
      expect(maximumSourceSpan(dataSteps)).toBeLessThanOrEqual(132);
      expect(maximumDatumChunkBytes(outputTrace)).toBeLessThanOrEqual(
        MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
      );
      const reconstructed = measureSignedCardanoNestedDatum(
        Buffer.from(midgardNativeTxFullToCardanoTxEncoding(native)).toString(
          "hex",
        ),
      );
      expect({
        address: reconstructed.outputAddress,
        lovelace: reconstructed.outputLovelace,
        datumCborHex: reconstructed.datumCborHex,
      }).toEqual({
        address: accepted.outputAddress,
        lovelace: accepted.outputLovelace,
        datumCborHex: accepted.datumCborHex,
      });

      const emulator = new Emulator(
        [funder],
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
      );
      const txHash = await emulator.submitTx(boundary.accepted.cborHex);
      await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
      await exerciseMidgardRetainedDaCanonicalBoundary({
        canonicalTransactionCbor: canonical,
        corpusLabel: `maximum-${kind}-datum-breadth`,
      });

      const vector = {
        breadth: boundary.accepted.requestedItemCount,
        nodeCount: dataNodeCount(kind, boundary.accepted.requestedItemCount),
        dataCborBytes: acceptedDataCborHex.length / 2,
        signedCardanoBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
        nativeCanonicalBytes: canonical.length,
        outputProofSteps: outputTrace.steps.length,
        dataTraverseSteps: dataSteps.length,
        maximumSourceSpan: maximumSourceSpan(dataSteps),
        maximumChunkBytes: maximumDatumChunkBytes(outputTrace),
        broadFrontier: exactBroadFrontierVector(kind, dataSteps),
        terminal: exactTerminalVector(dataSteps),
      };

      if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
        console.info(
          JSON.stringify({
            dataBreadthBoundaryV1: { [`datum_${kind}`]: vector },
          }),
        );
      }
    },
    600_000,
  );

  /**
   * §3.2 complete-item-first ordering for C23/C24/C25.
   *
   * Before any bounded Data traversal is admitted as a fallback, the complete
   * proof item carrying the maximum Data must be constructed and measured on
   * both complete routes: direct carriage in the proof transaction, and
   * single-publication inline-datum carriage consumed as a reference input.
   *
   * The measured outcome is recorded as found, not as hoped. Each maximum
   * breadth is bound by the 16,384-byte signed Cardano transaction, so the
   * complete item is itself ~16 KB and overflows both complete routes — which
   * requires the bounded carriage specified in `docs/spec/midgard-tx.md`.
   * The case therefore pins
   * both sides of the real carriage boundary per kind: the largest complete
   * Data item that both complete routes admit, its adjacent overflow, and the
   * maximum shape's exact overshoot.
   */
  it.each(["constructor", "list", "map"] as const)(
    "measures complete %s Data direct and reference carriage before any bounded fallback",
    async (kind) => {
      const privateKey = deterministicCardanoBoundaryPrivateKey(0);
      const addressBytes = Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(privateKey.to_public().hash()),
        )
          .to_address()
          .to_raw_bytes(),
      );
      const outputItemForBreadth = (breadth: number): Buffer =>
        encodeMidgardTxOutput({
          address: addressBytes,
          value: { lovelace: 30_000_000n, assets: new Map() },
          datum: {
            kind: "inline",
            cbor: Buffer.from(cardanoBreadthDataCbor(kind, breadth), "hex"),
          },
        });
      const fitForBreadth = (breadth: number) =>
        measureMidgardCompleteItemCarriageFit({
          fieldIndex: 2,
          itemIndex: 0,
          itemCbor: outputItemForBreadth(breadth),
        });

      // Largest complete Data item both complete routes admit, found by
      // bisection on the exact encoded item length.
      const publicationBound =
        fitForBreadth(1).maxSinglePublicationCompleteItemBytes;
      let low = 1;
      let high = 32_768;
      while (low + 1 < high) {
        const middle = Math.floor((low + high) / 2);
        if (outputItemForBreadth(middle).length <= publicationBound) {
          low = middle;
        } else {
          high = middle;
        }
      }
      const acceptedBreadth = low;
      const adjacentBreadth = low + 1;
      const acceptedFit = fitForBreadth(acceptedBreadth);
      const adjacentFit = fitForBreadth(adjacentBreadth);
      expect(acceptedFit.itemBytes).toBeLessThanOrEqual(publicationBound);
      expect(adjacentFit.itemBytes).toBeGreaterThan(publicationBound);
      expect(acceptedFit).toMatchObject({
        carriage: "reference",
        fitsSinglePublicationCarriage: true,
        requiresBoundedFallback: false,
      });
      expect(acceptedFit.publicationTransactionBytes).toBeLessThanOrEqual(
        acceptedFit.maxL1TransactionBytes,
      );
      // Above the 13,522-byte direct frontier (#622 ruling (b), re-pinned at
      // the #617 wave sign-off) the applied direct route is measured out, so
      // the complete item survives only through reference carriage — this is the
      // "reference before fallback" step, not a fallback.
      expect(acceptedFit.fitsDirectCarriage).toBe(false);
      expect(adjacentFit).toMatchObject({
        fitsDirectCarriage: false,
        fitsSinglePublicationCarriage: false,
        requiresBoundedFallback: true,
      });

      // The direct route is not vacuous either: a smaller complete Data item
      // of the same kind is admitted directly, so both complete routes are
      // exercised before any bounded traversal is considered.
      let directLow = 1;
      let directHigh = acceptedBreadth;
      const directBound = acceptedFit.maxReliableDirectCompleteItemBytes;
      while (directLow + 1 < directHigh) {
        const middle = Math.floor((directLow + directHigh) / 2);
        if (outputItemForBreadth(middle).length <= directBound) {
          directLow = middle;
        } else {
          directHigh = middle;
        }
      }
      const directFit = fitForBreadth(directLow);
      expect(directFit).toMatchObject({
        carriage: "direct",
        fitsDirectCarriage: true,
        fitsSinglePublicationCarriage: true,
        requiresBoundedFallback: false,
      });

      // The genuine Cardano maximum for this kind, measured against both
      // complete routes. Its overflow is the §3.2 necessity for the bounded
      // Data traversal the sibling cases exercise.
      const buildCandidate = (breadth: number) =>
        buildSignedCardanoNestedDatumCandidate({
          privateKeyBech32: privateKey.to_bech32(),
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: 40_000_000_000n,
          recipientAddress: CML.EnterpriseAddress.new(
            0,
            CML.Credential.new_pub_key(privateKey.to_public().hash()),
          )
            .to_address()
            .to_bech32(),
          requestedNestedLeafCount: breadth,
          nestedDatumCborHex: cardanoBreadthDataCbor(kind, breadth),
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        });
      const boundary = await findSignedCardanoCollectionBoundary({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
        buildSignedCandidate: buildCandidate,
      });
      const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      );
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
      expect(validateMidgardConsensusTx(native, canonical.length)).toBeNull();
      const outputCbors = decodeMidgardNativeByteListPreimage(
        native.body.outputsPreimageCbor,
        "native.outputs",
      );
      expect(outputCbors).toHaveLength(1);
      expect(
        decodeMidgardTxOutput(outputCbors[0]!).datum?.cbor.toString("hex"),
      ).toBe(
        cardanoBreadthDataCbor(kind, boundary.accepted.requestedItemCount),
      );
      const maximumFit = measureMidgardCompleteItemCarriageFit({
        fieldIndex: 2,
        itemIndex: 0,
        itemCbor: outputCbors[0]!,
      });
      expect(maximumFit).toMatchObject({
        fitsDirectCarriage: false,
        fitsSinglePublicationCarriage: false,
        requiresBoundedFallback: true,
      });
      expect(maximumFit.itemBytes).toBeGreaterThan(
        maximumFit.maxSinglePublicationCompleteItemBytes,
      );
      expect(maximumFit.publicationTransactionBytes).toBeGreaterThan(
        maximumFit.maxL1TransactionBytes,
      );
      // Bounded chunk fallback is the only remaining representation, and it is
      // the one the deployed traversal uses.
      expect(maximumFit.boundedFallbackChunkCount).toBeGreaterThan(1);

      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            dataBreadthCompleteItemFitV1: {
              [kind]: {
                directCarriageBreadth: directLow,
                directCarriageItemBytes: directFit.itemBytes,
                referenceCarriageBreadth: acceptedBreadth,
                referenceCarriageItemBytes: acceptedFit.itemBytes,
                referenceCarriagePublicationTransactionBytes:
                  acceptedFit.publicationTransactionBytes,
                adjacentBreadth,
                adjacentItemBytes: adjacentFit.itemBytes,
                cardanoMaximumBreadth: boundary.accepted.requestedItemCount,
                cardanoMaximumItemBytes: maximumFit.itemBytes,
                cardanoMaximumPublicationTransactionBytes:
                  maximumFit.publicationTransactionBytes,
                cardanoMaximumOvershootBytes:
                  maximumFit.publicationTransactionBytes -
                  maximumFit.maxL1TransactionBytes,
                boundedFallbackChunkCount: maximumFit.boundedFallbackChunkCount,
              },
            },
          }),
        );
      }
    },
    600_000,
  );

  it("retains maximum constructor/list/map breadth through genuine Cardano redeemers and the Midgard schema projection", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKey(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress("Custom", spendingScript);
    const genesis = [
      {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 1_000_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 1_000_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: "",
        address: scriptAddress,
        assets: { lovelace: 10_000_000n },
        outputData: { inline: Data.void() },
      },
    ];
    const seedEmulator = new Emulator(
      genesis,
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const walletInputs = (await seedEmulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    const scriptInputs = await seedEmulator.getUtxos(scriptAddress);
    const lucid = await Lucid(seedEmulator, "Custom");
    lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
    const completedSeed = await lucid
      .newTx()
      .collectFrom([walletInputs[0]!])
      .collectFrom([scriptInputs[0]!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(spendingScript)
      .complete({ localUPLCEval: true });
    const signedSeed = await completedSeed.sign.withWallet().complete();
    const seed = measureCollateralizedPlutusFeasibilityCandidate(
      signedSeed.toCBOR(),
    );
    const seedTransaction = CML.Transaction.from_cbor_hex(signedSeed.toCBOR());
    const seedScripts = seedTransaction.witness_set().plutus_v3_scripts();
    expect(seedScripts?.len()).toBe(1);
    const vectors: Record<string, unknown> = {};

    for (const kind of ["constructor", "list", "map"] as const) {
      const buildCandidate = async (breadth: number) => {
        const candidate = await buildSignedCardanoSpendRedeemersCandidate({
          privateKeyBech32: privateKey.to_bech32(),
          feeFundingInput: walletInputs[0]!,
          collateralInput: walletInputs[1]!,
          availableScriptInputs: scriptInputs,
          recipientAddress: walletAddress,
          plutusV3ScriptCborHex: seedScripts!.get(0).to_cbor_hex(),
          redeemerDataCborHex: cardanoBreadthDataCbor(kind, breadth),
          executionMemory: seed.executionMemory,
          executionSteps: seed.executionSteps,
          requestedRedeemerCount: 1,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
          priceMem: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.priceMem,
          priceStep: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.priceStep,
          collateralPercentage:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.collateralPercentage,
          costModels: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.costModels,
        });
        return { ...candidate, requestedItemCount: breadth };
      };
      const boundary = await findSignedCardanoCollectionBoundary({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
        buildSignedCandidate: buildCandidate,
      });
      const accepted = measureCollateralizedPlutusFeasibilityCandidate(
        boundary.accepted.cborHex,
      );
      const adjacent = measureCollateralizedPlutusFeasibilityCandidate(
        boundary.adjacent.cborHex,
      );
      const acceptedDataCborHex = cardanoBreadthDataCbor(
        kind,
        boundary.accepted.requestedItemCount,
      );
      const adjacentDataCborHex = cardanoBreadthDataCbor(
        kind,
        boundary.adjacent.requestedItemCount,
      );
      expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
        CARDANO_BOUNDARY_MAX_TX_SIZE,
      );
      expect(boundary.adjacent.signedBytes).toBeGreaterThan(
        CARDANO_BOUNDARY_MAX_TX_SIZE,
      );
      expect(boundary.adjacent.requestedItemCount).toBe(
        boundary.accepted.requestedItemCount + 1,
      );

      // The genuine maximum and its immediately adjacent control are exact, not
      // merely "whatever the search returned".
      expect({
        acceptedBreadth: boundary.accepted.requestedItemCount,
        acceptedDataCborBytes: acceptedDataCborHex.length / 2,
        acceptedSignedBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedBytes: boundary.adjacent.signedBytes,
      }).toEqual(MAXIMUM_REDEEMER_BREADTH_BOUNDARY[kind]);
      expect(accepted.redeemerCount).toBe(1);
      expect(adjacent.redeemerCount).toBe(1);
      expect(accepted.redeemerTags).toEqual([CML.RedeemerTag.Spend]);
      expect(accepted.redeemerIndexes).toEqual([1n]);
      expect(accepted.redeemerDataCborHexes).toEqual([acceptedDataCborHex]);
      expect(adjacent.redeemerDataCborHexes).toEqual([adjacentDataCborHex]);
      assertExactBreadthSemantics(
        kind,
        boundary.accepted.requestedItemCount,
        acceptedDataCborHex,
      );
      assertExactBreadthSemantics(
        kind,
        boundary.adjacent.requestedItemCount,
        adjacentDataCborHex,
      );
      expect(accepted.executionMemory).toBe(seed.executionMemory);
      expect(accepted.executionSteps).toBe(seed.executionSteps);
      expect(accepted.totalCollateral).toBe(CARDANO_BOUNDARY_TOTAL_COLLATERAL);
      const acceptedTransaction = CML.Transaction.from_cbor_hex(
        boundary.accepted.cborHex,
      );
      expect(acceptedTransaction.body().withdrawals()).toBeUndefined();
      expect(acceptedTransaction.body().mint()).toBeUndefined();
      const acceptedScripts = acceptedTransaction
        .witness_set()
        .plutus_v3_scripts();
      expect(acceptedScripts?.len()).toBe(1);
      const sourceRawFlatProgramBytes = Buffer.from(
        acceptedScripts!.get(0).to_raw_bytes(),
      );
      const artifact = buildMidgardCanonicalScriptArtifact({
        language: "PlutusV3",
        sourceRawFlatProgramBytes,
      });
      const canonicalMaterialEntries = decodeMidgardCekProgramMaterialSidecar(
        artifact.canonicalMaterialSidecarCbor,
      );
      expect(canonicalMaterialEntries).toEqual(
        artifact.canonicalMaterialEntries,
      );
      expect(
        verifyMidgardCekProgramMaterialBundle(
          [artifact.canonicalProgram.envelope],
          canonicalMaterialEntries,
        ),
      ).toHaveLength(1);
      expect(artifact.canonicalMidgardCredentialScriptHash).toBe(
        hashMidgardVersionedScript(artifact.canonicalMidgardCredentialScript),
      );
      expect(artifact.sourceRawScriptAuditHash).toBe(
        hashMidgardVersionedScript({
          language: "PlutusV3",
          scriptBytes: sourceRawFlatProgramBytes,
        }),
      );
      expect(artifact.sourceRawScriptAuditHash).not.toBe(
        artifact.canonicalMidgardCredentialScriptHash,
      );

      let collateralRejection:
        | {
            readonly message: string;
            readonly code: string | null;
            readonly detail: string | null;
          }
        | undefined;
      try {
        cardanoTxBytesToMidgardNativeTxCanonicalCbor(
          Buffer.from(boundary.accepted.cborHex, "hex"),
        );
      } catch (error) {
        const structured = error as {
          readonly code?: unknown;
          readonly detail?: unknown;
        };
        collateralRejection = {
          message: error instanceof Error ? error.message : String(error),
          code: typeof structured.code === "string" ? structured.code : null,
          detail:
            typeof structured.detail === "string" ? structured.detail : null,
        };
      }
      expect(collateralRejection).toEqual({
        message:
          "Cardano tx cannot be converted to Midgard native format without dropping fields",
        code: "E_CONVERSION_UNSUPPORTED_FEATURE",
        detail: "collateral_inputs",
      });

      const schemaProjectionSource =
        buildCollateralFreeMidgardSchemaParallelCandidate({
          collateralizedCardanoCborHex: boundary.accepted.cborHex,
          privateKeyBech32: privateKey.to_bech32(),
        });
      const schemaProjectionSourceShape = CML.Transaction.from_cbor_hex(
        schemaProjectionSource.cborHex,
      );
      expect(
        schemaProjectionSourceShape.body().collateral_inputs(),
      ).toBeUndefined();
      expect(
        schemaProjectionSourceShape.body().collateral_return(),
      ).toBeUndefined();
      expect(
        schemaProjectionSourceShape.body().total_collateral(),
      ).toBeUndefined();
      expect(
        Array.from(
          {
            length: schemaProjectionSourceShape.body().inputs().len(),
          },
          (_, index) =>
            schemaProjectionSourceShape
              .body()
              .inputs()
              .get(index)
              .to_cbor_hex(),
        ),
      ).toEqual(
        Array.from(
          {
            length: acceptedTransaction.body().inputs().len(),
          },
          (_, index) =>
            acceptedTransaction.body().inputs().get(index).to_cbor_hex(),
        ),
      );
      expect(
        Array.from(
          {
            length: schemaProjectionSourceShape.body().outputs().len(),
          },
          (_, index) =>
            schemaProjectionSourceShape
              .body()
              .outputs()
              .get(index)
              .to_cbor_hex(),
        ),
      ).toEqual(
        Array.from(
          {
            length: acceptedTransaction.body().outputs().len(),
          },
          (_, index) =>
            acceptedTransaction.body().outputs().get(index).to_cbor_hex(),
        ),
      );
      expect(schemaProjectionSourceShape.body().fee()).toBe(
        acceptedTransaction.body().fee(),
      );
      expect(
        schemaProjectionSourceShape.body().script_data_hash()?.to_hex(),
      ).toBe(acceptedTransaction.body().script_data_hash()?.to_hex());
      const schemaSourceCanonical =
        cardanoTxBytesToMidgardNativeTxCanonicalCbor(
          Buffer.from(schemaProjectionSource.cborHex, "hex"),
        );
      const schemaSourceNative = decodeMidgardNativeTxFullFromCanonicalCbor(
        schemaSourceCanonical,
      );
      const sourceScripts = decodeMidgardVersionedScriptListPreimage(
        schemaSourceNative.witnessSet.scriptTxWitsPreimageCbor,
      );
      expect(sourceScripts).toEqual([
        {
          language: "PlutusV3",
          scriptBytes: sourceRawFlatProgramBytes,
        },
      ]);
      expect(hashMidgardVersionedScript(sourceScripts[0]!)).toBe(
        artifact.sourceRawScriptAuditHash,
      );
      expect(schemaSourceNative.witnessSet.addrTxWitsPreimageCbor).not.toEqual(
        Buffer.from([0x80]),
      );
      const projectedScriptIntegrityHash =
        computeScriptIntegrityHashForLanguages(
          midgardFieldCommitment(
            schemaSourceNative.witnessSet.redeemerTxWitsPreimageCbor,
          ),
          ["PlutusV3"],
        );
      expect(projectedScriptIntegrityHash).toHaveLength(32);
      expect(artifact.canonicalMidgardCredentialScript.language).toBe(
        "PlutusV3",
      );
      expect(projectedScriptIntegrityHash).toEqual(
        computeScriptIntegrityHashForLanguages(
          midgardFieldCommitment(
            schemaSourceNative.witnessSet.redeemerTxWitsPreimageCbor,
          ),
          ["PlutusV3"],
        ),
      );

      const schemaProjection = materializeMidgardNativeTxFromCanonical({
        version: schemaSourceNative.version,
        validity: schemaSourceNative.validity,
        body: {
          ...schemaSourceNative.body,
          scriptIntegrityHash: projectedScriptIntegrityHash,
        },
        witnessSet: {
          ...schemaSourceNative.witnessSet,
          addrTxWitsPreimageCbor: Buffer.from([0x80]),
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            artifact.canonicalMidgardCredentialScript,
          ]),
        },
      });
      const canonical = encodeMidgardNativeTxCanonical(schemaProjection);
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
      expect(native.witnessSet.addrTxWitsPreimageCbor).toEqual(
        Buffer.from([0x80]),
      );
      expect(native.body.scriptIntegrityHash).toEqual(
        projectedScriptIntegrityHash,
      );
      const projectedScripts = decodeMidgardVersionedScriptListPreimage(
        native.witnessSet.scriptTxWitsPreimageCbor,
      );
      expect(projectedScripts).toEqual([
        artifact.canonicalMidgardCredentialScript,
      ]);
      expect(hashMidgardVersionedScript(projectedScripts[0]!)).toBe(
        artifact.canonicalMidgardCredentialScriptHash,
      );
      expect(validateMidgardConsensusTx(native, canonical.length)).toBeNull();
      const redeemers = decodeMidgardRedeemers(
        native.witnessSet.redeemerTxWitsPreimageCbor,
      );
      expect(redeemers).toHaveLength(1);
      expect(redeemers[0]!.dataCborHex).toBe(acceptedDataCborHex);
      expect(redeemers[0]).toMatchObject({
        tag: CML.RedeemerTag.Spend,
        index: 1n,
        exUnits: {
          memory: seed.executionMemory,
          steps: seed.executionSteps,
        },
      });
      const field = countedMachineFieldTrace(
        8,
        native.witnessSet.redeemerTxWitsPreimageCbor,
      );
      expect(field.items).toHaveLength(1);
      const item = field.items[0]!;
      const itemTrace = buildMidgardRedeemerItemProofTrace({
        itemIndex: item.itemIndex,
        itemCount: field.items.length,
        itemBytes: item.bytes,
        mode: MidgardRedeemerItemProofModes.Data,
        expectedPurposeTag: CML.RedeemerTag.Spend,
        expectedPointerIndex: 1,
      });
      expect(itemTrace.steps[0]?.witness.action.kind).toBe("openHeader");
      expect(itemTrace.steps[1]?.witness.action.kind).toBe("openTail");
      const descriptor = midgardRedeemerItemDescriptor(
        itemTrace.steps[1]!.next,
      );
      expect(descriptor).toMatchObject({
        itemIndex: 0,
        itemCount: 1,
        totalLength: item.bytes.length,
        purposeTag: CML.RedeemerTag.Spend,
        pointerIndex: 1,
        dataLength: acceptedDataCborHex.length / 2,
        executionMemory: seed.executionMemory,
        executionSteps: seed.executionSteps,
      });
      expect(descriptor?.itemCommitment).toEqual(item.commitment);
      const dataSteps = replayRedeemerItemProof(itemTrace);
      expect(dataSteps.at(-1)?.action?.kind).toBe("finalizeFrame");
      assertExactFoldSemantics({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        steps: dataSteps,
      });
      const terminalSummary = finalizeMidgardRedeemerItemProof(
        itemTrace.terminal,
      );
      expect(terminalSummary).not.toBeNull();
      assertExactTerminalSummary({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        dataCborHex: acceptedDataCborHex,
        summary: terminalSummary!,
      });
      const maximumItemSourceSpan = itemTrace.steps.reduce(
        (maximum, { control }) =>
          Math.max(
            maximum,
            nextMidgardRedeemerItemProofSpan(control)?.length ?? 0,
          ),
        0,
      );
      expect(maximumItemSourceSpan).toBeLessThanOrEqual(
        MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
      );
      expect(maximumItemSourceSpan).toBeLessThanOrEqual(132);
      expect(maximumRedeemerChunkBytes(itemTrace)).toBeLessThanOrEqual(
        MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
      );
      const emulator = new Emulator(
        genesis,
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
      );
      const txHash = await emulator.submitTx(boundary.accepted.cborHex);
      await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
      await exerciseMidgardRetainedDaCanonicalBoundary({
        canonicalTransactionCbor: canonical,
        corpusLabel: `maximum-${kind}-redeemer-breadth`,
        canonicalMaterialSidecarCbor: artifact.canonicalMaterialSidecarCbor,
        sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
      });

      vectors[`redeemer_${kind}`] = {
        breadth: boundary.accepted.requestedItemCount,
        nodeCount: dataNodeCount(kind, boundary.accepted.requestedItemCount),
        dataCborBytes: acceptedDataCborHex.length / 2,
        signedCardanoBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
        cardanoCapacityAuthority: {
          signedCardanoBytes: boundary.accepted.signedBytes,
          adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
          collateralRejection,
        },
        midgardSchemaProjection: {
          sourceShapeBytes: schemaProjectionSource.cborHex.length / 2,
          nativeCanonicalBytes: canonical.length,
          sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
          canonicalMidgardCredentialScriptHash:
            artifact.canonicalMidgardCredentialScriptHash,
          canonicalMaterialEntryCount: canonicalMaterialEntries.length,
          canonicalMaterialSidecarBytes:
            artifact.canonicalMaterialSidecarCbor.length,
          redeemerFieldBytes:
            native.witnessSet.redeemerTxWitsPreimageCbor.length,
          redeemerItemBytes: item.bytes.length,
          itemProofSteps: itemTrace.steps.length,
          dataTraverseSteps: dataSteps.length,
          maximumSourceSpan: maximumItemSourceSpan,
          maximumChunkBytes: maximumRedeemerChunkBytes(itemTrace),
          broadFrontier: exactBroadFrontierVector(kind, dataSteps),
          terminal: exactTerminalVector(dataSteps),
        },
        collateralRejection,
      };
    }

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(JSON.stringify({ dataBreadthBoundaryV1: vectors }));
    }
  }, 600_000);
});
