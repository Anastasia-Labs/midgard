import { createHash } from "node:crypto";

export const ARCHITECTURE_G_CORPUS_SELECTION_ALGORITHM =
  "named-slice-file-order-prefix-v1";

const requireString = (value, field, rowNumber) => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Corpus row ${rowNumber.toString()} has invalid ${field}`);
  }
  return value;
};

export const validateCanonicalCorpusVerificationEvidence = ({
  artifact,
  corpusSha256,
  indexSha256,
  rowCount,
  chainCount,
}) => {
  if (artifact?.schemaVersion !== "midgard-stress-corpus-generation-v1") {
    throw new Error(
      "Formal corpus verification evidence must be a stress-corpus generation result",
    );
  }
  const verification = artifact.verified;
  const rebuildSample = verification?.rebuildSample;
  const sampledChainIds = rebuildSample?.sampledChainIds;
  if (
    verification?.corpusSha256 !== corpusSha256 ||
    verification?.indexSha256 !== indexSha256 ||
    verification?.rowCount !== rowCount ||
    verification?.chainCount !== chainCount ||
    rebuildSample?.algorithm !== "sha256-corpus-chain-id-order-v1" ||
    !Number.isFinite(rebuildSample.sampleRate) ||
    rebuildSample.sampleRate <= 0 ||
    rebuildSample.sampleRate > 1 ||
    !Number.isSafeInteger(rebuildSample.checkedChainCount) ||
    rebuildSample.checkedChainCount <= 0 ||
    !Number.isSafeInteger(rebuildSample.checkedRowCount) ||
    rebuildSample.checkedRowCount <= 0 ||
    !Array.isArray(sampledChainIds) ||
    sampledChainIds.length !== rebuildSample.checkedChainCount ||
    sampledChainIds.some(
      (chainId) => typeof chainId !== "string" || chainId.length === 0,
    ) ||
    new Set(sampledChainIds).size !== sampledChainIds.length
  ) {
    throw new Error(
      "Corpus verification evidence does not bind corpus/index/counts and a complete deterministic rebuild sample",
    );
  }
  return verification;
};

const parseSelectedRow = (row, rowNumber) => {
  const txHash = requireString(row.txHash, "txHash", rowNumber).toLowerCase();
  const canonicalCborHex = requireString(
    row.canonicalCborHex,
    "canonicalCborHex",
    rowNumber,
  ).toLowerCase();
  const canonicalCborSha256 = requireString(
    row.canonicalCborSha256,
    "canonicalCborSha256",
    rowNumber,
  ).toLowerCase();
  const senderWalletId = requireString(
    row.senderWalletId,
    "senderWalletId",
    rowNumber,
  );
  const selectedInputOutref = requireString(
    row.selectedInputOutref,
    "selectedInputOutref",
    rowNumber,
  );
  if (!/^[0-9a-f]{64}$/.test(txHash)) {
    throw new Error(`Corpus row ${rowNumber.toString()} has invalid txHash`);
  }
  const cbor = Buffer.from(canonicalCborHex, "hex");
  if (
    canonicalCborHex.length === 0 ||
    canonicalCborHex.length % 2 !== 0 ||
    cbor.toString("hex") !== canonicalCborHex
  ) {
    throw new Error(
      `Corpus row ${rowNumber.toString()} has invalid canonical CBOR`,
    );
  }
  const actualCborSha256 = createHash("sha256").update(cbor).digest("hex");
  if (
    !/^[0-9a-f]{64}$/.test(canonicalCborSha256) ||
    actualCborSha256 !== canonicalCborSha256 ||
    row.canonicalCborByteLength !== cbor.length
  ) {
    throw new Error(
      `Corpus row ${rowNumber.toString()} failed CBOR SHA/length checks`,
    );
  }
  if (
    !Array.isArray(row.outputOutrefs) ||
    row.outputOutrefs.length === 0 ||
    row.outputOutrefs.some(
      (outputOutref) =>
        typeof outputOutref !== "string" || outputOutref.length === 0,
    )
  ) {
    throw new Error(
      `Corpus row ${rowNumber.toString()} has invalid outputOutrefs`,
    );
  }
  const parentTxHash =
    row.parentTxHash === null
      ? null
      : requireString(
          row.parentTxHash,
          "parentTxHash",
          rowNumber,
        ).toLowerCase();
  if (parentTxHash !== null && !/^[0-9a-f]{64}$/.test(parentTxHash)) {
    throw new Error(
      `Corpus row ${rowNumber.toString()} has invalid parentTxHash`,
    );
  }
  if (row.planShape !== "chain") {
    throw new Error(
      `Corpus row ${rowNumber.toString()} is not a chain workload`,
    );
  }
  return {
    txHash,
    canonicalCborHex,
    canonicalCborSha256,
    canonicalCborByteLength: cbor.length,
    senderWalletId,
    selectedInputOutref,
    outputOutrefs: row.outputOutrefs,
    parentTxHash,
  };
};

export const createCanonicalCorpusPrefixSelector = ({
  corpusSliceId,
  transactionCount,
}) => {
  if (typeof corpusSliceId !== "string" || corpusSliceId.length === 0) {
    throw new Error("corpusSliceId must be non-empty");
  }
  if (!Number.isSafeInteger(transactionCount) || transactionCount <= 0) {
    throw new Error("transactionCount must be positive");
  }
  const selectedLines = [];
  const selectedRowsByHash = new Map();
  const closedChains = new Set();
  const sliceByChain = new Map();
  const observedClosedSliceChains = new Set();
  const fundingRootOutrefs = [];
  const fundingRoots = [];
  const verifiedSliceTxHashes = new Set();
  const verifiedSliceRowsByHash = new Map();
  let currentChainId;
  let currentChainLastTxHash;
  let observedSliceChainId;
  let observedSliceLastRow;
  let currentChainSelectedRows = 0;
  let completedChainCount = 0;
  let finalChainPrefixLength;
  let sourceStartCorpusRow;
  let sourceEndCorpusRow;
  let sliceRowsSeen = 0;

  const closeSelectedChain = () => {
    if (currentChainId === undefined) return;
    closedChains.add(currentChainId);
    completedChainCount += 1;
  };

  const consider = ({ line, row, corpusRowNumber }) => {
    const rowSliceId = requireString(
      row.corpusSliceId,
      "corpusSliceId",
      corpusRowNumber,
    );
    const rowChainId = requireString(
      row.senderWalletId,
      "senderWalletId",
      corpusRowNumber,
    );
    const previousSliceId = sliceByChain.get(rowChainId);
    if (previousSliceId !== undefined && previousSliceId !== rowSliceId) {
      throw new Error(
        `Corpus chain ${rowChainId} crosses slice boundaries (${previousSliceId}, ${rowSliceId})`,
      );
    }
    sliceByChain.set(rowChainId, rowSliceId);
    if (rowSliceId !== corpusSliceId) return;
    if (rowChainId !== observedSliceChainId) {
      if (observedSliceChainId !== undefined) {
        observedClosedSliceChains.add(observedSliceChainId);
      }
      if (observedClosedSliceChains.has(rowChainId)) {
        throw new Error(
          `Corpus slice chain ${rowChainId} reappeared after another chain`,
        );
      }
      observedSliceChainId = rowChainId;
      observedSliceLastRow = undefined;
    }
    sliceRowsSeen += 1;
    const parsed = parseSelectedRow(row, corpusRowNumber);
    if (verifiedSliceTxHashes.has(parsed.txHash)) {
      throw new Error(
        `Corpus slice contains duplicate txHash ${parsed.txHash}`,
      );
    }
    verifiedSliceTxHashes.add(parsed.txHash);
    if (observedSliceLastRow === undefined) {
      if (parsed.parentTxHash !== null) {
        throw new Error(
          `Corpus chain ${parsed.senderWalletId} does not begin at a declared funding root`,
        );
      }
    } else {
      const declaredParent = verifiedSliceRowsByHash.get(parsed.parentTxHash);
      if (
        declaredParent !== undefined &&
        declaredParent.senderWalletId !== parsed.senderWalletId
      ) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} cross-links parent ${parsed.parentTxHash} from another wallet chain`,
        );
      }
      if (parsed.parentTxHash !== observedSliceLastRow.txHash) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} parent ${String(parsed.parentTxHash)} is not the immediate predecessor ${observedSliceLastRow.txHash}`,
        );
      }
      if (
        !observedSliceLastRow.outputOutrefs.includes(parsed.selectedInputOutref)
      ) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} does not spend a declared parent output`,
        );
      }
    }
    observedSliceLastRow = parsed;
    verifiedSliceRowsByHash.set(parsed.txHash, parsed);
    if (selectedLines.length >= transactionCount) {
      if (finalChainPrefixLength === undefined) {
        if (row.senderWalletId === currentChainId) {
          finalChainPrefixLength = currentChainSelectedRows;
        } else {
          closeSelectedChain();
          finalChainPrefixLength = 0;
        }
      }
      return;
    }

    if (selectedRowsByHash.has(parsed.txHash)) {
      throw new Error(
        `Corpus selection contains duplicate txHash ${parsed.txHash}`,
      );
    }
    if (parsed.senderWalletId !== currentChainId) {
      closeSelectedChain();
      if (closedChains.has(parsed.senderWalletId)) {
        throw new Error(
          `Corpus chain ${parsed.senderWalletId} reappeared after another chain`,
        );
      }
      if (parsed.parentTxHash !== null) {
        throw new Error(
          `Corpus chain ${parsed.senderWalletId} does not begin at a declared funding root`,
        );
      }
      currentChainId = parsed.senderWalletId;
      currentChainLastTxHash = undefined;
      currentChainSelectedRows = 0;
      fundingRootOutrefs.push(parsed.selectedInputOutref);
      fundingRoots.push({
        walletId: parsed.senderWalletId,
        outref: parsed.selectedInputOutref,
      });
    } else {
      if (parsed.parentTxHash === null) {
        throw new Error(
          `Corpus chain ${parsed.senderWalletId} contains multiple funding roots`,
        );
      }
      const parent = selectedRowsByHash.get(parsed.parentTxHash);
      if (parent === undefined) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} does not follow a previously selected parent`,
        );
      }
      if (parent.senderWalletId !== parsed.senderWalletId) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} cross-links parent ${parsed.parentTxHash} from another wallet chain`,
        );
      }
      if (parsed.parentTxHash !== currentChainLastTxHash) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} parent ${parsed.parentTxHash} is not the immediate predecessor ${currentChainLastTxHash ?? "missing"}`,
        );
      }
      if (!parent.outputOutrefs.includes(parsed.selectedInputOutref)) {
        throw new Error(
          `Corpus transaction ${parsed.txHash} does not spend a declared parent output`,
        );
      }
    }
    currentChainSelectedRows += 1;
    currentChainLastTxHash = parsed.txHash;
    selectedRowsByHash.set(parsed.txHash, parsed);
    selectedLines.push(line);
    sourceStartCorpusRow ??= corpusRowNumber;
    sourceEndCorpusRow = corpusRowNumber;
  };

  const finish = () => {
    if (selectedLines.length !== transactionCount) {
      throw new Error(
        `Corpus slice ${corpusSliceId} has ${selectedLines.length.toString()} selectable rows, need ${transactionCount.toString()}`,
      );
    }
    if (finalChainPrefixLength === undefined) {
      closeSelectedChain();
      finalChainPrefixLength = 0;
    }
    const fundingRootsSha256 = createHash("sha256")
      .update(JSON.stringify(fundingRoots))
      .digest("hex");
    return {
      selectionAlgorithm: ARCHITECTURE_G_CORPUS_SELECTION_ALGORITHM,
      parentSliceId: corpusSliceId,
      parentSliceRowsSeen: sliceRowsSeen,
      parentSliceChainCount:
        observedClosedSliceChains.size +
        (observedSliceChainId === undefined ? 0 : 1),
      verifiedCorpusChainCount: sliceByChain.size,
      sliceChainsContiguous: true,
      chainsCrossSliceBoundaries: false,
      sourceCorpusRowRange: {
        start: sourceStartCorpusRow,
        end: sourceEndCorpusRow,
      },
      sourceSliceOrdinalRange: { start: 1, end: transactionCount },
      selectedLines,
      selectedRowCount: selectedLines.length,
      completeChainCount: completedChainCount,
      finalChainPrefixLength,
      fundingRootOutrefs,
      fundingRoots,
      fundingRootsSha256,
    };
  };

  return { consider, finish };
};
