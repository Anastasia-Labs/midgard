import { transactionConsumesOutRef } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  defaultWebSocketFactory,
  parseOgmiosTip,
} from "./local-kupmios-http-ogmios-source.acquire-ogmios-session.js";
import {
  readAdmittedLocalKupmiosBoundary,
  requireOgmiosRawTransactionCbor,
} from "./local-kupmios-http-ogmios-source.admit-local-kupmios-raw-block-at-point.js";
import {
  assertMatchOutput,
  rawUtxoFromOutput,
  transactionInputs,
  transactionOutput,
} from "./local-kupmios-http-ogmios-source.assert-match-output.js";
import {
  fetchJson,
  parseKupoMatches,
  parseKupoPoint,
  rawPoint,
} from "./local-kupmios-http-ogmios-source.fetch-json.js";
import {
  openOgmiosSession,
  sameKupoPoint,
  sameRawPoint,
} from "./local-kupmios-http-ogmios-source.open-ogmios-session.js";
import {
  assertLoopbackUrl,
  boundedReferenceCbor,
  debitReferenceMembers,
  digest,
  exactKeys,
  IMMUTABLE_CHECKPOINT_SLOT_DISTANCE,
  joinUrl,
  LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS,
  MAX_RESPONSE_BYTES,
  naturalNumber,
  normalizeHttpUrl,
  normalizeWebSocketUrl,
  parseOgmiosBlock,
  record,
  type ReferenceReadScope,
  throwIfSourceAborted,
  validateSourceSignal,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  admittedHistoricalPageReaders,
  admittedHttpOgmiosSourceDetails,
  admittedHttpOgmiosSources,
  admittedPredecessorReaders,
  admittedReferenceBodyReaders,
  DEFAULT_BLOCK_SCAN_LIMIT,
  DEFAULT_TIMEOUT_MS,
  type KupoMatch,
  type KupoPoint,
  LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  LocalKupmiosExactPointNotCanonicalError,
  type LocalKupmiosHttpOgmiosSourceConfig,
  type LocalKupmiosRawBlockAtPoint,
  type LocalKupmiosReferenceBodiesAtPoint,
  type OgmiosRawTransactionAtPoint,
  type OgmiosTip,
  signedTransactionRebroadcasters,
  signedTransactionRecoveryReaders,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import { type LocalKupmiosVerifiedSpend } from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-utxos-by-out-ref-at-point.js";
import {
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  settleLocalKupmiosReads,
} from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
} from "./signed-transaction-reconciliation.js";

export const createLocalKupmiosHttpOgmiosRawSource = (
  config: LocalKupmiosHttpOgmiosSourceConfig,
): LocalKupmiosFraudProofRawSource => {
  const kupoHttpUrl = normalizeHttpUrl(config.kupoHttpUrl);
  const ogmiosWebSocketUrl = normalizeWebSocketUrl(config.ogmiosUrl);
  const ogmiosHttpUrl = normalizeHttpUrl(config.ogmiosUrl);
  assertLoopbackUrl(kupoHttpUrl, "Kupo URL");
  assertLoopbackUrl(ogmiosWebSocketUrl, "Ogmios URL");
  if (
    config.sourceId.length === 0 ||
    config.sourceId.trim() !== config.sourceId
  ) {
    throw new Error("Kupmios sourceId must be canonical and non-empty");
  }
  const sourceId = `${LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE}:${config.sourceId}`;
  const fetchImpl = config.fetchImpl ?? fetch;
  const webSocketFactory = config.webSocketFactory ?? defaultWebSocketFactory;
  const timeoutMs = config.timeoutMs ?? DEFAULT_TIMEOUT_MS;
  const blockScanLimit = config.blockScanLimit ?? DEFAULT_BLOCK_SCAN_LIMIT;
  const signal = config.signal;
  const observationDepth = config.observationDepth ?? "release_finality";
  const minimumObservationDepth =
    observationDepth === "inclusion"
      ? 1
      : config.releaseFinality.policy.confirmationDepth;
  const maxResponseBytes = config.maxResponseBytes;
  validateSourceSignal(signal);
  throwIfSourceAborted(signal);
  if (
    maxResponseBytes !== undefined &&
    (!Number.isSafeInteger(maxResponseBytes) ||
      maxResponseBytes <= 0 ||
      maxResponseBytes > MAX_RESPONSE_BYTES)
  ) {
    throw new Error(
      "raw-source maxResponseBytes must be positive and at most 64 MiB",
    );
  }
  if (!Number.isSafeInteger(blockScanLimit) || blockScanLimit <= 0) {
    throw new Error("Ogmios blockScanLimit must be positive");
  }

  let pinnedKupoResponseHead: KupoPoint | undefined;
  // Kupo's most recent checkpoint, as seen by any response of this instance.
  let latestKupoHeadSlot = -1;
  const getKupoJson = async (
    path: string,
    referenceScope?: ReferenceReadScope,
  ): Promise<unknown> => {
    const response = await fetchJson({
      fetchImpl,
      url: joinUrl(kupoHttpUrl, path),
      timeoutMs,
      signal,
      maxResponseBytes: maxResponseBytes ?? MAX_RESPONSE_BYTES,
      referenceScope,
      init: {
        headers: {
          accept: "application/json;asset-quantity=string",
        },
      },
    });
    if (response.checkpointHeaders === null) {
      throw new Error("Kupo response omitted X-Most-Recent-Checkpoint or ETag");
    }
    latestKupoHeadSlot = Math.max(
      latestKupoHeadSlot,
      response.checkpointHeaders.slot,
    );
    if (referenceScope !== undefined) {
      if (referenceScope.head === undefined)
        referenceScope.head = Object.freeze({ ...response.checkpointHeaders });
      else if (!sameKupoPoint(referenceScope.head, response.checkpointHeaders))
        throw new LocalKupmiosCheckpointChangedError(
          "Kupo changed during reference acquisition",
        );
    }
    if (pinnedKupoResponseHead === undefined) {
      pinnedKupoResponseHead = response.checkpointHeaders;
    } else if (
      !sameKupoPoint(pinnedKupoResponseHead, response.checkpointHeaders)
    ) {
      throw new LocalKupmiosCheckpointChangedError(
        `Kupo advanced or rolled back during raw snapshot capture: ${path} (${pinnedKupoResponseHead.slot} -> ${response.checkpointHeaders.slot})`,
      );
    }
    return response.value;
  };

  const queryTip = async (): Promise<OgmiosTip> => {
    // queryNetwork/tip omits height. Chain-sync returns one atomic tip with
    // its actual block number, avoiding a race between separate tip queries.
    const session = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      const result = exactKeys(
        await session.request("findIntersection", { points: ["origin"] }),
        ["intersection", "tip"],
        [],
        "Ogmios tip intersection",
      );
      if (result.intersection !== "origin")
        throw new Error("Ogmios tip query did not intersect origin");
      return parseOgmiosTip(result.tip, "Ogmios chain-sync tip");
    } finally {
      await session.close();
    }
  };

  // A checkpoint deeper than the security parameter below Kupo's head can
  // never change, so it is answered from memory without touching the pinned
  // head. Younger checkpoints are always re-read.
  const immutableCheckpoints = new Map<number, KupoPoint>();
  const getKupoCheckpoint = async (
    slot: number,
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoPoint> => {
    const memoized = immutableCheckpoints.get(slot);
    if (memoized !== undefined) return memoized;
    const checkpoint = parseKupoPoint(
      await getKupoJson(`/checkpoints/${slot.toString()}`, referenceScope),
      `Kupo checkpoint ${slot.toString()}`,
    );
    if (slot <= latestKupoHeadSlot - IMMUTABLE_CHECKPOINT_SLOT_DISTANCE) {
      immutableCheckpoints.set(slot, checkpoint);
    }
    return checkpoint;
  };

  const readPredecessorCheckpoint = async (
    target: KupoPoint,
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoPoint> => {
    if (target.slot === 0) throw new Error("cannot chain-sync before genesis");
    const ancestor = await getKupoCheckpoint(target.slot - 1, referenceScope);
    if (ancestor.slot >= target.slot) {
      throw new Error("Kupo did not return an earlier ancestor checkpoint");
    }
    return ancestor;
  };

  const rawBlockCache = new Map<
    string,
    Promise<{
      readonly point: OgmiosTip;
      readonly parentBlockHash: string | null;
      readonly transactions: readonly unknown[];
    }>
  >();
  const readBlock = async (
    target: KupoPoint,
    referenceScope?: ReferenceReadScope,
  ): Promise<{
    readonly point: OgmiosTip;
    readonly parentBlockHash: string | null;
    readonly transactions: readonly unknown[];
  }> => {
    throwIfSourceAborted(signal);
    const key = `${target.slot.toString()}:${target.blockHash}`;
    const cache = referenceScope?.rawBlocks ?? rawBlockCache;
    const cached =
      cache.get(key) ??
      (referenceScope === undefined ? undefined : rawBlockCache.get(key));
    if (cached !== undefined) {
      const block = await cached;
      throwIfSourceAborted(signal);
      debitReferenceMembers(referenceScope, block.transactions.length);
      return block;
    }
    const read = (async () => {
      const ancestor = await readPredecessorCheckpoint(target, referenceScope);
      const session = await openOgmiosSession({
        url: ogmiosWebSocketUrl,
        timeoutMs,
        webSocketFactory,
        signal,
        maxResponseBytes,
        referenceScope,
      });
      try {
        const intersection = record(
          await session.request("findIntersection", {
            points: [{ slot: ancestor.slot, id: ancestor.blockHash }],
          }),
          "Ogmios findIntersection result",
        );
        const found = record(
          intersection.intersection,
          "Ogmios findIntersection result.intersection",
        );
        if (
          naturalNumber(found.slot, "Ogmios intersection slot") !==
            ancestor.slot ||
          digest(found.id, "Ogmios intersection id") !== ancestor.blockHash
        ) {
          throw new Error("Ogmios did not intersect the Kupo ancestor");
        }
        let acknowledged = false;
        for (let scanned = 0; scanned < blockScanLimit; scanned += 1) {
          const next = record(
            await session.request("nextBlock", {}),
            "Ogmios nextBlock result",
          );
          if (next.direction === "backward") {
            if (acknowledged) {
              throw new Error("Ogmios rolled back during raw transaction scan");
            }
            acknowledged = true;
            scanned -= 1;
            continue;
          }
          if (next.direction !== "forward") {
            throw new Error("Ogmios nextBlock has no supported direction");
          }
          acknowledged = true;
          const block = parseOgmiosBlock(
            next.block,
            "Ogmios nextBlock.block",
            referenceScope,
          );
          if (block.point.blockHash === target.blockHash) {
            if (block.point.slot !== target.slot) {
              throw new Error("Kupo/Ogmios block slot disagreement");
            }
            return block;
          }
          if (block.point.slot > target.slot) {
            throw new Error("Ogmios passed the Kupo block without finding it");
          }
        }
        throw new Error("Ogmios block scan exceeded its safety bound");
      } finally {
        await session.close();
      }
    })();
    cache.set(key, read);
    try {
      const block = await read;
      throwIfSourceAborted(signal);
      return block;
    } catch (cause) {
      cache.delete(key);
      throw cause;
    }
  };

  const readRawTransaction = async (
    {
      txHash,
      point,
    }: {
      readonly txHash: string;
      readonly point: KupoPoint;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<OgmiosRawTransactionAtPoint> => {
    const block = await readBlock(point, referenceScope);
    const candidates = block.transactions.filter(
      (entry) => record(entry, "Ogmios block transaction").id === txHash,
    );
    if (candidates.length !== 1) {
      throw new Error(`Ogmios block does not contain exactly one ${txHash}`);
    }
    if (referenceScope !== undefined)
      boundedReferenceCbor(
        record(candidates[0], "creating transaction").cbor,
        "creating full transaction",
      );
    return {
      txHash,
      transactionCbor: requireOgmiosRawTransactionCbor({
        value: candidates[0],
        expectedTxHash: txHash,
        label: `Ogmios transaction ${txHash}`,
      }),
      point: rawPoint(block.point),
    };
  };

  const fetchMatches = async (
    pattern: string,
    referenceScope?: ReferenceReadScope,
  ): Promise<readonly KupoMatch[]> =>
    parseKupoMatches(
      await getKupoJson(
        `/matches/${encodeURIComponent(pattern)}?resolve_hashes&order=oldest_first`,
        referenceScope,
      ),
      `Kupo matches ${pattern}`,
      referenceScope,
    );

  const fetchOutRefMatch = async (
    {
      txHash,
      outputIndex,
    }: {
      readonly txHash: string;
      readonly outputIndex: number;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoMatch> => {
    const matches = (
      await fetchMatches(`${outputIndex.toString()}@${txHash}`, referenceScope)
    ).filter(
      (candidate) =>
        candidate.txHash === txHash && candidate.outputIndex === outputIndex,
    );
    if (matches.length !== 1) {
      throw new Error(
        `Kupo has no unique match for ${txHash}#${outputIndex.toString()}`,
      );
    }
    return matches[0]!;
  };

  const utxoFromMatch = async (
    match: KupoMatch,
  ): Promise<FraudProofRawL1Utxo> => {
    const transaction = await readRawTransaction({
      txHash: match.txHash,
      point: match.createdAt,
    });
    const output = transactionOutput({
      transactionCbor: transaction.transactionCbor,
      outputIndex: match.outputIndex,
      label: `Kupo output ${match.txHash}#${match.outputIndex.toString()}`,
    });
    assertMatchOutput({
      match,
      output,
      label: `Kupo output ${match.txHash}#${match.outputIndex.toString()}`,
    });
    return rawUtxoFromOutput({
      txHash: match.txHash,
      outputIndex: match.outputIndex,
      output,
    });
  };

  const pointCache = new Map<string, Promise<FraudProofRawL1Point>>();
  const admittedPoint = async (
    point: KupoPoint,
  ): Promise<FraudProofRawL1Point> => {
    throwIfSourceAborted(signal);
    const key = `${point.slot.toString()}:${point.blockHash}`;
    const cached = pointCache.get(key);
    if (cached !== undefined) {
      const admitted = await cached;
      throwIfSourceAborted(signal);
      return admitted;
    }
    const read = readBlock(point).then((block) => rawPoint(block.point));
    pointCache.set(key, read);
    const admitted = await read;
    throwIfSourceAborted(signal);
    return admitted;
  };

  let activeBoundary:
    | {
        readonly point: FraudProofRawL1Point;
        readonly tip: FraudProofRawL1Point;
      }
    | undefined;
  const addressCache = new Map<
    string,
    Promise<readonly FraudProofRawL1Utxo[]>
  >();
  const historyCache = new Map<
    string,
    Promise<
      readonly {
        readonly txHash: string;
        readonly inclusionPoint: FraudProofRawL1Point;
      }[]
    >
  >();

  const assertBoundary = (point: FraudProofRawL1Point): void => {
    throwIfSourceAborted(signal);
    if (
      activeBoundary === undefined ||
      !sameRawPoint(activeBoundary.point, point)
    ) {
      throw new Error("Kupmios source call is outside its pinned boundary");
    }
  };

  const readBlockAtPoint = async (
    {
      point: requested,
    }: {
      readonly point: FraudProofRawL1Point;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<LocalKupmiosRawBlockAtPoint> => {
    throwIfSourceAborted(signal);
    const point = admitFraudProofRawL1Point(
      requested,
      "local Kupmios exact block point",
    );
    const slot = Number(point.slot);
    if (!Number.isSafeInteger(slot)) {
      throw new Error("local Kupmios exact block slot exceeds safe range");
    }
    const expectedKupoPoint = {
      slot,
      blockHash: point.blockHash,
    };
    const before = await getKupoCheckpoint(slot, referenceScope);
    if (!sameKupoPoint(before, expectedKupoPoint)) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        `Kupo exact checkpoint does not contain the requested block (requested slot ${slot.toString()}, checkpoint slot ${before.slot.toString()}, Kupo head slot ${latestKupoHeadSlot.toString()})`,
        {
          requestedSlot: slot,
          checkpointSlot: before.slot,
          kupoHeadSlot: latestKupoHeadSlot,
        },
      );
    }
    const block = await readBlock(before, referenceScope);
    const observedPoint = rawPoint(block.point);
    if (!sameRawPoint(observedPoint, point)) {
      throw new Error("Ogmios exact block point differs from the request");
    }
    if (
      referenceScope !== undefined &&
      block.transactions.length >
        LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactions
    )
      throw new Error("reference target transaction count exceeds bounds");
    let targetBytes = 0;
    const transactions = block.transactions.map((value, index) => {
      const transaction = record(
        value,
        `Ogmios exact block transaction ${index.toString()}`,
      );
      const transactionHash = digest(
        transaction.id,
        `Ogmios exact block transaction ${index.toString()}.id`,
      );
      if (referenceScope !== undefined) {
        const bytes =
          boundedReferenceCbor(
            transaction.cbor,
            "reference target full transaction",
          ).length / 2;
        if (
          bytes >
          LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactionBytes -
            targetBytes
        )
          throw new Error("reference target transaction bytes exceed bounds");
        targetBytes += bytes;
      }
      return {
        txHash: transactionHash,
        transactionCbor: requireOgmiosRawTransactionCbor({
          value: transaction,
          expectedTxHash: transactionHash,
          label: `Ogmios exact block transaction ${index.toString()}`,
        }),
      };
    });
    if (
      new Set(transactions.map(({ txHash }) => txHash)).size !==
      transactions.length
    ) {
      throw new Error("Ogmios exact block contains duplicate transaction ids");
    }
    const after = await getKupoCheckpoint(slot, referenceScope);
    throwIfSourceAborted(signal);
    if (
      !sameKupoPoint(after, expectedKupoPoint) ||
      !sameKupoPoint(after, before)
    ) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        "Kupo rolled back during exact raw block capture",
      );
    }
    return {
      schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
      sourceId,
      point: observedPoint,
      parentBlockHash: block.parentBlockHash,
      kupoCheckpoint: after,
      transactions,
    };
  };

  const readReferenceBodies = async (
    point: FraudProofRawL1Point,
  ): Promise<LocalKupmiosReferenceBodiesAtPoint> => {
    const scope: ReferenceReadScope = {
      responseBytes: 0,
      inspectedMembers: 0,
      head:
        pinnedKupoResponseHead === undefined
          ? undefined
          : Object.freeze({ ...pinnedKupoResponseHead }),
      rawBlocks: new Map(),
    };
    try {
      throwIfSourceAborted(signal);
      const target = await readBlockAtPoint({ point }, scope);
      throwIfSourceAborted(signal);
      const required = new Map<string, Map<number, number>>();
      let referenceOccurrences = 0;
      for (const raw of target.transactions) {
        let transaction: CML.Transaction | undefined;
        let body: CML.TransactionBody | undefined;
        let references: CML.TransactionInputList | undefined;
        try {
          transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
          if (transaction.to_cbor_hex() !== raw.transactionCbor)
            throw new Error(
              "reference target transaction encoding is not preserved",
            );
          if (!transaction.is_valid()) continue;
          body = transaction.body();
          references = body.reference_inputs();
          const count = references?.len() ?? 0;
          if (
            count >
              LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.transactionReferences ||
            count >
              LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.referenceOccurrences -
                referenceOccurrences
          )
            throw new Error("reference target input roster exceeds bounds");
          referenceOccurrences += count;
          const unique = new Set<string>();
          for (let index = 0; index < count; index += 1) {
            const input = references!.get(index);
            const id = input.transaction_id();
            try {
              const txHash = id.to_hex();
              const outputIndex = Number(input.index());
              if (!Number.isSafeInteger(outputIndex) || outputIndex < 0)
                throw new Error("reference output index exceeds safe range");
              const outRef = `${txHash}#${outputIndex.toString()}`;
              if (unique.has(outRef))
                throw new Error("reference target input roster is not unique");
              unique.add(outRef);
              let outputs = required.get(txHash);
              if (outputs === undefined) {
                if (
                  required.size >=
                  LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.creatingBodies
                )
                  throw new Error(
                    "reference creating-body collection exceeds bounds",
                  );
                outputs = new Map();
                required.set(txHash, outputs);
              }
              outputs.set(outputIndex, (outputs.get(outputIndex) ?? 0) + 1);
            } finally {
              id.free();
              input.free();
            }
          }
        } finally {
          references?.free();
          body?.free();
          transaction?.free();
        }
      }
      let evidenceBytes = 0;
      const bodies: string[] = [];
      for (const [txHash, requiredOutputs] of [...required].sort(
        ([left], [right]) => left.localeCompare(right),
      )) {
        let creatingPoint: KupoPoint | undefined;
        for (const outputIndex of requiredOutputs.keys()) {
          const match = await fetchOutRefMatch({ txHash, outputIndex }, scope);
          throwIfSourceAborted(signal);
          if (match.createdAt.slot > Number(target.point.slot))
            throw new Error("reference creating point is after its target");
          if (
            creatingPoint !== undefined &&
            !sameKupoPoint(creatingPoint, match.createdAt)
          )
            throw new Error(
              "reference creating transaction has inconsistent points",
            );
          creatingPoint = match.createdAt;
        }
        if (creatingPoint === undefined)
          throw new Error(
            "reference creating transaction has no requested output",
          );
        const before = await getKupoCheckpoint(creatingPoint.slot, scope);
        if (!sameKupoPoint(before, creatingPoint))
          throw new LocalKupmiosExactPointNotCanonicalError(
            "reference creating checkpoint differs from its match",
          );
        const raw = await readRawTransaction(
          { txHash, point: creatingPoint },
          scope,
        );
        throwIfSourceAborted(signal);
        if (
          raw.point.blockHash !== creatingPoint.blockHash ||
          raw.point.slot !== creatingPoint.slot.toString() ||
          BigInt(raw.point.blockNo) > BigInt(target.point.blockNo)
        )
          throw new Error(
            "reference creating transaction point differs from its lookup",
          );
        const after = await getKupoCheckpoint(creatingPoint.slot, scope);
        if (!sameKupoPoint(after, before))
          throw new LocalKupmiosExactPointNotCanonicalError(
            "reference creating checkpoint changed during acquisition",
          );
        let transaction: CML.Transaction | undefined;
        let body: CML.TransactionBody | undefined;
        let outputs: CML.TransactionOutputList | undefined;
        let bodyHash: CML.TransactionHash | undefined;
        try {
          transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
          if (transaction.to_cbor_hex() !== raw.transactionCbor)
            throw new Error(
              "reference creating transaction encoding is not preserved",
            );
          body = transaction.body();
          const bodyCbor = boundedReferenceCbor(
            body.to_cbor_hex(),
            "reference creating body",
          );
          bodyHash = CML.hash_transaction(body);
          if (bodyHash.to_hex() !== txHash)
            throw new Error(
              "reference creating body differs from requested ledger identity",
            );
          const bodyBytes = bodyCbor.length / 2;
          if (
            bodyBytes >
            LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.evidenceBytes -
              evidenceBytes
          )
            throw new Error(
              "reference creating-body/output byte budget exceeded",
            );
          evidenceBytes += bodyBytes;
          outputs = body.outputs();
          for (const [outputIndex, occurrences] of requiredOutputs) {
            const output =
              outputIndex < outputs.len()
                ? outputs.get(outputIndex)
                : outputIndex === outputs.len()
                  ? body.collateral_return()
                  : undefined;
            if (output === undefined)
              throw new Error("reference creating output index does not exist");
            try {
              const outputBytes =
                boundedReferenceCbor(
                  output.to_canonical_cbor_hex(),
                  "reference selected output",
                ).length / 2;
              if (
                outputBytes >
                Math.floor(
                  (LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.evidenceBytes -
                    evidenceBytes) /
                    occurrences,
                )
              )
                throw new Error(
                  "reference creating-body/output byte budget exceeded",
                );
              evidenceBytes += outputBytes * occurrences;
            } finally {
              output.free();
            }
          }
          bodies.push(bodyCbor);
        } finally {
          bodyHash?.free();
          outputs?.free();
          body?.free();
          transaction?.free();
        }
      }
      const after = await readBlockAtPoint({ point }, scope);
      throwIfSourceAborted(signal);
      if (
        !sameRawPoint(after.point, target.point) ||
        after.parentBlockHash !== target.parentBlockHash ||
        after.transactions.length !== target.transactions.length ||
        after.transactions.some(
          (transaction, index) =>
            transaction.txHash !== target.transactions[index]!.txHash ||
            transaction.transactionCbor !==
              target.transactions[index]!.transactionCbor,
        )
      )
        throw new Error("reference acquisition complete target changed");
      return Object.freeze({
        targetBlock: Object.freeze({
          ...after,
          point: Object.freeze({ ...after.point }),
          kupoCheckpoint: Object.freeze({ ...after.kupoCheckpoint }),
          transactions: Object.freeze(
            after.transactions.map((transaction) =>
              Object.freeze({ ...transaction }),
            ),
          ),
        }),
        creatingTransactionBodies: Object.freeze(bodies),
      });
    } finally {
      scope.rawBlocks.clear();
    }
  };

  const scanAddressPage: LocalKupmiosFraudProofRawSource["scanAddressPage"] =
    async ({ address, throughPoint, after }) => {
      if (after !== null) {
        throw new Error("Kupo match streams have no continuation cursor");
      }
      const key = `${throughPoint.pointId}:${address}`;
      let cached = addressCache.get(key);
      if (cached === undefined) {
        cached = (async () => {
          const matches = await fetchMatches(address);
          const current = matches.filter((match) => {
            if (match.createdAt.slot > Number(throughPoint.slot)) return false;
            if (
              match.createdAt.slot === Number(throughPoint.slot) &&
              match.createdAt.blockHash !== throughPoint.blockHash
            ) {
              throw new Error("Kupo address history forks at the pinned slot");
            }
            if (match.spentAt === null) return true;
            if (match.spentAt.slot > Number(throughPoint.slot)) return true;
            if (
              match.spentAt.slot === Number(throughPoint.slot) &&
              match.spentAt.blockHash !== throughPoint.blockHash
            ) {
              throw new Error(
                "Kupo address spend history forks at the pinned slot",
              );
            }
            return false;
          });
          return await settleLocalKupmiosReads(current.map(utxoFromMatch));
        })();
        addressCache.set(key, cached);
      }
      const utxos = await cached;
      throwIfSourceAborted(signal);
      return {
        checkpoint: throughPoint,
        utxos,
        nextCursor: null,
        complete: true,
      };
    };

  const scanUnitHistoryPage: LocalKupmiosFraudProofRawSource["scanUnitHistoryPage"] =
    async ({ unit, throughPoint, after }) => {
      if (after !== null) {
        throw new Error("Kupo match streams have no continuation cursor");
      }
      if (!/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(unit)) {
        throw new Error("unit history request is not a canonical Cardano unit");
      }
      const key = `${throughPoint.pointId}:${unit}`;
      let cached = historyCache.get(key);
      if (cached === undefined) {
        cached = (async () => {
          const pattern = `${unit.slice(0, 56)}.${unit.slice(56)}`;
          const matches = await fetchMatches(pattern);
          const points = new Map<string, KupoPoint>();
          for (const match of matches) {
            if (match.createdAt.slot <= Number(throughPoint.slot)) {
              points.set(match.txHash, match.createdAt);
            }
            if (
              match.spentAt !== null &&
              match.spentAt.slot <= Number(throughPoint.slot)
            ) {
              const previous = points.get(match.spentAt.txHash);
              if (
                previous !== undefined &&
                !sameKupoPoint(previous, match.spentAt)
              ) {
                throw new Error(
                  "Kupo unit history assigns one transaction to two points",
                );
              }
              points.set(match.spentAt.txHash, match.spentAt);
            }
          }
          return await settleLocalKupmiosReads(
            [...points.entries()]
              .sort(([left], [right]) => left.localeCompare(right))
              .map(async ([txHash, point]) => ({
                txHash,
                inclusionPoint: await admittedPoint(point),
              })),
          );
        })();
        historyCache.set(key, cached);
      }
      const transactions = await cached;
      throwIfSourceAborted(signal);
      return {
        checkpoint: throughPoint,
        transactions,
        nextCursor: null,
        complete: true,
      };
    };

  const readAtHistoricalPoint = async <T>(
    point: FraudProofRawL1Point,
    read: () => Promise<T>,
  ): Promise<T> => {
    const boundary = activeBoundary;
    assertBoundary(boundary?.point ?? point);
    if (boundary === undefined)
      throw new Error("Kupmios boundary is not pinned");
    if (sameRawPoint(point, boundary.point)) return await read();
    const blockDistance = BigInt(boundary.tip.blockNo) - BigInt(point.blockNo);
    if (
      BigInt(point.blockNo) > BigInt(boundary.point.blockNo) ||
      BigInt(point.slot) > BigInt(boundary.point.slot) ||
      blockDistance + 1n < BigInt(minimumObservationDepth) ||
      blockDistance >
        BigInt(config.releaseFinality.policy.automaticRecoveryMaxDepth)
    ) {
      throw new Error(
        "historical Kupmios point is outside the pinned release recovery window",
      );
    }
    // Exact Kupo checkpoints and Ogmios bytes authenticate this historical
    // context while retaining the active capture's provider head and tip.
    await readBlockAtPoint({ point });
    const result = await read();
    await readBlockAtPoint({ point });
    assertBoundary(boundary.point);
    return result;
  };

  const source: LocalKupmiosFraudProofRawSource = {
    sourceVersion: LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
    sourceId,
    kupoHttpUrl,
    ogmiosWebSocketUrl,
    readOutRefsAtPoint: async ({ point, outRefs }) => {
      assertBoundary(point);
      const result: FraudProofRawL1Utxo[] = [];
      const spends: LocalKupmiosVerifiedSpend[] = [];
      for (const outRef of outRefs) {
        const [txHash, index] = outRef.split("#");
        const matches = await fetchMatches(`${index}@${txHash}`);
        if (
          matches.length > 1 ||
          matches.some(
            (match) =>
              match.txHash !== txHash || match.outputIndex.toString() !== index,
          )
        ) {
          throw new Error("Kupo substituted exact output-reference history");
        }
        const match = matches[0];
        if (match === undefined || match.createdAt.slot > Number(point.slot))
          continue;
        if (
          match.createdAt.slot === Number(point.slot) &&
          match.createdAt.blockHash !== point.blockHash
        ) {
          throw new LocalKupmiosExactPointNotCanonicalError(
            "Output creation forks at pinned point",
          );
        }
        if (
          match.spentAt !== null &&
          match.spentAt.slot <= Number(point.slot)
        ) {
          if (
            match.spentAt.slot === Number(point.slot) &&
            match.spentAt.blockHash !== point.blockHash
          ) {
            throw new LocalKupmiosExactPointNotCanonicalError(
              "Output spend forks at pinned point",
            );
          }
          // A spend is reported only with its consuming transaction verified;
          // a claim the raw block does not bear out reads as missing.
          let spending: OgmiosRawTransactionAtPoint;
          try {
            spending = await readRawTransaction({
              txHash: match.spentAt.txHash,
              point: match.spentAt,
            });
          } catch (cause) {
            if (
              cause instanceof LocalKupmiosCheckpointChangedError ||
              cause instanceof LocalKupmiosExactPointNotCanonicalError
            )
              throw cause;
            continue;
          }
          if (
            transactionConsumesOutRef({
              transactionCbor: spending.transactionCbor,
              transactionId: match.spentAt.txHash,
              outRef,
            })
          )
            spends.push({
              outRef,
              spendingTxHash: match.spentAt.txHash,
              spendPoint: spending.point,
            });
          continue;
        }
        result.push(await utxoFromMatch(match));
      }
      return { outputs: result, spends };
    },
    pinBoundaryAtPoint: async ({ point }) => {
      pinnedKupoResponseHead = undefined;
      const exact = admitFraudProofRawL1Point(
        point,
        "exact native availability boundary",
      );
      const checkpoint = await getKupoCheckpoint(Number(exact.slot));
      const block = await readBlock(checkpoint);
      if (!sameRawPoint(rawPoint(block.point), exact)) {
        throw new LocalKupmiosExactPointNotCanonicalError(
          "Exact boundary differs from canonical Kupo/Ogmios block",
        );
      }
      const tip = await queryTip();
      const depth = tip.blockNo - Number(exact.blockNo) + 1;
      if (
        depth < minimumObservationDepth ||
        depth > config.releaseFinality.policy.automaticRecoveryMaxDepth
      ) {
        throw new Error(
          "Exact boundary is outside the release finality/recovery window",
        );
      }
      activeBoundary = { point: exact, tip: rawPoint(tip) };
      addressCache.clear();
      historyCache.clear();
      return exact;
    },
    resolveTransactionInclusion: async ({ txHash }) => {
      const matches = await fetchMatches(
        `*@${digest(txHash, "transaction inclusion hash")}`,
      );
      if (matches.length === 0) return null;
      const first = matches[0]!;
      const indices = matches
        .map((match) => match.outputIndex)
        .sort((left, right) => left - right);
      if (
        matches.some(
          (match) =>
            match.txHash !== txHash ||
            !sameKupoPoint(match.createdAt, first.createdAt),
        ) ||
        indices.some((index, position) => index !== position)
      ) {
        throw new Error(
          "Transaction inclusion history is incomplete or substituted",
        );
      }
      const point = await admittedPoint(first.createdAt);
      const canonical = await getKupoCheckpoint(Number(point.slot));
      if (!sameKupoPoint(canonical, first.createdAt)) {
        throw new LocalKupmiosExactPointNotCanonicalError(
          "Transaction inclusion is no longer canonical",
        );
      }
      return point;
    },
    readBoundary: async (input) => {
      throwIfSourceAborted(signal);
      pinnedKupoResponseHead = undefined;
      activeBoundary = undefined;
      addressCache.clear();
      historyCache.clear();
      rawBlockCache.clear();
      pointCache.clear();
      const tip = await queryTip();
      const minimum =
        (input?.observationDepth ?? observationDepth) === "inclusion"
          ? 1
          : config.releaseFinality.policy.confirmationDepth;
      const maximum = config.releaseFinality.policy.automaticRecoveryMaxDepth;
      let lookbackSlots = Math.max(0, minimum - 1);
      let newerSlot = tip.slot + 1;
      for (let attempt = 0; attempt < 12; attempt += 1) {
        const lookupSlot = Math.max(0, tip.slot - lookbackSlots);
        const checkpoint = await getKupoCheckpoint(lookupSlot);
        const point = await admittedPoint(checkpoint);
        throwIfSourceAborted(signal);
        const depth = tip.blockNo - Number(point.blockNo) + 1;
        if (depth >= minimum) {
          // Slot density varies by chain and by leader election. Refine the
          // bracket by observed block height instead of treating seconds as
          // confirmations; an unnecessarily old boundary can predate activation.
          let selected = point;
          let lower = checkpoint.slot;
          let upper = newerSlot - 1;
          while (depth !== minimum && lower < upper) {
            const probe = Math.floor(lower + (upper - lower + 1) / 2);
            const candidate = await admittedPoint(
              await getKupoCheckpoint(probe),
            );
            throwIfSourceAborted(signal);
            const candidateDepth = tip.blockNo - Number(candidate.blockNo) + 1;
            if (candidateDepth >= minimum) {
              selected = candidate;
              lower = probe;
              if (candidateDepth === minimum) break;
            } else {
              upper = probe - 1;
            }
          }
          if (tip.blockNo - Number(selected.blockNo) + 1 > maximum) break;
          activeBoundary = { point: selected, tip: rawPoint(tip) };
          return {
            kupoCheckpoint: activeBoundary.point,
            ogmiosTip: activeBoundary.tip,
          };
        }
        if (lookupSlot === 0) break;
        newerSlot = lookupSlot;
        lookbackSlots *= 2;
      }
      throw new Error(
        "Kupo/Ogmios could not establish a release-final boundary within the automatic recovery window",
      );
    },
    readBlockAtPoint,
    scanAddressPage: async (input) => {
      assertBoundary(input.throughPoint);
      return await scanAddressPage(input);
    },
    scanUnitHistoryPage: async (input) => {
      assertBoundary(input.throughPoint);
      return await scanUnitHistoryPage(input);
    },
    readTransaction: async ({ txHash, expectedInclusionPoint }) => {
      assertBoundary(activeBoundary?.point ?? expectedInclusionPoint);
      const matches = await fetchMatches(`*@${txHash}`);
      if (
        matches.length === 0 ||
        matches.some((match) => match.txHash !== txHash)
      ) {
        throw new Error(
          `Kupo has no complete transaction-output match for ${txHash}`,
        );
      }
      const indices = matches
        .map((match) => match.outputIndex)
        .sort((left, right) => left - right);
      if (indices.some((value, index) => value !== index)) {
        throw new Error(
          `Kupo transaction-output set for ${txHash} is incomplete`,
        );
      }
      const raw = await readRawTransaction({
        txHash,
        point: {
          slot: Number(expectedInclusionPoint.slot),
          blockHash: expectedInclusionPoint.blockHash,
        },
      });
      if (!sameRawPoint(raw.point, expectedInclusionPoint)) {
        throw new Error(`Ogmios placed ${txHash} at a substituted chain point`);
      }
      const transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
      if (!transaction.is_valid()) {
        throw new Error(`transaction ${txHash} is phase-2 invalid`);
      }
      const resolve = async (input: {
        readonly txHash: string;
        readonly outputIndex: number;
      }): Promise<FraudProofRawL1Utxo> =>
        await utxoFromMatch(await fetchOutRefMatch(input));
      const body = transaction.body();
      const resolvedInputs = await settleLocalKupmiosReads(
        transactionInputs(body.inputs()).map(resolve),
      );
      const resolvedReferenceInputs = await settleLocalKupmiosReads(
        transactionInputs(body.reference_inputs()).map(resolve),
      );
      throwIfSourceAborted(signal);
      const witnessSet = transaction.witness_set();
      const redeemers = witnessSet.redeemers();
      const tip = activeBoundary?.tip;
      if (tip === undefined) throw new Error("Kupmios boundary is not pinned");
      const confirmationDepth =
        Number(tip.blockNo) - Number(expectedInclusionPoint.blockNo) + 1;
      const ogmios: FraudProofRawL1Transaction = {
        txHash,
        bodyCbor: body.to_cbor_hex(),
        witnessSetCbor: witnessSet.to_cbor_hex(),
        redeemersCbor: redeemers?.to_canonical_cbor_hex() ?? null,
        isValid: true,
        inclusionPoint: expectedInclusionPoint,
        confirmationDepth,
        resolvedInputs,
        resolvedReferenceInputs,
      };
      return {
        kupo: { txHash, inclusionPoint: expectedInclusionPoint },
        ogmios,
      };
    },
    confirmCanonicalPoint: async ({ point }) => {
      assertBoundary(point);
      const [checkpoint, tip] = await settleLocalKupmiosReads([
        getKupoCheckpoint(Number(point.slot)),
        queryTip(),
      ] as const);
      let canonical = sameKupoPoint(checkpoint, {
        slot: Number(point.slot),
        blockHash: point.blockHash,
      });
      if (canonical) {
        rawBlockCache.delete(`${point.slot}:${point.blockHash}`);
        pointCache.delete(`${point.slot}:${point.blockHash}`);
        const block = await readBlock({
          slot: Number(point.slot),
          blockHash: point.blockHash,
        });
        canonical = sameRawPoint(rawPoint(block.point), point);
      }
      throwIfSourceAborted(signal);
      if (tip.blockNo < Number(point.blockNo)) canonical = false;
      return { canonical, point };
    },
  };
  admittedHistoricalPageReaders.set(
    source,
    Object.freeze({
      address: (input) =>
        readAtHistoricalPoint(input.throughPoint, () => scanAddressPage(input)),
      history: (input) =>
        readAtHistoricalPoint(input.throughPoint, () =>
          scanUnitHistoryPage(input),
        ),
    }),
  );
  admittedReferenceBodyReaders.set(source, readReferenceBodies);
  admittedPredecessorReaders.set(source, async (requestedPoint) => {
    const child = await readBlockAtPoint({ point: requestedPoint });
    if (child.parentBlockHash === null) {
      throw new Error("local Kupmios child has no block predecessor");
    }
    const checkpoint = await readPredecessorCheckpoint(child.kupoCheckpoint);
    const predecessor = await readBlockAtPoint({
      point: await admittedPoint(checkpoint),
    });
    if (
      predecessor.point.blockHash !== child.parentBlockHash ||
      BigInt(predecessor.point.blockNo) + 1n !== BigInt(child.point.blockNo) ||
      BigInt(predecessor.point.slot) >= BigInt(child.point.slot)
    ) {
      throw new Error("local Kupmios blocks do not form a direct predecessor");
    }
    const after = await getKupoCheckpoint(Number(child.point.slot));
    throwIfSourceAborted(signal);
    if (!sameKupoPoint(after, child.kupoCheckpoint)) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        "Kupo rolled back during predecessor point capture",
      );
    }
    return Object.freeze({
      sourceId,
      point: Object.freeze(
        admitFraudProofRawL1Point(child.point, "local Kupmios child point"),
      ),
      predecessorPoint: Object.freeze(
        admitFraudProofRawL1Point(
          predecessor.point,
          "local Kupmios predecessor point",
        ),
      ),
    });
  });
  signedTransactionRecoveryReaders.set(source, async (input) => {
    const signed = inspectSignedWorkflowTransaction(input);
    // Replacement authorization still requires stable expiry/spend evidence,
    // even when this source normally observes action prerequisites at inclusion.
    const boundary = await readAdmittedLocalKupmiosBoundary({ source });
    const canonicalPoint = boundary.ogmiosTip;
    const releaseFinalPoint = boundary.kupoCheckpoint;
    const inputs: { outRef: string; outputCbor: string }[] = [];
    const result = (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ): SignedTransactionRecoveryObservation =>
      Object.freeze({
        transactionHash: input.transactionHash,
        signedTransactionCborHex: input.signedTransactionCborHex,
        status,
        reason,
        canonicalPoint,
        releaseFinalPoint,
        inputs: Object.freeze(inputs),
      });
    const finish = async (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ) => {
      const confirmation = exactKeys(
        await source.confirmCanonicalPoint({ point: releaseFinalPoint }),
        ["canonical", "point"],
        [],
        "signed recovery canonical confirmation",
      );
      if (
        confirmation.canonical !== true ||
        !sameRawPoint(
          admitFraudProofRawL1Point(
            confirmation.point,
            "signed recovery confirmed point",
          ),
          releaseFinalPoint,
        )
      )
        throw new LocalKupmiosCheckpointChangedError(
          "Signed recovery release-final boundary rolled back",
        );
      const after = await queryTip();
      if (!sameRawPoint(rawPoint(after), canonicalPoint))
        throw new LocalKupmiosCheckpointChangedError(
          "Canonical tip changed during signed transaction recovery",
        );
      return result(status, reason);
    };
    const tipCheckpoint = await getKupoCheckpoint(Number(canonicalPoint.slot));
    if (
      !sameKupoPoint(tipCheckpoint, {
        slot: Number(canonicalPoint.slot),
        blockHash: canonicalPoint.blockHash,
      })
    )
      return result("unknown", "Kupo has not indexed the exact canonical tip");
    const inclusion = await source.resolveTransactionInclusion!({
      txHash: input.transactionHash,
    });
    if (inclusion !== null) {
      const point = admitFraudProofRawL1Point(
        inclusion,
        "signed recovery inclusion",
      );
      const raw = await readRawTransaction({
        txHash: input.transactionHash,
        point: { slot: Number(point.slot), blockHash: point.blockHash },
      });
      const included = CML.Transaction.from_cbor_hex(raw.transactionCbor);
      if (
        !included.is_valid() ||
        included.body().to_cbor_hex() !== signed.body.to_cbor_hex() ||
        included.witness_set().to_canonical_cbor_hex() !==
          signed.transaction.witness_set().to_canonical_cbor_hex()
      )
        throw new Error(
          "Canonical transaction differs from the recorded signed body",
        );
      return finish(
        "included",
        "Exact recorded transaction body is on the canonical chain",
      );
    }
    // Stable expiry and exact canonical absence retire the attempt even when a
    // rollback erased its parent's output creation. Input history is required
    // for rebroadcast or spend-based invalidation, not for proving elapsed TTL.
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(releaseFinalPoint.slot) >= signed.expiresAtSlot
    )
      return finish(
        "expired",
        "Recorded TTL passed at the canonical release-final boundary and the exact transaction is absent",
      );
    let status: SignedTransactionRecoveryObservation["status"] = "rebroadcast";
    let reason =
      "Canonical transaction absent and every recorded input remains unspent";
    for (const outRef of signed.inputOutRefs) {
      const [transactionHash, index] = outRef.split("#");
      const matches = await fetchMatches(`${index}@${transactionHash}`);
      if (
        matches.length !== 1 ||
        matches[0]!.txHash !== transactionHash ||
        matches[0]!.outputIndex.toString() !== index
      ) {
        status = "unknown";
        reason = "A recorded input lacks exact canonical creation history";
        break;
      }
      const match = matches[0]!;
      const output = await utxoFromMatch(match);
      inputs.push({ outRef, outputCbor: output.outputCbor });
      if (match.spentAt !== null) {
        const spending = await readRawTransaction({
          txHash: match.spentAt.txHash,
          point: match.spentAt,
        });
        if (
          !transactionConsumesOutRef({
            transactionCbor: spending.transactionCbor,
            transactionId: match.spentAt.txHash,
            outRef,
          })
        )
          throw new Error(
            "Kupo input spend lacks its exact canonical consuming transaction",
          );
        const stableSpend =
          BigInt(match.spentAt.slot) <= BigInt(releaseFinalPoint.slot);
        // An exact stable spend makes this signed body impossible regardless of
        // input role. This retires only the attempt: funding is re-observed and
        // reserved separately after every recorded signed attempt is resolved.
        // Keep scanning so missing canonical history still prevents retirement.
        if (stableSpend) {
          status = "invalidated";
          reason =
            "A recorded input is stably spent by another canonical transaction";
        } else if (status !== "invalidated") {
          status = "pending";
          reason = "A recorded input spend is not yet release-final";
        }
      }
    }
    if (status !== "rebroadcast") return finish(status, reason);
    // Missing TTL prevents expiry-based replacement, but does not prevent
    // observing the mempool or replaying the exact still-valid signed body.
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(canonicalPoint.slot) >= signed.expiresAtSlot
    )
      return finish(
        "pending",
        "Recorded TTL passed at the tip; release-final expiry proof is not yet available",
      );
    if (
      signed.validFromSlot !== undefined &&
      BigInt(canonicalPoint.slot) < signed.validFromSlot
    )
      return finish(
        "pending",
        "Recorded lower validity bound has not reached the canonical tip",
      );
    const mempool = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      const acquired = record(
        await mempool.request("acquireMempool", {}),
        "signed recovery mempool snapshot",
      );
      if (
        acquired.acquired !== "mempool" ||
        naturalNumber(acquired.slot, "mempool snapshot slot") <
          Number(canonicalPoint.slot)
      )
        return finish(
          "unknown",
          "Mempool snapshot predates the observed canonical tip",
        );
      const present = await mempool.request("hasTransaction", {
        id: input.transactionHash,
      });
      if (typeof present !== "boolean")
        throw new Error("Invalid mempool transaction verdict");
      if (present)
        return finish(
          "pending",
          "Recorded transaction remains in the node mempool",
        );
    } finally {
      await mempool.close();
    }
    return finish(status, reason);
  });
  signedTransactionRebroadcasters.set(source, async (input, authorize) => {
    const session = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      // All transport setup awaits precede the live authorization checkpoint.
      await authorize(input);
      const submitted = record(
        await session.request("submitTransaction", {
          transaction: { cbor: input.signedTransactionCborHex },
        }),
        "recorded transaction submission",
      );
      const transaction = record(
        submitted.transaction,
        "recorded submission transaction",
      );
      const hash = digest(transaction.id, "recorded submission hash");
      if (hash !== input.transactionHash)
        throw new Error(
          "Recorded transaction rebroadcast returned a different hash",
        );
      return hash;
    } finally {
      await session.close();
    }
  });
  admittedHttpOgmiosSources.add(source);
  admittedHttpOgmiosSourceDetails.set(
    source,
    Object.freeze({
      sourceId,
      kupoHttpUrl,
      ogmiosUrl: ogmiosHttpUrl,
      deploymentIdentityDigest: config.releaseFinality.deploymentIdentityDigest,
      blueprintHash: config.releaseFinality.blueprintHash,
      finalityPolicyDigest: config.releaseFinality.policyDigest,
      observationDepth,
      confirmationDepth: config.releaseFinality.policy.confirmationDepth,
      automaticRecoveryMaxDepth:
        config.releaseFinality.policy.automaticRecoveryMaxDepth,
    }),
  );
  return source;
};
