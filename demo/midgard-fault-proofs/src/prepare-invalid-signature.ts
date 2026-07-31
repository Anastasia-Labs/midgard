import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { deriveNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  computeAddressWitnessesHash,
  computeWitnessSetHash,
  decodeAddressWitnessPreimage,
  findInvalidAddressWitnessIndex,
  type MidgardAddressWitness,
  type NativeTxCompact as NativeTxCompactData,
  type NativeTxWitnessSetCompact,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  type DecodedTransactionMaterial,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  nativeTrieItem,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
} from "./prepare-double-spend.js";

export type PrepareInvalidSignatureCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareInvalidSignatureFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly txId?: string;
  readonly outputDir?: string;
};

export type PreparedInvalidSignatureTx = {
  readonly nodeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  /** Witness-set hash the block committed to, carried by the step-01 datum. */
  readonly badTxWitsHash: string;
  readonly witnessSetPreimage: NativeTxWitnessSetCompact;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddressWitnessIndex: number;
  readonly badAddressWitnessVerificationKey: string;
};

export type PreparedInvalidSignatureOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root opened by the transaction membership proof. */
  readonly transactionsPhasRoot: string;
  /** Counted, domain-separated transactions root committed by the block header. */
  readonly committedTransactionsRoot: string;
  readonly expectedTransactionsRoot: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly tx: PreparedInvalidSignatureTx;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly witnessSetPreimagePath: string;
    readonly addrTxWitsPreimagePath: string;
    readonly planPath: string;
  };
};

type InvalidSignatureCandidate = {
  readonly material: DecodedTransactionMaterial;
  readonly witnessSetPreimage: NativeTxWitnessSetCompact;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddressWitnessIndex: number;
};

/**
 * Reduce one decoded transaction to an invalid-signature candidate, or
 * `undefined` when every address witness verifies.
 *
 * The signed message is the native transaction id, which is the blake2b-256 of
 * the compact body — the same value the step-01 validator carries forward.
 */
const toCandidate = (
  material: DecodedTransactionMaterial,
): InvalidSignatureCandidate | undefined => {
  const compact = deriveNativeTxWitnessSetCompact(material.nativeTx.witnessSet);
  const witnessSetPreimage: NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
  const addrTxWitsPreimage = decodeAddressWitnessPreimage(
    material.nativeTx.witnessSet.addrTxWitsPreimageCbor,
  );
  const badAddressWitnessIndex = findInvalidAddressWitnessIndex({
    txId: material.nodeTxId,
    addrTxWits: addrTxWitsPreimage,
  });
  if (badAddressWitnessIndex === null) {
    return undefined;
  }
  return {
    material,
    witnessSetPreimage,
    addrTxWitsPreimage,
    badAddressWitnessIndex,
  };
};

const writePreparedFiles = async ({
  output,
  outputDir,
}: {
  readonly output: PreparedInvalidSignatureOutput;
  readonly outputDir: string;
}): Promise<PreparedInvalidSignatureOutput["files"]> => {
  await mkdir(outputDir, { recursive: true });
  const paths = {
    txInclusionPath: join(outputDir, "invalid-signature-tx-inclusion.json"),
    witnessSetPreimagePath: join(
      outputDir,
      "invalid-signature-witness-set-preimage.json",
    ),
    addrTxWitsPreimagePath: join(
      outputDir,
      "invalid-signature-addr-tx-wits-preimage.json",
    ),
    planPath: join(outputDir, "invalid-signature-plan.json"),
  };
  await Promise.all([
    writeFile(paths.txInclusionPath, stringifyJson(output.tx.txInclusion)),
    writeFile(
      paths.witnessSetPreimagePath,
      stringifyJson({
        addrTxWitsHash: output.tx.witnessSetPreimage.addr_tx_wits_hash,
        scriptTxWitsHash: output.tx.witnessSetPreimage.script_tx_wits_hash,
        redeemerTxWitsHash: output.tx.witnessSetPreimage.redeemer_tx_wits_hash,
      }),
    ),
    writeFile(
      paths.addrTxWitsPreimagePath,
      stringifyJson(
        output.tx.addrTxWitsPreimage.map((witness) => ({
          verificationKey: witness.verification_key,
          signature: witness.signature,
        })),
      ),
    ),
    writeFile(
      paths.planPath,
      stringifyJson({
        headerHash: output.headerHash,
        txNodeTxId: output.tx.nodeTxId,
        badTxWitsHash: output.tx.badTxWitsHash,
        badAddressWitnessIndex: output.tx.badAddressWitnessIndex,
        badAddressWitnessVerificationKey:
          output.tx.badAddressWitnessVerificationKey,
        addrTxWitsCount: output.tx.addrTxWitsPreimage.length,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
        expectedTransactionsRoot: output.expectedTransactionsRoot,
      }),
    ),
  ]);
  return paths;
};

export const prepareInvalidSignatureFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  txId,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot: string;
  readonly txId?: string;
  readonly outputDir?: string;
}): Promise<PreparedInvalidSignatureOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot = parseHex(
    expectedTransactionsRoot,
    "--expected-transactions-root",
    32,
  );
  const normalizedTxId =
    txId === undefined ? undefined : parseHex(txId, "--tx-id", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );

  const selected =
    normalizedTxId === undefined
      ? decoded.reduce<InvalidSignatureCandidate | undefined>(
          (found, material) => found ?? toCandidate(material),
          undefined,
        )
      : (() => {
          const material = decoded.find((tx) => tx.nodeTxId === normalizedTxId);
          if (material === undefined) {
            throw new Error(
              `Requested --tx-id ${normalizedTxId} was not found in the block.`,
            );
          }
          const candidate = toCandidate(material);
          if (candidate === undefined) {
            throw new Error(
              `Requested --tx-id ${normalizedTxId} has a valid signature for every address witness, so it does not violate the signature ledger rule.`,
            );
          }
          return candidate;
        })();
  if (selected === undefined) {
    throw new Error(
      "No transaction with an invalid address-witness signature found in the selected block.",
    );
  }

  const badWitness =
    selected.addrTxWitsPreimage[selected.badAddressWitnessIndex]!;
  const badTxWitsHash = selected.material.nativeTxCompact.witness_set_hash;
  // The preimages a prover supplies must reproduce exactly what the block
  // committed to, otherwise step 02 could never conclude on-chain.
  const recomputedWitnessSetHash = computeWitnessSetHash(
    selected.witnessSetPreimage,
  );
  if (recomputedWitnessSetHash !== badTxWitsHash) {
    throw new Error(
      `Derived witness set preimage hashes to ${recomputedWitnessSetHash}, which does not match the witness set hash ${badTxWitsHash} committed by transaction ${selected.material.nodeTxId}.`,
    );
  }
  const recomputedAddrTxWitsHash = computeAddressWitnessesHash(
    selected.addrTxWitsPreimage,
  );
  if (
    recomputedAddrTxWitsHash !== selected.witnessSetPreimage.addr_tx_wits_hash
  ) {
    throw new Error(
      `Decoded address witnesses hash to ${recomputedAddrTxWitsHash}, which does not match the committed addr_tx_wits_hash ${selected.witnessSetPreimage.addr_tx_wits_hash}.`,
    );
  }

  const nativeTrie = await buildTrieView(decoded.map(nativeTrieItem));
  const proofCbor = requireProof(
    nativeTrie,
    nativeTrieItem(selected.material).key,
    "invalid-signature tx",
  );
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactions,
      phasRoot: nativeTrie.root,
      count: BigInt(decoded.length),
    }),
  );
  const expectedCheck = {
    value: normalizedExpectedRoot,
    matches: normalizedExpectedRoot === committedTransactionsRoot,
  };
  if (!expectedCheck.matches) {
    throw new Error(
      `Reconstructed raw transactions root ${nativeTrie.root} commits to counted root ${committedTransactionsRoot}, which does not match --expected-transactions-root ${expectedCheck.value}. The prepared proof would not verify against this block.`,
    );
  }
  const baseOutput: PreparedInvalidSignatureOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsPhasRoot: nativeTrie.root,
    committedTransactionsRoot,
    expectedTransactionsRoot: expectedCheck,
    tx: {
      nodeTxId: selected.material.nodeTxId,
      nativeTx: selected.material.nativeTxCompact,
      nativeTxCompactCbor: selected.material.nativeCompactCbor,
      txInclusion: {
        nativeTxId: selected.material.nodeTxId,
        nativeTx: selected.material.nativeTxCompact,
        nativeTxCompactCbor: selected.material.nativeCompactCbor,
        transactionsPhasRoot: nativeTrie.root,
        txMembershipProofCbor: proofCbor,
      },
      badTxWitsHash,
      witnessSetPreimage: selected.witnessSetPreimage,
      addrTxWitsPreimage: selected.addrTxWitsPreimage,
      badAddressWitnessIndex: selected.badAddressWitnessIndex,
      badAddressWitnessVerificationKey: badWitness.verification_key,
    },
  };
  if (outputDir === undefined) {
    return baseOutput;
  }
  const files = await writePreparedFiles({
    output: baseOutput,
    outputDir,
  });
  return { ...baseOutput, files };
};

export const prepareInvalidSignatureFromNode = async (
  config: PrepareInvalidSignatureCliConfig,
): Promise<PreparedInvalidSignatureOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    fetchImpl: config.fetchImpl,
  });
  return await prepareInvalidSignatureFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
  });
};

export const prepareInvalidSignatureFromFile = async (
  config: PrepareInvalidSignatureFromFileConfig,
): Promise<PreparedInvalidSignatureOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareInvalidSignatureFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
  });
};
