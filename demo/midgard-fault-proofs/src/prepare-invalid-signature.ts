/**
 * **Checked to be unaffected by #604.** This module emits evidence — the witness
 * list, the compact witness set and the inclusion argument — and constructs no
 * datum or redeemer, so the #575 rebind left its output shape alone. Its
 * consumers changed: `submit-invalid-signature-step-02` now opens §2.5 field 7
 * through the §8.8 door and takes the compact witness set this module already
 * emits.
 *
 * `invalid-signature` evidence builder (Goal task `Q15`).
 *
 * Harvested from PR #474 (`fp/invalid-signature`) and reconciled against the
 * current native-codec boundary. It locates a committed transaction carrying an
 * address witness whose Ed25519 signature does not verify against that
 * transaction's native id, and emits the two submit-step arguments the on-chain
 * chain consumes.
 *
 * The two openings the chain walks are, in order:
 *
 * 1. the witness-set compact, whose `blake2b_256` re-encoding must equal the
 *    committed `witness_set_hash` (step 01). #575 deleted the standalone
 *    `verify_native_tx_witness_set` helper that used to make this check; it now
 *    lives inside the §8.8 field-access door, where `authenticated_field_view`
 *    (`onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`) refuses to read
 *    any of fields 6–8 unless
 *    `blake2b_256(encode_native_tx_witness_set_compact(witness_set))` equals the
 *    `witness_set_hash` the compact transaction carries; and
 * 2. the address-witness list, whose `bounded_collection_v1.from_items(7, ...)`
 *    commitment must equal the `addr_tx_wits_hash` step 01 forwarded (step 02).
 *
 * The `--midgard-node-url` and `--transactions` routes below are
 * operator-diagnostic rehearsal routes: they read block material from the node's
 * REST surface or an operator-private file and can never mint a security-grade
 * claim. A Q03 evidence-gated entry point is not landed here yet; the two
 * `submit-invalid-signature-step-0N` builders are, and each re-derives its
 * commitments from the on-chain header and step datum rather than trusting
 * these artifacts.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { deriveMidgardNativeTxWitnessSetCompactV1 } from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  decodeAddressWitnessPreimage,
  findInvalidAddressWitnessIndex,
  invalidSignatureAddressWitnessesCommitmentV1,
  invalidSignatureWitnessSetCommitmentV1,
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
  /** step-01 material: the bad tx and its membership proof. */
  readonly txInclusion: PreparedTxInclusionJson;
  /** Witness-set hash the block committed to, opened by step 01. */
  readonly badTxWitnessSetHash: string;
  /** step-01 material: the preimage of that witness-set hash. */
  readonly badTxWitnessSetCompact: NativeTxWitnessSetCompact;
  /** Canonical address-witness commitment step 01 forwards to step 02. */
  readonly badAddrTxWitsHash: string;
  /** step-02 material: the complete positional address-witness list. */
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddrTxWitIndex: number;
  readonly badAddrTxWitVerificationKey: string;
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
    readonly witnessSetCompactPath: string;
    readonly addrTxWitsPreimagePath: string;
    readonly planPath: string;
  };
};

type InvalidSignatureCandidate = {
  readonly material: DecodedTransactionMaterial;
  readonly witnessSetCompact: NativeTxWitnessSetCompact;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddrTxWitIndex: number;
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
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(
    material.nativeTx.witnessSet,
  );
  const witnessSetCompact: NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
  const addrTxWitsPreimage = decodeAddressWitnessPreimage(
    material.nativeTx.witnessSet.addrTxWitsPreimageCbor,
  );
  const badAddrTxWitIndex = findInvalidAddressWitnessIndex({
    txId: material.nodeTxId,
    addrTxWits: addrTxWitsPreimage,
  });
  if (badAddrTxWitIndex === null) {
    return undefined;
  }
  return {
    material,
    witnessSetCompact,
    addrTxWitsPreimage,
    badAddrTxWitIndex,
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
    witnessSetCompactPath: join(
      outputDir,
      "invalid-signature-witness-set-compact.json",
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
      paths.witnessSetCompactPath,
      stringifyJson({
        addrTxWitsHash: output.tx.badTxWitnessSetCompact.addr_tx_wits_hash,
        scriptTxWitsHash: output.tx.badTxWitnessSetCompact.script_tx_wits_hash,
        redeemerTxWitsHash:
          output.tx.badTxWitnessSetCompact.redeemer_tx_wits_hash,
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
        badTxWitnessSetHash: output.tx.badTxWitnessSetHash,
        badAddrTxWitsHash: output.tx.badAddrTxWitsHash,
        badAddrTxWitIndex: output.tx.badAddrTxWitIndex,
        badAddrTxWitVerificationKey: output.tx.badAddrTxWitVerificationKey,
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

  const badWitness = selected.addrTxWitsPreimage[selected.badAddrTxWitIndex]!;
  const badTxWitnessSetHash =
    selected.material.nativeTxCompact.witness_set_hash;
  // The preimages a prover supplies must reproduce exactly what the block
  // committed to, otherwise neither step could conclude on-chain.
  const recomputedWitnessSetHash = invalidSignatureWitnessSetCommitmentV1(
    selected.witnessSetCompact,
  );
  if (recomputedWitnessSetHash !== badTxWitnessSetHash) {
    throw new Error(
      `Derived witness set compact hashes to ${recomputedWitnessSetHash}, which does not match the witness set hash ${badTxWitnessSetHash} committed by transaction ${selected.material.nodeTxId}.`,
    );
  }
  const recomputedAddrTxWitsHash = invalidSignatureAddressWitnessesCommitmentV1(
    selected.addrTxWitsPreimage,
  );
  if (
    recomputedAddrTxWitsHash !== selected.witnessSetCompact.addr_tx_wits_hash
  ) {
    throw new Error(
      `Decoded address witnesses commit to ${recomputedAddrTxWitsHash}, which does not match the committed addr_tx_wits_hash ${selected.witnessSetCompact.addr_tx_wits_hash}.`,
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
      domain: ROOT_DOMAINS.transactionsV1,
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
      badTxWitnessSetHash,
      badTxWitnessSetCompact: selected.witnessSetCompact,
      badAddrTxWitsHash: selected.witnessSetCompact.addr_tx_wits_hash,
      addrTxWitsPreimage: selected.addrTxWitsPreimage,
      badAddrTxWitIndex: selected.badAddrTxWitIndex,
      badAddrTxWitVerificationKey: badWitness.verification_key,
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

/**
 * Operator-diagnostic rehearsal route: block material is read from the node's
 * REST surface, an `operator_only_diagnostic_endpoint`. Never security grade.
 */
export const prepareInvalidSignatureFromNode = async (
  config: PrepareInvalidSignatureCliConfig,
): Promise<PreparedInvalidSignatureOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    ...(config.fetchImpl === undefined ? {} : { fetchImpl: config.fetchImpl }),
  });
  return await prepareInvalidSignatureFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    ...(config.txId === undefined ? {} : { txId: config.txId }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/** Operator-diagnostic rehearsal route over an `operator_private_file`. */
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
    ...(config.txId === undefined ? {} : { txId: config.txId }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};
