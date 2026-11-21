/**
 * What the chain shows of a family's correction, read from Kupo and the
 * confirmed transaction bytes alone: the watcher keeps no record of its
 * workflow for the journey to read.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { CML, toUnit } from "@lucid-evolution/lucid";

import { readIndexedStateQueueHeader } from "./honest-successor.observables.js";
import { httpFailureDetail } from "./http-failure.js";
import { readKupoUnitConsumptions } from "./kupo-consumption.js";
import type { JourneyChainTransactions } from "./stack-transactions.js";

/**
 * A family's correction as the chain holds it: the retained proof token, the
 * removal that spent the fraudulent header's output and referenced it, and
 * the slash that ended the operator's bond and paid the prover.
 */
export type JourneyChainCorrection = Readonly<{
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
  proofToken: Readonly<{
    unit: string;
    outRef: string;
    createdByTxHash: string;
  }>;
  correction: Readonly<{
    removalTxHash: string;
    removedStateQueueOutRef: string;
  }>;
  economics: Readonly<{
    operatorBondInputOutRef: string;
    operatorBondInputLovelace: string;
    slashTxHash: string;
    slashedLovelace: string;
    proverRewardOutputOutRef: string;
    proverRewardLovelace: string;
  }>;
  /** The removal's block and its depth when the journey read it. */
  observedAt: Readonly<{
    slot: number;
    blockHash: string;
    confirmationDepth: number;
  }>;
}>;

/** What a journey reads of a family's progress and correction on chain. */
export type JourneyCorrectionChain = Readonly<{
  /**
   * The transactions of ours that landed since the fault's header was
   * committed: every transaction the watcher signs pays its fee from the
   * prover's wallet and returns change to it, so each one leaves an output
   * at the prover's address. A top-up paid to that address counts as well.
   */
  landed(): Promise<ReadonlySet<string>>;
  /** The correction, once the chain holds its proof, removal and slash. */
  correction(): Promise<JourneyChainCorrection | undefined>;
}>;

type ReadTransaction = Pick<
  JourneyChainTransactions,
  "transaction" | "observedBlockNo"
>;

const TX_HASH = /^[0-9a-f]{64}$/u;

const outputsOf = (transaction: CML.Transaction) => {
  const txHash = CML.hash_transaction(transaction.body()).to_hex();
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) => {
    const output = outputs.get(index);
    return {
      outRef: `${txHash}#${index.toString()}`,
      address: output.address().to_bech32(),
      coin: output.amount().coin(),
      carries: (unit: string) => {
        const multiAsset = output.amount().multi_asset();
        const policy = multiAsset.get_assets(
          CML.ScriptHash.from_hex(unit.slice(0, 56)),
        );
        return (
          (policy?.get(
            CML.AssetName.from_raw_bytes(Buffer.from(unit.slice(56), "hex")),
          ) ?? 0n) > 0n
        );
      },
    };
  });
};

/** Whether `transaction` reads `outRef` as a reference input. */
const references = (transaction: CML.Transaction, outRef: string) => {
  const inputs = transaction.body().reference_inputs();
  return Array.from({ length: inputs?.len() ?? 0 }, (_, index) =>
    inputs!.get(index),
  ).some(
    (input) =>
      `${input.transaction_id().to_hex()}#${input.index().toString()}` ===
      outRef,
  );
};

/** The tx hashes of every output Kupo indexed at `address` from `slot` on. */
const readAddressCreations = async (
  kupoUrl: string,
  address: string,
  slot: number,
  fetchImpl: typeof fetch,
): Promise<ReadonlySet<string>> => {
  const path = `/matches/${address}?created_after=${Math.max(slot - 1, 0).toString()}`;
  const response = await fetchImpl(`${kupoUrl.replace(/\/+$/u, "")}${path}`, {
    signal: AbortSignal.timeout(30_000),
  });
  if (!response.ok)
    throw new Error(
      `Kupo ${path} failed with ${response.status.toString()}${await httpFailureDetail(response)}`,
    );
  const matches: unknown = await response.json();
  if (
    !Array.isArray(matches) ||
    !matches.every(
      (match: unknown) =>
        typeof match === "object" &&
        match !== null &&
        TX_HASH.test(
          String((match as { transaction_id?: unknown }).transaction_id),
        ),
    )
  )
    throw new Error(`Kupo ${path} answered malformed matches`);
  return new Set(
    (matches as { transaction_id: string }[]).map(
      ({ transaction_id }) => transaction_id,
    ),
  );
};

/**
 * The chain reads of one family's correction. The removal is the one spend
 * of the header's outputs that references the retained proof and leaves no
 * output carrying the header (a merge also ends the header, but references
 * no proof); the
 * slash is the one spend of the header operator's directory node since the fault
 * that continues neither its active nor its retired token; its reward is its
 * one output to the prover. A second removal or slash fails the read.
 */
export const journeyCorrectionChain = ({
  kupoUrl,
  contracts,
  provider,
  transactions,
  category,
  headerHash,
  proverAddress,
  fetchImpl = fetch,
}: {
  kupoUrl: string;
  contracts: Readonly<{
    stateQueue: Readonly<{ policyId: string }>;
    fraudProof: Readonly<{ policyId: string; spendingScriptAddress: string }>;
    activeOperators: Readonly<{ policyId: string }>;
    retiredOperators: Readonly<{ policyId: string }>;
  }>;
  provider: Readonly<{
    getUtxosWithUnit(
      address: string,
      unit: string,
    ): Promise<readonly { txHash: string; outputIndex: number }[]>;
  }>;
  transactions: ReadTransaction;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
  /** The prover's wallet, which pays our fees and receives the reward. */
  proverAddress: string;
  fetchImpl?: typeof fetch;
}): JourneyCorrectionChain => {
  const headerUnit = toUnit(
    contracts.stateQueue.policyId,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const proofUnit = toUnit(
    contracts.fraudProof.policyId,
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + headerHash,
  );
  // The fraudulent header's operator, whose bond the slash ends.
  const bondUnitsOf = (operatorVkey: string) => [
    toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorVkey,
    ),
    toUnit(
      contracts.retiredOperators.policyId,
      SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorVkey,
    ),
  ];
  const decoded = new Map<string, Promise<CML.Transaction>>();
  const read = (txHash: string) => {
    let found = decoded.get(txHash);
    if (found === undefined) {
      found = transactions.transaction(txHash).then(({ cbor }) => {
        const transaction = CML.Transaction.from_cbor_hex(cbor);
        if (CML.hash_transaction(transaction.body()).to_hex() !== txHash)
          throw new Error(
            `The bytes read for ${txHash} are another transaction`,
          );
        return transaction;
      });
      decoded.set(txHash, found);
      found.catch(() => decoded.delete(txHash));
    }
    return found;
  };
  const header = async () => {
    const outputs = await readIndexedStateQueueHeader(
      { kupoUrl, policyId: contracts.stateQueue.policyId, headerHash },
      fetchImpl,
    );
    const first = outputs[0];
    if (first === undefined)
      throw new Error(`Kupo indexes no output of header ${headerHash}`);
    return {
      faultSlot: first.createdAtSlot,
      operatorVkey: first.header.operatorVkey,
      outputs,
    };
  };
  return Object.freeze({
    landed: async () =>
      readAddressCreations(
        kupoUrl,
        proverAddress,
        (await header()).faultSlot,
        fetchImpl,
      ),
    correction: async () => {
      const proofs = await provider.getUtxosWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      );
      if (proofs.length === 0) return undefined;
      if (proofs.length > 1)
        throw new Error(`Proof token ${proofUnit} is retained more than once`);
      const proof = proofs[0]!;
      const proofOutRef = `${proof.txHash}#${proof.outputIndex.toString()}`;
      const { faultSlot, operatorVkey, outputs } = await header();
      const bondUnits = bondUnitsOf(operatorVkey);
      const removals = new Map<string, string>();
      for (const output of outputs) {
        if (output.spentAtSlot === undefined) continue;
        if (output.spentByTxHash === undefined)
          throw new Error(
            `Kupo names no spender of header output ${output.txHash}#${output.outputIndex.toString()}`,
          );
        const spender = await read(output.spentByTxHash);
        if (
          outputsOf(spender).some(({ carries }) => carries(headerUnit)) ||
          !references(spender, proofOutRef)
        )
          continue;
        removals.set(
          output.spentByTxHash,
          `${output.txHash}#${output.outputIndex.toString()}`,
        );
      }
      if (removals.size === 0) return undefined;
      if (removals.size > 1)
        throw new Error(`Header ${headerHash} was removed more than once`);
      const [[removalTxHash, removedStateQueueOutRef]] = [...removals];
      const slashes = new Map<string, string>();
      for (const unit of bondUnits)
        for (const spend of await readKupoUnitConsumptions(
          kupoUrl,
          unit,
          fetchImpl,
        )) {
          if (spend.spentAtSlot < faultSlot) continue;
          const spender = await read(spend.spentByTxHash);
          if (
            outputsOf(spender).some(({ carries }) =>
              bondUnits.some((bond) => carries(bond)),
            )
          )
            continue;
          slashes.set(
            spend.spentByTxHash,
            `${spend.txHash}#${spend.outputIndex.toString()}`,
          );
        }
      if (slashes.size === 0) return undefined;
      if (slashes.size > 1)
        throw new Error(
          `The bond of operator ${operatorVkey} ended more than once since header ${headerHash}`,
        );
      const [[slashTxHash, operatorBondInputOutRef]] = [...slashes];
      const slash = await read(slashTxHash!);
      const rewards = outputsOf(slash).filter(
        ({ address }) => address === proverAddress,
      );
      if (rewards.length !== 1)
        throw new Error(
          `Slash ${slashTxHash!} pays the prover ${rewards.length.toString()} outputs, not one`,
        );
      const [bondTxHash, bondIndex] = operatorBondInputOutRef!.split("#");
      const bond = outputsOf(await read(bondTxHash!))[Number(bondIndex)];
      if (bond === undefined)
        throw new Error(
          `Bond output ${operatorBondInputOutRef!} does not exist`,
        );
      const removal = await transactions.transaction(removalTxHash!);
      const observedBlockNo = transactions.observedBlockNo();
      return Object.freeze({
        category,
        headerHash,
        proofToken: Object.freeze({
          unit: proofUnit,
          outRef: proofOutRef,
          createdByTxHash: proof.txHash,
        }),
        correction: Object.freeze({
          removalTxHash: removalTxHash!,
          removedStateQueueOutRef: removedStateQueueOutRef!,
        }),
        economics: Object.freeze({
          operatorBondInputOutRef: operatorBondInputOutRef!,
          operatorBondInputLovelace: bond.coin.toString(),
          slashTxHash: slashTxHash!,
          slashedLovelace: slash.body().fee().toString(),
          proverRewardOutputOutRef: rewards[0]!.outRef,
          proverRewardLovelace: rewards[0]!.coin.toString(),
        }),
        observedAt: Object.freeze({
          slot: Number(removal.point.slot),
          blockHash: removal.point.blockHash,
          confirmationDepth:
            observedBlockNo === undefined
              ? 0
              : Number(observedBlockNo - BigInt(removal.point.blockNo) + 1n),
        }),
      });
    },
  });
};
