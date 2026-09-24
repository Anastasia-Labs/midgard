import { createHash } from "node:crypto";

import { verifyEventHistoryActivation } from "./l1-event-history-activation.js";
import { projectEventHistoryLedgerBlock } from "./l1-event-history-ledger-projection.js";
import { stageHistoryProvenance } from "./l1-event-history-provenance.js";
import {
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
} from "./l1-event-history-source.js";

const fail = (message: string): never => {
  throw new Error(`Invalid history initialization replay: ${message}`);
};

/** The source owner must supply a fresh authenticated raw parent capture and
 * the whole valid block admitted by its continuous bound ChainSync session.
 * Archived observations or manifest transaction locators alone do not satisfy
 * that precondition. The result is staged only: the owner must fence branch
 * changes and persist origin evidence, both lists and incarnations together.
 *
 * Both list recipes and the hub share one consumed nonce. Observing its valid
 * ordinary consumption proves availability immediately before activation; the
 * nonce may have been created earlier in this same block. Do not require it in
 * the parent capture or fabricate an initialized capture to skip the prefix.
 */
export const replayEventHistoryInitialization = async (
  input: Parameters<typeof projectEventHistoryLedgerBlock>[0] & {
    readonly captureBindingDigest: string;
  },
) => {
  const { ledger, binding, histories, block } = input;
  if (input.captureBindingDigest !== binding.digest)
    fail("parent capture belongs to another source binding");
  const addresses = historyInitializationAddresses(binding);
  if (
    ledger.addresses.length !== addresses.length ||
    new Set(ledger.addresses).size !== addresses.length ||
    addresses.some((address) => !ledger.addresses.includes(address)) ||
    ledger.outputs.some((output) => !addresses.includes(output.address))
  )
    fail("parent capture does not contain the exact complete history scope");
  const hubPolicy = binding.hubUnit.slice(0, 56);
  const policies = [
    hubPolicy,
    binding.deployments.deposit.policyId,
    binding.deployments.withdrawal.policyId,
  ];
  if (
    ledger.outputs.some((output) =>
      Object.entries(output.assets).some(
        ([unit, quantity]) =>
          quantity !== 0n && policies.includes(unit.slice(0, 56)),
      ),
    )
  )
    fail("parent already contains authenticated deployment state");
  const nonce = histories.deposit.recipe.initializationNonce;

  // Decode every transaction before returning any material. This includes
  // retention/dust before activation and admissions/continuations after it.
  const references = new Map<
    string,
    {
      transactionHash: string;
      output: NonNullable<ReturnType<typeof input.resolveReference>>;
    }
  >();
  const projected = await projectEventHistoryLedgerBlock({
    ...input,
    resolveReference: (transactionHash, ref) => {
      const output = input.resolveReference(transactionHash, ref);
      if (output !== undefined)
        references.set(`${transactionHash}:${ref.txHash}#${ref.outputIndex}`, {
          transactionHash,
          output,
        });
      return output;
    },
  });
  const { activationIndex, activationTransactionHash } =
    verifyEventHistoryActivation({
      block,
      binding,
      histories,
      transitions: projected.transitions,
    });
  const changes = stageHistoryProvenance({
    bindingDigest: binding.digest,
    block,
    transitions: projected.transitions,
    incarnations: [],
  });
  // Retain the exact replay inputs with their locator. This receipt is local
  // recovery evidence only; its hash cannot authenticate an archived prestate.
  const originReceipt = eventHistoryCanonicalJson({
    domain: "midgard-node-history-origin-v1",
    bindingDigest: binding.digest,
    manifestId: binding.manifestId,
    genesisAlgorithm: binding.genesisAlgorithm,
    genesisSha256: binding.genesisSha256,
    initializationNonce: nonce,
    parent: {
      ...ledger,
      addresses: [...ledger.addresses].sort(),
      outputs: [...ledger.outputs].sort(
        (a, b) =>
          a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
      ),
    },
    block,
    activationIndex,
    activationTransactionHash,
    initializedSnapshotDigest: projected.capture.snapshotDigest,
    resolvedReferences: [...references.entries()]
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([, value]) => value),
  });
  return Object.freeze({
    ...projected,
    originReceipt,
    originReceiptDigest: createHash("sha256")
      .update(originReceipt)
      .digest("hex"),
    activationIndex,
    activationTransactionHash,
    incarnations: Object.freeze(changes.map(({ after }) => after)),
  });
};

export const historyInitializationAddresses = (
  binding: EventHistorySourceBinding,
): readonly string[] =>
  Object.freeze(
    [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((deployment) => [
        deployment.address,
        deployment.retentionAddress,
      ]),
    ].sort(),
  );
